> module Euterpea.IO.MIDI.Play (
>     play -- standard playback, allows infinite values
>     ,playDev -- play to a custom device, allows infinite values
>     ,playS -- play with strict timing (finite values only)
>     ,playDevS -- play to a custom device with strict timing (finite values only)
>     ,playC -- custom playback implementation to replace playA, playS, playDev, etc.
>     ,devices -- function that prints available MIDI device information
>     ,musicToMsgs' -- music to MIDI message conversion
>     ,linearCP -- linear channel assignment policy
>     ,dynamicCP -- dynamic channel assignment policy
>     ,predefinedCP -- user-specified channel map (for MUIs)
>     ,defParams
>     ,playM'
>     ,PlayParams(..)
>     ,ChannelMapFun
>     ,ChannelMap
>     ) where

> import Codec.Midi hiding (Tempo)
> import Control.DeepSeq
> import Control.Monad
> import Control.Concurrent
> import Control.Exception
> import Data.List
> import Euterpea.IO.MIDI.MidiIO
> import Euterpea.IO.MIDI.ToMidi
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.Music
> import Sound.PortMidi

--------------------------
 | User-Level Functions |
--------------------------

Playback parameter data type.

> data PlayParams = PlayParams{
>     strict :: Bool, -- strict timing (False for infinite values)
>     chanPolicy :: ChannelMapFun, -- channel assignment policy
>     devID :: Maybe OutputDeviceID, -- output device (Nothing means to use the OS default)
>     closeDelay :: Time, -- offset in seconds to avoid truncated notes
>     perfAlg :: Music1 -> [MEvent]
>     }

Default parameters are the default pmap+context, allowing for infinite playback,
using a linear channel assignment policy for 16 channels with percussion on
channel 9 (which is channel 10 when indexing from 1), using the default MIDI
device as set by the operating system, and using a closing offset of 1.0sec.

> defParams = PlayParams False (linearCP 16 9) Nothing 1.0 perform1

New implementation of play using default parameters:

(Bug fix note 16-June-2019: play and playDev will now avoid having
back-to-back off/on messages with the same time stamp, which can
cause message ordering problems with lazy playback when the messages
finally reach the MIDI device. The solution used below is to subtract
such a tiny duration from each MEvent that it is imperceptable yet is
just enough to force the messages to be ordered correctly under average
conditions. This will not fix any problems associated with large
computation bubbles - but there is no reasonable way to compensate for
arbitrarily large delays)

> play :: (ToMusic1 a, NFData a) => Music a -> IO ()
> play = playC defParams{perfAlg=fixPerf} where
>     fixPerf = map (\e -> e{eDur = max 0 (eDur e - 0.000001)}) . perform

> playS :: (ToMusic1 a, NFData a) => Music a -> IO ()
> playS = playC defParams{strict=True}

> playDev :: (ToMusic1 a, NFData a) => Int -> Music a -> IO ()
> playDev i = playC defParams{devID = Just $ unsafeOutputID i, perfAlg=fixPerf} where
>     fixPerf = map (\e -> e{eDur = max 0 (eDur e - 0.000001)}) . perform

> playDevS :: (ToMusic1 a, NFData a) => Int -> Music a -> IO()
> playDevS i = playC defParams{strict=True, devID = Just $ unsafeOutputID i}

"Custom play" interface:

> playC :: (ToMusic1 a, NFData a) => PlayParams -> Music a -> IO ()
> playC p = if strict p then playStrict p else playInf p

Getting a list of all MIDI input and output devices, showing both
their device IDs and names.

> devices = do
>   (devsIn, devsOut) <- getAllDevices
>   let f (devid, devname) = "  "++show devid ++ "\t" ++ name devname ++ "\n"
>       strIn = concatMap f devsIn
>       strOut = concatMap f devsOut
>   putStrLn "\nInput devices: " >> putStrLn strIn
>   putStrLn "Output devices: " >> putStrLn strOut


------------------------------------
 | Supporting functions for playC |
------------------------------------

Strict playback: timing will be as close to perfect as possible, but the
Music value must be finite. Timing will be correct starting from the first
note, even if there is a long computation offset prior to any sound.

> playStrict :: (ToMusic1 a, NFData a) => PlayParams -> Music a -> IO ()
> playStrict p m = m `deepseq`
>     let x = toMidi (perfAlg p $ toMusic1 m)
>     in  x `deepseq` playM' (devID p) x

> playM' :: Maybe OutputDeviceID -> Midi -> IO ()
> playM' devID midi = handleCtrlC $ do
>     initialize
>     (maybe (defaultOutput playMidi) playMidi devID) midi
>     terminate
>     return () where
>     handleCtrlC :: IO a -> IO a
>     handleCtrlC op = onException op terminate


Infinite playback: arbitrarily long music values can be played, although
with the compromise that timing may be imperfect due to lazy evaluation of
the Music value. Delays may happen if a section of the Music value is time-
consuming to compute. Infinite parallelism is not supported.

> playInf :: ToMusic1 a => PlayParams -> Music a -> IO ()
> playInf p m = handleCtrlC $ do
>     initializeMidi
>     (maybe (defaultOutput playRec) playRec (devID p)) $ musicToMsgs' p m
>     threadDelay $ round (closeDelay p * 1000000)
>     terminateMidi
>     return () where
>     handleCtrlC :: IO a -> IO a
>     handleCtrlC op = do
>         dev <- resolveOutDev (devID p)
>         onException op (stopMidiOut (dev) 16)

Bug fix on Sept 24, 2018: on Mac, the default output device may not be zero.
In rare cases on Mac, there are outputs but the default ID is Nothing, but
in these cases the default always seems to be the first output in the list.

> resolveOutDev Nothing = do
>    outDevM <- getDefaultOutputDeviceID
>    (ins,outs) <- getAllDevices
>    let allOutDevs = map fst outs
>    let outDev = case outDevM of
>                     Nothing ->
>                            if null allOutDevs then error "No MIDI outputs!"
>                            else head allOutDevs
>                     Just x -> unsafeOutputID x
>    return outDev
> resolveOutDev (Just x) = return x

> stopMidiOut :: OutputDeviceID -> Channel -> IO ()
> stopMidiOut dev i = if i<0 then threadDelay 1000000 >> terminateMidi else do
>     deliverMidiEvent dev (0, Std $ ControlChange i 123 0)
>     stopMidiOut dev (i-1)

> playRec dev [] = return ()
> playRec dev (x@(t,m):ms) =
>     if t > 0 then threadDelay (toMicroSec t) >> playRec dev ((0,m):ms) else
>     let mNow = x : takeWhile ((<=0).fst) ms
>         mLater = drop (length mNow - 1) ms
>     in  doMidiOut dev (Just $ mNow) >> playRec dev mLater where
>     doMidiOut dev Nothing = outputMidi dev
>     doMidiOut dev (Just ms) = do
>         outputMidi dev
>         mapM_ (\(t,m) -> deliverMidiEvent dev (0, m)) ms
>     toMicroSec x = round (x * 1000000)


---------------------------------
 | Music to Message conversion |
---------------------------------

Music to message conversion will take place differently depending
on the channel assignment method. Using linearCP will assign the first
n instruments to channels 0 through n-1 (or 1 through n). Using
dynamicCP will fill up n channels and then replace the last-used
instrument's channel with the new instrument.

Some synthesizers only recognize 10 unique channels, others use the
full 16 allowed by general MIDI. Drums are usually on channel 9
(channel 10 when indexing from 1), but not always.  Sometimes drums
can be assigned to a custom channel.

A ChannelMap stores which instrument is assigned to which channel.
This table is built automatically when playing a Music value; the
user does not need to worry about constructing it.

> type ChannelMap = [(InstrumentName, Channel)]

Given an InstrumentName and a ChannelMap, a ChannelMapFun picks a new
channel to assign to the instrument and retruns both that and the
updated ChannelMap. This is done each time a new InstrumentName is
encountered (in other words, it is not in the current ChannelMap).

> type ChannelMapFun = InstrumentName -> ChannelMap -> (Channel, ChannelMap)

The function below first converts to ANote values and then to Std On/Off
pairs. This is needed to avoid timing issues associated with using ANote
and trying to call terminateMIDI, since if there is an ANote at the end
it will sometimes have its NoteOff lost, which can cause errors.

> musicToMsgs' :: (ToMusic1 a) => PlayParams -> Music a -> [(Time, MidiMessage)]
> musicToMsgs' p m =
>     let perf = perfAlg p $ toMusic1 m -- obtain the performance
>         evsA = channelMap (chanPolicy p) [] perf -- time-stamped ANote values
>         evs = stdMerge evsA -- merged On/Off events sorted by absolute time
>         times = map fst evs -- absolute times in seconds
>         newTimes = zipWith subtract (head times : times) times -- relative times
>     in  zip newTimes (map snd evs) where
>     -- stdMerge: converts ANotes into a sorted list of On/Off events
>     stdMerge :: [(Time, MidiMessage)] -> [(Time, MidiMessage)]
>     stdMerge [] = []
>     stdMerge ((t,ANote c k v d):es) =
>         (t, Std $ NoteOn c k v) :
>         stdMerge (insertBy (\(a,b) (x,y) -> compare a x) (t+d, Std $ NoteOff c k v) es)
>     stdMerge (e1:es) = e1 : stdMerge es
>     -- channelMap: performs instrument assignment for a list of Events
>     channelMap :: ChannelMapFun -> ChannelMap -> [MEvent] -> [(Time, MidiMessage)]
>     channelMap cf cMap [] = []
>     channelMap cf cMap (e:es) =
>         let i = eInst e
>             ((chan, cMap'), newI) = case lookup i cMap of Nothing -> (cf i cMap, True)
>                                                           Just x  -> ((x, cMap), False)
>             e' = (fromRational (eTime e),
>                   ANote chan (ePitch e) (eVol e) (fromRational $ eDur e))
>             es' = channelMap cf cMap' es
>             iNum = if i==Percussion then 0 else fromEnum i
>         in  if newI then (fst e', Std $ ProgramChange chan iNum) : e' : es'
>             else e' : es'

The linearCP channel map just fills up channels left to right until it hits
the maximum number and then throws an error. Percussion is handled as a
special case.

> type NumChannels = Int -- maximum number of channels (i.e. 0-15 is 16 channels)
> type PercChan = Int -- percussion channel, using indexing from zero

> linearCP :: NumChannels -> PercChan -> ChannelMapFun
> linearCP cLim pChan i cMap = if i==Percussion then (pChan, (i,pChan):cMap) else
>     let n = length $ filter ((/=Percussion). fst) cMap
>         newChan = if n>=pChan then n+1 else n -- step over the percussion channel
>     in if newChan < cLim then (newChan, (i, newChan) : cMap) else
>        error ("Cannot use more than "++show cLim++" instruments.")

For the dynamicCP channel map, new assignements are added in the left side
of the channel map/list. This means that the item farthest to the right
is the oldest and should be replaced when the table is full. Percussion
is handled separately.

> dynamicCP :: NumChannels -> PercChan -> ChannelMapFun
> dynamicCP cLim pChan i cMap =
>     if i==Percussion then (pChan, (i, pChan):cMap) else
>         let cMapNoP = filter ((/=Percussion). fst) cMap
>             extra = if length cMapNoP == length cMap then [] else [(Percussion, pChan)]
>             newChan = snd $ last cMapNoP
>         in  if length cMapNoP < cLim - 1 then linearCP cLim pChan i cMap
>         else (newChan, (i, newChan) : (take (length cMapNoP - 1) cMapNoP)++extra)


A predefined policy will send instruments to user-defined channels. If new
instruments are found that are not accounted for, an error is thrown.

> predefinedCP :: ChannelMap -> ChannelMapFun
> predefinedCP cMapFixed i _ = case lookup i cMapFixed of
>     Nothing -> error (show i ++ " is not included in the channel map.")
>     Just c -> (c, cMapFixed)

-------------------------------
 | NFData instances for Midi |
-------------------------------

> instance NFData FileType where
>     rnf x = ()

> instance NFData TimeDiv where
>     rnf (TicksPerBeat i) = rnf i
>     rnf (TicksPerSecond i j) = rnf j `seq` rnf i

> instance NFData Midi where
>     rnf (Midi ft td ts) = rnf ft `seq` rnf td `seq` rnf ts

> instance NFData Message where
>     rnf (NoteOff c k v) = rnf c `seq` rnf k `seq` rnf v
>     rnf (NoteOn c k v) = rnf c `seq` rnf k `seq` rnf v
>     rnf (KeyPressure c k v) = rnf c `seq` rnf k `seq` rnf v
>     rnf (ProgramChange c v) = rnf c `seq` rnf v
>     rnf (ChannelPressure c v) = rnf c `seq` rnf v
>     rnf (PitchWheel c v) = rnf c `seq` rnf v
>     rnf (TempoChange t) = rnf t
>     rnf x = () -- no other message types are currently used by Euterpea

> instance NFData MidiMessage where
>     rnf (Std m) = rnf m
>     rnf (ANote c k v d) = rnf c `seq` rnf k `seq` rnf v `seq` rnf d


--------------------------------
 | NFData instances for Music |
--------------------------------

> instance NFData a => NFData (Music a) where
>     rnf (a :+: b) = rnf a `seq` rnf b
>     rnf (a :=: b) = rnf a `seq` rnf b
>     rnf (Prim p) = rnf p
>     rnf (Modify c m) = rnf c `seq` rnf m

> instance NFData a => NFData (Primitive a) where
>     rnf (Note d a) = rnf d `seq` rnf a
>     rnf (Rest d) = rnf d

> instance NFData Control where
>     rnf (Tempo t) = rnf t
>     rnf (Transpose t) = rnf t
>     rnf (Instrument i) = rnf i
>     rnf (Phrase xs) = rnf xs
>     rnf (Custom s) = rnf s
>     rnf (KeySig r m) = rnf r `seq` rnf m

> instance NFData PitchClass where
>     rnf p = ()

> instance NFData Mode where
>     rnf x = ()

> instance NFData PhraseAttribute where
>     rnf (Dyn d) = rnf d
>     rnf (Tmp t) = rnf t
>     rnf (Art a) = rnf a
>     rnf (Orn o) = rnf o

> instance NFData Dynamic where
>     rnf (Accent r) = rnf r
>     rnf (Crescendo r) = rnf r
>     rnf (Diminuendo r) = rnf r
>     rnf (StdLoudness x) = rnf x
>     rnf (Loudness r) = rnf r

> instance NFData StdLoudness where
>     rnf x = ()

> instance NFData Articulation where
>     rnf (Staccato r) = rnf r
>     rnf (Legato r) = rnf r
>     rnf x = ()

> instance NFData Ornament where
>     rnf x = ()

> instance NFData Tempo where
>     rnf (Ritardando r) = rnf r
>     rnf (Accelerando r) = rnf r

> instance NFData InstrumentName where
>     rnf x = ()

> instance NFData NoteAttribute where
>     rnf (Volume v) = rnf v
>     rnf (Fingering f) = rnf f
>     rnf (Dynamics d) = rnf d
>     rnf (Params p) = rnf p
