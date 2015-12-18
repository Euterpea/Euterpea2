> {-# LINE 8 "ToMidi.lhs" #-}

> module Euterpea.IO.MIDI.ToMidi where

> import Euterpea.Music
> import Euterpea.IO.MIDI.MEvent
> import Euterpea.IO.MIDI.GeneralMidi
> import Euterpea.IO.MIDI.MidiIO
> import Euterpea.IO.MIDI.ExportMidiFile
> import Sound.PortMidi
> import Data.List(partition)
> import Data.Char(toLower,toUpper)
> import Codec.Midi

> type ProgNum     = Int

> type UserPatchMap = [(InstrumentName, Channel)]

> makeGMMap :: [InstrumentName] -> UserPatchMap
> makeGMMap ins = mkGMMap 0 ins
>   where mkGMMap _ []        = []
>         mkGMMap n _ | n>=15 = 
>                   error "makeGMMap: too many instruments."
>         mkGMMap n (Percussion : ins)    = 
>                   (Percussion, 9) : mkGMMap n ins
>         mkGMMap n (i : ins) = 
>                   (i, chanList !! n) : mkGMMap (n+1) ins
>         chanList = [0..8] ++ [10..15]  --  channel 9 is for percussion

> upmLookup :: UserPatchMap  -> InstrumentName 
>                            -> (Channel, ProgNum)
> upmLookup upm iName = (chan, toGM iName)
>   where chan = maybe  (error (  "instrument " ++ show iName ++ 
>                                 " not in patch map")  )
>                       id (lookup iName upm)

> toMidi :: [MEvent] -> Midi
> toMidi = toMidiUPM defUpm

> toMidiUPM :: UserPatchMap -> [MEvent] -> Midi
> toMidiUPM upm pf =
>    let split     = splitByInst pf
>        insts     = map fst split
>        rightMap  =  if (allValid upm insts) then upm
>                     else (makeGMMap insts)
>    in Midi  (if length split == 1  then SingleTrack 
>                                    else MultiTrack)
>             (TicksPerBeat division)
>             (map (fromAbsTime . mevsToMessages rightMap) split)

> division = 96 :: Int

> allValid :: UserPatchMap -> [InstrumentName] -> Bool
> allValid upm = and . map (lookupB upm)

> lookupB :: UserPatchMap -> InstrumentName -> Bool
> lookupB upm x = or (map ((== x) . fst) upm)

> splitByInst :: [MEvent] ->  [(InstrumentName, [MEvent])]
> splitByInst [] = []
> splitByInst pf = (i, pf1) : splitByInst pf2
>        where i          = eInst (head pf)
>              (pf1, pf2) = partition (\e -> eInst e == i) pf

> type MidiEvent = (Ticks, Message)

> defST = 500000

> mevsToMessages ::  UserPatchMap
>                   -> (InstrumentName, [MEvent]) 
>                   -> [MidiEvent]
> mevsToMessages upm (inm, pf) =
>   let  (chan,progNum)   = upmLookup upm inm
>        setupInst        = (0, ProgramChange chan progNum)
>        setTempo         = (0, TempoChange defST)
>        loop []      =  []
>        loop (e:es)  =  let (mev1,mev2) = mkMEvents chan e
>                        in mev1 : insertMEvent mev2 (loop es)
>   in setupInst : setTempo : loop pf

  
> mkMEvents :: Channel -> MEvent -> (MidiEvent,MidiEvent)
> mkMEvents  mChan (MEvent {  eTime = t, ePitch = p, 
>                            eDur = d, eVol = v})
>                   = (  (toDelta t, NoteOn  mChan p v'),
>                        (toDelta (t+d), NoteOff mChan p v') )
>            where v' = max 0 (min 127 (fromIntegral v))

> toDelta t = round (t * 2.0 * fromIntegral division)

> insertMEvent :: MidiEvent -> [MidiEvent] -> [MidiEvent]
> insertMEvent mev1  []         = [mev1]
> insertMEvent mev1@(t1,_) mevs@(mev2@(t2,_):mevs') = 
>       if t1 <= t2 then mev1 : mevs
>                   else mev2 : insertMEvent mev1 mevs'

> defUpm :: UserPatchMap
> defUpm = [(AcousticGrandPiano,0),
>           (Marimba,1),
>           (Vibraphone,2),
>           (AcousticBass,3),
>           (Flute,4),
>           (TenorSax,5),
>           (AcousticGuitarSteel,6),
>           (Viola,7),
>           (StringEnsemble1,8),
>           (AcousticGrandPiano,9)]
>            --  the GM name for drums is unimportant, only channel 9


> writeMidi :: ToMusic1 a => FilePath -> Music a -> IO ()
> writeMidi fn m = exportMidiFile fn $ toMidi $ perform m

 play :: ToMusic1 a => Music a -> IO ()
 play = playM . toMidi . perform

 playM :: Midi -> IO ()
 playM midi = do
   initialize
   (defaultOutput playMidi) midi 
   terminate
   return ()

