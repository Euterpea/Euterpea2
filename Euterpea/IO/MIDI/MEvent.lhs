-- ==========================================================================================
-- Conversion to MEvent datatype

> module Euterpea.IO.MIDI.MEvent where
> import Euterpea.Music

> data MEvent = MEvent {  
>     eTime    :: PTime, -- onset time
>     eInst    :: InstrumentName, -- instrument
>     ePitch   :: AbsPitch, -- pitch number
>     eDur     :: DurT, -- note duration
>     eVol     :: Volume, -- volume
>     eParams  :: [Double]} -- optional other parameters 
>     deriving (Show,Eq,Ord)

> type Performance = [MEvent]

> type PTime = Rational
> type DurT = Rational

> merge :: Performance -> Performance -> Performance
> merge []          es2         =  es2
> merge es1         []          =  es1
> merge a@(e1:es1)  b@(e2:es2)  =  
>   if eTime e1 < eTime e2  then  e1  : merge es1 b
>                           else  e2  : merge a es2

> data MContext = MContext {mcTime    :: PTime, 
>                           mcInst    :: InstrumentName, 
>                           mcDur     :: DurT,
>                           mcVol     :: Volume}
>     deriving Show

> perform :: (ToMusic1 a) => Music a -> Performance
> perform = perform1 . toMusic1

> perform1 :: Music1 -> Performance
> perform1 = fst . perform1Dur

> perform1Dur :: Music1 -> (Performance, DurT)
> perform1Dur = musicToMEvents defCon . applyControls where
>     defCon  = MContext {mcTime = 0, mcInst = AcousticGrandPiano, mcDur = metro 120 qn, mcVol=127}
>     -- timing musicToMEventss
>     metro :: Int -> Dur -> DurT
>     metro setting dur  = 60 / (fromIntegral setting * dur)

> applyControls :: Music1 -> Music1
> applyControls (Modify (Tempo r) m) = scaleDurations r $ applyControls m
> applyControls (Modify (Transpose k) m) = shiftPitches1 k $ applyControls m
> applyControls (Modify x m) = Modify x $ applyControls m
> applyControls (m1 :+: m2) = applyControls m1 :+: applyControls m2
> applyControls (m1 :=: m2) = applyControls m1 :=: applyControls m2
> applyControls x = x

> musicToMEvents :: MContext -> Music1 -> (Performance, DurT)
> musicToMEvents c@MContext{mcTime=t, mcDur=dt} (Prim (Note d p)) = ([noteToMEvent c d p], d*dt)
> musicToMEvents c@MContext{mcTime=t, mcDur=dt}  (Prim (Rest d)) = ([], d*dt)
> musicToMEvents c@MContext{mcTime=t, mcDur=dt} (m1 :+: m2) = 
>     let (evs1, d1) = musicToMEvents c m1
>         (evs2, d2) = musicToMEvents c{mcTime = t+d1} m2
>     in  (evs1 ++ evs2, d1+d2)
> musicToMEvents c@MContext{mcTime=t, mcDur=dt} (m1 :=: m2) = 
>     let (evs1, d1) = musicToMEvents c m1
>         (evs2, d2) = musicToMEvents c m2
>     in  (merge evs1 evs2, max d1 d2)
> musicToMEvents c (Modify (Instrument i) m) = musicToMEvents c{mcInst=i} m
> musicToMEvents c (Modify (Phrase pas) m) = phraseToMEvents c pas m
> musicToMEvents c (Modify (KeySig x y) m) = musicToMEvents c m -- KeySig causes no change
> musicToMEvents c (Modify (Custom x) m) = musicToMEvents c m -- Custom cuases no change
> musicToMEvents c m@(Modify x m') = musicToMEvents c $ applyControls m -- Transpose and Tempo addressed by applyControls

> noteToMEvent :: MContext -> Dur -> (Pitch, [NoteAttribute]) -> MEvent
> noteToMEvent c@(MContext ct ci cdur cvol) d (p, nas) = 
>     let e0 = MEvent{eTime=ct, ePitch=absPitch p, eInst=ci, eDur=d*cdur, eVol=cvol, eParams=[]}
>     in  foldr nasFun e0 nas where
>     nasFun :: NoteAttribute -> MEvent -> MEvent
>     nasFun (Volume v) ev = ev {eVol = v}
>     nasFun (Params pms) ev = ev {eParams = pms}
>     nasFun _ ev = ev

> phraseToMEvents :: MContext -> [PhraseAttribute] -> Music1 -> (Performance, DurT)
> phraseToMEvents c [] m = musicToMEvents c m
> phraseToMEvents c@MContext{mcTime=t, mcInst=i, mcDur=dt} (pa:pas) m =
>  let  pfd@(pf,dur)  =  phraseToMEvents c pas m
>       loud x        =  phraseToMEvents c (Dyn (Loudness x) : pas) m
>       stretch x     =  let  t0 = eTime (head pf);  r  = x/dur
>                             upd (e@MEvent {eTime = t, eDur = d}) = 
>                               let  dt  = t-t0
>                                    t'  = (1+dt*r)*dt + t0
>                                    d'  = (1+(2*dt+d)*r)*d
>                               in e {eTime = t', eDur = d'}
>                        in (map upd pf, (1+x)*dur)
>       inflate x     =  let  t0  = eTime (head pf);  
>                             r   = x/dur
>                             upd (e@MEvent {eTime = t, eVol = v}) = 
>                                 e {eVol =  round ( (1+(t-t0)*r) * 
>                                            fromIntegral v)}
>                        in (map upd pf, dur)
>  in case pa of
>    Dyn (Accent x) ->
>        ( map (\e-> e {eVol = round (x * fromIntegral (eVol e))}) pf, dur)
>    Dyn (StdLoudness l) -> 
>        case l of 
>           PPP  -> loud 40;       PP -> loud 50;   P    -> loud 60
>           MP   -> loud 70;       SF -> loud 80;   MF   -> loud 90
>           NF   -> loud 100;      FF -> loud 110;  FFF  -> loud 120
>    Dyn (Loudness x)     ->  phraseToMEvents c{mcVol = round x} pas m
>    Dyn (Crescendo x)    ->  inflate   x ; Dyn (Diminuendo x)  -> inflate (-x)
>    Tmp (Ritardando x)   ->  stretch   x ; Tmp (Accelerando x) -> stretch (-x)
>    Art (Staccato x)     ->  (map (\e-> e {eDur = x * eDur e}) pf, dur)
>    Art (Legato x)       ->  (map (\e-> e {eDur = x * eDur e}) pf, dur)
>    Art (Slurred x)      -> 
>        let  lastStartTime  = foldr (\e t -> max (eTime e) t) 0 pf
>             setDur e       =   if eTime e < lastStartTime
>                                then e {eDur = x * eDur e}
>                                else e
>        in (map setDur pf, dur) 
>    Art _                -> pfd -- not supported
>    Orn _                -> pfd -- not supported
