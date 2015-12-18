module Euterpea.IO.MIDI 
  ( 
    fromMidi            -- :: Midi -> (Music1, Context (Pitch, [NoteAttribute]), UserPatchMap)
  , module Euterpea.IO.MIDI.GeneralMidi
--  , fromGM              -- :: Int -> InstrumentName
--  , toGM                -- :: InstrumentName -> Int
  , defaultOutput         -- :: (OutputDeviceID -> a -> IO b) -> a -> IO b
  , defaultInput          -- :: (InputDeviceID -> a -> IO b) -> a -> IO b
  , playMidi              -- :: OutputDeviceID -> Midi -> IO ()
  , MidiMessage(..)       -- data MidiMessage = ANote { .. } | Std Message
  , Message(..)           -- data Message           (from Codec.Midi)
  , DeviceInfo(..)        -- data DeviceInfo        (from Sound.PortMidi)
  , OutputDeviceID        -- newtype OutputDeviceID
  , InputDeviceID         -- newtype InputDeviceID
  , module Euterpea.IO.MIDI.ToMidi
  , module Euterpea.IO.MIDI.ExportMidiFile
  , module Euterpea.IO.MIDI.Play
  , module Euterpea.IO.MIDI.MEvent
  ) where

import Euterpea.IO.MIDI.FromMidi
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Euterpea.IO.MIDI.ToMidi
import Euterpea.IO.MIDI.ExportMidiFile
import Euterpea.IO.MIDI.Play
import Euterpea.IO.MIDI.MEvent