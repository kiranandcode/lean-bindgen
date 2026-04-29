import Clingo.Types

namespace Clingo

open Clingo.Generated
open Clingo.Generated.ClingoBindings

abbrev StatisticsType := ClingoBindings.StatisticsType

namespace Statistics

def root (s : @& Statistics) : IO (Except (Error × String) UInt64) := statisticsRoot s

def type (s : @& Statistics) (key : UInt64) : IO (Except (Error × String) StatisticsType) := statisticsType s key

def arraySize (s : @& Statistics) (key : UInt64) : IO (Except (Error × String) USize) := statisticsArraySize s key

def arrayRef (s : @& Statistics) (key : UInt64) (offset : USize) : IO (Except (Error × String) UInt64) := statisticsArrayAt s key offset

def mapSize (s : @& Statistics) (key : UInt64) : IO (Except (Error × String) USize) := statisticsMapSize s key

def mapHasKey? (s : @& Statistics) (key : UInt64) (name : @& String) : IO (Except (Error × String) Bool) := statisticsMapHasSubkey s key name

def mapRef (s : @& Statistics) (key : UInt64) (name : @& String) : IO (Except (Error × String) UInt64) := statisticsMapAt s key name

def valueGet (s : @& Statistics) (key : UInt64) : IO (Except (Error × String) Float) := statisticsValueGet s key

end Statistics

end Clingo
