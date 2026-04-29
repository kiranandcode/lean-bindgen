import Clingo.Types

namespace Clingo

open Clingo.Generated
open Clingo.Generated.ClingoBindings

-- Re-export solve-related types.
abbrev SolveResult := ClingoBindings.SolveResult
abbrev SolveMode := ClingoBindings.SolveMode
abbrev SolveEvent := ClingoBindings.SolveEvent

namespace SolveMode

def Neither : SolveMode := { async := false, yield := false }
def Async : SolveMode := { async := true, yield := false }
def Yield : SolveMode := { async := false, yield := true }
def AsyncYield : SolveMode := { async := true, yield := true }

end SolveMode

namespace SolveHandle

def get (h : @& SolveHandle) : IO (Except (Error × String) SolveResult) := solveHandleGet h

def wait (h : @& SolveHandle) (timeout : Float) : IO Bool := solveHandleWait h timeout

def model (h : @& SolveHandle) : IO (Except (Error × String) (Option Model)) := solveHandleModel h

def resume (h : @& SolveHandle) : IO (Except (Error × String) Unit) := solveHandleResume h

def cancel (h : @& SolveHandle) : IO (Except (Error × String) Unit) := solveHandleCancel h

def close (h : @& SolveHandle) : IO (Except (Error × String) Unit) := solveHandleClose h

end SolveHandle

end Clingo
