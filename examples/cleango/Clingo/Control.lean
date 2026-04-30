import Clingo.Types
import Clingo.SolveHandle
import Clingo.AstConvert

namespace Clingo

open Clingo.Generated.ClingoBindings
open Clingo.Generated

namespace Control

def mk (args : @& Array String := #[]) (logger : @& Logger := fun _ _ => return ()) (limit : UInt32 := 0) : IO (Except (Error × String) Control) :=
  controlNew args logger limit

def mk! (args : @& Array String := #[]) (logger : @& Logger := fun _ _ => return ()) (limit : UInt32 := 0) : IO Control := do
  let r ← controlNew args logger limit
  match r with
  | .ok ctrl => return ctrl
  | .error (_, msg) => throw (IO.userError msg)

def load (ctrl : @& Control) (path : @& String) : IO (Except (Error × String) Unit) :=
  controlLoad ctrl path

def add (ctrl : @& Control) (name : @& String) (params : @& Array String) (program : @& String) : IO (Except (Error × String) Unit) :=
  controlAdd ctrl name params program

def ground (ctrl : @& Control) (parts : @& Array Part) (callback : @& GroundCallback) : IO (Except (Error × String) Unit) :=
  controlGround ctrl parts callback

def solve (ctrl : @& Control) (mode : @& SolveMode) (assumptions : @& Array Literal) (callback : @& SolveEventCallback) : IO (Except (Error × String) SolveHandle) :=
  controlSolve ctrl mode assumptions callback

def statistics (ctrl : @& Control) : IO (Except (Error × String) Statistics) :=
  controlStatistics ctrl

def interrupt (ctrl : @& Control) : IO Unit :=
  controlInterrupt ctrl

private def programBuilder (ctrl : @& Control) : IO (Except (Error × String) ProgramBuilder) :=
  controlProgramBuilder ctrl

def withProgramBuilder (ctrl : @& Control) (f : ProgramBuilder → IO A) : IO A := do
  let .ok pb ← ctrl.programBuilder | throw (IO.userError "failed to get program builder")
  let .ok () ← programBuilderBegin pb | throw (IO.userError "failed to begin program builder")
  let res ← f pb
  let .ok () ← programBuilderEnd pb | throw (IO.userError "failed to end program builder")
  return res

instance : Repr Control where
  reprPrec _ _ := s!"(Clingo.Control.mk)"

end Control

namespace ProgramBuilder

def addStatementRaw (pb : @& ProgramBuilder) (stmt : @& ClingoBindings.AstStatement) : IO (Except String Unit) :=
  programBuilderAdd pb stmt

def addStatement (pb : @& ProgramBuilder) (stmt : @& Ast.Statement) : IO Unit := do
  let converted ← AstConvert.convertStatement stmt
  let _ ← pb.addStatementRaw converted

def addStatements (pb : @& ProgramBuilder) (stmts : List Ast.Statement) : IO Unit :=
  for stmt in stmts do
    pb.addStatement stmt

end ProgramBuilder

end Clingo
