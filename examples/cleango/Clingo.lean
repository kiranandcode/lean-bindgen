import Lean.Parser
import Lean.Parser.Term
import Lean.Elab.Term
import Clingo.Error
import Clingo.Types
import Clingo.Signature
import Clingo.Symbol
import Clingo.Model
import Clingo.Statistics
import Clingo.SolveHandle
import Clingo.Backend
import Clingo.Control
import Clingo.Ast
import Clingo.AstConvert
import Clingo.Lang

open Lean

namespace Clingo

open Clingo.Generated

def version : Version :=
  let (major, (minor, rev)) := ClingoBindings.version ()
  ⟨major, minor, rev⟩

def error_code : IO Error := ClingoBindings.errorCode

def error_message : IO (Option String) := ClingoBindings.errorMessage

end Clingo
