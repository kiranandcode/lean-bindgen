import Clingo.Types

namespace Clingo

open Clingo.Generated.ClingoBindings

namespace Model

inductive ModelType where | Stable | Brave | Cautious
  deriving Repr, Inhabited

-- FilterFlags wraps the generated ShowType with cleango-compatible names.
abbrev FilterFlags := ShowType

namespace FilterFlags

def selectCSPAssignments : FilterFlags :=
  { shown := false, atoms := false, terms := false, theory := true, all := false, complement := false }

def selectShown : FilterFlags :=
  { shown := true, atoms := false, terms := false, theory := false, all := false, complement := false }

def selectAllAtoms : FilterFlags :=
  { shown := false, atoms := true, terms := false, theory := false, all := false, complement := false }

def selectAllTerms : FilterFlags :=
  { shown := false, atoms := false, terms := true, theory := false, all := false, complement := false }

def selectAll : FilterFlags :=
  { shown := false, atoms := false, terms := false, theory := false, all := true, complement := false }

def selectComplement : FilterFlags :=
  { shown := false, atoms := false, terms := false, theory := false, all := false, complement := true }

end FilterFlags

def type (m : @& Model) : IO (Except (Error × String) ModelType) := do
  let r ← modelType m
  return r.map fun
    | .stableModel => .Stable
    | .braveConsequences => .Brave
    | .cautiousConsequences => .Cautious

def id (m : @& Model) : IO (Except (Error × String) UInt64) := modelNumber m

private def size_inner (m : @& Model) (flags : @& FilterFlags) : IO (Except (Error × String) USize) :=
  modelSymbolsSize m flags

def size (m : @& Model) (flags : @& FilterFlags := FilterFlags.selectAll) : IO (Except (Error × String) USize) :=
  size_inner m flags

private def symbols_inner (m : @& Model) (flags : @& FilterFlags) : IO (Except (Error × String) (Array Symbol)) :=
  modelSymbols m flags

def symbols (m : @& Model) (flags : @& FilterFlags := FilterFlags.selectAll) : IO (Except (Error × String) (Array Symbol)) :=
  symbols_inner m flags

def contains? (m : @& Model) (a : Symbol) : IO (Except (Error × String) Bool) :=
  modelContains m a

def is_true? (m : @& Model) (l : Literal) : IO (Except (Error × String) Bool) :=
  modelIsTrue m l

def costs (m : @& Model) : IO (Except (Error × String) (Array Int64)) := modelCost m

def optimal? (m : @& Model) : IO (Except (Error × String) Bool) := modelOptimalityProven m

end Model
end Clingo
