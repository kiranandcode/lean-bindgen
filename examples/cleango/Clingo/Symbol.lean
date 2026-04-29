import Clingo.Types
import Lean.Elab.Term

namespace Clingo

open Clingo.Generated.ClingoBindings

inductive SymbolType where | Infimum | Number | String | Function | Supremum
  deriving Repr, Inhabited

-- Symbol.Repr: custom recursive representation for pattern matching.
inductive Symbol.Repr where
  | Infimum
  | Number (n : UInt32)
  | String (s : _root_.String)
  | Function (name : _root_.String) (args : Array Symbol.Repr) (is_positive : Bool)
  | Supremum
  deriving Inhabited

partial def Symbol.Repr.reprImpl : Symbol.Repr → Nat → Std.Format
  | .Infimum, _ => "Symbol.Repr.Infimum"
  | .Number n, _ => f!"Symbol.Repr.Number {_root_.repr n}"
  | .String s, _ => f!"Symbol.Repr.String {_root_.repr s}"
  | .Function name args positive, _ =>
    f!"Symbol.Repr.Function {_root_.repr name} #[{Std.Format.joinSep (args.toList.map (Symbol.Repr.reprImpl · 0)) ", "}] {_root_.repr positive}"
  | .Supremum, _ => "Symbol.Repr.Supremum"

instance : Repr Symbol.Repr where reprPrec := Symbol.Repr.reprImpl

namespace Symbol

-- Direct symbol creation (wrapping generated functions).
def mk_number (n : UInt32) : Symbol := symbolCreateNumber n

def mk_supremum : Symbol := symbolCreateSupremum ()

def mk_infimum : Symbol := symbolCreateInfimum ()

def mk_string (s : @& _root_.String) : IO (Except _root_.String Symbol) := symbolCreateString s

def mk_id (name : @& _root_.String) (positive : Bool) : IO (Except _root_.String Symbol) :=
  symbolCreateId name positive

def mk_fun (name : @& _root_.String) (args : @& Array Symbol) (positive : Bool) : IO (Except _root_.String Symbol) :=
  symbolCreateFunction name args positive

-- Accessors.
def number? (s : Symbol) : Option UInt32 := symbolNumber s

def name? (s : Symbol) : Option _root_.String := symbolName s

def string? (s : Symbol) : Option _root_.String := symbolString s

def isPositive? (s : Symbol) : Option Bool := symbolIsPositive s

def isNegative? (s : Symbol) : Option Bool := symbolIsNegative s

def args? (s : Symbol) : Option (Array Symbol) := symbolArguments s

def type (s : Symbol) : SymbolType :=
  match symbolType s with
  | .infimum => .Infimum
  | .number => .Number
  | .string => .String
  | .function => .Function
  | .supremum => .Supremum

def toString (s : Symbol) : IO (Except (Error × _root_.String) _root_.String) := symbolToString s

def beq (s1 s2 : Symbol) : Bool := symbolBeq s1 s2

def blt (s1 s2 : Symbol) : Bool := symbolBlt s1 s2

def hash (s1 : Symbol) : USize := symbolHash s1

instance : BEq Symbol where beq := beq

/-- Type class for the canonical homomorphism `Symbol → R`. -/
class SymbolCast (R : Type u) where
  protected symCast : Symbol → R

instance : SymbolCast Symbol where symCast n := n

@[coe, reducible, match_pattern] protected def Symbol.cast {R : Type u} [SymbolCast R] : Symbol → R :=
  SymbolCast.symCast

instance [SymbolCast R] : CoeTail Symbol R where coe := Symbol.cast

instance [SymbolCast R] : CoeHTCT Symbol R where coe := Symbol.cast

-- Pure Lean implementation using generated FFI functions.
-- No hand-written C shim needed.
partial def toRepr (s : Symbol) : Symbol.Repr :=
  match symbolType s with
  | .infimum => .Infimum
  | .number => .Number (symbolNumber s |>.getD 0)
  | .string => .String (symbolString s |>.getD "")
  | .function =>
    let name := (symbolName s).getD ""
    let args := (symbolArguments s).getD #[]
    let positive := (symbolIsPositive s).getD false
    .Function name (args.map toRepr) positive
  | .supremum => .Supremum

partial def mk (r : @& Symbol.Repr) : IO Symbol :=
  match r with
  | .Infimum => pure (symbolCreateInfimum ())
  | .Number n => pure (symbolCreateNumber n)
  | .String s => do
    let .ok sym ← symbolCreateString s | throw (IO.userError "failed to create string symbol")
    pure sym
  | .Function name args positive => do
    let mut syms : Array Symbol := #[]
    for a in args do
      let s ← mk a
      syms := syms.push s
    let .ok sym ← symbolCreateFunction name syms positive | throw (IO.userError "failed to create function symbol")
    pure sym
  | .Supremum => pure (symbolCreateSupremum ())

instance : SymbolCast Symbol.Repr where symCast := toRepr

namespace Repr

  def toSymbol : Symbol.Repr → IO Symbol := fun r => Symbol.mk r

  def Variable v := Function v #[] true

end Repr

open Lean Elab

def matchAlts : Lean.Parser.Parser := Lean.Parser.Term.matchAlts (rhsParser := Lean.Parser.termParser)

elab_rules : term
| `(term| match $x:term with $m:matchAlts) => do
   let x_stx ← Term.elabTerm x (some (mkConst ``Symbol))
   let ty ← Meta.inferType x_stx
   let is_sym ← Meta.isDefEq ty (mkConst ``Symbol)
   if not is_sym then throwUnsupportedSyntax
   Term.elabTerm (← `(term| match Symbol.toRepr $x:term with $m:matchAlts)) none

end Symbol
end Clingo
