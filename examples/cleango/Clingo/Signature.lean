import Clingo.Types

namespace Clingo
namespace Signature

open Clingo.Generated.ClingoBindings

def mk (name : String) (arity : UInt32) (positive : Bool := false) : IO (Except String Signature) :=
  signatureCreate name arity positive

def name (sig : Signature) : String := signatureName sig

def arity (sig : Signature) : UInt32 := signatureArity sig

def isPositive (sig : Signature) : Bool := signatureIsPositive sig

def isNegative (sig : Signature) : Bool := signatureIsNegative sig

def beq (s1 s2 : Signature) : Bool := signatureBeq s1 s2

def blt (s1 s2 : Signature) : Bool := signatureBlt s1 s2

def hash (s1 : Signature) : USize := signatureHash s1

instance : BEq Signature where beq := beq

end Signature
end Clingo
