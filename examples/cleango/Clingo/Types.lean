import Clingo.Generated.ClingoBindings

/-! Re-exports and type aliases matching the cleango user-facing API. -/

namespace Clingo

open Clingo.Generated

-- Re-export core scalar types.
abbrev Literal := ClingoBindings.Literal
abbrev Atom := ClingoBindings.Atom
abbrev Id := ClingoBindings.ClingoId
abbrev Weight := ClingoBindings.Weight
abbrev Signature := ClingoBindings.Signature
abbrev Symbol := ClingoBindings.Symbol

-- Re-export opaque types.
abbrev Control := ClingoBindings.Control
abbrev Model := ClingoBindings.Model
abbrev SolveHandle := ClingoBindings.SolveHandle
abbrev Statistics := ClingoBindings.Statistics
abbrev ProgramBuilder := ClingoBindings.ProgramBuilder
abbrev Backend := ClingoBindings.Backend

-- Re-export enum types.
abbrev Warning := ClingoBindings.Warning
abbrev TruthValue := ClingoBindings.TruthValue

-- Re-export struct types.
abbrev Location := ClingoBindings.Location
abbrev Part := ClingoBindings.Part

-- Re-export callback types.
abbrev Logger := ClingoBindings.Logger
abbrev SymbolCallback := ClingoBindings.SymbolCallback
abbrev GroundCallback := ClingoBindings.GroundCallback
abbrev SolveEventCallback := ClingoBindings.SolveEventCallback

-- Version as a named structure for ToString.
structure Version where
  major : UInt32
  minor : UInt32
  revision : UInt32

instance : ToString Version where
  toString v := s!"{v.major}.{v.minor}.{v.revision}"

end Clingo
