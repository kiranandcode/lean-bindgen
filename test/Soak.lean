import LeanBindgen.C.Token
import LeanBindgen.C.Parser
import LeanBindgen.C.Pretty

open LeanBindgen.C

private def kindLabel : CDecl → String
  | .typedef ..    => "typedef"
  | .structDef ..  => "struct"
  | .unionDef ..   => "union"
  | .enumDef ..    => "enum"
  | .function ..   => "function"
  | .variable ..   => "variable"
  | .macroConst .. => "macro"

private def declName : CDecl → String
  | .typedef n _      => n
  | .structDef tag? _ => "struct " ++ tag?.getD "<anon>"
  | .unionDef tag? _  => "union "  ++ tag?.getD "<anon>"
  | .enumDef tag? _   => "enum "   ++ tag?.getD "<anon>"
  | .function n _ _ _ => n
  | .variable n _     => n
  | .macroConst n _   => n

def main (args : List String) : IO Unit := do
  let path := args.head?.getD "reference/cleango/bindings/clingo.h"
  IO.println s!"Soak test: {path}"
  let src ← IO.FS.readFile path
  IO.println s!"  source: {src.length} chars"
  match tokenize src with
  | .error e =>
    IO.eprintln s!"TOKENIZER ERROR: {e}"
    return
  | .ok toks =>
    IO.println s!"  tokens: {toks.size}"
    match parseHeader toks with
    | .error e =>
      IO.eprintln s!"PARSER ERROR: {e}"
      return
    | .ok h =>
      IO.println s!"  decls : {h.decls.size}"
      -- Tally kinds
      let mut counts : Std.HashMap String Nat := {}
      for d in h.decls do
        let k := kindLabel d
        counts := counts.insert k ((counts.get? k).getD 0 + 1)
      let pairs := counts.toList.toArray.qsort (fun a b => a.snd > b.snd)
      IO.println "  by kind:"
      for (k, n) in pairs do
        IO.println s!"    {k}: {n}"
      -- A few examples of each kind
      IO.println "\n  first 5 functions:"
      let funcs := h.decls.filter (fun | .function .. => true | _ => false)
      for d in funcs.toList.take 5 do
        IO.println s!"    {declName d}"
      IO.println "\n  first 5 typedefs:"
      let typedefs := h.decls.filter (fun | .typedef .. => true | _ => false)
      for d in typedefs.toList.take 5 do
        IO.println s!"    {declName d}"
      -- Round-trip 6 representative functions through the pretty-printer.
      IO.println "\n  pretty-printed prototypes (sample):"
      let samples := h.decls.filter (fun | .function .. => true | _ => false)
                            |>.toList.take 6
      for d in samples do
        IO.println s!"    {d.toC}"
      -- Re-parse the full pretty-printed output and report drift.
      let regenerated := h.toC
      match tokenize regenerated with
      | .error e => IO.eprintln s!"\nROUND-TRIP TOKENIZE ERROR: {e}"
      | .ok toks2 =>
        match parseHeader toks2 with
        | .error e => IO.eprintln s!"\nROUND-TRIP PARSE ERROR: {e}"
        | .ok h2 =>
          IO.println s!"\n  round-trip: parse → print → parse"
          IO.println s!"    decls 1st pass: {h.decls.size}"
          IO.println s!"    decls 2nd pass: {h2.decls.size}"
          if h.decls.size = h2.decls.size then
            IO.println "    ✓ counts match"
          else
            IO.eprintln "    ✗ count drift!"
