import LeanBindgen.Gen.CShim

/-!
# C Shim AST → String

Renders `CExpr`, `CStmt`, and `CTopLevel` to C source text. No
precedence tracking — use `CExpr.paren` explicitly where needed.
-/

namespace LeanBindgen.Gen

private def indent (n : Nat) : String :=
  String.ofList (List.replicate (n * 2) ' ')

/-- Escape a C string literal (minimal: backslash, double-quote, newline). -/
private def escapeC (s : String) : String :=
  s.foldl (fun acc c =>
    match c with
    | '\\' => acc ++ "\\\\"
    | '"'  => acc ++ "\\\""
    | '\n' => acc ++ "\\n"
    | c    => acc.push c) ""

mutual

partial def CExpr.render : CExpr → String
  | .var name       => name
  | .intLit v       => toString v
  | .strLit v       => "\"" ++ escapeC v ++ "\""
  | .null           => "NULL"
  | .call fn args   =>
    fn ++ "(" ++ ", ".intercalate (args.toList.map CExpr.render) ++ ")"
  | .field obj n    => CExpr.render obj ++ "." ++ n
  | .arrow obj n    => CExpr.render obj ++ "->" ++ n
  | .subscript a i  => CExpr.render a ++ "[" ++ CExpr.render i ++ "]"
  | .deref e        => "*" ++ CExpr.render e
  | .addrOf e       => "&" ++ CExpr.render e
  | .cast ty e      => "(" ++ ty ++ ")" ++ CExpr.render e
  | .binop op l r   => CExpr.render l ++ " " ++ op ++ " " ++ CExpr.render r
  | .unop op e      => op ++ CExpr.render e
  | .ternary c t e  =>
    CExpr.render c ++ " ? " ++ CExpr.render t ++ " : " ++ CExpr.render e
  | .sizeof ty      => "sizeof(" ++ ty ++ ")"
  | .paren e        => "(" ++ CExpr.render e ++ ")"
  | .raw s          => s

partial def CStmt.render (depth : Nat) : CStmt → String
  | .varDecl ty name init =>
    let base := indent depth ++ ty ++ " " ++ name
    match init with
    | none   => base ++ ";"
    | some e => base ++ " = " ++ CExpr.render e ++ ";"
  | .assign lhs rhs =>
    indent depth ++ CExpr.render lhs ++ " = " ++ CExpr.render rhs ++ ";"
  | .assignOp op lhs rhs =>
    indent depth ++ CExpr.render lhs ++ " " ++ op ++ "= " ++ CExpr.render rhs ++ ";"
  | .expr e =>
    indent depth ++ CExpr.render e ++ ";"
  | .ret none =>
    indent depth ++ "return;"
  | .ret (some e) =>
    indent depth ++ "return " ++ CExpr.render e ++ ";"
  | .ifElse cond thenBody elseBody =>
    let head := indent depth ++ "if (" ++ CExpr.render cond ++ ") {\n"
    let thenS := "\n".intercalate (thenBody.toList.map (CStmt.render (depth + 1)))
    let tail := if elseBody.isEmpty then "\n" ++ indent depth ++ "}"
      else
        let elsS := "\n".intercalate (elseBody.toList.map (CStmt.render (depth + 1)))
        "\n" ++ indent depth ++ "} else {\n" ++ elsS ++ "\n" ++ indent depth ++ "}"
    head ++ thenS ++ tail
  | .switch e cases default =>
    let head := indent depth ++ "switch (" ++ CExpr.render e ++ ") {\n"
    let casesS := "\n".intercalate (cases.toList.map fun (pat, body) =>
      let caseHead := indent (depth + 1) ++ "case " ++ CExpr.render pat ++ ":"
      let bodyS := "\n".intercalate (body.toList.map (CStmt.render (depth + 2)))
      caseHead ++ "\n" ++ bodyS)
    let defaultS := if default.isEmpty then ""
      else
        let bodyS := "\n".intercalate (default.toList.map (CStmt.render (depth + 2)))
        "\n" ++ indent (depth + 1) ++ "default:\n" ++ bodyS
    head ++ casesS ++ defaultS ++ "\n" ++ indent depth ++ "}"
  | .forLoop init cond step body =>
    let head := indent depth ++ "for (" ++ init ++ "; " ++ cond ++ "; " ++ step ++ ") {\n"
    let bodyS := "\n".intercalate (body.toList.map (CStmt.render (depth + 1)))
    head ++ bodyS ++ "\n" ++ indent depth ++ "}"
  | .block stmts =>
    let head := indent depth ++ "{\n"
    let bodyS := "\n".intercalate (stmts.toList.map (CStmt.render (depth + 1)))
    head ++ bodyS ++ "\n" ++ indent depth ++ "}"
  | .comment text =>
    indent depth ++ "/* " ++ text ++ " */"
  | .blank => ""
  | .raw s => indent depth ++ s

end

def CTopLevel.render : CTopLevel → String
  | .include path true  => "#include <" ++ path ++ ">"
  | .include path false => "#include \"" ++ path ++ "\""
  | .forwardDecl sig    => sig ++ ";"
  | .globalVar ty name init =>
    let base := ty ++ " " ++ name
    match init with
    | none   => base ++ ";"
    | some v => base ++ " = " ++ v ++ ";"
  | .funcDef linkage retTy name params body =>
    let linkStr := match linkage with
      | .leanExport => "LEAN_EXPORT "
      | .static_    => "static "
      | .plain      => ""
    let paramStr := if params.isEmpty then "void"
      else ", ".intercalate (params.toList.map fun p => p.ty ++ " " ++ p.name)
    let sig := linkStr ++ retTy ++ " " ++ name ++ "(" ++ paramStr ++ ")"
    let bodyStr := "\n".intercalate (body.toList.map (CStmt.render 1))
    sig ++ " {\n" ++ bodyStr ++ "\n}"
  | .comment text => "/* " ++ text ++ " */"
  | .blank        => ""
  | .raw s        => s

def CShimFile.render (f : CShimFile) : String :=
  "\n\n".intercalate (f.toList.map CTopLevel.render)

end LeanBindgen.Gen
