import Lake
open Lake DSL

package «lean-bindgen» where

@[default_target]
lean_lib LeanBindgen where

lean_exe «lean-bindgen» where
  root := `Main

lean_exe «test-pretty» where
  root := `test.PrettyTest

lean_exe «test-token» where
  root := `test.TokenTest
