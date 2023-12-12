import Lake
open Lake DSL

package «aoc_lean» where
  -- Settings applied to both builds and interactive editing
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩, -- pretty-prints `fun a ↦ b`
    ⟨`pp.proofs.withType, false⟩
  ]


require aesop from git "https://github.com/JLimperg/aesop"

-- require proofwidgets from git "https://github.com/EdAyers/ProofWidgets4" @ "main"
require Paperproof from git "https://github.com/Paper-Proof/paperproof.git"@"main"/"lean"
-- require LeanCopilot from git "https://github.com/lean-dojo/LeanCopilot.git" @ "main"
require mathlib from git "https://github.com/leanprover-community/mathlib4.git"
require std from git "https://github.com/leanprover/std4.git" @ "main"
require Parser from git "https://github.com/fgdorais/lean4-parser" @ "main"
require algdata from git "https://github.com/Junology/algdata" @ "main"
@[default_target]
lean_lib «AocLean» where
  -- moreLinkArgs := #["-L./.lake/packages/LeanCopilot/.lake/build/lib", "-lctranslate2"]

  -- add any library configuration options here
