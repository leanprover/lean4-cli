import Lake
open Lake DSL

package Cli

@[default_target]
lean_lib Cli

require std from git
  "https://github.com/leanprover/std4/" @ "cad19f171ad4aebe091e6deeb9a43a14521bfbfe"
