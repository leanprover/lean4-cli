import Lake
open Lake DSL

package Cli

@[default_target]
lean_lib Cli

require std from git
  "https://github.com/leanprover/std4/" @ "2919713bde15d55e3ea3625a03546531283bcb54"
