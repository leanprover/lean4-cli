import Lake
open Lake DSL

package Cli

@[default_target]
lean_lib Cli

require std from git
  "https://github.com/leanprover/std4"@"f648e43ef696ce1cf7f6ec534ec44c06816380f9"