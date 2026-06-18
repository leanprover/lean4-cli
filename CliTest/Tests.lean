import Cli.Basic
import Cli.Extensions
import Cli.Typed

namespace Cli

section Utils

instance [BEq α] [BEq β] : BEq (Except α β) where
  beq
  | Except.ok a,    Except.ok a'    => a == a'
  | Except.error b, Except.error b' => b == b'
  | _,              _               => false

instance [Repr α] [Repr β] : Repr (Except α β) where
  reprPrec
  | Except.ok a,    _ => s!"Except.ok ({repr a})"
  | Except.error b, _ => s!"Except.error ({repr b})"

def Cmd.processParsed (c : Cmd) (args : String) : String := Id.run do
  let mut args := args.splitOn
  if args = [""] then
    args := []
  match c.process args with
  | Except.ok (_, parsed) =>
    return toString parsed
  | Except.error (_, error) =>
    return error

def Cmd.extendedHelp (c : Cmd) : String :=
  c.extension!.extend (.ofFullCmd c) |>.toFullCmd c |>.help

end Utils

def doNothing (_ : Parsed) : IO UInt32 := return 0

def testSubSubCmd : Cmd := `[Cli|
  testsubsubcommand VIA doNothing;
  "does this even do anything?"
]

def testSubCmd1 : Cmd := `[Cli|
  testsubcommand1 NOOP; ["0.0.1"]
  "a properly short description"

  FLAGS:
    "launch-the-nukes";  "please avoid passing this flag at all costs.\nif you like, you can have newlines in descriptions."

  ARGS:
    "city-location" : String;  "can also use hyphens"

  SUBCOMMANDS:
    testSubSubCmd

  EXTENSIONS:
    helpSubCommand;
    versionSubCommand!
]

def testSubCmd2 : Cmd := `[Cli|
  testsubcommand2 VIA doNothing; ["0.0.-1"]
  "does not do anything interesting"

  FLAGS:
    r, run; "really, this does not do anything. trust me."

  ARGS:
    "ominous-input" : Array String; "what could this be for?"
]

def testCmd : Cmd := `[Cli|
  testcommand VIA doNothing; ["0.0.0"]
  "some short description that happens to be much longer than necessary and hence needs to be wrapped to fit into an 80 character width limit"

  FLAGS:
    verbose;                     "a very verbose flag description that also needs to be wrapped to fit into an 80 character width limit"
    x, unknown1;                 "this flag has a short name"
    xn, unknown2;                "short names do not need to be prefix-free"
    ny, unknown3;                "-xny will parse as -x -ny and not fail to parse as -xn -y"
    t, typed1 : String;          "flags can have typed parameters"
    ty, typed2;                  "-ty parsed as --typed2, not -t=y"
    "p-n", "level-param" : Nat;  "hyphens work, too"

  ARGS:
    input1 : String;     "another very verbose description that also needs to be wrapped to fit into an 80 character width limit"
    input2 : Array Nat;  "arrays!"
    ...outputs : Nat;    "varargs!"

  SUBCOMMANDS: testSubCmd1; testSubCmd2

  EXTENSIONS:
    author "mhuisi";
    longDescription "this could be really long, but i'm too lazy to type it out.";
    defaultValues! #[⟨"level-param", "0"⟩];
    require! #["typed1"]
]

section ValidInputs

/--
info: "cmd: testcommand; flags: #[--typed1=a, --level-param=0]; positionalArgs: #[<input1=foo>, <input2=1>]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "foo 1 -ta"

/--
info: "cmd: testcommand; flags: #[--help, --level-param=0]; positionalArgs: #[]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "-h"

/--
info: "cmd: testcommand; flags: #[--version, --level-param=0]; positionalArgs: #[]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "--version"

/--
info: "cmd: testcommand; flags: #[--verbose, --level-param=2, --unknown2, --unknown1, --typed1=foo]; positionalArgs: #[<input1=foo>, <input2=1,2,3>]; variableArgs: #[<outputs=1>, <outputs=2>, <outputs=3>]"
-/
#guard_msgs in
#eval testCmd.processParsed "foo --verbose -p-n 2 1,2,3 1 -xnx 2 --typed1=foo 3"

/--
info: "cmd: testcommand; flags: #[--unknown1, --unknown3, --typed1=3, --level-param=0]; positionalArgs: #[<input1=foo>, <input2=1>]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "foo -xny 1 -t 3"

/--
info: "cmd: testcommand; flags: #[--typed1=3, --level-param=0]; positionalArgs: #[<input1=--input>, <input2=2>]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "-t 3 -- --input 2"

/--
info: "cmd: testcommand; flags: #[--typed1=1, --level-param=0]; positionalArgs: #[<input1=->, <input2=2>]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "-t1 - 2"

/--
info: "cmd: testcommand; flags: #[--typed2, --typed1=1, --level-param=0]; positionalArgs: #[<input1=foo>, <input2=1,2>]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "-ty -t1 foo 1,2"

/--
info: "cmd: testcommand testsubcommand1; flags: #[]; positionalArgs: #[<city-location=testsubsubcommand>]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "testsubcommand1 -- testsubsubcommand"

/--
info: "cmd: testcommand testsubcommand1; flags: #[--launch-the-nukes]; positionalArgs: #[<city-location=x>]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "testsubcommand1 --launch-the-nukes x"

/--
info: "cmd: testcommand testsubcommand1; flags: #[]; positionalArgs: #[<city-location=--launch-the-nukes>]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "testsubcommand1 -- --launch-the-nukes"

/--
info: "cmd: testcommand testsubcommand1 testsubsubcommand; flags: #[]; positionalArgs: #[]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "testsubcommand1 testsubsubcommand"

/--
info: "cmd: testcommand testsubcommand2; flags: #[--run]; positionalArgs: #[<ominous-input=asdf,geh>]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "testsubcommand2 --run asdf,geh"

/--
info: "cmd: testcommand testsubcommand1 help; flags: #[]; positionalArgs: #[]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "testsubcommand1 help"

/--
info: "cmd: testcommand testsubcommand1 version; flags: #[]; positionalArgs: #[]; variableArgs: #[]"
-/
#guard_msgs in
#eval testCmd.processParsed "testsubcommand1 version"

end ValidInputs

section InvalidInputs

/-- info: "Missing positional argument `<input1>.`" -/
#guard_msgs in
#eval testCmd.processParsed ""

/-- info: "Missing positional argument `<input2>.`" -/
#guard_msgs in
#eval testCmd.processParsed "foo"

/-- info: "Invalid type of argument `asdf` for positional argument `<input2 : Array Nat>`." -/
#guard_msgs in
#eval testCmd.processParsed "foo asdf"

/-- info: "Missing required flag `--typed1`." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3"

/-- info: "Missing argument for flag `-t`." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3 -t"

/-- info: "Invalid type of argument `` for flag `--level-param : Nat`." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3 -t1 --level-param="

/-- info: "Invalid type of argument `` for flag `-p-n : Nat`." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3 -t1 -p-n="

/-- info: "Unknown flag `--asdf`." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3 -t1 --asdf"

/-- info: "Duplicate flag `-t` (`--typed1`)." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3 -t1 -t2"

/-- info: "Duplicate flag `--typed1` (`-t`)." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3 -t1 --typed1=2"

/-- info: "Unknown flag `--typed12`." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3 --typed12"

/-- info: "Redundant argument `1` for flag `-x` that takes no arguments." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3 -t1 -x=1"

/-- info: "Invalid type of argument `bar` for variable argument `<outputs : Nat>...`." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3 -t1 bar"

/-- info: "Unknown flag `-xxn`." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3 -t1 -xxn=1"

/-- info: "Unknown flag `--t`." -/
#guard_msgs in
#eval testCmd.processParsed "foo 1,2,3 --t=1"

/-- info: "Redundant positional argument `geh`." -/
#guard_msgs in
#eval testCmd.processParsed "testsubcommand1 asdf geh"

end InvalidInputs

section Info

/--
info: "testcommand [0.0.0]\nsome short description that happens to be much longer than necessary and hence\nneeds to be wrapped to fit into an 80 character width limit\n\nUSAGE:\n    testcommand [SUBCOMMAND] [FLAGS] <input1> <input2> <outputs>...\n\nFLAGS:\n    -h, --help                 Prints this message.\n    --version                  Prints the version.\n    --verbose                  a very verbose flag description that also needs\n                               to be wrapped to fit into an 80 character width\n                               limit\n    -x, --unknown1             this flag has a short name\n    -xn, --unknown2            short names do not need to be prefix-free\n    -ny, --unknown3            -xny will parse as -x -ny and not fail to parse\n                               as -xn -y\n    -t, --typed1 : String      flags can have typed parameters\n    -ty, --typed2              -ty parsed as --typed2, not -t=y\n    -p-n, --level-param : Nat  hyphens work, too\n\nARGS:\n    input1 : String     another very verbose description that also needs to be\n                        wrapped to fit into an 80 character width limit\n    input2 : Array Nat  arrays!\n    outputs : Nat       varargs!\n\nSUBCOMMANDS:\n    testsubcommand1  a properly short description\n    testsubcommand2  does not do anything interesting"
-/
#guard_msgs in
#eval testCmd.help

/-- info: "0.0.0" -/
#guard_msgs in
#eval testCmd.meta.version!

/--
info: "some exceedingly long error that needs to be wrapped to fit within an 80\ncharacter width limit. none of our errors are really that long, but flag names\nmight be.\nRun `testcommand -h` for further information."
-/
#guard_msgs in
#eval testCmd.error "some exceedingly long error that needs to be wrapped to fit within an 80 character width limit. none of our errors are really that long, but flag names might be."

/--
info: "testsubcommand2 [0.0.-1]\ndoes not do anything interesting\n\nUSAGE:\n    testsubcommand2 [FLAGS] <ominous-input>\n\nFLAGS:\n    -h, --help  Prints this message.\n    --version   Prints the version.\n    -r, --run   really, this does not do anything. trust me.\n\nARGS:\n    ominous-input : Array String  what could this be for?"
-/
#guard_msgs in
#eval testSubCmd2.help

/--
info: "testcommand testsubcommand2 [0.0.-1]\ndoes not do anything interesting\n\nUSAGE:\n    testcommand testsubcommand2 [FLAGS] <ominous-input>\n\nFLAGS:\n    -h, --help  Prints this message.\n    --version   Prints the version.\n    -r, --run   really, this does not do anything. trust me.\n\nARGS:\n    ominous-input : Array String  what could this be for?"
-/
#guard_msgs in
#eval (testCmd.subCmd! "testsubcommand2").help

/--
info: "testcommand testsubcommand1 testsubsubcommand\ndoes this even do anything?\n\nUSAGE:\n    testcommand testsubcommand1 testsubsubcommand [FLAGS]\n\nFLAGS:\n    -h, --help  Prints this message."
-/
#guard_msgs in
#eval (testCmd.subCmd! "testsubcommand1" |>.subCmd! "testsubsubcommand").help

/--
info: "testcommand [0.0.0]\nmhuisi\nsome short description that happens to be much longer than necessary and hence\nneeds to be wrapped to fit into an 80 character width limit\n\nUSAGE:\n    testcommand [SUBCOMMAND] [FLAGS] <input1> <input2> <outputs>...\n\nFLAGS:\n    -h, --help                 Prints this message.\n    --version                  Prints the version.\n    --verbose                  a very verbose flag description that also needs\n                               to be wrapped to fit into an 80 character width\n                               limit\n    -x, --unknown1             this flag has a short name\n    -xn, --unknown2            short names do not need to be prefix-free\n    -ny, --unknown3            -xny will parse as -x -ny and not fail to parse\n                               as -xn -y\n    -t, --typed1 : String      [Required] flags can have typed parameters\n    -ty, --typed2              -ty parsed as --typed2, not -t=y\n    -p-n, --level-param : Nat  hyphens work, too [Default: `0`]\n\nARGS:\n    input1 : String     another very verbose description that also needs to be\n                        wrapped to fit into an 80 character width limit\n    input2 : Array Nat  arrays!\n    outputs : Nat       varargs!\n\nSUBCOMMANDS:\n    testsubcommand1  a properly short description\n    testsubcommand2  does not do anything interesting\n\nDESCRIPTION:\n    this could be really long, but i'm too lazy to type it out."
-/
#guard_msgs in
#eval testCmd.extendedHelp

/--
info: "testsubcommand1 [0.0.1]\na properly short description\n\nUSAGE:\n    testsubcommand1 [SUBCOMMAND] [FLAGS] <city-location>\n\nFLAGS:\n    -h, --help          Prints this message.\n    --version           Prints the version.\n    --launch-the-nukes  please avoid passing this flag at all costs.\n                        if you like, you can have newlines in descriptions.\n\nARGS:\n    city-location : String  can also use hyphens\n\nSUBCOMMANDS:\n    testsubsubcommand  does this even do anything?\n    version            Prints the version.\n    help               Prints this message."
-/
#guard_msgs in
#eval testSubCmd1.extendedHelp

end Info

section ModuleName

def ModuleName.parse? : String → Option ModuleName := ParseableType.parse?

section ValidInputs

/-- info: some `Lean.Mathlib.Data -/
#guard_msgs in
#eval ModuleName.parse? "Lean.Mathlib.Data"

/-- info: some `F00Bar.BarF00 -/
#guard_msgs in
#eval ModuleName.parse? "F00Bar.BarF00"

/-- info: some `foo_bar -/
#guard_msgs in
#eval ModuleName.parse? "foo_bar"

/-- info: some `asdf.«foo bar» -/
#guard_msgs in
#eval ModuleName.parse? "asdf.«foo bar»"

/-- info: some `«1».«2» -/
#guard_msgs in
#eval ModuleName.parse? "«1».«2»"

/-- info: some `« » -/
#guard_msgs in
#eval ModuleName.parse? "« »"

/-- info: some `Lean.Mathlib.Data.Afile -/
#guard_msgs in
#eval ModuleName.parse? "Lean/Mathlib/Data/Afile.lean"

/-- info: some `Foo -/
#guard_msgs in
#eval ModuleName.parse? "Foo.lean"

/-- info: some `«.» -/
#guard_msgs in
#eval ModuleName.parse? "..lean"

/-- info: some `« » -/
#guard_msgs in
#eval ModuleName.parse? " .lean"

/-- info: some `« ».Foo -/
#guard_msgs in
#eval ModuleName.parse? " /Foo.lean"

end ValidInputs

section InvalidInputs

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? ""

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? "."

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? ".asdf"

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? "asdf."

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? "1.asdf"

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? "asdf.1"

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? "1asdf"

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? "foo bar"

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? "foo,bar"

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? "«»"

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? "x.«»"

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? ".lean"

/-- info: none -/
#guard_msgs in
#eval ModuleName.parse? "/foo.lean"

end InvalidInputs

end ModuleName

section CliDef

-- Test cli_def VIA (backward-compatible mode)
cli_def viaSubCmd VIA doNothing;
  "a subcommand defined with cli_def VIA"

-- Test cli_def NOOP
cli_def noopSubCmd NOOP;
  "a subcommand defined with cli_def NOOP"

-- Test cli_def VIA with flags, args, extensions
cli_def viaCmd VIA doNothing; ["1.0.0"]
  "a command defined with cli_def VIA"

  FLAGS:
    verbose; "verbose flag"
    n, count : Nat; "a typed flag"

  ARGS:
    input : String; "an input"

  SUBCOMMANDS: viaSubCmd; noopSubCmd

  EXTENSIONS:
    defaultValues! #[("count", "0")]

section ViaValidInputs

/--
info: "cmd: viaCmd; flags: #[--count=0]; positionalArgs: #[<input=hello>]; variableArgs: #[]"
-/
#guard_msgs in
#eval viaCmd.processParsed "hello"

/--
info: "cmd: viaCmd; flags: #[--verbose, --count=5]; positionalArgs: #[<input=hello>]; variableArgs: #[]"
-/
#guard_msgs in
#eval viaCmd.processParsed "--verbose -n5 hello"

/--
info: "cmd: viaCmd noopSubCmd; flags: #[]; positionalArgs: #[]; variableArgs: #[]"
-/
#guard_msgs in
#eval viaCmd.processParsed "noopSubCmd"

end ViaValidInputs

section ViaInvalidInputs

/-- info: "Missing positional argument `<input>.`" -/
#guard_msgs in
#eval viaCmd.processParsed ""

end ViaInvalidInputs

-- Test cli_def RUN: typed struct generation and accessor correctness

-- A simple subcommand used by the typed command
cli_def typedSubCmd NOOP;
  "a subcommand for the typed command"

-- The main typed command exercises all feature combinations:
-- parameterless flags, typed flags, short aliases, hyphenated names,
-- positional args, variable args, subcommands, and extensions.
cli_def typedCmd; ["2.0.0"]
  "a command defined with cli_def RUN"

  FLAGS:
    verbose;                     "verbose flag"
    x, unknown1;                 "flag with short name"
    t, typed1 : String;          "typed flag"
    "p-n", "level-param" : Nat;  "hyphenated flag"

  ARGS:
    input1 : String;     "a string input"
    input2 : Array Nat;  "an array input"
    ...outputs : Nat;    "varargs"

  SUBCOMMANDS: typedSubCmd

  EXTENSIONS:
    defaultValues! #[("level-param", "0")]

  RUN fun p => do
    -- Exercise every accessor to verify types are correct
    IO.println <| "input1=" ++ p.input1
    IO.println <| "input2=" ++ toString p.input2
    IO.println <| "outputs=" ++ toString p.outputs
    IO.println <| "verbose=" ++ toString p.verbose
    IO.println <| "unknown1=" ++ toString p.unknown1
    IO.println <| "typed1=" ++ toString p.typed1?
    IO.println <| "levelParam=" ++ toString p.levelParam?
    return 0

section TypedStructExists

-- Verify the Parsed struct exists and is Inhabited
#check typedCmd.Parsed
#check (inferInstance : Inhabited typedCmd.Parsed)

-- Verify accessor types
#check @typedCmd.Parsed.verbose   -- typedCmd.Parsed → Bool
#check @typedCmd.Parsed.unknown1  -- typedCmd.Parsed → Bool
#check @typedCmd.Parsed.typed1?    -- typedCmd.Parsed → Option String
#check @typedCmd.Parsed.levelParam? -- typedCmd.Parsed → Option Nat
#check @typedCmd.Parsed.input1    -- typedCmd.Parsed → String
#check @typedCmd.Parsed.input2    -- typedCmd.Parsed → Array Nat
#check @typedCmd.Parsed.outputs   -- typedCmd.Parsed → Array Nat
#check @typedCmd.Parsed.raw       -- typedCmd.Parsed → Cli.Parsed

end TypedStructExists

section TypedValidInputs

/--
info: input1=foo
input2=#[1, 2, 3]
outputs=#[4, 5]
verbose=true
unknown1=false
typed1=(some hello)
levelParam=(some 7)
---
info: 0
-/
#guard_msgs in
#eval typedCmd.validate <| "--verbose -t hello --level-param=7 foo 1,2,3 4 5".splitOn " "

/--
info: input1=foo
input2=#[1]
outputs=#[]
verbose=false
unknown1=true
typed1=(some bar)
levelParam=(some 0)
---
info: 0
-/
#guard_msgs in
#eval typedCmd.validate <| "-x -tbar foo 1".splitOn " "

/--
info: input1=foo
input2=#[1]
outputs=#[]
verbose=false
unknown1=false
typed1=none
levelParam=(some 0)
---
info: 0
-/
#guard_msgs in
#eval typedCmd.validate <| "foo 1".splitOn " "

end TypedValidInputs

section TypedParsing

-- Verify that the typed command produces the same parse results as a
-- manually-defined equivalent using `[Cli| ...]`
def typedCmdEquiv : Cmd := `[Cli|
  typedCmd VIA doNothing; ["2.0.0"]
  "a command defined with cli_def RUN"

  FLAGS:
    verbose;                     "verbose flag"
    x, unknown1;                 "flag with short name"
    t, typed1 : String;          "typed flag"
    "p-n", "level-param" : Nat;  "hyphenated flag"

  ARGS:
    input1 : String;     "a string input"
    input2 : Array Nat;  "an array input"
    ...outputs : Nat;    "varargs"

  SUBCOMMANDS: typedSubCmd

  EXTENSIONS:
    defaultValues! #[("level-param", "0")]
]

/--
info: "cmd: typedCmd; flags: #[--verbose, --typed1=hello, --level-param=0]; positionalArgs: #[<input1=foo>, <input2=1,2,3>]; variableArgs: #[<outputs=4>, <outputs=5>]"
-/
#guard_msgs in
#eval typedCmd.processParsed "--verbose -t hello foo 1,2,3 4 5"

-- Same parse result from the equivalent command
/--
info: "cmd: typedCmd; flags: #[--verbose, --typed1=hello, --level-param=0]; positionalArgs: #[<input1=foo>, <input2=1,2,3>]; variableArgs: #[<outputs=4>, <outputs=5>]"
-/
#guard_msgs in
#eval typedCmdEquiv.processParsed "--verbose -t hello foo 1,2,3 4 5"

end TypedParsing

section TypedInvalidInputs

/-- info: "Missing positional argument `<input1>.`" -/
#guard_msgs in
#eval typedCmd.processParsed ""

/-- info: "Missing positional argument `<input2>.`" -/
#guard_msgs in
#eval typedCmd.processParsed "foo"

/-- info: "Invalid type of argument `abc` for positional argument `<input2 : Array Nat>`." -/
#guard_msgs in
#eval typedCmd.processParsed "foo abc"

/-- info: "Unknown flag `--nonexistent`." -/
#guard_msgs in
#eval typedCmd.processParsed "foo 1 --nonexistent"

end TypedInvalidInputs

section TypedInfo

/--
info: "typedCmd [2.0.0]\na command defined with cli_def RUN\n\nUSAGE:\n    typedCmd [SUBCOMMAND] [FLAGS] <input1> <input2> <outputs>...\n\nFLAGS:\n    -h, --help                 Prints this message.\n    --version                  Prints the version.\n    --verbose                  verbose flag\n    -x, --unknown1             flag with short name\n    -t, --typed1 : String      typed flag\n    -p-n, --level-param : Nat  hyphenated flag [Default: `0`]\n\nARGS:\n    input1 : String     a string input\n    input2 : Array Nat  an array input\n    outputs : Nat       varargs\n\nSUBCOMMANDS:\n    typedSubCmd  a subcommand for the typed command"
-/
#guard_msgs in
#eval typedCmd.extendedHelp

end TypedInfo

-- Test: cli_def RUN with no flags and no variable args
cli_def minimalCmd;
  "a minimal command with only a positional arg"

  ARGS:
    name : String; "your name"

  RUN fun p => do
    IO.println <| "Hello, " ++ p.name
    return 0

/--
info: Hello, World
---
info: 0
-/
#guard_msgs in
#eval minimalCmd.validate <| ["World"]

-- Test: cli_def RUN with only flags, no args
cli_def flagsOnlyCmd;
  "a command with only flags"

  FLAGS:
    a, alpha; "first flag"
    b, beta;  "second flag"

  RUN fun p => do
    IO.println <| "alpha=" ++ toString p.alpha ++ " beta=" ++ toString p.beta
    return 0

/--
info: alpha=true beta=false
---
info: 0
-/
#guard_msgs in
#eval flagsOnlyCmd.validate <| ["-a"]

/--
info: alpha=true beta=true
---
info: 0
-/
#guard_msgs in
#eval flagsOnlyCmd.validate <| ["-ab"]

-- Test: hyphenToCamelCase conversion for hyphenated arg names
cli_def hyphenCmd;
  "tests hyphenated names"

  FLAGS:
    "my-flag"; "a hyphenated flag"
    "another-long-flag" : String; "a typed hyphenated flag"

  ARGS:
    "my-input" : String; "a hyphenated arg"

  RUN fun p => do
    IO.println <| "myFlag=" ++ toString p.myFlag
    IO.println <| "anotherLongFlag=" ++ toString p.anotherLongFlag?
    IO.println <| "myInput=" ++ p.myInput
    return 0

/--
info: myFlag=true
anotherLongFlag=(some hello)
myInput=world
---
info: 0
-/
#guard_msgs in
#eval hyphenCmd.validate <| "--my-flag --another-long-flag=hello world".splitOn " "

-- Test: raw accessor provides access to underlying Cli.Parsed
cli_def rawAccessCmd;
  "tests raw accessor"

  ARGS:
    input : String; "an input"

  RUN fun p => do
    -- Access via typed accessor
    IO.println <| "typed: " ++ p.input
    -- Access via raw Cli.Parsed
    IO.println <| "raw: " ++ (p.raw.positionalArg! "input" |>.as! String)
    return 0

/--
info: typed: hello
raw: hello
---
info: 0
-/
#guard_msgs in
#eval rawAccessCmd.validate <| ["hello"]

-- Test: signature-only cli_def generates struct + mkCmd but no `def`
cli_def sigOnlyCmd; ["1.0.0"]
  "a command defined with signature-only cli_def"

  FLAGS:
    verbose;             "verbose flag"
    n, count : Nat;      "a count flag"

  ARGS:
    input : String;      "an input"
    ...extras : String;  "extra args"

section SigOnlyStructExists
-- Verify the struct and mkCmd exist
#check sigOnlyCmd.Parsed
#check (inferInstance : Inhabited sigOnlyCmd.Parsed)
#check @sigOnlyCmd.Parsed.verbose  -- sigOnlyCmd.Parsed → Bool
#check @sigOnlyCmd.Parsed.count?    -- sigOnlyCmd.Parsed → Option Nat
#check @sigOnlyCmd.Parsed.input    -- sigOnlyCmd.Parsed → String
#check @sigOnlyCmd.Parsed.extras   -- sigOnlyCmd.Parsed → Array String
#check @sigOnlyCmd.mkCmd           -- (sigOnlyCmd.Parsed → IO UInt32) → Cmd
end SigOnlyStructExists

-- Define the handler as a standalone function using the generated type
def runSigOnlyCmd (p : sigOnlyCmd.Parsed) : IO UInt32 := do
  IO.println <| "input=" ++ p.input
  IO.println <| "extras=" ++ toString p.extras
  IO.println <| "verbose=" ++ toString p.verbose
  IO.println <| "count=" ++ toString p.count?
  return 0

-- Attach the handler via mkCmd
def sigOnlyCmd : Cmd := sigOnlyCmd.mkCmd runSigOnlyCmd

/--
info: input=hello
extras=#[a, b]
verbose=true
count=(some 42)
---
info: 0
-/
#guard_msgs in
#eval sigOnlyCmd.validate <| "--verbose --count=42 hello a b".splitOn " "

/--
info: input=world
extras=#[]
verbose=false
count=none
---
info: 0
-/
#guard_msgs in
#eval sigOnlyCmd.validate <| ["world"]

-- Verify help output includes all metadata
/--
info: "sigOnlyCmd [1.0.0]\na command defined with signature-only cli_def\n\nUSAGE:\n    sigOnlyCmd [FLAGS] <input> <extras>...\n\nFLAGS:\n    -h, --help         Prints this message.\n    --version          Prints the version.\n    --verbose          verbose flag\n    -n, --count : Nat  a count flag\n\nARGS:\n    input : String   an input\n    extras : String  extra args"
-/
#guard_msgs in
#eval sigOnlyCmd.extendedHelp

-- Test: signature-only with RUN-equivalent pattern produces same results
-- as inline RUN
cli_def sigEquivCmd;
  "equivalent command"

  FLAGS:
    verbose; "verbose"

  ARGS:
    name : String; "a name"

  RUN fun p => do
    IO.println <| "name=" ++ p.name ++ " verbose=" ++ toString p.verbose
    return 0

cli_def sigEquivCmd2;
  "equivalent command"

  FLAGS:
    verbose; "verbose"

  ARGS:
    name : String; "a name"

def sigEquivCmd2 : Cmd := sigEquivCmd2.mkCmd fun p => do
  IO.println <| "name=" ++ p.name ++ " verbose=" ++ toString p.verbose
  return 0

-- Both should produce the same output
/--
info: name=Alice verbose=true
---
info: 0
-/
#guard_msgs in
#eval sigEquivCmd.validate <| "--verbose Alice".splitOn " "

/--
info: name=Alice verbose=true
---
info: 0
-/
#guard_msgs in
#eval sigEquivCmd2.validate <| "--verbose Alice".splitOn " "

-- Test: signature-only cli_def with subcommands
cli_def sigSubA NOOP;
  "subcommand A"

cli_def sigSubB;
  "subcommand B"

  FLAGS:
    force; "force flag"

  RUN fun p => do
    IO.println <| "subB force=" ++ toString p.force
    return 0

cli_def sigParentCmd; ["3.0.0"]
  "a parent command with subcommands, defined signature-only"

  FLAGS:
    verbose; "verbose flag"

  ARGS:
    target : String; "a target"

  SUBCOMMANDS:
    sigSubA;
    sigSubB

def runSigParentCmd (p : sigParentCmd.Parsed) : IO UInt32 := do
  IO.println <| "target=" ++ p.target ++ " verbose=" ++ toString p.verbose
  return 0

def sigParentCmd : Cmd := sigParentCmd.mkCmd runSigParentCmd

-- Run parent command
/--
info: target=foo verbose=true
---
info: 0
-/
#guard_msgs in
#eval sigParentCmd.validate <| "--verbose foo".splitOn " "

-- Run subcommand A (NOOP)
/-- info: 0 -/
#guard_msgs in
#eval sigParentCmd.validate <| "sigSubA".splitOn " "

-- Run subcommand B
/--
info: subB force=true
---
info: 0
-/
#guard_msgs in
#eval sigParentCmd.validate <| "sigSubB --force".splitOn " "

-- Verify help lists subcommands
/--
info: "sigParentCmd [3.0.0]\na parent command with subcommands, defined signature-only\n\nUSAGE:\n    sigParentCmd [SUBCOMMAND] [FLAGS] <target>\n\nFLAGS:\n    -h, --help  Prints this message.\n    --version   Prints the version.\n    --verbose   verbose flag\n\nARGS:\n    target : String  a target\n\nSUBCOMMANDS:\n    sigSubA  subcommand A\n    sigSubB  subcommand B"
-/
#guard_msgs in
#eval sigParentCmd.extendedHelp

end CliDef

end Cli
