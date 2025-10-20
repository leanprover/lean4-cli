import Cli.Basic
import Cli.Extensions

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

end Cli
