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
  #eval
    (testCmd.processParsed "foo 1 -ta")
    == "cmd: testcommand; flags: #[--typed1=a, --level-param=0]; positionalArgs: #[<input1=foo>, <input2=1>]; variableArgs: #[]"

  #eval
    (testCmd.processParsed "-h")
    == "cmd: testcommand; flags: #[--help, --level-param=0]; positionalArgs: #[]; variableArgs: #[]"

  #eval
    (testCmd.processParsed "--version")
    == "cmd: testcommand; flags: #[--version, --level-param=0]; positionalArgs: #[]; variableArgs: #[]"

  #eval
    (testCmd.processParsed "foo --verbose -p-n 2 1,2,3 1 -xnx 2 --typed1=foo 3")
    == "cmd: testcommand; flags: #[--verbose, --level-param=2, --unknown2, --unknown1, --typed1=foo]; positionalArgs: #[<input1=foo>, <input2=1,2,3>]; variableArgs: #[<outputs=1>, <outputs=2>, <outputs=3>]"

  #eval
    (testCmd.processParsed "foo -xny 1 -t 3")
    == "cmd: testcommand; flags: #[--unknown1, --unknown3, --typed1=3, --level-param=0]; positionalArgs: #[<input1=foo>, <input2=1>]; variableArgs: #[]"

  #eval
    (testCmd.processParsed "-t 3 -- --input 2")
    == "cmd: testcommand; flags: #[--typed1=3, --level-param=0]; positionalArgs: #[<input1=--input>, <input2=2>]; variableArgs: #[]"

  #eval
    (testCmd.processParsed "-t1 - 2")
    == "cmd: testcommand; flags: #[--typed1=1, --level-param=0]; positionalArgs: #[<input1=->, <input2=2>]; variableArgs: #[]"

  #eval
    (testCmd.processParsed "-ty -t1 foo 1,2")
    == "cmd: testcommand; flags: #[--typed2, --typed1=1, --level-param=0]; positionalArgs: #[<input1=foo>, <input2=1,2>]; variableArgs: #[]"

  #eval
    (testCmd.processParsed "testsubcommand1 -- testsubsubcommand")
    == "cmd: testcommand testsubcommand1; flags: #[]; positionalArgs: #[<city-location=testsubsubcommand>]; variableArgs: #[]"

  #eval
    (testCmd.processParsed "testsubcommand1 --launch-the-nukes x")
    == "cmd: testcommand testsubcommand1; flags: #[--launch-the-nukes]; positionalArgs: #[<city-location=x>]; variableArgs: #[]"

  #eval
    (testCmd.processParsed "testsubcommand1 -- --launch-the-nukes")
    == "cmd: testcommand testsubcommand1; flags: #[]; positionalArgs: #[<city-location=--launch-the-nukes>]; variableArgs: #[]"

  #eval
    (testCmd.processParsed "testsubcommand1 testsubsubcommand")
    == "cmd: testcommand testsubcommand1 testsubsubcommand; flags: #[]; positionalArgs: #[]; variableArgs: #[]"

  #eval (testCmd.processParsed "testsubcommand2 --run asdf,geh")
    == "cmd: testcommand testsubcommand2; flags: #[--run]; positionalArgs: #[<ominous-input=asdf,geh>]; variableArgs: #[]"
end ValidInputs

section InvalidInputs
  #eval
    (testCmd.processParsed "")
    == "Missing positional argument `<input1>.`"

  #eval
    (testCmd.processParsed "foo")
    == "Missing positional argument `<input2>.`"

  #eval
    (testCmd.processParsed "foo asdf")
    == "Invalid type of argument `asdf` for positional argument `<input2 : Array Nat>`."

  #eval
    (testCmd.processParsed "foo 1,2,3")
    == "Missing required flag `--typed1`."

  #eval
    (testCmd.processParsed "foo 1,2,3 -t")
    == "Missing argument for flag `-t`."

  #eval
    (testCmd.processParsed "foo 1,2,3 -t1 --level-param=")
    == "Invalid type of argument `` for flag `--level-param : Nat`."

  #eval
    (testCmd.processParsed "foo 1,2,3 -t1 -p-n=")
    == "Invalid type of argument `` for flag `-p-n : Nat`."

  #eval
    (testCmd.processParsed "foo 1,2,3 -t1 --asdf")
    == "Unknown flag `--asdf`."

  #eval
    (testCmd.processParsed "foo 1,2,3 -t1 -t2")
    == "Duplicate flag `-t` (`--typed1`)."

  #eval
    (testCmd.processParsed "foo 1,2,3 -t1 --typed1=2")
    == "Duplicate flag `--typed1` (`-t`)."

  #eval
    (testCmd.processParsed "foo 1,2,3 --typed12")
    == "Unknown flag `--typed12`."

  #eval
    (testCmd.processParsed "foo 1,2,3 -t1 -x=1")
    == "Redundant argument `1` for flag `-x` that takes no arguments."

  #eval
    (testCmd.processParsed "foo 1,2,3 -t1 bar")
    == "Invalid type of argument `bar` for variable argument `<outputs : Nat>...`."

  #eval
    (testCmd.processParsed "foo 1,2,3 -t1 -xxn=1")
    == "Unknown flag `-xxn`."

  #eval
    (testCmd.processParsed "foo 1,2,3 --t=1")
    == "Unknown flag `--t`."

  #eval
    (testCmd.processParsed "testsubcommand1 asdf geh")
    == "Redundant positional argument `geh`."
end InvalidInputs

section Info
  /-
  testcommand [0.0.0]
  some short description that happens to be much longer than necessary and hence
  needs to be wrapped to fit into an 80 character width limit

  USAGE:
      testcommand [SUBCOMMAND] [FLAGS] <input1> <input2> <outputs>...

  FLAGS:
      -h, --help                 Prints this message.
      --version                  Prints the version.
      --verbose                  a very verbose flag description that also needs
                                 to be wrapped to fit into an 80 character width
                                 limit
      -x, --unknown1             this flag has a short name
      -xn, --unknown2            short names do not need to be prefix-free
      -ny, --unknown3            -xny will parse as -x -ny and not fail to parse
                                 as -xn -y
      -t, --typed1 : String      flags can have typed parameters
      -ty, --typed2              -ty parsed as --typed2, not -t=y
      -p-n, --level-param : Nat  hyphens work, too

  ARGS:
      input1 : String     another very verbose description that also needs to be
                          wrapped to fit into an 80 character width limit
      input2 : Array Nat  arrays!
      outputs : Nat       varargs!

  SUBCOMMANDS:
      testsubcommand1  a properly short description
      testsubcommand2  does not do anything interesting
  -/
  #eval
    testCmd.help
    == "testcommand [0.0.0]\nsome short description that happens to be much longer than necessary and hence\nneeds to be wrapped to fit into an 80 character width limit\n\nUSAGE:\n    testcommand [SUBCOMMAND] [FLAGS] <input1> <input2> <outputs>...\n\nFLAGS:\n    -h, --help                 Prints this message.\n    --version                  Prints the version.\n    --verbose                  a very verbose flag description that also needs\n                               to be wrapped to fit into an 80 character width\n                               limit\n    -x, --unknown1             this flag has a short name\n    -xn, --unknown2            short names do not need to be prefix-free\n    -ny, --unknown3            -xny will parse as -x -ny and not fail to parse\n                               as -xn -y\n    -t, --typed1 : String      flags can have typed parameters\n    -ty, --typed2              -ty parsed as --typed2, not -t=y\n    -p-n, --level-param : Nat  hyphens work, too\n\nARGS:\n    input1 : String     another very verbose description that also needs to be\n                        wrapped to fit into an 80 character width limit\n    input2 : Array Nat  arrays!\n    outputs : Nat       varargs!\n\nSUBCOMMANDS:\n    testsubcommand1  a properly short description\n    testsubcommand2  does not do anything interesting"

  #eval
    testCmd.version!
    == "0.0.0"

  /-
  some exceedingly long error that needs to be wrapped to fit within an 80
  character width limit. none of our errors are really that long, but flag names
  might be.
  Run `testcommand -h` for further information.
  -/
  #eval
    (testCmd.error "some exceedingly long error that needs to be wrapped to fit within an 80 character width limit. none of our errors are really that long, but flag names might be.")
    == "some exceedingly long error that needs to be wrapped to fit within an 80\ncharacter width limit. none of our errors are really that long, but flag names\nmight be.\nRun `testcommand -h` for further information."

  /-
  testsubcommand2 [0.0.-1]
  does not do anything interesting

  USAGE:
      testsubcommand2 [FLAGS] <ominous-input>

  FLAGS:
      -h, --help  Prints this message.
      --version   Prints the version.
      -r, --run   really, this does not do anything. trust me.

  ARGS:
      ominous-input : Array String  what could this be for?
  -/
  #eval
    testSubCmd2.help
    == "testsubcommand2 [0.0.-1]\ndoes not do anything interesting\n\nUSAGE:\n    testsubcommand2 [FLAGS] <ominous-input>\n\nFLAGS:\n    -h, --help  Prints this message.\n    --version   Prints the version.\n    -r, --run   really, this does not do anything. trust me.\n\nARGS:\n    ominous-input : Array String  what could this be for?"

  /-
  testcommand testsubcommand2 [0.0.-1]
  does not do anything interesting

  USAGE:
      testcommand testsubcommand2 [FLAGS] <ominous-input>

  FLAGS:
      -h, --help  Prints this message.
      --version   Prints the version.
      -r, --run   really, this does not do anything. trust me.

  ARGS:
      ominous-input : Array String  what could this be for?
  -/
  #eval
    (testCmd.subCmd! "testsubcommand2").help
    == "testcommand testsubcommand2 [0.0.-1]\ndoes not do anything interesting\n\nUSAGE:\n    testcommand testsubcommand2 [FLAGS] <ominous-input>\n\nFLAGS:\n    -h, --help  Prints this message.\n    --version   Prints the version.\n    -r, --run   really, this does not do anything. trust me.\n\nARGS:\n    ominous-input : Array String  what could this be for?"

  /-
  testcommand testsubcommand1 testsubsubcommand
  does this even do anything?

  USAGE:
      testcommand testsubcommand1 testsubsubcommand [FLAGS]

  FLAGS:
      -h, --help  Prints this message.
  -/
  #eval
    (testCmd.subCmd! "testsubcommand1" |>.subCmd! "testsubsubcommand").help
    == "testcommand testsubcommand1 testsubsubcommand\ndoes this even do anything?\n\nUSAGE:\n    testcommand testsubcommand1 testsubsubcommand [FLAGS]\n\nFLAGS:\n    -h, --help  Prints this message."

  /-
  testcommand [0.0.0]
  mhuisi
  some short description that happens to be much longer than necessary and hence
  needs to be wrapped to fit into an 80 character width limit

  USAGE:
      testcommand [SUBCOMMAND] [FLAGS] <input1> <input2> <outputs>...

  FLAGS:
      -h, --help                 Prints this message.
      --version                  Prints the version.
      --verbose                  a very verbose flag description that also needs
                                 to be wrapped to fit into an 80 character width
                                 limit
      -x, --unknown1             this flag has a short name
      -xn, --unknown2            short names do not need to be prefix-free
      -ny, --unknown3            -xny will parse as -x -ny and not fail to parse
                                 as -xn -y
      -t, --typed1 : String      [Required] flags can have typed parameters
      -ty, --typed2              -ty parsed as --typed2, not -t=y
      -p-n, --level-param : Nat  hyphens work, too [Default: `0`]

  ARGS:
      input1 : String     another very verbose description that also needs to be
                          wrapped to fit into an 80 character width limit
      input2 : Array Nat  arrays!
      outputs : Nat       varargs!

  SUBCOMMANDS:
      testsubcommand1  a properly short description
      testsubcommand2  does not do anything interesting

  DESCRIPTION:
      this could be really long, but i'm too lazy to type it out.
  -/
  #eval
    (testCmd.update' (meta := testCmd.extension!.extendMeta testCmd.meta)).help
    == "testcommand [0.0.0]\nmhuisi\nsome short description that happens to be much longer than necessary and hence\nneeds to be wrapped to fit into an 80 character width limit\n\nUSAGE:\n    testcommand [SUBCOMMAND] [FLAGS] <input1> <input2> <outputs>...\n\nFLAGS:\n    -h, --help                 Prints this message.\n    --version                  Prints the version.\n    --verbose                  a very verbose flag description that also needs\n                               to be wrapped to fit into an 80 character width\n                               limit\n    -x, --unknown1             this flag has a short name\n    -xn, --unknown2            short names do not need to be prefix-free\n    -ny, --unknown3            -xny will parse as -x -ny and not fail to parse\n                               as -xn -y\n    -t, --typed1 : String      [Required] flags can have typed parameters\n    -ty, --typed2              -ty parsed as --typed2, not -t=y\n    -p-n, --level-param : Nat  hyphens work, too [Default: `0`]\n\nARGS:\n    input1 : String     another very verbose description that also needs to be\n                        wrapped to fit into an 80 character width limit\n    input2 : Array Nat  arrays!\n    outputs : Nat       varargs!\n\nSUBCOMMANDS:\n    testsubcommand1  a properly short description\n    testsubcommand2  does not do anything interesting\n\nDESCRIPTION:\n    this could be really long, but i'm too lazy to type it out."
end Info

end Cli