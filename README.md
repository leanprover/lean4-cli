# lean4-cli
## Usage
Add the library to your `leanpkg.toml` by appending the following to the file with the git revision that you'd like to install:
```
[dependencies]
Cli = {git = "https://github.com/mhuisi/lean4-cli", rev = "<REVISION>"}
```
Then, run `leanpkg build` and add an `import Cli` declaration to the top of your Lean 4 file. See the sections below on how to use the library.

As of now, if you want to build the binary of your application, you need to link it manually. The following describes how to do so, assuming that your current working directory is the root folder of your Lean 4 project.
```
$ leanpkg build
$ cd ./build/deps/Cli
$ leanpkg build lib
$ cd ../../..
$ leanpkg build bin LINK_OPTS="./build/deps/Cli/build/lib/libCli.a"
```
After executing the above commands, you can find your binary in `./build/bin`.

### Configuration
Commands are configured with a lightweight DSL. The following declarations define a command `exampleCmd` with two subcommands `installCmd` and `testCmd`. `doNothing` and `runExampleCmd` denote the handlers that are called when the command is called and are written out further down below in the **Command Handlers** subsection.

```Lean
open Cli

def installCmd := `[Cli|
  installCmd VIA doNothing; ["0.0.1"]
  "installCmd provides an example for a subcommand without flags or arguments."
]

def testCmd := `[Cli|
  testCmd VIA doNothing; ["0.0.1"]
  "testCmd provides another example for a subcommand without flags or arguments."
]

def exampleCmd : Cmd := `[Cli|
  exampleCmd VIA runExampleCmd; ["0.0.1"]
  "This string denotes the description of `exampleCmd`."

  FLAGS:
    verbose;                    "Declares a flag `--verbose`. This is the description of the flag."
    i, invert;                  "Declares a flag `--invert` with an associated short alias `-i`."
    o, optimize;                "Declares a flag `--optimize` with an associated short alias `-o`."
    p, priority : Nat;          "Declares a flag `--priority` with an associated short alias `-p` " ++
                                "that takes an argument of type `Nat`."
    "set-paths" : Array String; "Declares a flag `--set-paths` " ++
                                "that takes an argument of type `Array Nat`. " ++
                                "Quotation marks allow the use of hyphens."

  ARGS:
    input : String;      "Declares a positional argument <input> " ++
                         "that takes an argument of type `String`."
    ...outputs : String; "Declares a variable argument <output>... " ++
                         "that takes an arbitrary amount of arguments of type `String`."

  SUBCOMMANDS:
    installCmd;
    testCmd

  -- The EXTENSIONS section denotes features that
  -- were added as an external extension to the library.
  -- `./Cli/Extensions.lean` provides some commonly useful examples.
  EXTENSIONS:
    author "mhuisi";
    defaultValues! #[("priority", "0")]
]
```

### Command handlers
The command handlers `doNothing` and `runExampleCmd` demonstrate how to use the parsed user input.

```Lean
def doNothing (p : Parsed) : IO UInt32 :=
  return 0

def runExampleCmd (p : Parsed) : IO UInt32 := do
  let input   : String       := p.positionalArg! "input" |>.as! String
  let outputs : Array String := p.variableArgsAs! String
  IO.println <| "Input: " ++ input
  IO.println <| "Outputs: " ++ toString outputs

  if p.hasFlag "verbose" then
    IO.println "Flag `--verbose` was set."
  if p.hasFlag "invert" then
    IO.println "Flag `--invert` was set."
  if p.hasFlag "optimize" then
    IO.println "Flag `--optimize` was set."

  let priority : Nat := p.flag! "priority" |>.as! Nat
  IO.println <| "Flag `--priority` always has at least a default value: " ++ toString priority

  if let some setPathsFlag := p.flag? "set-paths" then
    IO.println <| toString <| setPathsFlag.as! (Array String)
  return 0
```

### Running the command
Below you can find some simple examples of how to pass user input to the Cli library.

```lean
def main (args : List String) : IO UInt32 :=
  exampleCmd.validate args

#eval main <| "-i -o -p 1 --set-paths=path1,path2,path3 input output1 output2".splitOn " "
/-
Yields:
  Input: input
  Outputs: #[output1, output2]
  Flag `--invert` was set.
  Flag `--optimize` was set.
  Flag `--priority` always has at least a default value: 1
  #[path1, path2, path3]
-/

-- Short parameterless flags can be grouped,
-- short flags with parameters do not need to be separated from
-- the corresponding value.
#eval main <| "-io -p1 input".splitOn " "
/-
Yields:
  Input: input
  Outputs: #[]
  Flag `--invert` was set.
  Flag `--optimize` was set.
  Flag `--priority` always has at least a default value: 1
-/

#eval main <| "--version".splitOn " "
/-
Yields:
  0.0.1
-/
```


### Help
Upon calling `-h`, the above configuration produces the following help.
```
exampleCmd [0.0.1]
mhuisi
This string denotes the description of `exampleCmd`.

USAGE:
    exampleCmd [SUBCOMMAND] [FLAGS] <input> <outputs>...

FLAGS:
    -h, --help                  Prints this message.
    --version                   Prints the version.
    --verbose                   Declares a flag `--verbose`. This is the
                                description of the flag.
    -i, --invert                Declares a flag `--invert` with an associated
                                short alias `-i`.
    -o, --optimize              Declares a flag `--optimize` with an associated
                                short alias `-o`.
    -p, --priority : Nat        Declares a flag `--priority` with an associated
                                short alias `-p` that takes an argument of type
                                `Nat`. [Default: `0`]
    --set-paths : Array String  Declares a flag `--set-paths` that takes an
                                argument of type `Array Nat`. Quotation marks
                                allow the use of hyphens.

ARGS:
    input : String    Declares a positional argument <input> that takes an
                      argument of type `String`.
    outputs : String  Declares a variable argument <output>... that takes an
                      arbitrary amount of arguments of type `String`.

SUBCOMMANDS:
    installCmd  installCmd provides an example for a subcommand without flags or
                arguments.
    testCmd     testCmd provides another example for a subcommand without flags
                or arguments.
```

The full example can be found under `./Cli/Example.lean`.

## Ad Hoc Documentation
This section documents only the most common features of the library. For the full documentation, peek into `./Cli/Basic.lean` and `./Cli/Extensions.lean`! All definitions below live in the `Cli` namespace.

```Lean
-- For many terms below, when the term is an identifier, it is expanded literally.
syntax positionalArg := colGe term " : " term "; " term

syntax variableArg := colGe "..." term " : " term "; " term

syntax flag := colGe term ("," term)? (" : " term)? "; " term

syntax "`[Cli|\n"
    term " VIA " term "; " "[" term "]"
    term
    ("FLAGS:\n" withPosition((flag)*))?
    ("ARGS:\n" withPosition((positionalArg)* (variableArg)?))?
    ("SUBCOMMANDS: " sepBy(term, ";", "; "))?
    ("EXTENSIONS: " sepBy(term, ";", "; "))?
  "\n]" : term
```

```Lean
/--
Validates `args` by `Cmd.process?`ing the input according to `c`.
Note that `args` designates the list `<foo>` in `somebinary <foo>`.
Prints the help or the version of the called (sub)command if the respective flag was passed and
returns `0` for the exit code.
If neither of these flags were passed and processing was successful, the `run` handler of the
called command is executed.
In the case of a processing error, the error is printed and an exit code of `1` is returned.
-/
def validate (c : Cmd) (args : List String) : IO UInt32 := do
  let result := c.process args
  match result with
  | Except.ok (cmd, parsed) =>
    if parsed.hasFlag "help" then
      cmd.printHelp
      return 0
    if parsed.hasFlag "version" then
      cmd.printVersion
      return 0
    cmd.run parsed
  | Except.error (cmd, err) =>
    cmd.printError err
    return 1
```
```Lean
structure Parsed where
  /-- Non-recursive meta-data of the associated command. -/
  cmd            : Cmd.Meta
  /-- Parsed flags. -/
  flags          : Array Parsed.Flag
  /-- Parsed positional arguments. -/
  positionalArgs : Array Parsed.Arg
  /-- Parsed variable arguments. -/
  variableArgs   : Array Parsed.Arg

namespace Parsed
  /-- Finds the parsed flag in `p` with the corresponding `longName`. -/
  def flag?          (p : Parsed) (longName : String) : Option Flag
  /-- Finds the parsed positional argument in `p` with the corresponding `name`. -/
  def positionalArg? (p : Parsed) (name : String)     : Option Arg

  /-- Finds the parsed flag in `p` with the corresponding `longName`. -/
  def flag!          (p : Parsed) (longName : String) : Flag
  /-- Finds the parsed positional argument in `p` with the corresponding `name`. -/
  def positionalArg! (p : Parsed) (name : String)     : Arg

  /-- Checks whether `p` has a parsed flag with the corresponding `longName`. -/
  def hasFlag          (p : Parsed) (longName : String) : Bool
  /-- Checks whether `p` has a positional argument with the corresponding `longName`. -/
  def hasPositionalArg (p : Parsed) (name : String)     : Bool

  /--
  Converts all `p.variableArgs` values to `τ`, which should be the same type
  that was designated in the corresponding `Cli.Arg`.
  Yields `none` if the conversion was unsuccessful, which can only
  happen if `τ` is not the same type as the one designated in the corresponding `Cli.Arg`.
  -/
  def variableArgsAs? (p : Parsed) (τ) [ParseableType τ] : Option (Array τ)

  /--
  Converts all `p.variableArgs` values to `τ`, which should be the same type
  that was designated in the corresponding `Cli.Arg`.
  Panics if the conversion was unsuccessful, which can only
  happen if `τ` is not the same type as the one designated in the corresponding `Cli.Arg`.
  -/
  def variableArgsAs! (p : Parsed) (τ) [Inhabited τ] [ParseableType τ] : Array τ
end Parsed
```
```Lean
namespace Parsed
  /--
  Represents a flag and its parsed value.
  Use `Parsed.Flag.as!` to convert the value to some `ParseableType`.
  -/
  structure Flag where
    /-- Associated flag meta-data. -/
    flag  : Flag
    /-- Parsed value that was validated and conforms to `flag.type`. -/
    value : String

  namespace Flag
    /--
    Converts `f.value` to `τ`, which should be the same type
    that was designated in `f.flag.type`.
    Yields `none` if the conversion was unsuccessful, which can only
    happen if `τ` is not the same type as the one designated in `f.flag.type`.
    -/
    def as? (f : Flag) (τ) [ParseableType τ] : Option τ
    /--
    Converts `f.value` to `τ`, which should be the same type
    that was designated in `f.flag.type`.
    Panics if the conversion was unsuccessful, which can only
    happen if `τ` is not the same type as the one designated in `f.flag.type`.
    -/
    def as! (f : Flag) (τ) [Inhabited τ] [ParseableType τ] : τ
  end Flag

  /--
  Represents an argument and its parsed value.
  Use `Parsed.Arg.as!` to convert the value to some `ParseableType`.
  -/
  structure Arg where
    /-- Associated argument meta-data. -/
    arg   : Arg
    /-- Parsed value that was validated and conforms to `arg.type`. -/
    value : String

  namespace Arg
    /--
    Converts `a.value` to `τ`, which should be the same type
    that was designated in `a.arg.type`.
    Yields `none` if the conversion was unsuccessful, which can only
    happen if `τ` is not the same type as the one designated in `a.arg.type`.
    -/
    def as? (a : Arg) (τ) [ParseableType τ] : Option τ
    /--
    Converts `a.value` to `τ`, which should be the same type
    that was designated in `a.arg.type`.
    Panics if the conversion was unsuccessful, which can only
    happen if `τ` is not the same type as the one designated in `a.arg.type`.
    -/
    def as! (a : Arg) (τ) [Inhabited τ] [ParseableType τ] : τ
  end Arg
end Parsed
```
```Lean
/--
Creates a new command. Adds a `-h, --help` and a `--version` flag.
Updates the `parentNames` of all subcommands.
- `name`:                Name that is displayed in the help.
- `version`:             Version that is displayed in the help and when the version is queried.
- `description`:         Description that is displayed in the help.
- `furtherInformation?`: Information appended to the end of the help. Useful for command extensions.
- `flags`:               Supported flags ("options" in standard terminology).
- `positionalArgs`:      Supported positional arguments ("operands" in standard terminology).
- `variableArg?`:        Variable argument at the end of the positional arguments.
- `run`:                 Handler to run when the command is called and flags/arguments have been successfully processed.
- `subCmds`:             Subcommands.
- `extension?`:          Extension of the Cli library.
-/
def Cmd.mk
  (name                : String)
  (version             : String)
  (description         : String)
  (furtherInformation? : Option String := none)
  (flags               : Array Flag    := #[])
  (positionalArgs      : Array Arg     := #[])
  (variableArg?        : Option Arg    := none)
  (run                 : Parsed → IO UInt32)
  (subCmds             : Array Cmd        := #[])
  (extension?          : Option Extension := none)
  : Cmd
```
