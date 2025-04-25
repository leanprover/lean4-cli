# lean4-cli
This project is maintained by [@mhuisi](https://github.com/mhuisi).

## Usage
See [the documentation of Lake](https://github.com/leanprover/lean4/blob/master/src/lake/README.md).

### Configuration
Commands are configured with a lightweight DSL. The following declarations define a command `exampleCmd` with two subcommands `installCmd` and `testCmd`. `runExampleCmd` denotes a handler that is called when the command is run and is described further down below in the **Command Handlers** subsection.

```Lean
open Cli

def installCmd := `[Cli|
  installCmd NOOP;
  "installCmd provides an example for a subcommand without flags or arguments that does nothing. " ++
  "Versions can be omitted."
]

def testCmd := `[Cli|
  testCmd NOOP;
  "testCmd provides another example for a subcommand without flags or arguments that does nothing."
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
    module : ModuleName;        "Declares a flag `--module` that takes an argument of type `ModuleName` " ++
                                "which be can used to reference Lean modules like `Init.Data.Array` " ++
                                "or Lean files using a relative path like `Init/Data/Array.lean`."
    "set-paths" : Array String; "Declares a flag `--set-paths` " ++
                                "that takes an argument of type `Array String`. " ++
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
The command handler `runExampleCmd` demonstrates how to use the parsed user input.

```Lean
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

  if p.hasFlag "module" then
    let moduleName : ModuleName := p.flag! "module" |>.as! ModuleName
    IO.println <| s!"Flag `--module` was set to `{moduleName}`."

  if let some setPathsFlag := p.flag? "set-paths" then
    IO.println <| toString <| setPathsFlag.as! (Array String)
  return 0
```

### Running the command
Below you can find some simple examples of how to pass user input to the Cli library.

```lean
def main (args : List String) : IO UInt32 :=
  exampleCmd.validate args

#eval main <| "-i -o -p 1 --module=Lean.Compiler --set-paths=path1,path2,path3 input output1 output2".splitOn " "
/-
Yields:
  Input: input
  Outputs: #[output1, output2]
  Flag `--invert` was set.
  Flag `--optimize` was set.
  Flag `--priority` always has at least a default value: 1
  Flag `--module` was set to `Lean.Compiler`.
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
    --module : ModuleName       Declares a flag `--module` that takes an
                                argument of type `ModuleName` which can be used
                                to reference Lean modules like `Init.Data.Array`
                                or Lean files using a relative path like
                                `Init/Data/Array.lean`.
    --set-paths : Array String  Declares a flag `--set-paths` that takes an
                                argument of type `Array String`. Quotation marks
                                allow the use of hyphens.

ARGS:
    input : String    Declares a positional argument <input> that takes an
                      argument of type `String`.
    outputs : String  Declares a variable argument <output>... that takes an
                      arbitrary amount of arguments of type `String`.

SUBCOMMANDS:
    installCmd  installCmd provides an example for a subcommand without flags or
                arguments that does nothing. Versions can be omitted.
    testCmd     testCmd provides another example for a subcommand without flags
                or arguments that does nothing.
```

The full example can be found under `./Cli/Example.lean`.

## Ad Hoc Documentation
This section documents only the most common features of the library. For the full documentation, peek into `./Cli/Basic.lean` and `./Cli/Extensions.lean`! All definitions below live in the `Cli` namespace.

```Lean
-- In a literalIdent, identifiers are expanded as `String`s.
syntax literalIdent := term

syntax runFun := (" VIA " term) <|> " NOOP"

syntax positionalArg := colGe literalIdent " : " term "; " term

syntax variableArg := colGe "..." literalIdent " : " term "; " term

syntax flag := colGe literalIdent ("," literalIdent)? (" : " term)? "; " term

syntax "`[Cli|\n"
    literalIdent runFun "; " ("[" term "]")?
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
In the case of a processing error, the error is printed to stderr and an exit code of `1` is returned.
-/
def validate (c : Cmd) (args : List String) : IO UInt32 := do
  let result := c.process args
  match result with
  | .ok (cmd, parsed) =>
    if parsed.hasFlag "help" then
      parsed.printHelp
      return 0
    if parsed.cmd.meta.hasVersion ∧ parsed.hasFlag "version" then
      parsed.printVersion!
      return 0
    cmd.run parsed
  | .error (cmd, err) =>
    cmd.printError err
    return 1
```
```Lean
  /-- Represents parsed user input data. -/
  structure Parsed where
    /-- Recursive meta-data of the associated command. -/
    cmd            : Parsed.Cmd
    /-- Parent of the associated command. -/
    parent?        : Option Parsed.Cmd
    /-- Parsed flags. -/
    flags          : Array Parsed.Flag
    /-- Parsed positional arguments. -/
    positionalArgs : Array Parsed.Arg
    /-- Parsed variable arguments. -/
    variableArgs   : Array Parsed.Arg
    deriving Inhabited

namespace Parsed
  /-- Parent of the associated command. -/
  def parent! (p : Parsed) : Parsed.Cmd

  /-- Checks whether the associated command has a parent, i.e. whether it is not the root command. -/
  def hasParent (p : Parsed) : Bool

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
Creates a new command. Adds a `-h, --help` and a `--version` flag if a version is designated.
Updates the `parentNames` of all subcommands.
- `name`:                Name that is displayed in the help.
- `version?`:            Version that is displayed in the help and when the version is queried.
- `description`:         Description that is displayed in the help.
- `furtherInformation?`: Information appended to the end of the help. Useful for command extensions.
- `flags`:               Supported flags ("options" in standard terminology).
- `positionalArgs`:      Supported positional arguments ("operands" in standard terminology).
- `variableArg?`:        Variable argument at the end of the positional arguments.
- `run`:                 Handler to run when the command is called and flags/arguments have been successfully processed.
- `subCmds`:             Subcommands.
- `extension?`:          Extension of the Cli library.
-/
def mk
  (name                : String)
  (version?            : Option String)
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
