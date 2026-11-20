import Cli

open Cli

cli_def installCmd NOOP;
  "installCmd provides an example for a subcommand without flags or arguments that does nothing. \
   Versions can be omitted."

cli_def testCmd NOOP;
  "testCmd provides another example for a subcommand without flags or arguments that does nothing."

cli_def exampleCmd; ["0.0.1"]
  "This string denotes the description of `exampleCmd`."

  FLAGS:
    verbose;                    "Declares a flag `--verbose`. This is the description of the flag."
    i, invert;                  "Declares a flag `--invert` with an associated short alias `-i`."
    o, optimize;                "Declares a flag `--optimize` with an associated short alias `-o`."
    p, priority : Nat;          "Declares a flag `--priority` with an associated short alias `-p` \
                                 that takes an argument of type `Nat`."
    module : ModuleName;        "Declares a flag `--module` that takes an argument of type `ModuleName` \
                                 which can be used to reference Lean modules like `Init.Data.Array` \
                                 or Lean files using a relative path like `Init/Data/Array.lean`."
    "set-paths" : Array String; "Declares a flag `--set-paths` \
                                 that takes an argument of type `Array String`. \
                                 Quotation marks allow the use of hyphens."

  ARGS:
    input : String;      "Declares a positional argument <input> \
                          that takes an argument of type `String`."
    ...outputs : String; "Declares a variable argument <output>... \
                          that takes an arbitrary amount of arguments of type `String`."

  SUBCOMMANDS:
    installCmd;
    testCmd

  -- The EXTENSIONS section denotes features that
  -- were added as an external extension to the library.
  -- `./Cli/Extensions.lean` provides some commonly useful examples.
  EXTENSIONS:
    author "mhuisi";
    defaultValues! #[("priority", "0")]

def runExampleCmd (p : exampleCmd.Parsed) : IO UInt32 := do
  IO.println <| "Input: " ++ p.input
  IO.println <| "Outputs: " ++ toString p.outputs

  if p.verbose then
    IO.println "Flag `--verbose` was set."
  if p.invert then
    IO.println "Flag `--invert` was set."
  if p.optimize then
    IO.println "Flag `--optimize` was set."

  let priority : Nat := p.priority?.getD 0
  IO.println <| "Flag `--priority` always has at least a default value: " ++ toString priority

  if let some moduleName := p.module? then
    IO.println <| s!"Flag `--module` was set to `{moduleName}`."

  if let some setPaths := p.setPaths? then
    IO.println <| toString setPaths

  return 0

def exampleCmd : Cmd := exampleCmd.mkCmd runExampleCmd

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

#eval main <| "-h".splitOn " "
/-
Yields:
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
-/
