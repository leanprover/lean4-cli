import Lean.Data.RBTree

section Utils
  /--
  Matches the lengths of lists `a` and `b` by filling the shorter one with
  `unit` elements at the tail end. The matched lists are returned in the same order
  as they were passed.
  -/
  def List.matchLength (a : List α) (b : List α) (unit : α) : List α × List α :=
    if a.length < b.length then
      (a ++ .replicate (b.length - a.length) unit, b)
    else
      (a, b ++ .replicate (a.length - b.length) unit)

  namespace Array
    def join (xss : Array (Array α)) : Array α := Id.run do
      let mut r := #[]
      for xs in xss do
        r := r ++ xs
      return r

    def flatMap (f : α → Array β) (xs : Array α) : Array β :=
      xs.map f |>.join
  end Array

  namespace String
    /--
    Inserts newlines `\n` into `s` after every `maxWidth` characters so that the result
    contains no line longer than `maxWidth` characters. Retains newlines `\n` in `s`.
    Yields `none` if `maxWidth = 0`.
    -/
    def wrapAt? (s : String) (maxWidth : Nat) : Option String := Id.run do
      if maxWidth = 0 then
        return none
      let lines := s.splitOn "\n" |>.map fun line => Id.run do
        let resultLineCount :=
          if line.length % maxWidth = 0 then
            line.length / maxWidth
          else
            line.length / maxWidth + 1
        let mut line := line
        let mut result := #[]
        for _ in [:resultLineCount] do
          result := result.push <| line.take maxWidth
          line := line.drop maxWidth
        return "\n".intercalate result.toList
      return "\n".intercalate lines

    /--
    Inserts newlines `\n` into `s` after every `maxWidth` characters so that the result
    contains no line longer than `maxWidth` characters. Retains newlines `\n` in `s`.
    Panics if `maxWidth = 0`.
    -/
    def wrapAt! (s : String) (maxWidth : Nat) : String :=
      s.wrapAt? maxWidth |>.get!

    /--
    Deletes all trailing spaces at the end of every line, as seperated by `\n`.
    -/
    def trimTrailingSpaces (s : String) : String :=
      s.splitOn "\n" |>.map (·.dropRightWhile (· = ' ')) |> "\n".intercalate

    /--
    Inserts newlines `\n` into `s` after every `maxWidth` characters before words
    that would otherwise be broken apart by the newline. The result will contain
    no line longer than `maxWidth` characters and no words except those already
    longer than `maxWidth` characters will be broken up in the middle.
    Removes trailing whitespace before each inserted newline. Retains newlines `\n` in `s`.
    Returns `none` if `maxWidth = 0`.
    -/
    def wrapWordsAt? (s : String) (maxWidth : Nat) : Option String := Id.run do
      if maxWidth = 0 then
        return none
      let wordWrappedLines : List String := s.splitOn "\n" |>.map fun s => Id.run do
        let words                : Array String := s.splitOn.toArray
        let mut currentLineWidth : Nat          := 0
        let mut result           : Array String := #[]
        for i in [:words.size] do
          let w := words[i]!
          -- `w = ""`: we will never insert a newline on a space. this has the effect
          -- of inserting the newline only after a bunch of trailing whitespace, which we remove later.
          -- similarly, `currentLineWidth + w.length ≤ maxWidth` does not count the space after `w` so that
          -- we do not insert a newline before a word that fits except for a trailing space.
          if w = "" ∨ currentLineWidth + w.length ≤ maxWidth then
            -- `+ 1` because we count the space after `w` to accurately keep track of spaces.
            currentLineWidth := currentLineWidth + w.length + 1
            result := result.push w
            continue
          -- if `w` is the first proper word, this will insert a `\n` before the text, which we remove later.
          -- `w.wrapAt! maxWidth` ensures that our new line is not already too large.
          let wordOnNewLine := "\n" ++ w.wrapAt! maxWidth
          result := result.push wordOnNewLine
          let wrappedLines : Array String := wordOnNewLine.splitOn "\n" |>.toArray
          currentLineWidth := wrappedLines[wrappedLines.size - 1]!.length + 1
        return " ".intercalate result.toList
      let trimmed : List String :=
        wordWrappedLines.map trimTrailingSpaces |>.map fun line => Id.run do
          if line = "" then
            return ""
          if line.get! 0 ≠ '\n' then
            return line
          return line.drop 1
      return "\n".intercalate trimmed

    /--
    Inserts newlines `\n` into `s` after every `maxWidth` characters before words
    that would otherwise be broken apart by the newline. The result will contain
    no line longer than `maxWidth` characters and no words except those already
    longer than `maxWidth` characters will be broken up in the middle.
    Removes trailing whitespace before each inserted newline. Retains newlines `\n` in `s`.
    Panics if `maxWidth = 0`.
    -/
    def wrapWordsAt! (s : String) (maxWidth : Nat) : String :=
      s.wrapWordsAt? maxWidth |>.get!

    /--
    Inserts `n` spaces before each line as seperated by `\n` in `s`.
    Does not indent `s = ""`.
    -/
    def indent (s : String) (n : Nat := 4) : String := Id.run do
      if s = "" then
        return ""
      return s.splitOn "\n" |>.map ("".pushn ' ' n ++ ·) |> "\n".intercalate

    /--
    Intercalates elements `≠ ""` in `xs` using `sep`.
    -/
    def optJoin (xs : Array String) (sep : String) : String :=
      xs.filter (· ≠ "") |>.toList |> sep.intercalate
  end String

  namespace Array
    /--
    Renders `rows` as a table with a maximum row width of `maxWidth` and a margin of `margin`
    between the columns. Wraps words according to `String.wrapWordsAt?` to ensure that no
    rendered row of the table is longer than `maxWidth`.
    Returns `none` if `(maxWidth-margin)/2` < 1, i.e. if there is not enough space for
    text in both columns.
    -/
    def renderTable? (rows : Array (String × String)) (maxWidth : Nat) (margin : Nat := 2)
      : Option String := Id.run do
      if rows.isEmpty then
        return ""
      let rightColumnWidth := rows.map (·.2.length) |>.getMax? (· < ·) |>.get!
      let minRightColumnWidth := Nat.min rightColumnWidth <| Nat.max ((maxWidth-margin)/2) 1
      if maxWidth - margin - minRightColumnWidth < 1 then
        return none
      let rows : Array (List String × String) := rows.map fun (left, right) =>
        (maxWidth - margin - minRightColumnWidth |> left.wrapWordsAt! |>.splitOn "\n", right)
      let leftColumnWidth :=
        rows.flatMap (·.1.map (·.length) |>.toArray)
          |>.getMax? (· < ·)
          |>.get!
      let leftColumnWidth := leftColumnWidth + margin
      let rows : Array (List String × List String) := rows.map fun (left, right) =>
        (left, maxWidth - leftColumnWidth |> right.wrapWordsAt! |>.splitOn "\n")
      let rows : Array (String × String) := rows.flatMap fun (left, right) =>
        let (left, right) : List String × List String := left.matchLength right ""
        left.zip right |>.toArray
      let rows : Array String := rows.map fun (left, right) =>
        if right = "" then
          left
        else
          let padding := "".pushn ' ' (leftColumnWidth - left.length)
          left ++ padding ++ right
      return "\n".intercalate rows.toList

    /--
    Renders `rows` as a table with a maximum row width of `maxWidth` and a margin of `margin`
    between the columns. Wraps words according to `String.wrapWordsAt?` to ensure that no
    rendered row of the table is longer than `maxWidth`.
    Panics if `(maxWidth-margin)/2` < 1, i.e. if there is not enough space for
    text in both columns.
    -/
    def renderTable! (rows : Array (String × String)) (maxWidth : Nat) (margin : Nat := 2)
      : String :=
      rows.renderTable? maxWidth margin |>.get!
  end Array

  namespace Option
    def join (x : Option (Option α)) : Option α := do ←x

    /--
    Returns `""` if the passed `Option` is `none`, otherwise
    converts the contained value using a `ToString` instance.
    -/
    def optStr [ToString α] : Option α → String
      | none   => ""
      | some v => toString v
  end Option
end Utils

namespace Cli

section Configuration
  /--
  Represents a type that can be parsed to a string and the corresponding name of the type.
  Used for converting parsed user input to the respective expected type.
  -/
  class ParseableType (τ) where
    /-- Name of the type, used when displaying the help. -/
    name   : String
    /-- Function to parse a value to the type that returns `none` if it cannot be parsed. -/
    parse? : String → Option τ

  instance : ParseableType Unit where
    name     := "Unit"
    parse? _ := ()

  instance : ParseableType Bool where
    name := "Bool"
    parse?
    | "true"  => true
    | "false" => false
    | _       => none

  instance : ParseableType String where
    name     := "String"
    parse? s := s

  instance : ParseableType Nat where
    name := "Nat"
    parse?
      -- HACK: temporary workaround for minor bug in String.toNat?
      | "" => none
      | s  => s.toNat?

  instance : ParseableType Int where
    name := "Int"
    parse?
      -- HACK: temporary workaround for minor bug in String.toInt?
      | "" => none
      | s  => s.toInt?

  instance [inst : ParseableType α] : ParseableType (Array α) where
    name :=
      if inst.name.contains ' ' then
        s!"Array ({inst.name})"
      else
        s!"Array {inst.name}"
    parse?
    | "" => some #[]
    | s  => do return (← s.splitOn "," |>.mapM inst.parse?).toArray

  /--
  Represents the type of some flag or argument parameter. Typically coerced from types with
  `ParseableType` instances such that `isValid := (ParseableType.parse? · |>.isSome)`.
  -/
  structure ParamType where
    /-- Name of the type, used when displaying the help. -/
    name    : String
    /-- Function to check whether a value conforms to the type. -/
    isValid : String → Bool
    deriving Inhabited

  instance : BEq ParamType where
    beq a b := a.name == b.name

  instance : Repr ParamType where
    reprPrec p _ := p.name

  instance [inst : ParseableType τ] : CoeDep Type τ ParamType where
    coe := ⟨inst.name τ, (inst.parse? · |>.isSome)⟩

  /--
  Represents a flag, usually known as "option" in standard terminology.
  -/
  structure Flag where
    /-- Designates `x` in `-x`. -/
    shortName?  : Option String := none
    /-- Designates `x` in `--x`. -/
    longName    : String
    /-- Description that is displayed in the help. -/
    description : String
    /--
    Type according to which the parameter is validated.
    `Unit` is used to designate flags without a parameter.
    -/
    type        : ParamType
    deriving Inhabited, BEq, Repr

  namespace Flag
    /--
    Initializes a flag without a parameter. Parameterless flags are
    designated by the `Unit` type.
    - `shortName?`:  Designates `x` in `-x`.
    - `longName`:    Designates `x` in `--x`.
    - `description`: Description that is displayed in the help.
    -/
    def paramless
      (shortName?  : Option String := none)
      (longName    : String)
      (description : String)
      : Flag := {
        shortName?  := shortName?
        longName    := longName
        description := description
        type        := Unit
      }

    /-- Designates `x` in `-x`. -/
    def shortName!   (f : Flag) : String := f.shortName?.get!
    /-- Checks whether `f` has an associated short flag name `x` in `-x`. -/
    def hasShortName (f : Flag) : Bool   := f.shortName?.isSome

    /-- Checks whether `f` has a `Unit` type. -/
    def isParamless  (f : Flag) : Bool := f.type == Unit
  end Flag

  /--
  Represents an argument (either positional or variable),
  usually known as "operand" in standard terminology
  -/
  structure Arg where
    /- Name that is displayed in the help. -/
    name        : String
    /- Description that is displayed in the help. -/
    description : String
    /- Description that is displayed in the help. -/
    type        : ParamType
    deriving Inhabited, BEq, Repr

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
      deriving Inhabited, BEq, Repr

    instance : ToString Flag where
      toString f := s!"--{f.flag.longName}" ++ (if f.value ≠ "" then s!"={f.value}" else "")

    namespace Flag
      /--
      Converts `f.value` to `τ`, which should be the same type
      that was designated in `f.flag.type`.
      Yields `none` if the conversion was unsuccessful, which can only
      happen if `τ` is not the same type as the one designated in `f.flag.type`.
      -/
      def as? (f : Flag) (τ) [ParseableType τ] : Option τ :=
        ParseableType.parse? f.value
      /--
      Converts `f.value` to `τ`, which should be the same type
      that was designated in `f.flag.type`.
      Panics if the conversion was unsuccessful, which can only
      happen if `τ` is not the same type as the one designated in `f.flag.type`.
      -/
      def as! (f : Flag) (τ) [Inhabited τ] [ParseableType τ] : τ :=
        f.as? τ |>.get!
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
      deriving Inhabited, BEq, Repr

    instance : ToString Arg where
      toString a := s!"<{a.arg.name}={a.value}>"

    namespace Arg
      /--
      Converts `a.value` to `τ`, which should be the same type
      that was designated in `a.arg.type`.
      Yields `none` if the conversion was unsuccessful, which can only
      happen if `τ` is not the same type as the one designated in `a.arg.type`.
      -/
      def as? (a : Arg) (τ) [ParseableType τ] : Option τ :=
        ParseableType.parse? a.value
      /--
      Converts `a.value` to `τ`, which should be the same type
      that was designated in `a.arg.type`.
      Panics if the conversion was unsuccessful, which can only
      happen if `τ` is not the same type as the one designated in `a.arg.type`.
      -/
      def as! (a : Arg) (τ) [Inhabited τ] [ParseableType τ] : τ :=
        a.as? τ |>.get!
    end Arg
  end Parsed

  /-- Represents all the non-recursive meta-data of a command. -/
  structure Cmd.Meta where
    /-- Name that is displayed in the help. -/
    name                : String
    /--
    Names of the commands of which this command is a subcommand.
    Corresponds to the path from the root to this command.
    -/
    parentNames         : Array String
    /-- Version of the command that is displayed in the help and when the version is queried. -/
    version?            : Option String
    /-- Description that is displayed in the help. -/
    description         : String
    /-- Information appended to the end of the help. Useful for command extensions. -/
    furtherInformation? : Option String := none
    /-- Supported flags ("options" in standard terminology). -/
    flags               : Array Flag
    /-- Supported positional arguments ("operands" in standard terminology). -/
    positionalArgs      : Array Arg
    /-- Variable argument after the end of the positional arguments. -/
    variableArg?        : Option Arg
    deriving Inhabited, BEq, Repr

  namespace Cmd.Meta
    /-- Full name from the root to this command, including the name of the command itself. -/
    def fullName (m : Meta) : String := m.parentNames.push m.name |>.toList |> " ".intercalate

    /-- Version of the command that is displayed in the help and when the version is queried. -/
    def version!            (m : Meta) : String := m.version?.get!
    /-- Information appended to the end of the help. Useful for command extensions. -/
    def furtherInformation! (m : Meta) : String := m.furtherInformation?.get!
    /-- Variable argument after the end of the positional arguments. -/
    def variableArg!        (m : Meta) : Arg    := m.variableArg?.get!

    /-- Checks whether `m` has a version. -/
    def hasVersion            (m : Meta) : Bool := m.version?.isSome
    /-- Checks whether `m` has information appended to the end of the help. -/
    def hasFurtherInformation (m : Meta) : Bool := m.furtherInformation?.isSome
    /-- Checks whether `m` supports a variable argument. -/
    def hasVariableArg        (m : Meta) : Bool := m.variableArg?.isSome

    /-- Finds the flag in `m` with the corresponding `longName`. -/
    def flag?          (m : Meta) (longName : String) : Option Flag   := m.flags.find? (·.longName = longName)
    /-- Finds the positional argument in `m` with the corresponding `name`. -/
    def positionalArg? (m : Meta) (name     : String) : Option Arg    := m.positionalArgs.find? (·.name = name)

    /-- Finds the flag in `m` with the corresponding `longName`. -/
    def flag!          (m : Meta) (longName : String) : Flag   := m.flag? longName |>.get!
    /-- Finds the positional argument in `m` with the corresponding `name`. -/
    def positionalArg! (m : Meta) (name     : String) : Arg    := m.positionalArg? name |>.get!

    /-- Checks whether `m` contains a flag with the corresponding `longName`. -/
    def hasFlag          (m : Meta) (longName : String) : Bool := m.flag? longName |>.isSome
    /-- Checks whether `m` contains a positional argument with the corresponding `name`. -/
    def hasPositionalArg (m : Meta) (name     : String) : Bool := m.positionalArg? name |>.isSome

    /-- Finds the flag in `m` with the corresponding `shortName`. -/
    def flagByShortName? (m : Meta) (name : String) : Option Flag :=
      m.flags.findSome? fun flag => do
        let shortName ← flag.shortName?
        guard <| shortName = name
        return flag

    /-- Finds the flag in `m` with the corresponding `shortName`. -/
    def flagByShortName! (m : Meta) (name : String) : Flag := m.flagByShortName? name |>.get!

    /-- Checks whether `m` has a flag with the corresponding `shortName`. -/
    def hasFlagByShortName (m : Meta) (name : String) : Bool := m.flagByShortName? name |>.isSome

    /-- 
    Adds help (`-h, --help`) and version (`--version`) flags to `m`. Does not add
    a version flag if `m` does not designate a version. 
    -/
    def addHelpAndVersionFlags (m : Meta) : Meta := Id.run do
      let helpFlag := .paramless
        (shortName?  := "h")
        (longName    := "help")
        (description := "Prints this message.")
      let mut fixedFlags := #[helpFlag]
      if m.hasVersion then
        let versionFlag := .paramless
          (longName    := "version")
          (description := "Prints the version.")
        fixedFlags := fixedFlags.push versionFlag
      { m with flags := fixedFlags ++ m.flags }
  end Cmd.Meta

  /-- 
  Represents a recursive variant of `Cmd.Meta` that is used in `Parsed`
  to replicate the recursive subcommand structure of a command
  without referring to the command itself.
  -/
  inductive Parsed.Cmd
    | init
      (meta    : Cmd.Meta)
      (subCmds : Array Parsed.Cmd)
    deriving Inhabited

  namespace Parsed.Cmd
    /-- Meta of this command. -/
    def meta    : Parsed.Cmd → Cmd.Meta         | init v _ => v
    /-- Subcommands. -/
    def subCmds : Parsed.Cmd → Array Parsed.Cmd | init _ v => v

    /-- Finds the subcommand in `c` with the corresponding `name`. -/
    def subCmd? (c : Parsed.Cmd) (name : String) : Option Parsed.Cmd := c.subCmds.find? (·.meta.name = name)

    /-- Finds the subcommand in `c` with the corresponding `name`. -/
    def subCmd! (c : Parsed.Cmd) (name : String) : Parsed.Cmd := c.subCmd? name |>.get!

    /-- Checks whether `c` contains a subcommand with the corresponding `name`. -/
    def hasSubCmd (c : Parsed.Cmd) (name : String) : Bool := c.subCmd? name |>.isSome
  end Parsed.Cmd

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
    def parent! (p : Parsed) : Parsed.Cmd := p.parent?.get!

    /-- Checks whether the associated command has a parent, i.e. whether it is not the root command. -/
    def hasParent (p : Parsed) : Bool := p.parent?.isSome

    /-- Finds the parsed flag in `p` with the corresponding `longName`. -/
    def flag?          (p : Parsed) (longName : String) : Option Flag := p.flags.find? (·.flag.longName = longName)
    /-- Finds the parsed positional argument in `p` with the corresponding `name`. -/
    def positionalArg? (p : Parsed) (name     : String) : Option Arg  := p.positionalArgs.find? (·.arg.name = name)

    /-- Finds the parsed flag in `p` with the corresponding `longName`. -/
    def flag!          (p : Parsed) (longName : String) : Flag := p.flag? longName |>.get!
    /-- Finds the parsed positional argument in `p` with the corresponding `name`. -/
    def positionalArg! (p : Parsed) (name     : String) : Arg  := p.positionalArg? name |>.get!

    /-- Checks whether `p` has a parsed flag with the corresponding `longName`. -/
    def hasFlag          (p : Parsed) (longName : String) : Bool := p.flag? longName |>.isSome
    /-- Checks whether `p` has a positional argument with the corresponding `longName`. -/
    def hasPositionalArg (p : Parsed) (name     : String) : Bool := p.positionalArg? name |>.isSome

    /--
    Converts all `p.variableArgs` values to `τ`, which should be the same type
    that was designated in the corresponding `Cli.Arg`.
    Yields `none` if the conversion was unsuccessful, which can only
    happen if `τ` is not the same type as the one designated in the corresponding `Cli.Arg`.
    -/
    def variableArgsAs? (p : Parsed) (τ) [ParseableType τ] : Option (Array τ) :=
      p.variableArgs.mapM (·.as? τ)

    /--
    Converts all `p.variableArgs` values to `τ`, which should be the same type
    that was designated in the corresponding `Cli.Arg`.
    Panics if the conversion was unsuccessful, which can only
    happen if `τ` is not the same type as the one designated in the corresponding `Cli.Arg`.
    -/
    def variableArgsAs! (p : Parsed) (τ) [Inhabited τ] [ParseableType τ] : Array τ :=
      p.variableArgsAs? τ |>.get!

    instance : ToString Parsed where
      toString p :=
        s!"cmd: {p.cmd.meta.fullName}; flags: {toString p.flags}; positionalArgs: {toString p.positionalArgs}; " ++
        s!"variableArgs: {toString p.variableArgs}"
  end Parsed

  open Cmd in
  /--
  Represents a view of `Cmd` that can be passed to `Extension`s, i.e. it does
  not itself reference the `Extension` provided for each `Cmd`.
  -/
  inductive ExtendableCmd
    | init
      (meta              : Meta)
      (run               : Parsed → IO UInt32)
      (subCmds           : Array ExtendableCmd)
      (originalFullName? : Option String)
    deriving Inhabited

  namespace ExtendableCmd
    /-- Non-recursive meta-data. -/
    def         meta              : ExtendableCmd → Cmd.Meta             | init v _ _ _ => v
    /-- Handler to run when the command is called and flags/arguments have been successfully processed. -/
    def         run               : ExtendableCmd → (Parsed → IO UInt32) | init _ v _ _ => v
    /-- Subcommands. May be mutated by extensions. -/
    def         subCmds           : ExtendableCmd → Array ExtendableCmd  | init _ _ v _ => v
    private def originalFullName? : ExtendableCmd → Option String        | init _ _ _ v => v

    /--
    Updates the designated fields in `c`.
    - `meta`:    Non-recursive meta-data.
    - `run`:     Handler to run when the command is called and flags/arguments have been successfully processed.
    - `subCmds`: Subcommands.
    -/
    def update'
      (c       : ExtendableCmd)
      (meta    : Cmd.Meta            := c.meta)
      (run     : Parsed → IO UInt32  := c.run)
      (subCmds : Array ExtendableCmd := c.subCmds)
      : ExtendableCmd :=
        .init meta run subCmds c.originalFullName?

    /--
    Updates the designated fields in `c`.
    - `name`:                Name that is displayed in the help.
    - `version?`:            Version that is displayed in the help and when the version is queried.
    - `description`:         Description that is displayed in the help.
    - `furtherInformation?`: Information appended to the end of the help. Useful for command extensions.
    - `flags`:               Supported flags ("options" in standard terminology).
    - `positionalArgs`:      Supported positional arguments ("operands" in standard terminology).
    - `variableArg?`:        Variable argument at the end of the positional arguments.
    - `run`:                 Handler to run when the command is called and flags/arguments have been successfully processed.
    - `subCmds`:             Subcommands.
    -/
    def update
      (c                   : ExtendableCmd)
      (name                : String              := c.meta.name)
      (version?            : Option String       := c.meta.version?)
      (description         : String              := c.meta.description)
      (furtherInformation? : Option String       := c.meta.furtherInformation?)
      (flags               : Array Flag          := c.meta.flags)
      (positionalArgs      : Array Arg           := c.meta.positionalArgs)
      (variableArg?        : Option Arg          := c.meta.variableArg?)
      (run                 : Parsed → IO UInt32  := c.run)
      (subCmds             : Array ExtendableCmd := c.subCmds)
      : ExtendableCmd :=
        .init
          ⟨name, c.meta.parentNames, version?, description, furtherInformation?, flags, positionalArgs, variableArg?⟩
          run subCmds c.originalFullName?

    /--
    Creates a new `ExtendableCmd`. The resulting `ExtendableCmd` will not have an extension.
    - `meta`:    Non-recursive meta-data.
    - `run`:     Handler to run when the command is called and flags/arguments have been successfully processed.
    - `subCmds`: Subcommands.
    -/
    def mk'
      (meta    : Cmd.Meta)
      (run     : Parsed → IO UInt32)
      (subCmds : Array ExtendableCmd := #[])
      : ExtendableCmd :=
        .init meta run subCmds none

    /--
    Creates a new `ExtendableCmd`. The resulting `ExtendableCmd` will not have an extension.
    Adds a `-h, --help` and a `--version` flag if `meta` designates a version.
    - `meta`:    Non-recursive meta-data.
    - `run`:     Handler to run when the command is called and flags/arguments have been successfully processed.
    - `subCmds`: Subcommands.
    -/
    def mkWithHelpAndVersionFlags'
      (meta    : Cmd.Meta)
      (run     : Parsed → IO UInt32)
      (subCmds : Array ExtendableCmd := #[])
      : ExtendableCmd :=
        .mk' meta.addHelpAndVersionFlags run subCmds

    /--
    Creates a new `ExtendableCmd`. The resulting `ExtendableCmd` will not have an extension.
    - `parent`:              Parent of this command.
    - `name`:                Name that is displayed in the help.
    - `version?`:            Version that is displayed in the help and when the version is queried.
    - `description`:         Description that is displayed in the help.
    - `furtherInformation?`: Information appended to the end of the help. Useful for command extensions.
    - `flags`:               Supported flags ("options" in standard terminology).
    - `positionalArgs`:      Supported positional arguments ("operands" in standard terminology).
    - `variableArg?`:        Variable argument at the end of the positional arguments.
    - `run`:                 Handler to run when the command is called and flags/arguments have been successfully processed.
    - `subCmds`:             Subcommands.
    -/
    def mk
      (parent              : ExtendableCmd)
      (name                : String)
      (version?            : Option String)
      (description         : String)
      (furtherInformation? : Option String := none)
      (flags               : Array Flag    := #[])
      (positionalArgs      : Array Arg     := #[])
      (variableArg?        : Option Arg    := none)
      (run                 : Parsed → IO UInt32)
      (subCmds             : Array ExtendableCmd := #[])
      : ExtendableCmd :=
        .mk'
          ⟨name, parent.meta.parentNames.push parent.meta.name, version?, description, furtherInformation?, 
            flags, positionalArgs, variableArg?⟩
          run subCmds

    /--
    Creates a new `ExtendableCmd`. The resulting `ExtendableCmd` will not have an extension.
    Adds a `-h, --help` and a `--version` flag if `version?` is present.
    - `parent`:              Parent of this command.
    - `name`:                Name that is displayed in the help.
    - `version?`:            Version that is displayed in the help and when the version is queried.
    - `description`:         Description that is displayed in the help.
    - `furtherInformation?`: Information appended to the end of the help. Useful for command extensions.
    - `flags`:               Supported flags ("options" in standard terminology).
    - `positionalArgs`:      Supported positional arguments ("operands" in standard terminology).
    - `variableArg?`:        Variable argument at the end of the positional arguments.
    - `run`:                 Handler to run when the command is called and flags/arguments have been successfully processed.
    - `subCmds`:             Subcommands.
    -/
    def mkWithHelpAndVersionFlags
      (parent              : ExtendableCmd)
      (name                : String)
      (version?            : Option String)
      (description         : String)
      (furtherInformation? : Option String := none)
      (flags               : Array Flag    := #[])
      (positionalArgs      : Array Arg     := #[])
      (variableArg?        : Option Arg    := none)
      (run                 : Parsed → IO UInt32)
      (subCmds             : Array ExtendableCmd := #[])
      : ExtendableCmd :=
        .mkWithHelpAndVersionFlags'
          ⟨name, parent.meta.parentNames.push parent.meta.name, version?, description, furtherInformation?, 
            flags, positionalArgs, variableArg?⟩
          run subCmds

    /-- Name that is displayed in the help. -/
    def name                (c : ExtendableCmd) : String        := c.meta.name
    /-- Version of the command that is displayed in the help and when the version is queried. -/
    def version?            (c : ExtendableCmd) : Option String := c.meta.version?
    /-- Description that is displayed in the help. -/
    def description         (c : ExtendableCmd) : String        := c.meta.description
    /-- Information appended to the end of the help. Useful for command extensions. -/
    def furtherInformation? (c : ExtendableCmd) : Option String := c.meta.furtherInformation?
    /-- Supported flags ("options" in standard terminology). -/
    def flags               (c : ExtendableCmd) : Array Flag    := c.meta.flags
    /-- Supported positional arguments ("operands" in standard terminology). -/
    def positionalArgs      (c : ExtendableCmd) : Array Arg     := c.meta.positionalArgs
    /-- Variable argument after the end of the positional arguments. -/
    def variableArg?        (c : ExtendableCmd) : Option Arg    := c.meta.variableArg?

    /-- Full name from the root to this command, including the name of the command itself. -/
    def fullName (c : ExtendableCmd) : String := c.meta.fullName

    /-- Version of the command that is displayed in the help and when the version is queried. -/
    def version!            (c : ExtendableCmd) : String := c.meta.version!
    /-- Information appended to the end of the help. Useful for command extensions. -/
    def furtherInformation! (c : ExtendableCmd) : String := c.meta.furtherInformation!
    /-- Variable argument after the end of the positional arguments. -/
    def variableArg!        (c : ExtendableCmd) : Arg    := c.meta.variableArg!

    /-- Checks whether `c` has a version. -/
    def hasVersion            (c : ExtendableCmd) : Bool := c.meta.hasVersion
    /-- Checks whether `c` has information appended to the end of the help. -/
    def hasFurtherInformation (c : ExtendableCmd) : Bool := c.meta.hasFurtherInformation
    /-- Checks whether `c` supports a variable argument. -/
    def hasVariableArg        (c : ExtendableCmd) : Bool := c.meta.hasVariableArg

    /-- Finds the flag in `c` with the corresponding `longName`. -/
    def flag?          (c : ExtendableCmd) (longName : String) : Option Flag          := c.meta.flag? longName
    /-- Finds the positional argument in `c` with the corresponding `name`. -/
    def positionalArg? (c : ExtendableCmd) (name     : String) : Option Arg           := c.meta.positionalArg? name

    /-- Finds the flag in `c` with the corresponding `longName`. -/
    def flag!          (c : ExtendableCmd) (longName : String) : Flag          := c.meta.flag! longName
    /-- Finds the positional argument in `c` with the corresponding `name`. -/
    def positionalArg! (c : ExtendableCmd) (name     : String) : Arg           := c.meta.positionalArg! name

    /-- Checks whether `c` contains a flag with the corresponding `longName`. -/
    def hasFlag          (c : ExtendableCmd) (longName : String) : Bool := c.meta.hasFlag longName
    /-- Checks whether `c` contains a positional argument with the corresponding `name`. -/
    def hasPositionalArg (c : ExtendableCmd) (name     : String) : Bool := c.meta.hasPositionalArg name

    /-- Finds the flag in `c` with the corresponding `shortName`. -/
    def flagByShortName? (c : ExtendableCmd) (name : String) : Option Flag          := c.meta.flagByShortName? name
    /-- Finds the subcommand in `c` with the corresponding `name`. -/
    def subCmd?          (c : ExtendableCmd) (name : String) : Option ExtendableCmd := c.subCmds.find? (·.meta.name = name)

    /-- Finds the flag in `c` with the corresponding `shortName`. -/
    def flagByShortName! (c : ExtendableCmd) (name : String) : Flag          := c.meta.flagByShortName! name
    /-- Finds the subcommand in `c` with the corresponding `name`. -/
    def subCmd!          (c : ExtendableCmd) (name : String) : ExtendableCmd := c.subCmd? name |>.get!

    /-- Checks whether `c` has a flag with the corresponding `shortName`. -/
    def hasFlagByShortName (c : ExtendableCmd) (name : String) : Bool := c.meta.hasFlagByShortName name
    /-- Checks whether `c` contains a subcommand with the corresponding `name`. -/
    def hasSubCmd          (c : ExtendableCmd) (name : String) : Bool := c.subCmd? name |>.isSome
  end ExtendableCmd

  /--
  Allows user code to extend the library in two ways:
  - A command can be replaced or extended with new components.
  - The output of the parser can be postprocessed and validated.
  -/
  structure Extension where
    /-- 
    Priority that dictates how early an extension is applied.
    The lower the priority, the later it is applied.
    -/
    priority : Nat := 1024
    /-- 
    Extends a command to adjust the displayed help.
    The recursive subcommand structure may be mutated.
    -/
    extend : ExtendableCmd → ExtendableCmd := id
    /-- 
    Processes and validates the output of the parser for the given `ExtendableCmd`. 
    Takes the `ExtendableCmd` that results from all extensions being applied.
    If postprocessing mutates the subcommand structure in `Parsed.cmd`, care must be taken to update
    `Parsed.parent?` accordingly as well.
    -/
    postprocess : ExtendableCmd → Parsed → Except String Parsed := fun _ => pure
    deriving Inhabited

  /--
  Composes both extensions so that the `Cmd`s are extended in succession
  and postprocessed in succession. Postprocessing errors if any of the two
  `postprocess` functions errors.
  -/
  def Extension.then (a : Extension) (b : Extension) : Extension := {
    extend  := b.extend ∘ a.extend
    postprocess := fun cmd parsed => do
      b.postprocess cmd <| ← a.postprocess cmd parsed
  }

  open Cmd in
  /--
  Represents a command, i.e. either the application root or some subcommand of the root.
  -/
  inductive Cmd
    | init
      (meta       : Meta)
      (run        : Parsed → IO UInt32)
      (subCmds    : Array Cmd)
      (extension? : Option Extension)
    deriving Inhabited

  namespace Cmd
    /-- Non-recursive meta-data. -/
    def meta       : Cmd → Cmd.Meta             | init v _ _ _ => v
    /-- Handler to run when the command is called and flags/arguments have been successfully processed. -/
    def run        : Cmd → (Parsed → IO UInt32) | init _ v _ _ => v
    /-- Subcommands. -/
    def subCmds    : Cmd → Array Cmd            | init _ _ v _ => v
    /-- Extension of the Cli library. -/
    def extension? : Cmd → Option Extension     | init _ _ _ v => v

    private def update
      (c          : Cmd)
      (meta       : Meta               := c.meta)
      (run        : Parsed → IO UInt32 := c.run)
      (subCmds    : Array Cmd          := c.subCmds)
      (extension? : Option Extension   := c.extension?)
      : Cmd :=
        .init meta run subCmds extension?

    /-- Recomputes all the parent names of subcommands in `c` with `c` as the root command. -/
    partial def updateParentNames (c : Cmd) : Cmd :=
      loop c #[]
    where
      loop (c : Cmd) (parentNames : Array String) : Cmd :=
        let subCmdParentNames := parentNames.push c.meta.name
        let subCmds := c.subCmds.map (loop · subCmdParentNames)
        .init { c.meta with parentNames := parentNames } c.run subCmds c.extension?
    
    /--
    Creates a new command. Adds a `-h, --help` and a `--version` flag if `meta` designates a version.
    Updates the `parentNames` of all subcommands.
    - `meta`:       Non-recursive meta-data.
    - `run`:        Handler to run when the command is called and flags/arguments have been successfully processed.
    - `subCmds`:    Subcommands.
    - `extension?`: Extension of the Cli library.
    -/
    partial def mk'
      (meta       : Meta)
      (run        : Parsed → IO UInt32)
      (subCmds    : Array Cmd        := #[])
      (extension? : Option Extension := none)
      : Cmd :=
        let meta := meta.addHelpAndVersionFlags
        let c := .init meta run subCmds extension?
        updateParentNames c

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
      : Cmd :=
        mk'
          ⟨name, #[], version?, description, furtherInformation?, flags, positionalArgs, variableArg?⟩
          run subCmds extension?

    /-- Finds the subcommand in `c` with the corresponding `name`. -/
    def subCmd?    (c : Cmd) (name : String) : Option Cmd := c.subCmds.find? (·.meta.name = name)
    /-- Extension of the Cli library. -/
    def extension! (c : Cmd)                 : Extension  := c.extension?.get!

    /-- Finds the subcommand in `c` with the corresponding `name`. -/
    def subCmd! (c : Cmd) (name : String) : Cmd := c.subCmd? name |>.get!

    /-- Checks whether `c` contains a subcommand with the corresponding `name`. -/
    def hasSubCmd    (c : Cmd) (name : String) : Bool := c.subCmd? name |>.isSome
    /-- Checks whether `c` is being extended. -/
    def hasExtension (c : Cmd)                 : Bool := c.extension?.isSome
  end Cmd

  namespace Parsed
    namespace Cmd
      /-- Extracts `meta` and the recursive subcommand structure from `c` to create a `Parsed.Cmd`. -/
      partial def ofFullCmd (c : Cli.Cmd) : Cmd :=
        .init c.meta (c.subCmds.map ofFullCmd)

      /-- Embeds `c` into a `Cli.Cmd` that does nothing. -/
      partial def toFullCmd (c : Cmd) : Cli.Cmd :=
        let meta       := c.meta
        let run        := fun _ => pure 0
        let subCmds    := c.subCmds.map toFullCmd
        let extension? := { : Extension }
        .init meta run subCmds extension?
    end Cmd

    /-- Embeds `p.cmd` into a `Cli.Cmd` that does nothing. -/
    def toCmd (p : Parsed) : Cli.Cmd :=
      p.cmd.toFullCmd
  end Parsed

  namespace ExtendableCmd
    /-- Creates a view of `c` that can be extended by extensions. -/
    partial def ofFullCmd (c : Cli.Cmd) : ExtendableCmd :=
      .init c.meta c.run (c.subCmds.map ofFullCmd) c.meta.fullName

    /-- Converts `c` back into a `Cli.Cmd`, using the extensions denoted in `original`. -/
    partial def toFullCmd (c : ExtendableCmd) (original : Cli.Cmd) : Cli.Cmd := Id.run do
      let extensions := collectExtensions original
      let mut extensionIndex := Std.mkRBMap String (Option Extension) compare
      for ⟨fullName, extension?⟩ in extensions do
        extensionIndex := extensionIndex.insert fullName extension?
      let rec loop (c : ExtendableCmd) : Cli.Cmd :=
        let extension? := do extensionIndex.find? (← c.originalFullName?) |>.join
        let subCmds := c.subCmds.map loop
        .init c.meta c.run subCmds extension?
      loop c |>.updateParentNames |> prependOriginalParentNames
    where
      collectExtensions (currentCmd : Cli.Cmd) : Array (String × Option Extension) := Id.run do
        let mut extensions := #[(currentCmd.meta.fullName, currentCmd.extension?)]
        for subCmd in currentCmd.subCmds do
          extensions := extensions ++ collectExtensions subCmd
        return extensions
      prependOriginalParentNames (currentCmd : Cli.Cmd) : Cli.Cmd := Id.run do
        let parentNames := original.meta.parentNames ++ currentCmd.meta.parentNames
        let meta := { currentCmd.meta with parentNames := parentNames }
        let subCmds := currentCmd.subCmds.map prependOriginalParentNames
        currentCmd.update (meta := meta) (subCmds := subCmds)

    /-- Converts `c` back into `Cli.Cmd` while retaining none of the extensions. -/
    partial def toFullCmdWithoutExtensions (c : ExtendableCmd) : Cli.Cmd :=
      Cmd.init c.meta c.run (c.subCmds.map toFullCmdWithoutExtensions) none

  end ExtendableCmd
end Configuration

section Macro
  open Lean

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

  def expandIdentLiterally (t : Term) : Term :=
    match t with
    | `($i:ident) =>
      quote i.getId.toString
    | `($t:term) =>
      t

  def expandRunFun (runFun : TSyntax `Cli.runFun) : MacroM Term :=
    match runFun with
    | `(Cli.runFun| VIA $run) =>
      `($run)
    | `(Cli.runFun| NOOP) =>
      `(fun _ => pure 0)
    | _ => Macro.throwUnsupported

  def expandPositionalArg (positionalArg : TSyntax `Cli.positionalArg) : MacroM Term := do
    let `(Cli.positionalArg| $name:term : $type; $description) := positionalArg
      | Macro.throwUnsupported
    `(Arg.mk $(expandIdentLiterally name) $description $type)

  def expandVariableArg (variableArg : TSyntax `Cli.variableArg) : MacroM Term := do
    let `(Cli.variableArg| ...$name:term : $type; $description) := variableArg
      | Macro.throwUnsupported
    `(Arg.mk $(expandIdentLiterally name) $description $type)

  def expandFlag (flag : TSyntax `Cli.flag) : MacroM Term := do
    let `(Cli.flag| $flagName1:term $[, $flagName2:term]? $[ : $type]?; $description) := flag
      | Macro.throwUnsupported
    let mut shortName := quote (none : Option String)
    let mut longName := flagName1
    if let some flagName2 := flagName2 then
      shortName ← `(some $(expandIdentLiterally flagName1))
      longName := flagName2
    let unitType : Term ← `(Unit)
    let type :=
      match type with
      | none => unitType
      | some type => type
    `(Flag.mk $shortName $(expandIdentLiterally longName) $description $type)

  macro_rules
    | `(`[Cli|
        $name:term $run:runFun; $[[$version]]?
        $description
        $[FLAGS:
          $flags*
        ]?
        $[ARGS:
          $positionalArgs*
          $[$variableArg]?
        ]?
        $[SUBCOMMANDS: $subCommands;*]?
        $[EXTENSIONS: $extensions;*]?
      ]) => do
        `(Cmd.mk
          (name           := $(expandIdentLiterally name))
          (version?       := $(quote version))
          (description    := $description)
          (flags          := $(quote (← flags.getD #[] |>.mapM expandFlag)))
          (positionalArgs := $(quote (← positionalArgs.getD #[] |>.mapM expandPositionalArg)))
          (variableArg?   := $(quote (← variableArg.join.mapM expandVariableArg)))
          (run            := $(← expandRunFun run))
          (subCmds        := $(quote (subCommands.getD ⟨#[]⟩).getElems))
          (extension?     := some <| Array.foldl Extension.then { : Extension } <| Array.qsort
            $(quote (extensions.getD ⟨#[]⟩).getElems) (·.priority > ·.priority)))
end Macro

section Info
  /-- Maximum width within which all formatted text should fit. -/
  def maxWidth                  : Nat := 80
  /-- Amount of spaces with which section content is indented. -/
  def indentation               : Nat := 4
  /-- Maximum width within which all formatted text should fit, after indentation. -/
  def maxIndentedWidth          : Nat := maxWidth - indentation
  /-- Formats `xs` by `String.optJoin`ing the components with a single space. -/
  def line  (xs : Array String) : String := " ".optJoin xs
  /-- Formats `xs` by `String.optJoin`ing the components with a newline `\n`. -/
  def lines (xs : Array String) : String := "\n".optJoin xs

  /--
  Renders a help section with the designated `title` and `content`.
  If `content = ""`, `emptyContentPlaceholder?` will be used instead.
  If `emptyContentPlaceholder? = none`, neither the title nor the content
  will be rendered.
  -/
  def renderSection
    (title                    : String)
    (content                  : String)
    (emptyContentPlaceholder? : Option String := none)
    : String :=
    let titleLine? : Option String := do
      if content = "" then
        return s!"{title}: {← emptyContentPlaceholder?}"
      else
        return s!"{title}:"
    lines #[
      titleLine?.optStr,
      content |>.wrapWordsAt! maxIndentedWidth |>.indent indentation
    ]

  /--
  Renders a table using `Array.renderTable!` and then renders a section with
  the designated `title` and the rendered table as content.
  -/
  def renderTable
    (title                  : String)
    (columns                : Array (String × String))
    (emptyTablePlaceholder? : Option String := none)
    : String :=
    let table := columns.renderTable! (maxWidth := maxIndentedWidth)
    renderSection title table emptyTablePlaceholder?

  namespace Cmd
    private def metaDataInfo (c : Cmd) : String :=
      let version? : Option String := do return s!"[{← c.meta.version?}]"
      lines #[
        line #[c.meta.fullName, version?.optStr] |>.wrapWordsAt! maxWidth,
        c.meta.description.wrapWordsAt! maxWidth
      ]

    private def usageInfo (c : Cmd) : String :=
      let subCmdTitle? : Option String := if ¬c.subCmds.isEmpty then "[SUBCOMMAND]" else none
      let posArgNames  : String        := line <| c.meta.positionalArgs.map (s!"<{·.name}>")
      let varArgName?  : Option String := do return s!"<{(← c.meta.variableArg?).name}>..."
      let info := line #[c.meta.fullName, subCmdTitle?.optStr, "[FLAGS]", posArgNames, varArgName?.optStr]
      renderSection "USAGE" info

    private def flagInfo (c : Cmd) : String :=
      let columns : Array (String × String) := c.meta.flags.map fun flag =>
        let shortName?    : Option String := do return s!"-{← flag.shortName?}"
        let names         : String        := ", ".optJoin #[shortName?.optStr, s!"--{flag.longName}"]
        let type?         : Option String := if ¬ flag.isParamless then s!": {flag.type.name}" else none
        (line #[names, type?.optStr], flag.description)
      renderTable "FLAGS" columns (emptyTablePlaceholder? := "None")

    private def positionalArgInfo (c : Cmd) : String :=
      let args :=
        if let some variableArg := c.meta.variableArg? then
          c.meta.positionalArgs ++ #[variableArg]
        else
          c.meta.positionalArgs
      args.map (fun arg => (line #[arg.name, s!": {arg.type.name}"], arg.description))
        |> renderTable "ARGS"

    private def subCommandInfo (c : Cmd) : String :=
      c.subCmds.map (fun subCmd => (subCmd.meta.name, subCmd.meta.description))
        |> renderTable "SUBCOMMANDS"

    /-- Renders the help for `c`. -/
    def help (c : Cmd) : String :=
      lines #[
        c.metaDataInfo,
        "\n" ++ c.usageInfo,
        "\n" ++ c.flagInfo,
        (if ¬c.meta.positionalArgs.isEmpty ∨ c.meta.hasVariableArg then "\n" else "") ++ c.positionalArgInfo,
        (if ¬c.subCmds.isEmpty then "\n" else "") ++ c.subCommandInfo,
        (if c.meta.hasFurtherInformation then "\n" else "") ++ c.meta.furtherInformation?.optStr
      ]

    /-- Renders an error for `c` with the designated message `msg`. -/
    def error (c : Cmd) (msg : String) : String :=
      lines #[
        msg.wrapWordsAt! maxWidth,
        s!"Run `{c.meta.fullName} -h` for further information.".wrapWordsAt! maxWidth
      ]

    /-- Prints the help for `c`. -/
    def printHelp     (c : Cmd)                : IO Unit := IO.println c.help
    /-- Prints an error for `c` with the designated message `msg`. -/
    def printError    (c : Cmd) (msg : String) : IO Unit := IO.eprintln <| c.error msg
    /-- Prints the version of `c`. Panics if `c` has no version. -/
    def printVersion! (c : Cmd)                : IO Unit := IO.println c.meta.version!
  end Cmd

  namespace Parsed
      /-- Renders the help for `p.cmd`. -/
    def help (p : Parsed) : String := p.toCmd.help

    /-- Renders an error for `p.cmd` with the designated message `msg`. -/
    def error (p : Parsed) (msg : String) : String := p.toCmd.error msg

    /-- Prints the help for `p.cmd`. -/
    def printHelp     (p : Parsed)                : IO Unit := p.toCmd.printHelp
    /-- Prints an error for `p.cmd` with the designated message `msg`. -/
    def printError    (p : Parsed) (msg : String) : IO Unit := p.toCmd.printError msg
    /-- Prints the version of `p.cmd`. Panics if `p.cmd` has no version. -/
    def printVersion! (p : Parsed)                : IO Unit := p.toCmd.printVersion!
  end Parsed
end Info

section Parsing
  /-- Represents the exact representation of a flag as input by the user. -/
  structure InputFlag where
    /-- Flag name input by the user. -/
    name    : String
    /-- Whether the flag input by the user was a short one. -/
    isShort : Bool

  instance : ToString InputFlag where
    toString f :=
      let pre := if f.isShort then "-" else "--"
      s!"{pre}{f.name}"

  namespace ParseError
    /-- Represents the kind of error that occured during parsing. -/
    inductive Kind
    | unknownFlag
      (inputFlag : InputFlag)
      (msg       : String :=
        s!"Unknown flag `{inputFlag}`.")
    | missingFlagArg
      (flag      : Flag)
      (inputFlag : InputFlag)
      (msg       : String :=
        s!"Missing argument for flag `{inputFlag}`.")
    | duplicateFlag
      (flag      : Flag)
      (inputFlag : InputFlag)
      (msg       : String :=
        let complementaryName? : Option String := do
          if inputFlag.isShort then
            return s!" (`--{flag.longName}`)"
          else
            return s!" (`-{← flag.shortName?}`)"
        s!"Duplicate flag `{inputFlag}`{complementaryName?.optStr}.")
    | redundantFlagArg
      (flag       : Flag)
      (inputFlag  : InputFlag)
      (inputValue : String)
      (msg        : String :=
        s!"Redundant argument `{inputValue}` for flag `{inputFlag}` that takes no arguments.")
    | invalidFlagType
      (flag       : Flag)
      (inputFlag  : InputFlag)
      (inputValue : String)
      (msg        : String :=
        s!"Invalid type of argument `{inputValue}` for flag `{inputFlag} : {flag.type.name}`.")
    | missingPositionalArg
      (arg : Arg)
      (msg : String :=
        s!"Missing positional argument `<{arg.name}>.`")
    | invalidPositionalArgType
      (arg      : Arg)
      (inputArg : String)
      (msg      : String :=
        s!"Invalid type of argument `{inputArg}` for positional argument `<{arg.name} : {arg.type.name}>`.")
    | redundantPositionalArg
      (inputArg : String)
      (msg      : String :=
        s!"Redundant positional argument `{inputArg}`.")
    | invalidVariableArgType
      (arg      : Arg)
      (inputArg : String)
      (msg      : String :=
        s!"Invalid type of argument `{inputArg}` for variable argument `<{arg.name} : {arg.type.name}>...`.")

    /-- Associated message of the error. -/
    def Kind.msg : Kind → String
      | unknownFlag              _     msg
      | missingFlagArg           _ _   msg
      | duplicateFlag            _ _   msg
      | redundantFlagArg         _ _ _ msg
      | invalidFlagType          _ _ _ msg
      | missingPositionalArg     _     msg
      | invalidPositionalArgType _ _   msg
      | redundantPositionalArg   _     msg
      | invalidVariableArgType   _ _   msg => msg
  end ParseError

  open ParseError in
  /-- Error that occured during parsing. Contains the command that the error occured in and the kind of the error. -/
  structure ParseError where
    /-- Command that the error occured in. -/
    cmd         : Cmd
    /-- Kind of error that occured. -/
    kind        : Kind

  private structure ParseState where
    idx                  : Nat
    cmd                  : Cmd
    parent?              : Option Cmd
    parsedFlags          : Array Parsed.Flag
    parsedPositionalArgs : Array Parsed.Arg
    parsedVariableArgs   : Array Parsed.Arg

  private abbrev ParseM := ExceptT ParseError (ReaderT (Array String) (StateM ParseState))

  namespace ParseM
    open ParseError.Kind

    private def args                 : ParseM (Array String)      := read
    private def idx                  : ParseM Nat                 := do return (← get).idx
    private def cmd                  : ParseM Cmd                 := do return (← get).cmd
    private def parent?              : ParseM (Option Cmd)        := do return (← get).parent?
    private def parsedFlags          : ParseM (Array Parsed.Flag) := do return (← get).parsedFlags
    private def parsedPositionalArgs : ParseM (Array Parsed.Arg)  := do return (← get).parsedPositionalArgs
    private def parsedVariableArgs   : ParseM (Array Parsed.Arg)  := do return (← get).parsedVariableArgs
    private def peek?                : ParseM (Option String)     := do return (← args).get? (← idx)
    private def peekNext?            : ParseM (Option String)     := do return (← args).get? <| (← idx) + 1
    private def flag? (inputFlag : InputFlag) : ParseM (Option Flag) := do
      if inputFlag.isShort then
        return (← cmd).meta.flagByShortName? inputFlag.name
      else
        return (← cmd).meta.flag? inputFlag.name

    private def setIdx (idx : Nat) : ParseM Unit := do
      set { ← get with idx := idx }
    private def setCmd (c : Cmd) : ParseM Unit := do
      set { ← get with cmd := c }
    private def setParent (c? : Option Cmd) : ParseM Unit := do
      set { ← get with parent? := c? }
    private def pushParsedFlag (parsedFlag : Parsed.Flag) : ParseM Unit := do
      set { ← get with parsedFlags := (← parsedFlags).push parsedFlag }
    private def pushParsedPositionalArg (parsedPositionalArg : Parsed.Arg) : ParseM Unit := do
      set { ← get with parsedPositionalArgs := (← parsedPositionalArgs).push parsedPositionalArg }
    private def pushParsedVariableArg (parsedVariableArg : Parsed.Arg) : ParseM Unit := do
      set { ← get with parsedVariableArgs := (← parsedVariableArgs).push parsedVariableArg }
    private def skip : ParseM Unit := do
      setIdx <| (← idx) + 1

    private def parseError (kind : ParseError.Kind) : ParseM ParseError := do return ⟨← cmd, kind⟩

    private partial def parseSubCmds : ParseM Unit := do
      let mut parent? := none
      let mut lastSubCmd ← cmd
      repeat
        let some arg ← peek?
          | break
        let some subCmd := lastSubCmd.subCmd? arg
          | break
        skip
        parent? := lastSubCmd
        lastSubCmd := subCmd
      setCmd lastSubCmd
      setParent parent?

    private def parseEndOfFlags : ParseM Bool := do
      let some arg ← peek?
        | return false
      if arg = "--" then
        skip
        return true
      return false

    private def readFlagContent? : ParseM (Option (String × Bool)) := do
      let some arg ← peek?
        | return none
      if arg = "--" ∨ arg = "-" then
        return none
      if arg.take 2 = "--" then
        return (arg.drop 2, false)
      if arg.take 1 = "-" then
        return (arg.drop 1, true)
      return none

    private def ensureFlagUnique (flag : Flag) (inputFlag : InputFlag) : ParseM Unit := do
      if (← parsedFlags).find? (·.flag.longName = flag.longName) |>.isSome then
        throw <| ← parseError <| duplicateFlag flag inputFlag

    private def ensureFlagWellTyped (flag : Flag) (inputFlag : InputFlag) (value : String) : ParseM Unit := do
      if ¬ flag.type.isValid value then
        throw <| ← parseError <| invalidFlagType flag inputFlag value

    private partial def readMultiFlag? : ParseM (Option (Array Parsed.Flag)) := do
      let some (flagContent, true) ← readFlagContent?
        | return none
      let some (parsedFlags : Array (String × Parsed.Flag)) ← loop flagContent Std.RBTree.empty
        | return none
      for (inputFlagName, parsedFlag) in parsedFlags do
        ensureFlagUnique parsedFlag.flag ⟨inputFlagName, true⟩
      skip
      return some <| parsedFlags.map (·.2)
    where
      loop (flagContent : String) (matched : Std.RBTree String compare)
        : ParseM (Option (Array (String × Parsed.Flag))) := do
        -- this is not tail recursive, but that's fine: `loop` will only recurse further if the corresponding
        -- flag has not been matched yet, meaning that this can only overflow if the application has an
        -- astronomical amount of short flags.
        if flagContent = "" then
          return some #[]
        let parsedFlagsCandidates : Array (Array (String × Parsed.Flag)) ←
          (← cmd).meta.flags.filter (·.isParamless)
            |>.filter               (·.hasShortName)
            |>.filter               (·.shortName!.isPrefixOf flagContent)
            |>.filter               (¬ matched.contains ·.shortName!)
            |>.qsort                (·.shortName!.length > ·.shortName!.length)
            |>.filterMapM fun flag => do
              let inputFlagName := flagContent.take flag.shortName!.length
              let restContent   := flagContent.drop flag.shortName!.length
              let newMatched    := matched.insert flag.shortName!
              let some tail ← loop restContent newMatched
                | return none
              return some <| #[(inputFlagName, ⟨flag, ""⟩)] ++ tail
        return parsedFlagsCandidates.get? 0

    private def readEqFlag? : ParseM (Option Parsed.Flag) := do
      let some (flagContent, isShort) ← readFlagContent?
        | return none
      match flagContent.splitOn "=" with
      | [] => panic! "Cli.readEqFlag?: String.splitOn returned empty list"
      | [_] => return none
      | (flagName :: flagArg :: rest) =>
        let flagValue := "=".intercalate <| flagArg :: rest
        let inputFlag : InputFlag := ⟨flagName, isShort⟩
        let some flag ← flag? inputFlag
          | throw <| ← parseError <| unknownFlag inputFlag
        if flag.isParamless then
          throw <| ← parseError <| redundantFlagArg flag inputFlag flagValue
        ensureFlagUnique flag inputFlag
        ensureFlagWellTyped flag inputFlag flagValue
        skip
        return some ⟨flag, flagValue⟩

    private def readWsFlag? : ParseM (Option Parsed.Flag) := do
      let some (flagName, isShort) ← readFlagContent?
        | return none
      let some flagValue ← peekNext?
        | return none
      let inputFlag : InputFlag := ⟨flagName, isShort⟩
      let some flag ← flag? inputFlag
        | return none
      if flag.isParamless then
        return none
      ensureFlagUnique flag inputFlag
      ensureFlagWellTyped flag inputFlag flagValue
      skip; skip
      return some ⟨flag, flagValue⟩

    private def readPrefixFlag? : ParseM (Option Parsed.Flag) := do
      let some (flagContent, true) ← readFlagContent?
        | return none
      let some flag :=
          (← cmd).meta.flags.filter (¬ ·.isParamless)
            |>.filter               (·.hasShortName)
            |>.filter               (·.shortName!.isPrefixOf flagContent)
            |>.filter               (·.shortName!.length < flagContent.length)
            |>.qsort                (·.shortName!.length > ·.shortName!.length)
            |>.get? 0
        | return none
      let flagName  := flag.shortName!
      let flagValue := flagContent.drop flagName.length
      let inputFlag : InputFlag := ⟨flagName, true⟩
      ensureFlagUnique flag inputFlag
      ensureFlagWellTyped flag inputFlag flagValue
      skip
      return some ⟨flag, flagValue⟩

    private def readParamlessFlag? : ParseM (Option Parsed.Flag) := do
      let some (flagName, isShort) ← readFlagContent?
        | return none
      let inputFlag : InputFlag := ⟨flagName, isShort⟩
      let some flag ← flag? inputFlag
        | throw <| ← parseError <| unknownFlag inputFlag
      if ¬ flag.isParamless then
        throw <| ← parseError <| missingFlagArg flag inputFlag
      ensureFlagUnique flag inputFlag
      skip
      return some ⟨flag, ""⟩

    private def parseFlag : ParseM Bool := do
      if let some parsedFlags ← readMultiFlag? then
        for parsedFlag in parsedFlags do
          pushParsedFlag parsedFlag
        return true
      let tryRead parse : OptionT ParseM Parsed.Flag := parse
      let some parsedFlag ←
          tryRead readEqFlag?     <|>
          tryRead readWsFlag?     <|>
          tryRead readPrefixFlag? <|>
          tryRead readParamlessFlag?
        | return false
      pushParsedFlag parsedFlag
      return true

    private def parsePositionalArg : ParseM Bool := do
      let some positionalArgValue ← peek?
        | return false
      let some positionalArg := (← cmd).meta.positionalArgs.get? (← parsedPositionalArgs).size
        | return false
      if ¬ positionalArg.type.isValid positionalArgValue then
        throw <| ← parseError <| invalidPositionalArgType positionalArg positionalArgValue
      skip
      pushParsedPositionalArg ⟨positionalArg, positionalArgValue⟩
      return true

    private def parseVariableArg : ParseM Bool := do
      let some variableArgValue ← peek?
        | return false
      let some variableArg := (← cmd).meta.variableArg?
        | throw <| ← parseError <| redundantPositionalArg variableArgValue
      if ¬ variableArg.type.isValid variableArgValue then
        throw <| ← parseError <| invalidVariableArgType variableArg variableArgValue
      skip
      pushParsedVariableArg ⟨variableArg, variableArgValue⟩
      return true

    private partial def parseArgs : ParseM Unit := do
      let mut parseEverythingAsArg := false
      let mut noEndOfInput := true
      while noEndOfInput do
        if ← (pure !parseEverythingAsArg) <&&> parseEndOfFlags then
          parseEverythingAsArg := true
        noEndOfInput := ← (pure !parseEverythingAsArg) <&&> parseFlag
                      <||> parsePositionalArg
                      <||> parseVariableArg

    private def parse (c : Cmd) (args : List String) : Except ParseError (Cmd × Parsed) :=
      parse' args.toArray |>.run' {
        idx                  := 0
        cmd                  := c
        parent?              := none
        parsedFlags          := #[]
        parsedPositionalArgs := #[]
        parsedVariableArgs   := #[]
      }
    where
      parse' : ParseM (Cmd × Parsed) := do
        parseSubCmds
        parseArgs
        let parsed : Parsed := {
          cmd            := .ofFullCmd (← cmd)
          parent?        := (← parent?).map .ofFullCmd
          flags          := ← parsedFlags
          positionalArgs := ← parsedPositionalArgs
          variableArgs   := ← parsedVariableArgs
        }
        if parsed.hasFlag "help" ∨ parsed.cmd.meta.hasVersion ∧ parsed.hasFlag "version" then
          return (← cmd, parsed)
        if (← parsedPositionalArgs).size < (← cmd).meta.positionalArgs.size then
          throw <| ← parseError <| missingPositionalArg <| (← cmd).meta.positionalArgs.get! (← parsedPositionalArgs).size
        return (← cmd, parsed)
  end ParseM

  namespace Cmd
    /--
    Parses `args` according to the specification provided in `c`, returning either a `ParseError` or the
    (sub)command that was called and the parsed content of the input.
    Note that `args` designates the list `<foo>` in `somebinary <foo>`.
    -/
    def parse (c : Cmd) (args : List String) : Except ParseError (Cmd × Parsed) :=
      ParseM.parse c args

    /-- Recursively applies extensions in all subcommands in `c` bottom-up and `c` itself. -/
    partial def applyExtensions (c : Cmd) : Cmd := Id.run do
      let subCmds := c.subCmds.map applyExtensions
      let c := c.update (subCmds := subCmds)
      let some extension := c.extension? 
        | return c
      extension.extend (.ofFullCmd c) |>.toFullCmd c

    /--
    Processes `args` by applying all extensions in `c`, `Cmd.parse?`ing the input according to `c` 
    and then applying the postprocessing extension of the respective (sub)command that was called.
    Note that `args` designates the list `<foo>` in `somebinary <foo>`.
    Returns either the (sub)command that an error occured in and the corresponding error message or
    the (sub)command that was called and the parsed input after postprocessing.
    -/
    def process (c : Cmd) (args : List String) : Except (Cmd × String) (Cmd × Parsed) := do
      let c := c.applyExtensions
      let result := c.parse args
      match result with
      | .ok (cmd, parsed) =>
        let some ext := cmd.extension?
          | return (cmd, parsed)
        match ext.postprocess (.ofFullCmd cmd) parsed with
        | .ok newParsed =>
          return (cmd, newParsed)
        | .error msg =>
          throw (cmd, msg)
      | .error err =>
        throw (err.cmd, err.kind.msg)
  end Cmd
end Parsing

section IO
  namespace Cmd
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
  end Cmd
end IO

end Cli