module

public import Cli.Basic
meta import Init.Data.Slice.Array.Iterator
meta import Init.Data.Slice.Array.Basic

section

namespace Cli

open Lean

section TypedCli
  private meta def kebabToCamelCase (s : String) : String := Id.run do
    let parts := s.split "-" |>.toArray
    let mut result : String := parts[0]!.toString
    for part in parts[1...*] do
      let some frontChar := part.front?
        | continue
      let frontChar := frontChar.toUpper
      let rest := part.drop 1
      result := result.push frontChar ++ rest
    return result

  private meta def mkTypedParsedDecls
      (name : TSyntax ``Cli.literalIdent)
      (version? : Option (TSyntax ``Cli.nameableStringArg))
      (description : TSyntax ``Cli.nameableStringArg)
      (flags? : Option (TSyntaxArray ``Cli.flag))
      (positionalArgs? : Option (TSyntaxArray ``Cli.positionalArg))
      (variableArg? : Option (Option (TSyntax ``Cli.variableArg)))
      (subCommands? : Option (Lean.Syntax.TSepArray `ident ";"))
      (extensions? : Option (Lean.Syntax.TSepArray `term ";"))
      : MacroM (Array Syntax) := do
    let cmdName := .mkSimple <| extractLiteralIdent name
    let cmdConstructorIdent := mkIdent (.str cmdName "mkCmd")
    let typedParsedDeclName := .str cmdName "Parsed"
    let typedParsedDeclIdent := mkIdent typedParsedDeclName
    let typedParsedDeclRawConstructorIdent := mkIdent (.str typedParsedDeclName "mk")

    let typedParsedDecl ← `(
      structure $typedParsedDeclIdent where
        raw : Cli.Parsed
      deriving Inhabited
    )

    let mut typedParsedDeclAccessors : Array Syntax := #[]
    for flag in flags?.getD #[] do
      let `(Cli.flag| $flagName1 $[, $flagName2?]? $[ : $flagType?]?; $_) := flag
        | Macro.throwUnsupported
      let longFlagName := extractLiteralIdent <| flagName2?.getD flagName1
      let longFlagNameTerm := quote longFlagName
      let flagAccessorName := kebabToCamelCase longFlagName
      let flagAccessor ←
        match flagType? with
        | none =>
          let flagAccessorIdent := mkIdent (.str typedParsedDeclName flagAccessorName)
          `(
            @[inline] def $flagAccessorIdent (p : $typedParsedDeclIdent) : Bool :=
              p.raw.hasFlag $longFlagNameTerm
          )
        | some flagType =>
          let flagAccessorIdent := mkIdent (.str typedParsedDeclName (flagAccessorName ++ "?"))
          `(
            @[inline] def $flagAccessorIdent (p : $typedParsedDeclIdent) : Option $flagType :=
              p.raw.flag? $longFlagNameTerm |>.map (·.as! $flagType)
          )
      typedParsedDeclAccessors := typedParsedDeclAccessors.push flagAccessor
    for arg in positionalArgs?.getD #[] do
      let `(Cli.positionalArg| $argName : $argType; $_) := arg
        | Macro.throwUnsupported
      let argName := extractLiteralIdent argName
      let argNameTerm := quote argName
      let argAccessorName := kebabToCamelCase argName
      let argAccessorIdent := mkIdent (.str typedParsedDeclName argAccessorName)
      let argAccessor ← `(
        @[inline] def $argAccessorIdent (p : $typedParsedDeclIdent) : $argType :=
          p.raw.positionalArg! $argNameTerm |>.as! $argType
      )
      typedParsedDeclAccessors := typedParsedDeclAccessors.push argAccessor
    if let some variableArg := Option.join variableArg? then
      let `(Cli.variableArg| ...$variableArgName : $variableArgType; $_) := variableArg
        | Macro.throwUnsupported
      let variableArgAccessorName := extractLiteralIdent variableArgName |> kebabToCamelCase
      let variableArgAccessorIdent := mkIdent (.str typedParsedDeclName variableArgAccessorName)
      let variableArgAccessor ← `(
        @[inline] def $variableArgAccessorIdent (p : $typedParsedDeclIdent) : Array $variableArgType :=
          p.raw.variableArgsAs! $variableArgType
      )
      typedParsedDeclAccessors := typedParsedDeclAccessors.push variableArgAccessor

    let cmdInstantiationTerm ← mkCmdInstantiationTerm
      name
      version?
      description
      flags?
      positionalArgs?
      variableArg?
      subCommands?
      extensions?
      (← `(fun raw => handler ($typedParsedDeclRawConstructorIdent raw)))
    let cmdConstructor ← `(
      def $cmdConstructorIdent
          (handler : $typedParsedDeclIdent → IO UInt32)
          : Cmd :=
        $cmdInstantiationTerm
    )

    return #[typedParsedDecl] ++ typedParsedDeclAccessors ++ #[cmdConstructor]

  syntax "cli_def " literalIdent runFun "; " ("[" nameableStringArg "]")?
      nameableStringArg
      ("FLAGS:\n" withPosition((flag)*))?
      ("ARGS:\n" withPosition((positionalArg)* (variableArg)?))?
      ("SUBCOMMANDS: " sepBy(ident, ";", "; "))?
      ("EXTENSIONS: " sepBy(term, ";", "; "))?
    : command

  syntax "cli_def " literalIdent "; " ("[" nameableStringArg "]")?
      nameableStringArg
      ("FLAGS:\n" withPosition((flag)*))?
      ("ARGS:\n" withPosition((positionalArg)* (variableArg)?))?
      ("SUBCOMMANDS: " sepBy(ident, ";", "; "))?
      ("EXTENSIONS: " sepBy(term, ";", "; "))?
      (" RUN " term)?
    : command

  macro_rules
    | `(cli_def $name $run:runFun; $[[$version?]]?
        $description
        $[FLAGS:
          $flags?*
        ]?
        $[ARGS:
          $positionalArgs?*
          $[$variableArg?]?
        ]?
        $[SUBCOMMANDS: $subCommands?;*]?
        $[EXTENSIONS: $extensions?;*]? ) => do
      let cmdIdent := mkIdent (.mkSimple (extractLiteralIdent name))
      let cmdMk ← mkCmdInstantiationTerm
        name
        version?
        description
        flags?
        positionalArgs?
        variableArg?
        subCommands?
        extensions?
        (← expandRunFun run)
      `(def $cmdIdent : Cmd := $cmdMk)

  macro_rules
    | `(cli_def $name; $[[$version?]]?
        $description
        $[FLAGS:
          $flags?*
        ]?
        $[ARGS:
          $positionalArgs?*
          $[$variableArg?]?
        ]?
        $[SUBCOMMANDS: $subCommands?;*]?
        $[EXTENSIONS: $extensions?;*]?
        $[RUN $handlerTerm?]?) => do
      let typedParsedDecls ← mkTypedParsedDecls
        name
        version?
        description
        flags?
        positionalArgs?
        variableArg?
        subCommands?
        extensions?
      let some handlerTerm := handlerTerm?
        | return mkNullNode typedParsedDecls
      let cmdName := .mkSimple <| extractLiteralIdent name
      let cmdIdent := mkIdent cmdName
      let cmdConstructorIdent := mkIdent (Name.mkStr cmdName "mkCmd")
      let cmd ← `(def $cmdIdent : Cmd := $cmdConstructorIdent $handlerTerm)
      return mkNullNode (typedParsedDecls ++ #[cmd])
end TypedCli

end Cli
