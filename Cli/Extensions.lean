import Cli.Basic

section Utils
  namespace Array
    /--
    Appends those elements of `right` to `left` whose `key` is not already
    contained in `left`.
    -/
    def leftUnionBy [Ord α] (key : β → α) (left : Array β) (right : Array β)
      : Array β := Id.run do
      let leftMap := left.map (fun v => (key v, v)) |>.toList |> Std.RBMap.ofList (cmp := compare)
      let mut result := left
      for v in right do
        if ¬ leftMap.contains (key v) then
          result := result.push v
      return result

    /--
    Prepends those elements of `left` to `right` whose `key` is not already
    contained in `right`.
    -/
    def rightUnionBy [Ord α] (key : β → α) (left : Array β) (right : Array β)
      : Array β := Id.run do
      let rightMap := right.map (fun v => (key v, v)) |>.toList |> Std.RBMap.ofList (cmp := compare)
      let mut result := right
      for v in left.reverse do
        if ¬ rightMap.contains (key v) then
          result := #[v] ++ result
      return result

    /-- Deletes all elements from `left` whose `key` is in `right`. -/
    def diffBy [Ord α] (key : β → α) (left : Array β) (right : Array α)
      : Array β :=
      let rightMap := Std.RBTree.ofList (cmp := compare) right.toList
      left.filter fun v => ¬ (rightMap.contains <| key v)
  end Array
end Utils

namespace Cli

section Extensions
  /-- Prepends an author name to the description of the command. -/
  def author (author : String) : Extension := {
      extendMeta := fun meta => { meta with description := s!"{author}\n{meta.description}" }
    }

  /-- Appends a longer description to the end of the help. -/
  def longDescription (description : String) : Extension := {
      extendMeta := fun meta => { meta with furtherInformation? :=
        some <| meta.furtherInformation?.optStr ++ lines #[
          meta.furtherInformation?.optStr,
          (if meta.furtherInformation?.isSome then "\n" else "") ++ renderSection "DESCRIPTION" description
        ]
      }
    }

  /--
  Sets default values for flags that were not set by the user according to
  `defaults := #[(long flag name, default value), ...]` and denotes the default value
  in the flag description of the help.
  Panics if one of the designated long flag names cannot be found in the command.
  -/
  def defaultValues! (defaults : Array (String × String)) : Extension :=
    let findDefaultFlags meta := defaults.map <| fun (longName, defaultValue) =>
      ⟨meta.flag! longName, defaultValue⟩
    {
      extendMeta := fun meta =>
        let defaultFlags := findDefaultFlags meta
        let newMetaFlags := meta.flags.map fun flag =>
          if let some defaultFlag := defaultFlags.find? (·.flag.longName = flag.longName) then
            { flag with description := flag.description ++ s!" [Default: `{defaultFlag.value}`]" }
          else
            flag
        { meta with flags := newMetaFlags }
      postprocess := fun meta parsed =>
        let defaultFlags := findDefaultFlags meta
        return { parsed with flags := parsed.flags.leftUnionBy (·.flag.longName) defaultFlags }
    }

  /--
  Errors if one of `requiredFlags := #[long flag name, ...]` were not passed by the user.
  Denotes that the flag is required in the flag description of the help.
  Panics if one of the designated long flag names cannot be found in the command.
  -/
  def require! (requiredFlags : Array String) : Extension :=
    let findRequiredFlags meta := requiredFlags.map (meta.flag! ·)
    {
      extendMeta := fun meta =>
        let requiredFlags := findRequiredFlags meta
        let newMetaFlags := meta.flags.map fun flag =>
          if requiredFlags.find? (·.longName = flag.longName) |>.isSome then
            { flag with description := "[Required] " ++ flag.description }
          else
            flag
        { meta with flags := newMetaFlags }
      postprocess := fun meta parsed => do
        if parsed.hasFlag "help" ∨ parsed.hasFlag "version" then
          return parsed
        let requiredFlags := findRequiredFlags meta
        let missingFlags := requiredFlags.diffBy (·.longName) <| parsed.flags.map (·.flag.longName)
        if let some missingFlag ← pure <| missingFlags.get? 0 then
          throw s!"Missing required flag `--{missingFlag.longName}`."
        return parsed
    }
end Extensions

end Cli