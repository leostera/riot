(**
   Command-line argument parsing.

   Type-safe, declarative command-line argument parser with support for
   flags, options, positional arguments, subcommands, and validation.

   ## Examples

   Basic flag and option parsing:

   ```ocaml
   open Std

   let cmd = Arg_Parser.command "myapp"
     |> Arg_Parser.about "My application"
     |> Arg_Parser.version "1.0.0"
     |> Arg_Parser.args [
         Arg_Parser.Arg.flag "verbose"
           |> Arg_Parser.Arg.short 'v'
           |> Arg_Parser.Arg.help "Enable verbose output";

         Arg_Parser.Arg.option "output"
           |> Arg_Parser.Arg.short 'o'
           |> Arg_Parser.Arg.long "output"
           |> Arg_Parser.Arg.value_name "FILE"
           |> Arg_Parser.Arg.help "Output file path";

         Arg_Parser.Arg.positional "input"
           |> Arg_Parser.Arg.required true
           |> Arg_Parser.Arg.help "Input file";
       ]

   let matches = Arg_Parser.get_matches cmd (List.tl (Array.to_list Sys.argv))
     |> Result.expect ~msg:"Failed to parse arguments"

   let verbose = Arg_Parser.get_flag matches "verbose" in
   let output = Arg_Parser.get_one matches "output" in
   let input = Arg_Parser.get_path matches "input"
     |> Option.expect ~msg:"Input required"
   ```

   Subcommands:

   ```ocaml
   let install_cmd = Arg_Parser.command "install"
     |> Arg_Parser.about "Install a package"
     |> Arg_Parser.arg (
         Arg_Parser.Arg.positional "package"
           |> Arg_Parser.Arg.required true
       )

   let remove_cmd = Arg_Parser.command "remove"
     |> Arg_Parser.about "Remove a package"

   let cmd = Arg_Parser.command "pkgman"
     |> Arg_Parser.subcommands [install_cmd; remove_cmd]

   let matches = Arg_Parser.get_matches cmd args
     |> Result.expect ~msg:"Failed to parse" in

   match Arg_Parser.get_subcommand matches with
   | Some ("install", sub_matches) -> install (Arg_Parser.get_one sub_matches "package")
   | Some ("remove", sub_matches) -> remove ()
   | _ -> Arg_Parser.print_help cmd
   ```

   ## Features

   - Type-safe argument definitions with builder pattern
   - Flags, options, positional args, trailing args
   - Subcommands with nested argument handling
   - Automatic help generation
   - Environment variable fallbacks
   - Validation (required, conflicts, possible values)
   - Multiple values and counting

   ## When to Use

   - Command-line tools with complex argument structures
   - Applications with subcommands (like git, cargo, etc.)
   - When you need validation and help text generation

   For simple scripts, manual argv parsing might be sufficient.
*)
open Global

(** How an argument's value should be set. *)
type action =
  | Set
  | SetTrue
  | SetFalse
  | Append
  | Count
(** Command-line argument definition. *)
type 'a arg
(** Command definition with arguments and subcommands. *)
type command
(** Parsed argument matches. *)
type matches
type error =
  | UnknownArgument of string
  | MissingRequired of string
  | InvalidValue of string * string
  | UnknownSubcommand of string
  | MissingSubcommand
  | ConflictingArguments of string * string
  | TooManyValues of string
  | TooFewValues of string

(** Parsing errors. *)
module Arg: sig
  type 'a t = 'a arg

  val flag: string -> unit t

  val option: string -> unit t

  val positional: string -> unit t

  val trailing: string -> unit t

  val short: char -> 'a t -> 'a t

  val long: string -> 'a t -> 'a t

  val help: string -> 'a t -> 'a t

  val value_name: string -> 'a t -> 'a t

  val default: string -> 'a t -> 'a t

  val required: bool -> 'a t -> 'a t

  val env: string -> 'a t -> 'a t

  val action: action -> 'a t -> 'a t

  val multiple: 'a t -> 'a t

  val count: 'a t -> 'a t

  val possible_values: string list -> 'a t -> 'a t

  val conflicts_with: string -> 'a t -> 'a t

  val requires: string -> 'a t -> 'a t
end

val command: string -> command

val version: string -> command -> command

val about: string -> command -> command

val author: string -> command -> command

val arg: unit arg -> command -> command

val args: unit arg list -> command -> command

val subcommand: command -> command -> command

val subcommands: command list -> command -> command

val allow_no_subcommand: command -> command

val allow_trailing_args: command -> command

val get_matches: command -> string list -> (matches, error) result

val get_one: matches -> string -> string option

val get_flag: matches -> string -> bool

val get_count: matches -> string -> int

val get_many: matches -> string -> string list

val get_int: matches -> string -> int option

val get_float: matches -> string -> float option

val get_path: matches -> string -> Path.t option

val get_subcommand: matches -> (string * matches) option

val subcommand_name: matches -> string option

val subcommand_matches: matches -> string -> matches option

val trailing_args: matches -> string list

val print_help: command -> unit

val error_message: error -> string

val print_error: error -> unit

val usage_string: command -> string

val print_usage: command -> unit
