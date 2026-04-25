(**
   Typ - Riot's type-checker engine.

   Current rewrite path:
   `Syn.Ast -> Typ.Ast -> Typ.Check -> Typ.Check.Typings`.
*)
module Model: module type of Model

module Ast: module type of Ast

module Diagnostics: module type of Diagnostics

module Check: module type of Check

type source = Model.Source.t
type checked_source = Check.Typings.t
