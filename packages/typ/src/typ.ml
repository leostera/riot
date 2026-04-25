module Model = Model
module Ast = Ast
module Diagnostics = Diagnostics
module Check = Check

type source = Model.Source.t

type checked_source = Check.Typings.t
