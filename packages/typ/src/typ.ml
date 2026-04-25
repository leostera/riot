module Model = Model
module Ast = Ast
module Analysis = Analysis
module SourceAnalysis = SourceAnalysis
module PackageEnv = PackageEnv
module ScopeView = ScopeView
module ImportedWorld = ImportedWorld
module ModulePairing = ModulePairing
module ModuleSurface = ModuleSurface
module Infer = Infer
module Event = Event
module Diagnostics = Diagnostics
module MissingRequirements = MissingRequirements
module Query = Query
module Session = Session
module Store = Store
module Check = Check
module Config = Config

type config = Config.t

type source = Model.Source.t

type checked_source = Analysis.Check_result.t

let check = fun ~config:_ ~source:_ ->
  Std.panic
    "Typ.check is not wired to the new Source surface yet; use Typ.Check.check"
