open Std

let materialize = fun (config: Context.t) ->
  Writer.write_file
    config
    ~relative_path:".gitignore"
    ~content:{|# Riot build artifacts
_build

# Riot fuzzing generated state
.riot/fuzzing/**/corpus/
.riot/fuzzing/**/redundant/
.riot/fuzzing/**/findings/
|}
    ~executable:false
