open Std

type size_filter =
  | All
  | Small
  | Large
type request = {
  package_filters: Riot_model.Package_name.t list;
  package_filter: Riot_model.Package_name.t option;
  suite_filter: string option;
  query: string option;
  size_filter: size_filter;
  flaky_only: bool;
}

val parse_request:
  filter:string option ->
  package_filters:Riot_model.Package_name.t list ->
  size_filter:size_filter ->
  flaky_only:bool ->
  (request, string) result

val extra_args:
  ?small_test_timeout:Time.Duration.t option ->
  ?flaky_max_retries:int ->
  request ->
  string list ->
  string list
