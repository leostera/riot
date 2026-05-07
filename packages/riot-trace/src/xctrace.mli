open Std

val summarize_time_profile_xml: string -> Profile.t

val summarize_file: Path.t -> (Profile.t, string) result
