(** Compatibility facade for the Riot actor runtime.

    The runtime implementation now lives in `Std.Runtime`. This package keeps
    the historical `Actors` entrypoint available while the rest of the repo
    migrates. *)

include module type of Std.Runtime
