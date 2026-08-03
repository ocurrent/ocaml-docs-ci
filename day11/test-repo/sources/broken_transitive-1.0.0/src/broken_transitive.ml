(** Broken_transitive — compiles fine on its own, but depends on
    {!Broken} whose build fails. Used to probe how the GUI surfaces
    transitive failures (a package that breaks because a dep broke,
    not because of anything in its own source). *)

let uses_broken () = Broken.x
