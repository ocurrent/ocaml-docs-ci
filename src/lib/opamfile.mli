type t = OpamParserTypes.opamfile

type pkg = { name : string; version : string; repo : string }

val get_packages : t -> pkg list

val marshal : t -> string

val unmarshal : string -> t

val digest : t -> string

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) result
