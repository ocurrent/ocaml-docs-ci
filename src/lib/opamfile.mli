type t = OpamParserTypes.opamfile

type pkg = { name : string; version : string; repo : string }

val get_packages : t -> pkg list

val marshal : t -> string

val unmarshal : string -> t

val digest : t -> string
