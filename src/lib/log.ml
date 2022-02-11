let logs = Logs.Src.create "ocaml-docs-ci"

module Log = (val Logs.src_log logs : Logs.LOG)
include Log
