type error =
| Unclosed of Location.t * string * Location.t * string
| Expected of Location.t * string
| Other of Location.t

exception Error of error

val report_error : Format.formatter -> error -> unit
