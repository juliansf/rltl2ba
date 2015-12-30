type level = int

type kind =
  | DEBUG
  | TRACE

val enable : kind -> level -> unit
val disable : kind -> unit
val is_enabled : kind  -> bool
val enable_all : unit -> unit
val level : kind -> level

val log:
  ?out:out_channel -> kind -> level -> ('a, out_channel, unit) format -> 'a
val debug:
  ?out:out_channel -> ?level:level -> ('a, out_channel, unit) format -> 'a
val trace:
  ?out:out_channel -> ?level:level -> ('a, out_channel, unit) format -> 'a

val log_exec:
  ?out:out_channel -> kind -> level -> (unit -> unit) -> unit
val debug_exec:
  ?out:out_channel -> ?level:level -> (unit -> unit) -> unit
val trace_exec:
  ?out:out_channel -> ?level:level -> (unit -> unit) -> unit
