

(* type event = Event of string | Time_stamp of int *)

val debuggee_put_things_in_here : Unix.file_descr
val send_event : string -> unit
val start : unit -> unit
val shutdown : unit -> unit
(* val before_go : unit -> unit *)
