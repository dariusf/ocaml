
let my_name = Unix.gethostname ()
let my_entry_byname = Unix.gethostbyname my_name
let my_addr = my_entry_byname.Unix.h_addr_list.(0)
let port = 12345

(* should only be accessed from parent *)
(* let input_descr = ref None *)

(* debuggee_put_things_in_here -> pipe -> debuggee_things_come_out_here *)
let (debuggee_things_come_out_here, debuggee_put_things_in_here) = Unix.pipe ()

let debuggee_read_from_here = Unix.in_channel_of_descr debuggee_things_come_out_here

(* let get_input_descr () =
  match !input_descr with
  | None -> failwith "input_descr should be set by now!"
  | Some d -> d
 *)

(* let output_channel = ref None *)

(* type client = Parent | Child *)

(* let string_of_client = function
  | Parent -> "Parent"
  | Child -> "Child"

let print_as client msg =
  print_endline @@ string_of_client client ^ ": " ^ msg
 *)


let client_socket = ref None

let shutdown () =
  match !client_socket with
  | None -> ()
  | Some socket -> Unix.close socket

(* let write_to_server content (* = *)
  match !output_channel with
  | None -> ()
  | Some channel ->
      (* print_endline @@ "sending " ^ content; *)
      output_string channel content;
      output_string channel "\n";
      flush channel

let exit_if_parent_dead () =
  let ppid = Unix.getppid () in
  if ppid = 1
  then begin
    (* print_endline "child: parent is dead, terminating"; *)
    (* shutdown (); *)
    exit 0
  end
  else begin
    (* print_endline "child: parent not dead, continuing"; *)
    ()
  end

let read_debuggee_output fd_in_channel =
  (* let i = ref 0 in *)
  while true do

    (* safeguard *)
    (* i := !i + 1; *)
    (* if !i > 3000 then (print_endline "dead, stopping"; exit 0) else (); *)

    (* block on the descriptor and read some stuff *)
    let line = try input_line fd_in_channel with End_of_file -> "" in

    match line with
    | "" -> ()
      (* print_endline "done for now"; *)
    | line ->
        (* print_endline @@ "received: " ^ line; *)
        write_to_server line;
        exit_if_parent_dead ()
  done *)

(*
let process_child fd_in fd_out =
  Unix.close fd_out;
  (* connect_to_socket Child; *)
  let fd_in_channel = Unix.in_channel_of_descr fd_in in
  read_debuggee_output fd_in_channel

let process_parent fd_in fd_out =
  Unix.close fd_in;
  (* connect_to_socket Parent; *)
  input_descr := Some fd_out
 *)

module CQueue = struct
  type 'a t =
    { queue : 'a Queue.t; lock : Mutex.t; non_empty : Condition.t }

  let create () =
    { queue = Queue.create ();
      lock = Mutex.create (); non_empty = Condition.create () }

  let add e q =
    Mutex.lock q.lock;
    if Queue.length q.queue = 0 then Condition.broadcast q.non_empty;
    Queue.add e q.queue;
    Mutex.unlock q.lock;;

  let take q =
    Mutex.lock q.lock;
    while Queue.length q.queue = 0
    do Condition.wait q.non_empty q.lock done;
    let x = Queue.take q.queue in
    Mutex.unlock q.lock; x;;
end

(*

let produce_continuously q n =
  while true do
    Thread.delay 1.0;
    CQueue.add n q;
    do_locked print_lock (fun () -> print_endline @@ "  produced " ^ string_of_int n)
  done

;; *)

(* let queue = CQueue.create () in
let _consumer = Thread.create consume_from queue in
let _producers = List.map (Thread.create (produce_continuously queue)) [1; 2; 3] in
Thread.join _consumer;
List.iter Thread.join _producers
;;
 *)


let output_queue = CQueue.create ()

(* type event = Event of string | Time_stamp of int *)

(* type color = Red of string | Blue of int *)

(* type buffer_event = A | B *)

(* let string_of_buffer_event be =
  match be with
  | A -> "A"
  | B -> "B"
 *)

(* let string_of_event be = function *)
  (* | Event s -> s *)
  (* | Time_stamp i -> string_of_int i *)

(* let send_event (e:event) = *)
let send_event e =
  CQueue.add e output_queue

let read_from_debuggee () =
  (* let i = ref 0 in *)
  while true do
    (* safeguard *)
    (* i := !i + 1; *)
    (* if !i > 3000 then (print_endline "dead, stopping"; exit 0) else (); *)

    (* block on the descriptor and read some stuff *)
    let line = try Some (input_line debuggee_read_from_here) with End_of_file -> None in

    match line with
    | None ->
      print_endline "done for now";
      ()
    | Some line ->
      print_endline @@ "received: " ^ line;
      send_event line
  done

let rec connect_to_buffer_server () =
  let rec try_connecting printed =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    try
      Unix.connect socket (Unix.ADDR_INET (my_addr, port));
      client_socket := Some socket;
      Unix.out_channel_of_descr socket
    with
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
      if not printed then
        print_endline "waiting to connect to socket..."
      else ();
      Unix.close socket;
      Unix.sleep 1;
      try_connecting true
  in try_connecting false

(* let do_locked lock f =
  Mutex.lock print_lock;
  f ();
  Mutex.unlock print_lock
 *)

let starts_with str e =
  let n_e = String.length e in
  let n_s = String.length str in
  if n_s < n_e then
    false
  else
    let sub = String.sub str 0 n_e in
    sub = e

let get_time input =
  (* print_endline @@ "get_time " ^ input; *)
  (* print_endline @@ "get_time blah " ^ string_of_int (String.length input - 6); *)
  if starts_with input ":time " then
    try
      Some (int_of_string @@ String.sub input 6 (String.length input - 6))
    with Failure "int_of_string" -> None
  else None

let split s =
  match s with
  | "" -> []
  | _ ->
  let rec aux s i start =
    try
      match String.get s i with
      | ' ' -> String.sub s start (i-start) :: aux s (i+1) i
      | _ -> aux s (i+1) start
    with Invalid_argument "index out of bounds" ->
      [String.sub s start (i-start)]
  in List.map String.trim @@ aux s 0 0
;;

(* split "a b" *)

;;


let get_time input =
  (* print_endline @@ "get_time " ^ input; *)
  (* print_endline @@ "get_time blah " ^ string_of_int (String.length input - 6); *)
  if starts_with input ":time " then
    try
      Some (int_of_string @@ List.nth (split input) 1)
    with Failure "int_of_string" -> None
  else None

let tests = [
  lazy (starts_with "" "");
  lazy (starts_with "a" "a");
  lazy (starts_with "abc" "ab");
  lazy (not @@ starts_with "a" "");
  lazy (not @@ starts_with "a" "s");
  lazy (not @@ starts_with "abc" "ac");

  lazy (get_time "" = None);
  lazy (get_time ":time" = None);
  lazy (get_time ":time " = None);
  lazy (get_time "asd" = None);
  lazy (get_time ":time 123" = Some 123);
  lazy (get_time ":time 1" = Some 1);

]

let tests2 = [
  lazy (split "" = []);
  lazy (split "a" = ["a"]);
  lazy (split "a b" = ["a"; "b"]);
  lazy (split "a b c" = ["a"; "b"; "c"]);
]
;;

(* List.iteri (fun i t -> print_endline @@ string_of_int i; print_endline @@ string_of_bool (Lazy.force t)) tests2 *)

;;

let write_to out_channel elt =
  output_string out_channel elt;
  output_string out_channel "\n";
  flush out_channel

let last_non_time_stamp = ref None

let before_go () =
  (* print_endline "just went"; *)
  last_non_time_stamp := None

let decide_what_to_write_to out_channel elt =
  (* write_to out_channel elt *)
  (* print_endline @@ " -> " ^ elt; *)
  match get_time elt with
  | Some time ->
      (* print_endline "got time"; *)
      begin match !last_non_time_stamp with
      | None ->
      (* print_endline "no buffering element"; *)
          ()
      | Some last ->
      (* print_endline @@ "buffering element " ^ last; *)
          last_non_time_stamp := None;
          write_to out_channel @@ Printf.sprintf ":time %d %s" time last
      end
  | None ->
      (* print_endline @@ "storing " ^ elt; *)
      begin match !last_non_time_stamp with
      | None -> 
          (* print_endline "just stored, nothing else" *)
            ()
      | Some thing ->
          (* print_endline @@ "also sending" ^ thing; *)
          write_to out_channel thing
      end;
      last_non_time_stamp := Some elt

let consume_and_write_to out_channel =
  
  while true do
    (* TODO get some escaping going on *)
    let elt = CQueue.take output_queue in
    decide_what_to_write_to out_channel elt
  done

let start () =

  (* connect to client *)
  let buffer_server_channel = connect_to_buffer_server () in

  (* spin up thread to read from queue and send everything to buffer server *)
  let _sender = Thread.create consume_and_write_to buffer_server_channel in

  (* another thread to receive input and add that to the queue, to be consumed by the sender *)
  let _debuggee_relay = Thread.create read_from_debuggee () in
  ()

  (* have a function to add events to the queue *)

  (* fd_out -> pipe -> fd_in *)
  (* let (fd_in, fd_out) = Unix.pipe () in *)

  (* match Unix.fork () with *)
  (* | 0 -> process_child fd_in fd_out *)
  (* | _ -> process_parent fd_in fd_out  *)
