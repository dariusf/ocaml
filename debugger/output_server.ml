
let my_name = Unix.gethostname ()
let my_entry_byname = Unix.gethostbyname my_name
let my_addr = my_entry_byname.Unix.h_addr_list.(0)
let port = 12345

(* should only be accessed from parent *)
let input_descr = ref None

let get_input_descr () =
  match !input_descr with
  | None -> failwith "input_descr should be set by now!"
  | Some d -> d

let output_channel = ref None

type client = Parent | Child

(* TODO close sockets properly on termination *)

let string_of_client = function
  | Parent -> "Parent"
  | Child -> "Child"

let print_as client msg =
  print_endline @@ string_of_client client ^ ": " ^ msg

let rec connect_to_socket client =
  let rec try_connecting printed =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    try
      Unix.connect socket (Unix.ADDR_INET (my_addr, port));
      let socket_out = Unix.out_channel_of_descr socket in
      output_channel := Some socket_out
    with
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
      if not printed then
        print_as client "waiting to connect to socket..."
      else ();
      Unix.close socket;
      Unix.sleep 1;
      try_connecting true
  in try_connecting false

(* let shutdown () = *)
(*   match !out_socket with *)
(*   | None -> () *)
(*   | Some socket -> Unix.close socket *)

let write_to_server content =
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
    print_endline "child: parent is dead, terminating";
    (* shutdown (); *)
    exit 0
  end
  else begin
    print_endline "child: parent not dead, continuing"; ()
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
    | "" -> print_endline "done for now"; ()
    | line ->
        print_endline @@ "received: " ^ line;
        write_to_server line;

        exit_if_parent_dead ()
  done

(* TODO close the used descriptors? *)

let process_child fd_in fd_out =
  Unix.close fd_out;
  connect_to_socket Child;
  let fd_in_channel = Unix.in_channel_of_descr fd_in in
  read_debuggee_output fd_in_channel

let process_parent fd_in fd_out =
  Unix.close fd_in;
  connect_to_socket Parent;
  input_descr := Some fd_out

let start () =

  (* fd_out -> pipe -> fd_in *)
  let (fd_in, fd_out) = Unix.pipe () in

  match Unix.fork () with
  | 0 -> process_child fd_in fd_out
  | _ -> process_parent fd_in fd_out 
