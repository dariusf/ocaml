(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*       Nicolas Pouillard, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Input_handling
open Primitives

(* Ask user a yes or no question. *)
let yes_or_no message =
  if !interactif then
      try
        question := Some (message ^ " ? (y or n) ");
        let answer =
          let rec ask () =
            resume_user_input ();
            let line =
              string_trim (Lexer.line (Lexing.from_function read_user_input))
            in
              stop_user_input ();
              match (if String.length line > 0 then line.[0] else ' ') with
                'y' -> true
              | 'n' -> false
              | _ ->
                print_string "Please answer y or n.";
                print_newline ();
                ask ()
          in
            ask ()
        in
          question := None;
          answer
      with
        x ->
          question := None;
          stop_user_input ();
          raise x
  else
    false
