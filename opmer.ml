module L = BatList
module String = BatString
module Ht = BatHashtbl

open Printf

type directory = string
type action = Sign of directory
            | Compare of directory * directory

let line_of_command cmd =
  Log.debug "line_of_command: %s" cmd;
  let input = Unix.open_process_in cmd in
  let line = try input_line input with End_of_file -> "" in
  ignore (Unix.close_process_in input);
  line

let lines_of_command cmd =
  Log.debug "lines_of_command: %s" cmd;
  let buff = Buffer.create 1024 in
  let input = Unix.open_process_in cmd in
  (try
     while true do
       let line = input_line input in
       Buffer.add_string buff line;
       Buffer.add_char buff '\n'
     done
   with End_of_file -> ());
  let (_: Unix.process_status) = Unix.close_process_in input in
  let big_string = Buffer.contents buff in
  BatString.split_on_char '\n' big_string

let get_nprocs () =
  (* warning: maybe no so portable *)
  int_of_string (line_of_command "getconf _NPROCESSORS_ONLN")

let hash_file fn =
  line_of_command (sprintf "sha256sum %s | cut -d' ' -f1" fn)

(* let get_one to_process () = *)
(*   match !to_process with *)
(*   | [] -> raise Parany.End_of_input *)
(*   | x :: xs -> *)
(*     let res = x in *)
(*     to_process := xs; *)
(*     res *)

(* let process_one fn = *)
(*   Log.info "%s" fn; *)
(*   hash_file fn *)

(* let collect_one ht (fn, hash) = *)
(*   Ht.add ht fn hash *)

(* Merkle tree signing of the directory *)
let sign dir =
  let files =
    lines_of_command
      (* find all pure files under dir, skipping hidden directories *)
      (sprintf "find %s -not -path '*/\\.*' -type f -not -type d | sort" dir) in
  (* sha256sum each one *)
  let fn2hash = Ht.create 11 in
  L.iter (fun fn ->
      Ht.add fn2hash fn (hash_file fn)
    ) files;
  (* let csize = 1 in *)
  (* let nprocs = get_nprocs () in *)
  (* Parany.run ~csize ~nprocs *)
  (*   ~demux:(get_one (ref files)) *)
  (*   ~work:process_one *)
  (*   ~mux:(collect_one fn2hash); *)
  Log.info "hashed %d" (Ht.length fn2hash)
    
let main () =
  Log.set_output stderr;
  Log.color_on ();
  Log.set_log_level Log.INFO;
  let argc, args = CLI.init () in
  if argc = 1 then
    (eprintf "usage:\n\
              %s {--sign <repo>|--diff <repo1>:<repo2>}\n" Sys.argv.(0);
     exit 1);
  let action =
    match CLI.get_string_opt ["-s";"--sign"] args with
    | Some fn -> Sign fn
    | None ->
      match CLI.get_string_opt ["-d";"--diff"] args with
      | None -> failwith "use either --diff <repo1>:<repo2> or --sign <repo>"
      | Some fn ->
        let left, right = BatString.split ~by:":" fn in
        Compare (left, right)
  in
  match action with
  | Sign dir -> sign dir
  | Compare (_, _) -> failwith "not implemented yet"

let () = main ()
