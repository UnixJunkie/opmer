module L = BatList
module String = BatString
module Ht = BatHashtbl

open Printf

type directory = string

type action = Sign of directory (* create .sha256 files in repository *)
            | Compare of directory * directory (* compare two already signed repos *)
            | Clear of directory (* remove all .sha256 files under dir *)

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
  (* skip last '\n' *)
  let n = Buffer.length buff in
  let big_string = Buffer.sub buff 0 (n - 1) in
  BatString.split_on_char '\n' big_string

let get_nprocs () =
  (* warning: maybe no so portable *)
  int_of_string (line_of_command "getconf _NPROCESSORS_ONLN")

let hash_file fn =
  if fn = "" then failwith (sprintf "hash_file: empty fn");
  line_of_command
    (sprintf "sha256sum %s | cut -d' ' -f1 | tee %s.sha256" fn fn)

let get_one to_process () =
  match !to_process with
  | [] -> raise Parany.End_of_input
  | x :: xs ->
    let res = x in
    to_process := xs;
    res

let process_one fn =
  Log.debug "%s" fn;
  (fn, hash_file fn)


let finished = ref 0

let collect_one nb_files ht (fn, hash) =
  Ht.add ht fn hash;
  incr finished;
  if !finished mod 1000 = 0 then
    eprintf "done: %.1f%%\r%!" (100.0 *. (float !finished /. float nb_files))

(* Merkle tree signing of the directory *)
let sign nprocs dir =
  let files =
    lines_of_command
      (* find all pure files under dir, skipping hidden files and directories
         and *.sha256 files *)
      (sprintf "find %s -not -type d | \
                grep -v -P '(/\\..+|.+\\.sha256$)' | \
                sort" dir) in
  let nb_files = L.length files in
  (* sha256sum each one *)
  let fn2hash = Ht.create 11 in
  Parany.run ~csize:1 ~nprocs
    ~demux:(get_one (ref files))
    ~work:process_one
    ~mux:(collect_one nb_files fn2hash);
  Log.info "hashed %d" (Ht.length fn2hash)

let clear dir =
  let (_: string) =
    line_of_command
      (sprintf "find %s -regex '.*\\.sha256$' -exec rm -f {} \\;" dir) in
  ()

let main () =
  Log.set_output stderr;
  Log.color_on ();
  Log.set_log_level Log.INFO;
  let argc, args = CLI.init () in
  if argc = 1 then
    (eprintf "usage:\n\
              %s {--sign <repo>|--diff <repo1>:<repo2>}\n" Sys.argv.(0);
     exit 1);
  if CLI.get_set_bool ["-v";"--verbose"] args
  then Log.set_log_level Log.DEBUG;
  let nprocs = match CLI.get_int_opt ["-n";"--nprocs"] args with
    | None -> get_nprocs () (* we use all detected CPUs by default *)
    | Some i -> i (* unless we are told not to do so *) in
  let action =
    match CLI.get_string_opt ["-s";"--sign"] args with
    | Some fn -> Sign fn
    | None ->
      match CLI.get_string_opt ["-d";"--diff"] args with
      | Some fn ->
        let left, right = BatString.split ~by:":" fn in
        Compare (left, right)
      | None ->
        match CLI.get_string_opt ["-c";"--clear"] args with
        | Some dir -> Clear dir
        | None ->
          failwith  "use either --diff <repo1>:<repo2> OR \
                     --sign <repo> OR --clear <repo>"
  in
  match action with
  | Sign dir -> sign nprocs dir
  | Clear dir -> clear dir
  | Compare (_, _) -> failwith "not implemented yet"

let () = main ()
