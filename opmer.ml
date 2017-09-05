module L = BatList
module String = BatString
module Ht = BatHashtbl

open Printf

type directory = string

(* FBR: assert we are given directories; not regular files *)
type action = Hash of directory (* create .sha256 files in repository *)
            | Compare of directory * directory (* compare two already signed repos *)
            | Clear of directory (* remove all .sha256 files under dir *)

let hash_file_persist fn =
  if fn = "" then failwith (sprintf "hash_file_persist: empty fn");
  Utls.line_of_command
    (sprintf "sha256sum %s | cut -d' ' -f1 | tee %s.sha256" fn fn)

let hash_file_volatile fn =
  if fn = "" then failwith (sprintf "hash_file: empty fn");
  Utls.line_of_command
    (sprintf "sha256sum %s | cut -d' ' -f1" fn)

let get_one to_process () =
  match !to_process with
  | [] -> raise Parany.End_of_input
  | x :: xs ->
    let res = x in
    to_process := xs;
    res

let process_one fn =
  Log.debug "%s" fn;
  (fn, hash_file_persist fn)

let finished = ref 0

let collect_one nb_files ht (fn, hash) =
  Ht.add ht fn hash;
  incr finished;
  if !finished mod 1000 = 0 then
    eprintf "done: %.1f%%\r%!" (100.0 *. (float !finished /. float nb_files))

(* recursively clear a directory from all the .sha256 files found in it *)
let clear dir =
  let (_: string) =
    Utls.line_of_command
      (sprintf "find %s -regex '.*\\.sha256$' -exec rm -f {} \\;" dir) in
  ()

(* the hash of a regular file is in the corresponding .sha256 file *)
let retrieve_hash_for_file fn =
  assert((not FileUtil.(test Is_dir fn)) && FileUtil.(test Is_file fn));
  Utls.line_of_command (sprintf "cat %s.sha256" fn)

(* the hash of a directory is the hash of all its .sha256 files;
   of course, all its subdirectories must have been hashed first *)
let hash_dir dir =
  let sha256_files =
    Utls.lines_of_command
      (sprintf "find %s -maxdepth 0 -regex '^.*\\.sha256$'" dir) in
  let sha256_sums =
    L.map (fun fn ->
        Utls.line_of_command (sprintf "cat %s" fn)
      ) sha256_files in
  let tmp_fn = Utls.create_temp_file () in
  Utls.lines_to_file tmp_fn sha256_sums;
  let dir_hash = hash_file_volatile tmp_fn in
  Sys.remove tmp_fn;
  dir_hash

(* compute sha256 sum of all files under dir *)
let hash_under_dir nprocs dir =
  let files =
    Utls.lines_of_command
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

let build_merkle_tree root =
  (* we have to do a depth fist hashing of all directories under root *)
  failwith "not implemented yet"

let usage () =
  eprintf "usage:\n\
           %s {--hash <repo>|--clear <repo>|--diff <repo1>:<repo2>}\n"
    Sys.argv.(0);
  exit 1

let main () =
  Log.set_output stderr;
  Log.color_on ();
  Log.set_log_level Log.INFO;
  let argc, args = CLI.init () in
  if argc = 1 then
    usage()
  ;
  if CLI.get_set_bool ["-v";"--verbose"] args then
    Log.set_log_level Log.DEBUG
  ;
  let nprocs = match CLI.get_int_opt ["-n";"--nprocs"] args with
    | None -> Utls.get_nprocs () (* we use all detected CPUs by default *)
    | Some i -> i (* unless we are told otherwise *) in
  let action =
    match CLI.get_string_opt ["-h";"--hash"] args with
    | Some fn -> Hash fn
    | None ->
      match CLI.get_string_opt ["-d";"--diff"] args with
      | Some fn ->
        let left, right = BatString.split ~by:":" fn in
        Compare (left, right)
      | None ->
        match CLI.get_string_opt ["-c";"--clear"] args with
        | Some dir -> Clear dir
        | None -> usage ();
  in
  match action with
  | Hash dir -> hash_under_dir nprocs dir
  | Clear dir -> clear dir
  | Compare (_, _) ->
    (* load the two trees *)
    (* print out differences *)
    failwith "not implemented yet"

let () = main ()
