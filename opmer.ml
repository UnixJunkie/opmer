
module L = BatList
module Para = Parallelization
module String = BatString
module Ht = BatHashtbl

open Printf

type directory = string

(* FBR: assert we are given directories; not regular files *)
type action = Hash of directory (* create .sha256 files in repository *)
            | Diff of directory * directory (* compute the diff between two already signed repos *)
            | Clear of directory (* remove all .sha256 files under dir *)

let hash_to_ascii (h: string): string =
  let to_base64 = Cryptokit.Base64.encode_compact () in
  Cryptokit.transform_string to_base64 h

let ascii_to_hash (a: string): string =
  let from_base64 = Cryptokit.Base64.decode () in
  Cryptokit.transform_string from_base64 a

let hash_file fn =
  let sha256 = Cryptokit.Hash.sha256 () in
  let res = Utls.with_in_file fn (Cryptokit.hash_channel sha256) in
  hash_to_ascii res

let hash_string s =
  let sha256 = Cryptokit.Hash.sha256 () in
  let res = Cryptokit.hash_string sha256 s in
  hash_to_ascii res

(* create and populate <file>.sha256 *)
let hash_file_persist fn =
  if fn = "" then failwith (sprintf "hash_file_persist: empty fn");
  let hash = hash_file fn in
  Utls.with_out_file (fn ^ ".sha256") (fun out ->
      fprintf out "%s" hash
    )

let get_hash_from_file fn =
  assert(String.ends_with fn ".sha256");
  assert(not FileUtil.(test Is_dir fn));
  assert(FileUtil.(test Is_file fn));
  ascii_to_hash (MyFile.as_string fn)

(* recursively clear a directory from all the .sha256 files found in it *)
let clear dir =
  let (_: string) =
    Utls.line_of_command
      (sprintf "find %s -regex '.*\\.sha256$' -exec rm -f {} \\;" dir) in
  ()

(*
  let sha256_files =
    Utls.lines_of_command
      (sprintf "find %s -maxdepth 0 -regex '^.*\\.sha256$'" dir) in
*)

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
    ~demux:(Para.get_one (ref files))
    ~work:(Para.process_one hash_file_persist)
    ~mux:(Para.collect_one nb_files fn2hash);
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
        Diff (left, right)
      | None ->
        match CLI.get_string_opt ["-c";"--clear"] args with
        | Some dir -> Clear dir
        | None -> usage ();
  in
  match action with
  | Hash dir -> hash_under_dir nprocs dir
  | Clear dir -> clear dir
  | Diff (_, _) ->
    (* load left tree *)
    (* load right tree *)
    (* delta *)
    (* print only in left *)
    (* print only in right *)
    (* inspect the intersection for details about changed ones *)
    failwith "not implemented yet"

let () = main ()
