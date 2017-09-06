
module L = BatList
module Para = Parallelization
module String = BatString
module Ht = BatHashtbl

open Printf

type directory = string

type action =
  | Hash of directory (* create .sha256 files in repository *)
  (* compute the diff between two already signed repos *)
  | Diff of directory * directory
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
  if not (FileUtil.(test Is_file fn)) then
    failwith ("not a regular file: " ^ fn);
  ascii_to_hash (MyFile.as_string fn)

(* recursively clear a directory from all the .sha256 files found in it *)
let clear dir =
  Log.info "おまちください... (please be patient)";
  assert(FileUtil.(test Is_dir dir));
  let (_: string) =
    Utls.line_of_command ~debug:true
      (sprintf "find %s -regex '.*\\.sha256$' -exec rm -f {} \\;" dir) in
  ()

(* a signed dir is just a set of strings: its list of *.sha256 files *)
let load_dir prfx dir =
  if not FileUtil.(test Is_dir dir) then
    failwith ("load_dir: not a directory: " ^ dir);
  let abs_sha256_files =
    Utls.lines_of_command
      (sprintf "find %s -regex '^.*\\.sha256$'" dir) in
  (* remove prefixes (everything up to '/opam-repository' *)
  let rel_sha256_files = L.map (MyString.chop_prfx prfx) abs_sha256_files in
  StringSet.of_list rel_sha256_files

(* compute sha256 sum of all files under dir *)
let hash_under_dir nprocs dir =
  assert(FileUtil.(test Is_dir dir));
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

let usage () =
  eprintf "usage:\n\
           %s {--hash <repo>|--clear <repo>|--diff <repo1>:<repo2>}\n"
    Sys.argv.(0);
  exit 1

(* determine if files have changed because their hashes are no more the same *)
let has_changed lfn rfn =
  let lhash = get_hash_from_file lfn in
  let rhash = get_hash_from_file rfn in
  lhash <> rhash

let diff logfile lpath rpath =
  let reg = Str.regexp_string "opam-repository" in
  let lprfx_end = Str.search_forward reg lpath 0 in
  let lprfx = String.sub lpath 0 lprfx_end in
  Log.debug "lprfx: %s" lprfx;
  let rprfx_end = Str.search_forward reg rpath 0 in
  let rprfx = String.sub rpath 0 rprfx_end in
  Log.debug "rprfx: %s" rprfx;
  (* load left tree *)
  let left = load_dir lprfx lpath in
  (* load right tree *)
  let right = load_dir rprfx rpath in
  let were_removed, common, were_added = StringSet.delta left right in
  Utls.with_out_file logfile (fun out ->
      (* print only in left *)
      Log.info "removed: %d" (StringSet.cardinal were_removed);
      StringSet.iter (fprintf out "- %s\n") were_removed;
      (* print only in right *)
      Log.info "added: %d" (StringSet.cardinal were_added);
      StringSet.iter (fprintf out "+ %s\n") were_added;
      (* inspect the intersection for details about changed ones *)
      Log.info "needs to inspect further: %d" (StringSet.cardinal common);
      StringSet.iter (fun fn ->
          let lfn = lprfx ^ fn in
          let rfn = rprfx ^ fn in
          if has_changed lfn rfn then
            fprintf out "~ %s\n" fn
        ) common
    )

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
  | Diff (left, right) ->
    let logfile = CLI.get_string ["-o";"--output"] args in
    diff logfile left right

let () = main ()
