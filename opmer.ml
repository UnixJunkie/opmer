
module Fn = Filename
module L = BatList
module Para = Parallelization
module String = BatString
module Ht = BatHashtbl

let verbose = ref false (* toggled by CLI -v option *)

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
      fprintf out "%s\n" hash
    )

let read_hash_file fn =
  assert(String.ends_with fn ".sha256");
  assert(not FileUtil.(test Is_dir fn));
  if not (FileUtil.(test Is_file fn)) then
    failwith ("not a regular file: " ^ fn);
  MyFile.as_string fn

let retrieve_hash_for_dir dir =
  assert(FileUtil.(test Is_dir dir));
  let dot_merkle_fn = dir ^ ".merkle" in
  assert(FileUtil.(test Is_file dot_merkle_fn));
  MyFile.as_string dot_merkle_fn

(* recursively clear a directory from all the *.sha256 and *.merkle
   files found in it *)
let clear dir =
  Log.info "おまちください... (please be patient)";
  assert(FileUtil.(test Is_dir dir));
  let root_merkle_fn = dir ^ ".merkle" in
  if FileUtil.(test Is_file root_merkle_fn) then
    Sys.remove root_merkle_fn; (* rm root one, if any *)
  let to_delete =
    Utls.lines_of_command ~debug:true
      (sprintf "find %s -regex '.*\\.\\(sha256\\|merkle\\)$'" dir) in
  L.iter Sys.remove to_delete

(* a signed dir is just a set of strings: its list of *.sha256 files *)
let load_dir ?(no_recurse = false) prfx dir =
  if not FileUtil.(test Is_dir dir) then
    failwith ("load_dir: not a directory: " ^ dir);
  let abs_sha256_files =
    Utls.lines_of_command
      (sprintf
         (if no_recurse then
            "find %s -maxdepth 1 -regex '.*\\.sha256$'"
          else
            "find %s -regex '.*\\.sha256$'")
         dir) in
  (* remove prefixes (everything up to '/opam-repository' *)
  let rel_sha256_files = L.map (MyString.chop_prfx prfx) abs_sha256_files in
  StringSet.of_list rel_sha256_files

(* create the <dir>.merkle by hashing the content of all *.sha256 and *.merkle
   files directly under it *)
let merkle_dir_persist dir =
  assert(FileUtil.(test Is_dir dir));
  let hashes =
    Utls.lines_of_command
      (sprintf "find %s -maxdepth 1 -regex '.*\\.\\(sha256\\|merkle\\)$'" dir) in
  let buff = Buffer.create 80 in
  if !verbose then
    Log.debug "hashing dir: %s" dir;
  L.iter (fun fn ->
      let hash = MyFile.as_string fn in
      Buffer.add_string buff hash
    ) hashes;
  let to_hash = Buffer.contents buff in
  let to_write = hash_string to_hash in
  Utls.with_out_file (dir ^ ".merkle") (fun out ->
      fprintf out "%s\n" to_write
    )

let rec encoding_loop = function
  | [] -> assert(false)
  | [root_dir] -> merkle_dir_persist root_dir
  | many_files ->
    (* get all files with max depth *)
    let depths = L.map MyFile.dir_depth many_files in
    let max_depth = L.max depths in
    let to_process', rest' =
      L.partition (fun fn -> MyFile.dir_depth fn = max_depth) many_files in
    let fst_fn = L.hd to_process' in
    let fst_dir = Fn.dirname fst_fn in
    let _under_same_dir, rest =
      L.partition (fun fn ->
          Fn.dirname fn = fst_dir
        ) to_process' in
    merkle_dir_persist fst_dir;
    encoding_loop (fst_dir :: L.rev_append rest rest')

(* compute sha256 sum of all files under dir
   first: the dir is cleaned of any previous run files *)
let hash_under_dir nprocs dir =
  assert(FileUtil.(test Is_dir dir));
  clear dir; (* rm all *.sha256 and *.merkle before anything else *)
  let all_files =
    Utls.lines_of_command
      (* find all pure files under dir, skipping hidden files and directories *)
      (sprintf "find %s -not -type d | \
                grep -v -P '/\\..+' | \
                sort" dir) in
  let nb_files = L.length all_files in
  (* sha256sum each one *)
  let fn2hash = Ht.create 11 in
  Parany.run ~csize:1 ~nprocs
    ~demux:(Para.get_one (ref all_files))
    ~work:(Para.process_one hash_file_persist)
    ~mux:(Para.collect_one nb_files fn2hash);
  Log.info "hashed %d" (Ht.length fn2hash);
  (* now, the hard part of the job: we must create a .merkle for each directory
     starting from the deepest first.
     The .merkle contains the hash of all *.sha256 and all *.merkle just under
     that directory (not recursive) *)
  let all_sha256_files = L.map (fun fn -> fn ^ ".sha256") all_files in
  encoding_loop all_sha256_files

let usage () =
  eprintf "usage:\n\
           %s {--hash <repo>|--clear <repo>|--diff <repo1>:<repo2>}\n"
    Sys.argv.(0);
  exit 1

(* determine if files have changed because their hashes are no more the same *)
let file_has_changed lfn rfn =
  let lhash = read_hash_file lfn in
  let rhash = read_hash_file rfn in
  lhash <> rhash

let dir_has_changed ldir rdir =
  (retrieve_hash_for_dir ldir) <> (retrieve_hash_for_dir rdir)

let diff_dir_common_files out lprfx ldir rprfx rdir =
  let lfiles = load_dir ~no_recurse:true lprfx ldir in
  let rfiles = load_dir ~no_recurse:true rprfx rdir in
  (* distinct files are dealt with somewhere else *)
  let common = StringSet.inter lfiles rfiles in
  StringSet.iter (fun fn ->
      let lfn = lprfx ^ fn in
      let rfn = rprfx ^ fn in
      if file_has_changed lfn rfn then
        fprintf out "~ %s\n" fn
    ) common

let process_one_dir out lprfx ldir rprfx rdir = function
  | [] -> assert(false)
  | fn :: others ->
    let dir = Fn.dirname fn in
    let ldir' = lprfx ^ dir in
    let rdir' = rprfx ^ dir in
    if dir_has_changed ldir' rdir' then
      (if !verbose then Log.info "changes under %s" dir;
       let () = diff_dir_common_files out lprfx ldir' rprfx rdir' in
       others)
    else
      (* remove all files under this dir from further detailed inspection;
         thank you Merkle *)
      L.filter (fun fn -> not (BatString.starts_with fn dir)) others

let rec diff_loop out lprfx ldir rprfx rdir = function
  | [] -> () (* job done *)
  | some_files ->
    let less_files = process_one_dir out lprfx ldir rprfx rdir some_files in
    diff_loop out lprfx ldir rprfx rdir less_files

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
      (* (\* linear, naive strategy *\) *)
      (* StringSet.iter (fun fn -> *)
      (*     let lfn = lprfx ^ fn in *)
      (*     let rfn = rprfx ^ fn in *)
      (*     if file_has_changed lfn rfn then *)
      (*       fprintf out "~ %s\n" fn *)
      (*   ) common *)
      (* strategy exploiting the Merkle tree *)
      let common_files = StringSet.to_list common in
      let shallow_files_first =
        List.sort (fun lfn rfn ->
            BatInt.compare (MyFile.dir_depth lfn) (MyFile.dir_depth rfn)
          ) common_files in
      diff_loop out lprfx lpath rprfx rpath shallow_files_first
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
    (Log.set_log_level Log.DEBUG;
     verbose := true)
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
        | None -> usage () in
  match action with
  | Hash dir -> hash_under_dir nprocs dir
  | Clear dir -> clear dir
  | Diff (left, right) ->
    let logfile = CLI.get_string ["-o";"--output"] args in
    diff logfile left right

let () = main ()
