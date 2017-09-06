
open Printf

module Ht = Hashtbl

(* get one job *)
let get_one to_process () =
  match !to_process with
  | [] -> raise Parany.End_of_input
  | x :: xs ->
    let res = x in
    to_process := xs;
    res

(* perform one job *)
let process_one f fn =
  Log.debug "%s" fn;
  (fn, f fn)

let finished = ref 0

(* collect one result *)
let collect_one nb_files ht (fn, hash) =
  Ht.add ht fn hash;
  incr finished;
  if !finished mod 1000 = 0 then
    eprintf "done: %.1f%%\r%!" (100.0 *. (float !finished /. float nb_files))
