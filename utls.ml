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

type filename = string

let with_out_file (fn: filename) (f: out_channel -> 'a): 'a =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

let lines_to_file fn lines =
  with_out_file fn (fun output ->
      L.iter (fprintf output "%s\n") lines
    )

let create_temp_file () =
  Filename.temp_file "" (* no_prefix *) "" (* no_suffix *)
