
(* return whole file content as a string *)
let as_string fn =
  let buff = Buffer.create 80 in
  Utls.with_in_file fn (fun input ->
      try
        Buffer.add_channel buff input (1024 * 1024);
        failwith "BatFile.as_string: file is too big"
      with End_of_file -> () (* OK, we read the whole file *)
    );
  Buffer.contents buff
