open Printf

type directory = string

type action = Sign of directory
            | Compare of directory * directory

let main () =
  let argc, args = CLI.init () in
  if argc = 0 then
    (eprintf "usage:\n\
              %s {--sign <repo>|--diff <repo1>:<repo2>}\n" Sys.argv.(0);
     exit 1);
  let action =
    match CLI.get_string_opt ["--sign";"-s"] args with
    | Some fn -> Sign fn
    | None ->
      match CLI.get_string_opt ["--diff"; "-d"] args with
      | None -> failwith "use either --diff <repo1>:<repo2> or --sign <repo>"
      | Some fn ->
        let left, right = BatString.split ~by:":" fn in
        Compare (left, right)
  in
  match action with
  | Sign _ -> failwith "not implemented yet"
  | Compare (_, _) -> failwith "not implemented yet"
