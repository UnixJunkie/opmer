
include BatSet.String

(* return (s1 \ s2, s1 n s2, s2 \ s1) *)
let delta s1 s2 =
  fold (fun x (only_in_s1, common, only_in_s2) ->
      if mem x s2 then
        (only_in_s1, add x common, remove x only_in_s2)
      else
        (add x only_in_s1, common, only_in_s2)
    ) s1 (empty, empty. empty)
