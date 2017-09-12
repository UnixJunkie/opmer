
let chop_prfx prfx s =
  assert(BatString.starts_with s prfx);
  let n = String.length prfx in
  BatString.lchop ~n s

let count_char c s =
  let res = ref 0 in
  let n = String.length s in
  for i = 0 to n - 1 do
    if String.get s i = c then incr res
  done;
  !res
