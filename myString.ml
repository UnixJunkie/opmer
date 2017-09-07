
let starts_with s prfx =
  let len_s = String.length s in
  let len_p = String.length prfx in
  if len_s < len_p then
    false
  else
    let sub = String.sub s 0 len_p in
    String.equal prfx sub

let chop_prfx prfx s =
  assert(starts_with s prfx);
  let n = String.length prfx in
  BatString.lchop ~n s

let count_char c s =
  let res = ref 0 in
  let n = String.length s in
  for i = 0 to n - 1 do
    if String.get s i = c then incr res
  done;
  !res
