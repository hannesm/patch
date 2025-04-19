module String = struct
  let is_prefix =
    let rec aux i ~prefix_len ~prefix ~str =
      if i < (prefix_len : int) then
        if String.unsafe_get str i =
           (String.unsafe_get prefix i : char) then
          aux (i + 1) ~prefix_len ~prefix ~str
        else
          false
      else
        true
    in
    fun ~prefix str ->
      let prefix_len = String.length prefix in
      if prefix_len <= (String.length str : int) then
        aux 0 ~prefix_len ~prefix ~str
      else
        false

  let is_suffix =
    let rec aux suffix_i str_i ~suffix_len ~suffix ~str =
      if suffix_i < (suffix_len : int) then
        if String.unsafe_get str str_i =
           (String.unsafe_get suffix suffix_i : char) then
          aux (suffix_i + 1) (str_i + 1) ~suffix_len ~suffix ~str
        else
          false
      else
        true
    in
    fun ~suffix str ->
      let suffix_len = String.length suffix in
      let str_len = String.length str in
      if suffix_len <= (str_len : int) then
        aux 0 (str_len - suffix_len) ~suffix_len ~suffix ~str
      else
        false

  let cut sep str =
    match String.index_opt str sep with
    | Some idx ->
        let l = String.length str in
        let sidx = idx + 1 in
        Some (String.sub str 0 idx, String.sub str sidx (l - sidx))
    | None -> None

  let slice ?(start = 0) ?stop str =
    let stop = match stop with
      | None -> String.length str
      | Some x -> x
    in
    let len = stop - start in
    String.sub str start len
end

module List = struct
  let rec last = function
    | [] -> invalid_arg "List.last"
    | [x] -> x
    | _::xs -> last xs
end
