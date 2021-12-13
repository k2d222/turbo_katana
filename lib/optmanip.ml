include Option

let get_or _else opt = match opt with
  | Some(a) -> a
  | None -> _else

let get_or_else _else opt = match opt with
  | Some(a) -> a
  | None -> _else ()

let and_ _then opt = match opt with
  | Some(_) -> _then
  | None -> None

let and_then _then opt = match opt with
  | Some(a) -> _then a
  | None -> None

let or_ _then opt = match opt with
  | Some(_) -> opt
  | None -> _then

let or_else _then opt = match opt with
  | Some(_) -> opt
  | None -> _then ()

let xor_ _then opt = match opt with
  | Some(_) -> (match _then with
    | Some(_) -> None
    | None -> opt)
  | None -> _then

let map_or fn default opt = match opt with
  | Some(a) -> fn a
  | None -> default

let map_or_else fn default opt = match opt with
  | Some(a) -> fn a
  | None -> default ()
