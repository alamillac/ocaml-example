let get_val default = function
  | None -> default
  | Some v -> v

let rec list_max' (lst: 'a list): 'a option =
  match lst with
  | [] -> None
  | h :: t -> (
    match list_max' t with
    | None -> Some h
    | Some m -> Some (max h m)
  )

let rec list_max (lst: 'a list): 'a option =
  match lst with
  | [] -> None
  | h :: t -> begin
      match list_max t with
      | None -> Some h
      | Some m -> Some (max h m)
    end
