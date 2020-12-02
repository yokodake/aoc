open Core_kernel;;

let (|.) a f = f a; a;;

let input () = In_channel.read_lines "d1/input.txt"
               |> List.map ~f:int_of_string;;

let hset_of_array m arr =
  let open Hash_set in
  let size = (Array.length arr * 3) / 5 in
  let s = create ~size m in
  Array.iter arr ~f:(fun x -> add s x); s
;;

let p1 xs set =
  (* let set = hset_of_array (module Int) xs in *)
  let com x = (let y = 2020 - x in
               Option.some_if (Hash_set.mem set y) y)
  in !xs
     |> Array.find_map ~f:com
     |> Option.map ~f:(fun x -> x * (2020 - x))
     |> Option.value ~default:(-1)
;;


let fuck x sorted ~f :int option =
  Array.fold_until ~init:x
    ~f:(fun x y ->
      if Int.(x + y > 2020)
      then Stop None
      else f y)
    ~finish:(fun _ -> None)
    sorted
;;

let p2 xs set =
  (* let set = hset_of_array (module Int) xs in *)
  Array.sort ~compare:Int.compare (!xs);
  (!xs)
  |> Array.find_map
       ~f:(fun (x : int) ->
         fuck x (!xs)
           ~f:(fun y ->
             let z = 2020 - (x + y) in
             if (Hash_set.mem set z)
             then Stop (Some (x * y * z))
             else Continue x)
       )
  |> Option.value ~default:(-1)
;;

let () =
  let xs = input () |> List.to_array in
  let set = hset_of_array (module Int) xs in
    printf "p1 %d\n" (p1 (ref xs) set);
    printf "p2 %d\n" (p2 (ref xs) set);
