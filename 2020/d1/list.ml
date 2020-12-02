open Core_kernel;;

let (|.) a f = f a; a;;

let input () = In_channel.read_lines "d1/input.txt"
               |> List.map ~f:int_of_string;;

let p1 ls set =
  let com x = (let y = 2020 - x in
               Option.some_if (Hash_set.mem set y) y)
  in ls
     |> List.find_map ~f:com
     |> Option.map ~f:(fun x -> x * (2020 - x))
     |> Option.value ~default:(-1);;


let fuck x sorted ~f :int option = List.fold_until ~init:x
                      ~f:(fun x y ->
                        if Int.(x + y > 2020)
                        then Stop None
                        else f y)
                      ~finish:(fun _ -> None)
                      sorted;;


let p2 ls set =
  let ls = List.sort ls ~compare:Int.compare in
  ls
  |> List.find_map
       ~f:(fun (x : int) ->
         fuck x ls
           ~f:(fun y ->
             let z = 2020 - (x + y) in
             if (Hash_set.mem set z)
             then Stop (Some (x * y * z))
             else Continue x)
       )
  |> Option.value ~default:(-1)

let () =
  let ls = input () in
  let set = Hash_set.of_list (module Int) ls in
    printf "p1 %d\n" (p1 ls set);
    printf "p2 %d\n" (p2 ls set);
