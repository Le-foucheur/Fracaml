exception Tcon

let debug () = Printf.printf "AHTCON\n";;

type arrayD = {mutable array: int array}

let rec print_list l = 
  match l with
  | [] -> print_newline ()
  | (e, a) :: q -> Printf.printf "(%Ld, %Ld); " e a; print_list q
;;

let rec addArray a e i = 
  let n = Array.length a.array in
  if i < n then begin
    a.array.(i) <- e;
  end
  else begin
    let arr = Array.make (2*n) 1 in
    for j = 0 to n - 1 do
      arr.(j) <- a.array.(j)
    done;
    arr.(i) <- e;
    a.array <- arr
  end
;;

(* le programe a executer *)
let prog = open_in (Sys.argv.(1))

(* racine carré d'un entier *)
let sqrti n = n |> Int64.to_float |> sqrt |> floor |> int_of_float

(* le prog et le met dans une list temporaire *)
let progBrut = 
  let tmp = ref [] in
  try
    while true do
      let ligne = input_line prog in
      let k = ref 0 in
      let c1 = ref "" in
      let c2 = ref "" in
      
      while ligne.[!k] != ' ' do 
        c1 := !c1^(String.make 1 ligne.[!k]);
        incr k  
      done;

      for i = !k + 1 to String.length ligne - 1 do
        c2 := !c2^(String.make 1 ligne.[i])
      done;

      tmp := (Int64.of_string !c1, Int64.of_string!c2)::!tmp;
    done
  with End_of_file -> List.rev !tmp
;;

let maxnb = 
  let rec aux l =
    match l with
    | [] -> 2L
    | (e, a) :: q -> max (max e a) (aux q)
  in
  max (aux progBrut) (Int64.of_string Sys.argv.(2))
;;

(* littéralement le crible d'ératostène *)
let crible = 
  let taille = sqrti maxnb + 1 in
  let cri = Array.make (taille + 2) true in
  for i = 2 to taille + 1 do (* commance a 2 car 0 et 1 pas vraiment premier*)
    if cri.(i) then
      for j = i to taille + 1 do 
        let ind = i * j in
        if ind <= taille + 1 then
          cri.(ind) <- false
      done
  done;
  cri
;;

(* créé un array qui comporte les nombre premier utilisé *)
let premier = 
  let taille = Array.fold_left (fun acc a -> if a then acc + 1 else acc) (-2) crible in
  let pre = Array.make taille 1 in
  let k = ref 0 in
  for i = 2 to Array.length crible - 1 do
    if crible.(i) then begin
      pre.(!k) <- i;
      incr k
    end
  done;
  {array = pre}
;;


let print_array a =
  let n = Array.length a in
  for i = 0 to n - 2 do 
    Printf.printf "%d^%Ld ;" premier.array.(i) a.(i)
  done;
  Printf.printf "%d^%Ld\n" premier.array.(n-1) a.(n-1)
;;

let print_arrayi a =
  let n = Array.length a in
  for i = 0 to n - 2 do 
    Printf.printf "%d ;" a.(i)
  done;
  Printf.printf "%d\n" a.(n-1)
;;

(* fonction de décomposition en nombres premier *)
let rec decompo n = 
  let tmp = ref n in
  let taille = Array.length premier.array in
  let res = ref (Array.make taille 0L) in
  let k = ref 0 in
  while !tmp > 1L && premier.array.(!k) <> 0 && !k < taille - 1 do
    if Int64.rem !tmp (Int64.of_int premier.array.(!k)) = 0L then begin
      !res.(!k) <- Int64.add !res.(!k) 1L;
      tmp := Int64.div !tmp (Int64.of_int premier.array.(!k));
    end
    else
      incr k
  done;
  if !k >= taille - 1 || premier.array.(!k) = 0 then begin
    addArray premier (Int64.to_int !tmp) (!k+1);
    print_arrayi premier.array;
    res := decompo n
  end;
  !res
;;

print_list progBrut;;

decompo maxnb |> print_array