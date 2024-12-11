exception Tcon

(* le programe a executer *)
let prog = open_in (Sys.argv.(1))

(* calule l'entier le plus proche supérieur de la racine d'un nombre *)
let sqrti n = int_of_float (floor (sqrt (Int64.to_float n)))

(* détermine le nombre le plus grand utilisé dans le prog *)
let maxnb = 
  let res = ref 2L in (* car 2 est le premier nombre premier *)
  let nb = ref "" in
  try 
    while true do
      let c = input_char prog in
      if c <> ' ' && c <> '\n' then begin
        nb := !nb^(String.make 1 c);
      end
      else begin
        res := max !res (Int64.of_string !nb);
        nb := "";
      end;
    done
  with End_of_file -> max !res (Int64.of_string !nb)
;;

(* littéralement le crible d'ératostène *)
let crible = 
  let taille = (Int64.to_int maxnb) in
  let cri = Array.make (taille + 1) true in
  for i = 2 to taille do (* commance a 2 car 0 et 1 pas vraiment premier*)
    if cri.(i) then
      for j = i to taille do 
        let ind = i * j in
        if ind <= taille then
          cri.(ind) <- false
      done
  done;
  cri
;;

(* créé un array qui comporte les nombre premier utilisé *)
let premier = 
  let taille = Array.fold_left (fun acc a -> if a then acc + 1 else acc) (-2) crible in
  let pre = Array.make taille 0 in
  let k = ref 0 in
  for i = 2 to Array.length crible - 1 do
    if crible.(i) then begin
      pre.(!k) <- i;
      incr k
    end
  done;
  pre
;;

(* fonction de décomposition en nombres premier *)
let decompo n = 
  let tmp = ref n in
  let taille = Array.length premier in
  let res = Array.make taille 0L in
  let k = ref 0 in
  while !tmp <> 1L && !k < taille do
    if Int64.rem !tmp (Int64.of_int premier.(!k)) = 0L then begin
      res.(!k) <- Int64.add res.(!k) 1L;
      tmp := Int64.div !tmp (Int64.of_int premier.(!k));
    end
    else
      incr k
  done;
  if !k >= taille then
    raise Tcon
  ;
  res
;;

let print_array a =
  let n = Array.length a in
  for i = 0 to n - 2 do 
    Printf.printf "%d^%Ld * " premier.(i) a.(i)
  done;
  Printf.printf "%d^%Ld\n" premier.(n-1) a.(n-1)
;;

decompo 5L |> print_array