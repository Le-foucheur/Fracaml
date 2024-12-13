let usage_msg = "./Fracaml <file1> N0 [option]"
let valpad = ref ""
let distance = ref (-1)
let input_files = ref []
let ah = ref true
let dernier = ref false

let anon_fun filename = input_files := filename::!input_files

let speclist =
  [
    ("-v", Arg.Set_string valpad, "affiche seulement les puissance des premiers spécifiés");
    ("-d", Arg.Set_int distance, "affiche jusqu'au n ieme terme");
    ("-D", Arg.Set dernier, "affiche le dernier nombre atteint quand le programme termine");
    ("-help", (Arg.Set ah), "")
  ]

let () = Arg.parse speclist anon_fun usage_msg;;

input_files := List.rev !input_files;;

let valpadl = 
  let n = String.length !valpad in
  if n = 0 then
    []
  else
    let tmp = ref "" in
    let res = ref [] in
    for i = 0 to n - 1 do
      if !valpad.[i] <> ' ' then
        tmp := !tmp ^ String.make 1 !valpad.[i]
      else begin
        Printf.printf"%s\n" !tmp;
        res := (int_of_string !tmp)::!res;
        tmp := ""
      end
    done;
    (int_of_string !tmp)::!res
;;

exception Tcon
exception NonEntier
exception FinProgramme

let debug () = Printf.printf "AH TCon\n"; flush_all ();;

type arrayD = {mutable array: int array}

type decompo = int64 array
type fracD = decompo * decompo

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
let prog =
  match !input_files with
  | [] -> raise FinProgramme
  | e::q ->
    input_files := q;
    open_in e
;;

let n0 = 
  match !input_files with
  | [] -> raise FinProgramme
  | e :: q ->
    Int64.of_string @@ e
;;

(* racine carré d'un entier *)
let sqrti n = n |> Int64.to_float |> sqrt |> floor |> int_of_float

let print_array2 a =
  let n = Array.length a in
  for i = 0 to n-1 do
    Printf.printf "%d " a.(i)
  done;
  print_newline ()
;;

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
  max (aux progBrut) (n0)
;;

(* littéralement le crible d'ératostène *)
let crible = 
  let taille = sqrti maxnb + 2 in
  let cri = Array.make (taille + 3) true in
  for i = 2 to taille + 2 do (* commance a 2 car 0 et 1 pas vraiment premier*)
    if cri.(i) then
      for j = i to taille + 2 do 
        let ind = i * j in
        if ind <= taille + 2 then
          cri.(ind) <- false
      done
  done;
  cri
;;

(* créé un array qui comporte les nombre premier utilisé : premier.array.(i) <=> la fonction qui a i associe le i ème nb premier *)
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

(* fonction de décomposition en nombres premier *)
let rec decompo n : decompo = 
  let tmp = ref n in
  let taille = Array.length premier.array in
  let res = ref (Array.make taille 0L) in
  let k = ref 0 in
  (* let deb () = Printf.printf "tmp = %Ld, k = %d, taille = %d, pre = %d\n" !tmp !k taille premier.array.(!k); print_array2 !res; flush_all ()
  in *)
  while !tmp > 1L && premier.array.(!k) <> 1 && !k < taille - 1 do
    if Int64.rem !tmp (Int64.of_int premier.array.(!k)) = 0L then begin
      !res.(!k) <- Int64.add !res.(!k) 1L;
      tmp := Int64.div !tmp (Int64.of_int premier.array.(!k));
    end
    else
      incr k
    done;
  if (!k >= taille - 1 || premier.array.(!k) = 1) && !tmp > 1L then begin
    if !k >= taille - 1 then begin
      addArray premier (Int64.to_int !tmp) (!k+1)
    end
    else begin
      addArray premier (Int64.to_int !tmp) !k
    end;
    res := decompo n;
  end;
  !res
;;

let print_decompo (a : decompo) =
  let n = Array.length a in
  let mark = ref false in
  if valpadl = [] then begin
    for i = 0 to n - 2 do
      if premier.array.(i) <> 1 && a.(i) <> 0L then begin
        if (i <> 0 && premier.array.(i - 1) <> 1 && a.(i - 1) <> 0L) || !mark then begin
          print_string " * ";
        end;
        mark := true;
        Printf.printf "%d^%Ld" premier.array.(i) a.(i)
      end
    done;
    if premier.array.(n-1) <> 1 && a.(n-1) <> 0L then
      Printf.printf " * %d^%Ld" premier.array.(n-1) a.(n-1)
  end
  else begin
    for i = 0 to n - 2 do
      if List.mem premier.array.(i) valpadl && premier.array.(i) <> 1 && a.(i) <> 0L then begin
        if (i <> 0 && List.mem premier.array.(i - 1) valpadl && premier.array.(i - 1) <> 1 && a.(i - 1) <> 0L) || !mark then begin
          print_string " * ";
        end;
        mark := true;
        Printf.printf "%d^%Ld" premier.array.(i) a.(i)
      end
    done;
    if List.mem premier.array.(n-1) valpadl  && premier.array.(n-1) <> 1 && a.(n-1) <> 0L && !mark then
      Printf.printf " * %d^%Ld" premier.array.(n-1) a.(n-1)
  end
;;

let print_frac f = 
  let t, b = f in
  print_decompo t;
  print_string " / ";
  print_decompo b;
  print_newline ()
;;

let rec print_list l = 
  match l with
  | [] -> print_newline ()
  | e :: q -> (print_frac e; print_list q)
;;

let simplifier (frac : fracD) : fracD = 
  let dividende, diviseur = frac in
  let taille = min (Array.length dividende) (Array.length diviseur) in
  for i = 0 to taille - 1 do
    let exp1 = dividende.(i) in
    let exp2 = diviseur.(i) in
    if exp1 >= exp2 then begin
      dividende.(i) <- Int64.sub exp1  exp2;
      diviseur.(i) <- 0L
    end
    else begin
      diviseur.(i) <- Int64.sub exp2  exp1;
      dividende.(i) <- 0L
    end
  done;
  (dividende, diviseur)
;;

let programme =
  let rec affinage p l = 
    match p with
    | [] -> l
    | (e, a)::q -> affinage q ((simplifier (decompo e, decompo a))::l)
  in
  affinage progBrut [] |> List.rev
;;

let n0 = decompo n0;;

let mult (n : decompo) (f : fracD) : decompo =
  let n = n in
  let f1, f2 = f in
  let taille = Array.length n in
  for i = 0 to taille - 1 do
    if (Int64.sub n.(i) f2.(i) |> Int64.add f1.(i)) < 0L then
      raise NonEntier
    else
      n.(i) <- (Int64.sub n.(i) f2.(i) |> Int64.add f1.(i))
  done;
  n
;;

let rec execute p n =
  let tmp = Array.copy n in
  match p with
  | [] -> raise FinProgramme
  | frac :: q ->
    try
      mult tmp frac
    with NonEntier -> execute q n
;;

let rec main compteur n : unit =
  try
    if !distance = -1 || compteur <= !distance then begin
      Printf.printf "%d :  " (compteur mod 10);
      print_decompo n;
      print_newline ();
      execute programme n |> main (compteur + 1)
    end
    else
      raise FinProgramme
  with FinProgramme -> Printf.printf "\nFin Du Programme :)\n"
;;

let rec main_D compteur n : unit =
  try
    if !distance = -1 || compteur <= !distance then begin
      execute programme n |> main_D (compteur + 1)
    end
    else
      raise FinProgramme
  with FinProgramme -> Printf.printf "\nRésulat : "; print_decompo n; Printf.printf "\n\nFin Du Programme :)\n"
;;

if !dernier then
  main_D 0 n0
else
  main 0 n0