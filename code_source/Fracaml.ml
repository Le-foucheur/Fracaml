(* la merde pour parser les option et tout le reste *)
let usage_msg = "  ./Fracaml <programme.txt> N0 [option]\n
- Le programme doit être dans un fichier txt
- N0 est le nombre par le quel la séquance commence
"
let valpad = ref ""
let valpadex = ref ""
let distance = ref (-1)
let input_files = ref []
let ah = ref true
let dernier = ref false

let decimal = ref false

let anon_fun filename = input_files := filename::!input_files

let speclist =
  [
    ("-v", Arg.Set_string valpad, "     Affiche seulement les puissance des premiers spécifiés");
    ("-V", Arg.Set_string valpadex, "     Affiche seulement les puissance des premiers spécifiés et si c'est les seul non nul");
    ("-d", Arg.Set_int distance, "     Affiche jusqu'au n ieme terme");
    ("-D", Arg.Set dernier, "     Affiche le dernier nombre atteint quand le programme termine");
    ("-dec", Arg.Set decimal, "     Affiche le compteur en décimal");
    ("-help", (Arg.Set ah), "")
  ]

let () = Arg.parse speclist anon_fun usage_msg;;

input_files := List.rev !input_files;;
(* fin de la merde *)

(* pour savoir quel sont les coef premier a afficher, si [] alors il sont tous a afficher *)
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
        res := (int_of_string !tmp)::!res;
        tmp := ""
      end
    done;
    (int_of_string !tmp)::!res
;;

(* pour savoir quel sont les coef premier exclusif a afficher, si [] alors il sont tous a afficher *)
let valpadlex = 
  let n = String.length !valpadex in
  if n = 0 then
    []
  else
    let tmp = ref "" in
    let res = ref [] in
    for i = 0 to n - 1 do
      if !valpadex.[i] <> ' ' then
        tmp := !tmp ^ String.make 1 !valpadex.[i]
      else begin
        res := (int_of_string !tmp)::!res;
        tmp := ""
      end
    done;
    (int_of_string !tmp)::!res
;;

(* les exection utilier *)
exception Tcon 
exception NonEntier
exception FinProgramme

(* les type utiliser *)
type arrayD = {mutable array: int array}
type decompo = int64 array
type fracD = decompo * decompo

(* pour rajouter un nombre dans un array redimentionable *)
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

(* permière lecture du n0 *)
let n0 = 
  match !input_files with
  | [] -> raise FinProgramme
  | e :: q ->
    Int64.of_string @@ e
;;

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

(* détermine le nombre le plus grand utilisé *)
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

(* print la décomposition d'un nombre, qui est représenter comme ce ci dans un array redimentionable *)
let print_decompo (a : decompo) =
  let n = Array.length a in
  let mark = ref false in (* marque le moment ou un nombre a été afficher *)
  if valpadl = [] && valpadlex = [] then begin
    for i = 0 to n - 2 do
      if premier.array.(i) <> 1 && a.(i) <> 0L then begin (* on n'affiche pas si c'est pas un premier ou si sa puissance est nul *)
        if (i <> 0 && premier.array.(i - 1) <> 1 && a.(i - 1) <> 0L) || !mark then begin (* affiche un « * » si c'est pas le permier élément afficher *)
          print_string " * ";
        end;
        mark := true;
        Printf.printf "%d^%Ld" premier.array.(i) a.(i)
      end
    done;
    if premier.array.(n-1) <> 1 && a.(n-1) <> 0L then
      Printf.printf " * %d^%Ld" premier.array.(n-1) a.(n-1)
  end
  else if valpadlex = [] then begin
    for i = 0 to n - 2 do (* ici le condition d'affichage sont les même, sauf si le nombre n'est pas un demender a être affiché représenter par le List.mem *)
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
  else begin
    for i = 0 to n - 2 do (* ici le condition d'affichage sont les même, sauf si le nombre n'est pas un demender a être affiché représenter par le List.mem sans la cond de la puissance nul *)
      if List.mem premier.array.(i) valpadlex && premier.array.(i) <> 1 then begin
        if (i <> 0 && List.mem premier.array.(i - 1) valpadlex && premier.array.(i - 1) <> 1) || !mark then begin
          print_string " * ";
        end;
        mark := true;
        Printf.printf "%d^%Ld" premier.array.(i) a.(i)
      end
    done;
    if List.mem premier.array.(n-1) valpadlex  && premier.array.(n-1) <> 1 && !mark then
      Printf.printf " * %d^%Ld" premier.array.(n-1) a.(n-1)
  end
;;

(* premet de print un type farction en métant un / entre les deux nombre *)
let print_frac f = 
  let t, b = f in
  print_decompo t;
  print_string " / ";
  print_decompo b;
  print_newline ()
;;

(* premet de simplifier une fraction *)
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

(* transforme le programme « brut » (les int) en le programme « raffiner » (leur décomposition en facteur premier) *)
let programme =
  let rec affinage p l = 
    match p with
    | [] -> l
    | (e, a)::q -> affinage q ((simplifier (decompo e, decompo a))::l)
  in
  affinage progBrut [] |> List.rev
;;

(* transforme n0 en sa décomposition en facteur premier *)
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

(* execute un tour de programme pour déterminer le prochain entier *)
let rec execute p n =
  let tmp = Array.copy n in
  match p with
  | [] -> raise FinProgramme
  | frac :: q ->
    try
      mult tmp frac
    with NonEntier -> execute q n
;;

(* execute le programme jusqu'a ce qu'il termine (si c'est le cas) *)
let rec main compteur n : unit =
  try
    if !distance = -1 || compteur <= !distance then begin
      if !decimal then begin
        Printf.printf "%-6d : " @@ compteur mod 100000
      end
      else begin
      Printf.printf "%-6X :  " @@ compteur mod 1048576
      end;
      print_decompo n;
      print_newline ();
      execute programme n |> main (compteur + 1)
    end
    else
      raise FinProgramme
  with FinProgramme -> Printf.printf "\nFin Du Programme :)\n"
;;

(* vérifie si les seuls coef premier != 1 sont bien ceux demander *)
let cond_exclu (dec : decompo) =
  let taille = Array.length dec in
  let res = ref true in
  for i = 0 to taille - 1 do
    if not (List.mem premier.array.(i) valpadlex) && dec.(i) <> 0L then
      res := false
  done;
  !res
;;

(* execute le programme jusqu'a ce qu'il termine (si c'est le cas) *)
let rec main_vpex compteur n : unit =
  try
    if !distance = -1 || compteur <= !distance then begin
      if cond_exclu n then begin
        if !decimal then begin
          Printf.printf "%-6d : " @@ compteur mod 100000
        end
        else begin
          Printf.printf "%-6X : " @@ compteur mod 1048576
        end;
        print_decompo n;
        print_newline ();
      end;
      execute programme n |> main_vpex (compteur + 1)
    end
    else
      raise FinProgramme
  with FinProgramme -> Printf.printf "\nFin Du Programme :)\n"
;;

(* sa variante si le flag -D est passé, et donc doit juste afficher le dernier nombre *)
let rec main_D compteur n : unit =
  try
    if !distance = -1 || compteur <= !distance then begin
      execute programme n |> main_D (compteur + 1)
    end
    else
      raise FinProgramme
  with FinProgramme -> Printf.printf "\nRésulat : "; print_decompo n; Printf.printf "\n\nFin Du Programme :)\n"
;;

(* execute le programme *)
if !dernier && valpadlex = [] then begin
  main_D 0 n0
end
else if valpadlex <> [] then begin
  main_vpex 0 n0
end
else begin
  main 0 n0
end