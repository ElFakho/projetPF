type t =
| Text of string 
| Directory of (string * bool * Digest.t * t) list

let rec chdc liste = match liste with
|[] -> ""
|(o,true,s,_)::tl -> if tl=[] then o^";"^"d"^";"^(Digest.to_hex s)^(chdc(tl)) else o^";"^"d"^";"^(Digest.to_hex s)^"\n"^(chdc(tl))
|(o,false,s,_)::tl -> if tl=[] then o^";"^"t"^";"^(Digest.to_hex s)^(chdc(tl)) else o^";"^"t"^";"^(Digest.to_hex s)^"\n"^(chdc(tl))
;;

let hash _obj = match _obj with
| Text(s) -> Digest.string s
| Directory(d) -> let objet = (chdc d) in Digest.string objet
;;

let is_known _h = Sys.file_exists ("./.ogit/objects/"^(Digest.to_hex _h));;

let store_object _obj = let s = (match _obj with 
|Text(s) -> s
|Directory(d) -> chdc d)
in let c = open_out ("./.ogit/objects/"^Digest.to_hex (Digest.string s)) in output_string c s ;close_out c; Digest.string s;;

let read_text_object _h = let f = ("./.ogit/objects/"^(Digest.to_hex _h)) in
  let g = In_channel.open_text f in
    In_channel.input_all g;;  


let rec file_to_object x = 
  if (Sys.is_directory x)=false then
    (let g = In_channel.open_text x in
      Text (In_channel.input_all g)) 
  else
      (let t = Sys.readdir(x) in 
      let l = ref [] in for i=0 to ((Array.length t)-1) do
        if (t.(i).[0]!='.') then
         (l := (t.(i),Sys.is_directory (x^"/"^(t.(i))), hash (file_to_object (x^"/"^(t.(i)))), file_to_object (x^"/"^(t.(i))))::!l)
        else (l:= !l)
      done; Directory(List.rev !l))
      ;;

let storeObj _obj = let s = (match _obj with 
|Text(s) -> s
|Directory(d) -> chdc d)
in let c = open_out ("./.ogit/objects/"^(Digest.to_hex (Digest.string s))) in output_string c s ;close_out c;;


let rec obj2files obj = match obj with
|Text (_) -> storeObj obj
|Directory (d) -> (match d with 
|[] -> ()
|(_,_,_,obj2)::tl -> storeObj obj2 ; obj2files (Directory (tl)))
;;

let store_work_directory () = let obj = file_to_object "." in obj2files obj; hash obj;;

let rec read_directory_object _h = 
  let s = read_text_object _h in
    let lignes = String.split_on_char '\n' s in match lignes with
    |hd::_ when List.length(String.split_on_char ';' hd)=1 -> Text(read_text_object _h)
    |_ -> Directory(let lTOl ss = (let ligne = String.split_on_char ';' ss in match ligne with
      |[e1;"t";e3] -> (e1,false,Digest.from_hex e3, Text(read_text_object (Digest.from_hex e3)))
      |[e1;_;e3] -> (e1,true,Digest.from_hex e3,read_directory_object (Digest.from_hex e3))
      |_ ->("",false,_h,Text("")))
        in List.map lTOl lignes)
;;

let rec clean x = let t = Sys.readdir x in for i=0 to (Array.length t)-1 do 
  if ((Sys.is_directory (x^"/"^t.(i)))=false)&&(t.(i).[0]!='.') then 
    Sys.remove (x^"/"^t.(i))
  else if (t.(i).[0]!='.') then
     (if ((Sys.is_directory (x^"/"^t.(i)))=true)&&(t.(i).[0]!='.') then 
      clean (x^"/"^t.(i)) ;
      if (Array.length (Sys.readdir (x^"/"^t.(i))))=0 then
        Sys.rmdir (x^"/"^t.(i)) )
  done;;
  
let clean_work_directory () = clean ".";;

let rec rwdirectory _obj x = match _obj with
|Text(s) -> let c = open_out x in output_string c s; close_out c
|Directory(d) -> if (Sys.file_exists x)=false then Unix.mkdir x 0o755; (match d with 
|[] -> ()
|(n,_,_,obj)::tl -> (rwdirectory (obj) (x^"/"^n)); rwdirectory (Directory(tl)) x)
;;

let restore_work_directory _obj = rwdirectory _obj "../repo"
;;

let merge_work_directory_I _obj = true;;
