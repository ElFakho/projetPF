type commit = {
    parents : Digest.t list;
    date : float;
    message : string;
    content : Digest.t
}

let twoDig x = if (x<10) then ("0"^(string_of_int x)) else string_of_int x

let date_fm _d= let t = Unix.localtime(_d) in 
(twoDig t.tm_hour)^":"^(twoDig t.tm_min)^":"^(twoDig t.tm_sec)^"-"^(twoDig t.tm_mday)^"/"^(twoDig (t.tm_mon+1))^"/"^(string_of_int (t.tm_year + 1900));;

let rec l_to_string l = match l with
|[]-> ""
|hd::tl when tl=[] -> hd^(l_to_string tl)
|hd::tl -> hd^"\n"^(l_to_string tl)

let set_head _l = let c = open_out ("./.ogit/HEAD") in 
  let l = List.map Digest.to_hex _l in 
    let s = l_to_string l in 
      output_string c s ;close_out c;;

let file_to_DigL filename = 
    let lignes = ref [] in
    let c = open_in filename in
    try
      while true; do
        lignes := Digest.from_hex (input_line c) :: !lignes
      done; !lignes
    with End_of_file ->
      close_in c;
      List.rev !lignes ;;


let get_head () =
    let g = "./.ogit/HEAD" in
        file_to_DigL g;;

let make_commit _s  _h = {parents=(get_head ());date=(Unix.time());message=_s;content=_h}
;;

let init_commit () = make_commit "init commit" (Objects.store_work_directory());;

let rec digL_to_text l = match l with
|[]-> ""
|hd::tl when tl=[] -> (Digest.to_hex hd)^(digL_to_text tl)
|hd::tl -> (Digest.to_hex hd)^";"^(digL_to_text tl)

let store_commit _c = 
    let s = ((digL_to_text (_c.parents))^"\n"^(date_fm (_c.date))^"\n"^(_c.message)^"\n"^(Digest.to_hex (_c.content))) in 
        let x = (Digest.to_hex(Digest.string s)) in
         let c = (open_out ("./.ogit/logs/"^x)) in
            (output_string c s ;close_out c);x;;

let heure2sec s = let l = String.split_on_char ':' s in List.map int_of_string l;;

let date2sec s = let l = String.split_on_char '/' s in List.map int_of_string l;;

let s_to_date s = let d = String.split_on_char '-' s in match d with
|hr::dt::_ -> (let datel = (heure2sec hr)@(date2sec dt) in
  (match datel with
  |e1::e2::e3::e4::e5::e6::_ -> let tm =  {Unix.tm_hour = e1; Unix.tm_min = e2;Unix.tm_sec=e3;tm_mday=e4;tm_mon = (e5-1);tm_year = e6-1900;Unix.tm_wday=1;Unix.tm_yday=1;Unix.tm_isdst=true} in Unix.mktime tm
  | _ -> failwith "Erreur"))
  | _ -> failwith "Erreur"
;;

let read_commit _h = let f = ("./.ogit/logs/"^(Digest.to_hex _h)) in
let g = In_channel.open_text f in
  let s = In_channel.input_all g in
    let l = String.split_on_char '\n' s in
  match l with
  |[hd1;hd2;hd3;hd4] -> {parents=(List.map Digest.from_hex (String.split_on_char ';' hd1));date = fst(s_to_date hd2);message=hd3;content=Digest.from_hex hd4}
  | _ -> failwith "Erreur"
;;
