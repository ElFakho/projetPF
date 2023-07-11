(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)



let ogit_init () = 
   (Unix.mkdir "./.ogit") 0o755;(Unix.mkdir "./.ogit/logs" 0o755);
   (Unix.mkdir "./.ogit/objects" 0o755);(
   let _c = Logs.init_commit() in
   let c = Logs.store_commit _c in 
   let x = Objects.store_work_directory() in print_string ((Digest.to_hex x)^"\n"^(Digest.to_hex c)))
;;


let ogit_commit _msg = failwith "TODO"

let ogit_checkout _hash = failwith "TODO"

let ogit_log () = failwith "TODO"

let ogit_merge _hash = failwith "TODO"