[@@@warning "-27-39"]
open Containers

let parse str writer =
  match String.to_list str with
  | '.' :: tl ->
    let s = String.of_list tl in
    if String.equal_caseless s "exit" then exit 0 
    else Eio.Buf_write.printf writer "Unrecognized command \'.%s\'\n" s 
  | x -> match String.split_on_char ' ' str with
         | [] -> 
            raise @@ Failure "Empty command please try again"
         | "insert" :: number :: table :: db :: _ ->
         | hd :: _ -> Eio.Buf_write.printf writer "Unrecognized keyword at the start of \'%s\'.\n" str

let repl env =
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  let rec loop () =
    let write db_name writer =
      Eio.Buf_write.printf writer "%s > " db_name
    in 
    Eio.Buf_write.with_flow stdout @@ write "db";
    let reader       = Eio.Buf_read.of_flow stdin ~max_size:1_000 in
    let str          = Eio.Buf_read.line reader in
    let write writer = parse str writer in
    Eio.Buf_write.with_flow stdout write;
    loop ()
  in
  loop ()

let run env =
  repl env |> ignore;
  ()

let () = Eio_main.run run