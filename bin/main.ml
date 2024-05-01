[@@@warning "-27-39-34"]
open Containers

let tbl : 'a list list ref = ref []

type insert_flag = 
  [
    | `Integer
    | `Varchar32
    | `Varchar255
  ]

type select_flag =
  [
    | `All
  ]

let rec insert acc flags inputs = List.(append acc @@ combine flags inputs)

let rec table_to_string table =
  match table with
  | [] -> []
  | hd :: tl ->
    let rec loop = function
      | [] -> ")"
      | (`Integer, x) :: inner_tl -> Printf.sprintf "(%s, " x ^ loop inner_tl
      | (`Varchar32, x) :: inner_tl -> Printf.sprintf "%s, " x ^ loop inner_tl 
      | (`Varchar255, x) :: inner_tl -> Printf.sprintf "%s" x ^ loop inner_tl
    in
    loop hd :: table_to_string tl

let parse str stdout =
  match String.to_list str with
  | '.' :: tl ->
    let s = String.of_list tl in
    if String.equal_caseless s "exit" then exit 0 
    else Eio.Flow.copy_string (Fmt.str "Unrecognized command \'.%s\'\n" s) stdout 
  | x -> match String.split_on_char ' ' str with
         | [] -> 
            raise @@ Failure "Empty command please try again"
         | "insert" :: inputs when Equal.physical (List.length inputs) 3 -> 
            (* create a table *)
            tbl := insert [] [`Integer; `Varchar32; `Varchar255] inputs :: !tbl;
            Eio.Flow.copy_string "Executed.\n" stdout
         | "insert" :: inputs when List.length inputs < 3 ->
            Eio.Flow.copy_string "Insert expects 3 inputs.\n" stdout
         | "insert" :: inputs when List.length inputs > 3 ->
            Eio.Flow.copy_string "Insert expects 3 inputs.\n" stdout
         | "select" :: _ when  List.length !tbl > 0 ->
            Eio.Flow.copy_string (Fmt.str "%s\n" (String.concat "\n" @@ table_to_string !tbl)) stdout;
            Eio.Flow.copy_string "Executed.\n" stdout
         | "select" :: _ ->
            Eio.Flow.copy_string "Table is empty.\n" stdout
         | hd :: _ -> Eio.Flow.copy_string (Fmt.str "Unrecognized keyword at the start of \'%s\'.\n" str) stdout

let repl env =
  let stdin  = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  let reader = Eio.Buf_read.of_flow stdin ~max_size:1_000 in
  let rec loop () =
    Eio.Flow.copy_string "db > " stdout;
    let line = Eio.Buf_read.line reader in
    Eio.traceln " output > %s" line;
    parse line stdout;
    loop ()
  in
  loop ()

let run env =
  repl env |> ignore;
  ()

let () = Eio_main.run run