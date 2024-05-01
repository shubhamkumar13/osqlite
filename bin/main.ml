[@@@warning "-27-39-34"]
open Containers

type cell = {
  id : int;
  username : string;
  email: string;
}

type table = {
  rows : int;
  index : int;
  stack : cell Stack.t
}

let empty_table () : table = {rows = 0; index = 0; stack = Stack.create ()}
let tbl = ref @@ empty_table ()

let rec insert tbl (id, username, email) =
  let current = {id; username; email} in
  let rows = tbl.rows + 1 in
  let index = tbl.index + 1 in
  Stack.push current tbl.stack;
  {tbl with rows; index}

let rec fmt_table =
  let fmt_cell = 
    let fmt_id =
      let f cell = cell.id in
      Fmt.Dump.field "id" f @@ Fmt.int
    in
    let fmt_username =
      let f cell = cell.username in
      Fmt.Dump.field "username" f @@ Fmt.string
    in
    let fmt_email =
      let f cell = cell.email in
      Fmt.Dump.field "email" f @@ Fmt.string
    in
  Fmt.record [fmt_id; fmt_username; fmt_email]
  in
  let fmt_row =
    let f table = table.rows in
    Fmt.Dump.field "rows" f @@ Fmt.int
  in
  let fmt_index =
    let f table = table.index in
    Fmt.Dump.field "index" f @@ Fmt.int
  in
  let fmt_cell_stack =
    let f table = table.stack in
    Fmt.Dump.field "stack" f @@ (Fmt.Dump.stack @@ fmt_cell)
  in
  Fmt.record [fmt_row; fmt_index; fmt_cell_stack]

let parse str stdout =
  match String.to_list str with
  | '.' :: tl ->
    let s = String.of_list tl in
    if String.equal_caseless s "exit" then exit 0 
    else Eio.Flow.copy_string (Fmt.str "Unrecognized command \'.%s\'\n" s) stdout 
  | x -> match String.split_on_char ' ' str with
         | [] -> 
            raise @@ Failure "Empty command please try again"
         | "insert" :: id :: username :: email :: []  -> 
            (* create a table *)
            let id = int_of_string id in
            tbl := insert !tbl (id, username, email);
            Eio.Flow.copy_string "Executed.\n" stdout
         | "insert" :: _ :: _ :: _ :: _ ->
            Eio.Flow.copy_string "Insert expects 3 inputs.\n" stdout
         | "insert" :: inputs when List.length inputs < 3 ->
            Eio.Flow.copy_string "Insert expects 3 inputs.\n" stdout
         | "select" :: _ when !tbl.rows > 0 ->
            Eio.Flow.copy_string (Fmt.to_to_string fmt_table !tbl) stdout;
            Eio.Flow.copy_string "\nExecuted.\n" stdout
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
    Eio.Flow.copy_string "--|| output ||---> " stdout;
    parse line stdout;
    loop ()
  in
  loop ()

let run env =
  repl env |> ignore;
  ()

let () = Eio_main.run run