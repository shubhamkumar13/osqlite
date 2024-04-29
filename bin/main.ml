[@@@warning "-27-39"]

let parse str writer =
  match str with
  | ".exit" -> exit 0
  | x -> Eio.Buf_write.printf writer "Unrecognized command %s \n" str

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