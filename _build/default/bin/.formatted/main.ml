open Lwt
open Batteries

let listen_address = Unix.inet_addr_loopback
let port = 9000
let backlog = 10
let buf_size = 4

type action =
  | Quit
  | Empty
  | Error

let handle_message msg =
  match msg with
  | "quit" -> Quit
  | "" -> Empty
  | _ -> Error
;;

let get_line ic () =
  Lwt_io.read_line_opt ic
  >>= fun msg ->
  match msg with
  | Some msg -> return (handle_message msg)
  | None -> return Error
;;

let rec _handle_connection ic oc () =
  get_line ic ()
  >>= fun res ->
  match res with
  | Quit -> Lwt_io.write_line oc "quit" >>= _handle_connection ic oc
  | Empty -> Lwt_io.write_line oc "empty!" >>= _handle_connection ic oc
  | Error -> Logs_lwt.info (fun m -> m "Connection closed") >>= return
;;

let rec handle_conn ic oc () =
  let buf = Bytes.init buf_size (fun _ -> Char.chr 0) in
  Lwt_io.read_into_exactly ic buf 0 buf_size
  >>= (fun _ ->
        Lwt_io.write_line oc ("{" ^ String.of_char (Bytes.get buf 0) ^ "}"))
  >>= (fun _ ->
        print_endline "test?";
        return ())
  >>= handle_conn ic oc
;;

(*   | _ -> Lwt_io.write_line oc "idk" >>= handle_connection ic oc *)

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.on_failure (handle_conn ic oc ()) (fun e ->
    Logs.err (fun m -> m "%s" (Printexc.to_string e)));
  Logs_lwt.info (fun m -> m "New Connection") >>= return
;;

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  (bind sock @@ ADDR_INET (listen_address, port) |> fun x -> ignore x);
  listen sock backlog;
  sock
;;

let create_server sock =
  let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
  serve
;;

let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let sock = create_socket () in
  let serve = create_server sock in
  Lwt_main.run @@ serve ()
;;
