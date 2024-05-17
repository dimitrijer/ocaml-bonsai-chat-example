open! Core
open! Async

let initialize_connection _initiated_from _addr _inet connection =
  { User_state.user = "sample-username"; connection }
;;

let deflate_string ?(level = 4) str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (String.length str - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string str ~src_off:!p buf ~dst_off:0 ~len;
    p := !p + len;
    len
  in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str
  in
  Zl.Higher.compress ~level ~dynamic:true ~w ~q ~refill ~flush i o;
  Buffer.contents r
;;

let respond_string ~content_type ?flush ?headers ?status s =
  let headers = Cohttp.Header.add_opt headers "Content-Type" content_type in
  let headers = Cohttp.Header.add headers "Content-Encoding" "deflate" in
  Cohttp_async.Server.respond_string ?flush ~headers ?status (deflate_string s)
;;

let not_found_html =
  {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <title>404 Not Found</title>
  </head>
  <body>
    <h1>404 Not Found</h1>
  </body>
</html>
|}
;;

let html =
  {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <script defer src="main.js"></script>
    <link rel="stylesheet" type="text/css" href="style.css">
    <title> RPC-Chat </title>
  </head>

  <body>
    <div id="app"></div>
  </body>
</html>
|}
;;

let handler ~body:_ _inet req =
  let path = Uri.path (Cohttp.Request.uri req) in
  match path with
  | "" | "/" | "/index.html" -> respond_string ~content_type:"text/html" html
  | "/main.js" ->
    respond_string
      ~content_type:"application/javascript"
      Embedded_files.main_dot_bc_dot_js
  | "/style.css" -> respond_string ~content_type:"text/css" Embedded_files.style_dot_css
  | other ->
    if String.is_prefix other ~prefix:"/img/"
    then (
      let filename =
        "./static/img" ^/ (Uri.of_string other |> Uri.path |> Filename.basename)
      in
      print_s [%message filename];
      let gz_filename = filename ^ ".gz" in
      let headers = None in
      let%bind headers, filename =
        match%bind Sys.file_exists gz_filename with
        | `Yes ->
          return
            (Some (Cohttp.Header.add_opt headers "content-encoding" "gzip"), gz_filename)
        | `No | `Unknown ->
          let%map file_exists = Sys.file_exists filename in
          (match file_exists with
           | `No | `Unknown ->
             print_s [%message "not found" (filename : string)];
             ()
           | `Yes -> ());
          headers, filename
      in
      Cohttp_async.Server.respond_with_file ?headers filename)
    else respond_string ~content_type:"text/html" ~status:`Not_found not_found_html
;;

let main ~port =
  let global_state = Global_state.create () in
  let hostname = Unix.gethostname () in
  printf "Serving http://%s:%d/\n%!" hostname port;
  let%bind server =
    let http_handler () = handler in
    Rpc_websocket.Rpc.serve
      ~on_handler_error:`Ignore
      ~mode:`TCP
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ~http_handler
      ~implementations:(Rpc_implementations.implementations global_state)
      ~initial_connection_state:initialize_connection
      ()
  in
  Cohttp_async.Server.close_finished server
;;

let command =
  Command.async
    ~summary:"Start server for rpc-chat"
    (let%map_open.Command port =
       flag "port" (optional_with_default 8080 int) ~doc:"port on which to serve"
     in
     fun () -> main ~port)
;;
