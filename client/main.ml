open! Core
open! Async_kernel
open! Bonsai_web
open Async_js
open Bonsai_chat_open_source_common
open Composition_infix

let run_refresh_rooms ~conn ~rooms_list_var =
  let%map rooms = Rpc.Rpc.dispatch_exn Protocol.List_rooms.t conn () in
  Bonsai.Var.set rooms_list_var rooms
;;

let refresh_rooms ~conn ~rooms_list_var =
  let dispatch =
    (fun () -> run_refresh_rooms ~conn ~rooms_list_var) |> Effect.of_deferred_fun
  in
  dispatch ()
;;

module Room_state = struct
  type t =
    { messages : Message.t list
    ; current_room : Room.t option
    }
  [@@deriving fields]
end

let process_message_stream ~conn ~room_state_var =
  let%bind pipe, _ = Rpc.Pipe_rpc.dispatch_exn Protocol.Message_stream.t conn () in
  Pipe.iter pipe ~f:(fun message ->
    Bonsai.Var.update
      room_state_var
      ~f:(fun ({ Room_state.messages; current_room } as prev) ->
        if [%equal: Room.t option] current_room (Some message.room)
        then { prev with messages = List.append messages [ message ] }
        else prev);
    Deferred.unit)
;;

let send_message ~conn =
  let obfuscate = Fn.id in
  let dispatch =
    Rpc.Rpc.dispatch_exn Protocol.Send_message.t conn
    |> Effect.of_deferred_fun
    >> Effect.bind ~f:(function
      | Ok a -> Effect.return a
      | Error _ -> Effect.Ignore)
  in
  fun ~room ~contents ->
    let contents = obfuscate contents in
    dispatch { Message.room; contents; author = "sample-username" }
;;

let change_room ~conn ~room_state_var =
  let on_room_switch room =
    let%map messages = Rpc.Rpc.dispatch_exn Protocol.Messages_request.t conn room in
    Bonsai.Var.set room_state_var { Room_state.messages; current_room = Some room }
  in
  let dispatch = on_room_switch |> Effect.of_deferred_fun in
  fun room -> dispatch room
;;

let run () =
  Async_js.init ();
  let%bind conn = Rpc.Connection.client_exn () in
  let rooms_list_var = Bonsai.Var.create [] in
  let room_state_var =
    Bonsai.Var.create { Room_state.messages = []; current_room = None }
  in
  let change_room = change_room ~conn ~room_state_var in
  let refresh_rooms = refresh_rooms ~conn ~rooms_list_var in
  let send_message = send_message ~conn in
  let () =
    Bonsai_web.Start.start
      (let open Bonsai.Let_syntax in
       App.component
         ~room_list:(Bonsai.Var.value rooms_list_var)
         ~current_room:(Room_state.current_room <$> Bonsai.Var.value room_state_var)
         ~messages:(Room_state.messages <$> Bonsai.Var.value room_state_var)
         ~refresh_rooms
         ~change_room
         ~send_message)
  in
  don't_wait_for (run_refresh_rooms ~conn ~rooms_list_var);
  don't_wait_for (process_message_stream ~conn ~room_state_var);
  return ()
;;

let () = don't_wait_for (run ())
