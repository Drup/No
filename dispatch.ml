open V1
open V1_LWT

open Lwt.Infix

(** Common signature for http and https. *)
module type HTTP = sig
  include Cohttp_lwt.Server
  val listen: t -> IO.conn -> unit Lwt.t
end

module Dispatch (C: CONSOLE) (FS: KV_RO) (S: HTTP) = struct

  let log c fmt = Printf.ksprintf (C.log c) fmt

  let read_fs fs name =
    FS.size fs name >>= function
    | `Error (FS.Unknown_key _) ->
      Lwt.fail (Failure ("read " ^ name))
    | `Ok size ->
      FS.read fs name 0 (Int64.to_int size) >>= function
      | `Error (FS.Unknown_key _) -> Lwt.fail (Failure ("read " ^ name))
      | `Ok bufs -> Lwt.return (Cstruct.copyv bufs)

  (** This is the part that is not boilerplate. *)

  let bool_of_string = function
    | "" | "0" | "false" -> false
    | _  -> true

  let accept_lang headers =
    if not @@ bool_of_string @@ Bootvar_gen.use_headers () then []
    else
      let open Cohttp in
      headers
      |> Header.get_acceptable_languages
      |> Accept.qsort
      |> CCList.filter_map (function
        | _, Accept.Language (tag :: _) -> Some tag
        | _ -> None
      )

  let get_content c fs request uri = match Uri.path uri with
    | "" | "/" | "index.html" ->
      let lang =
        CCOpt.get [] (Uri.get_query_param' uri "lang") @
        accept_lang (Cohttp.Request.headers request) @
        [Bootvar_gen.lang ()]
      in
      log c "Answering languages: %s" @@ String.concat ";" @@ lang ;
      Generator.page lang
    | s -> read_fs fs s


  (** Dispatching/redirecting boilerplate. *)

  let dispatcher fs c request uri =
    Lwt.catch
      (fun () ->
         get_content c fs request uri >>= fun body ->
         S.respond_string ~status:`OK ~body ())
      (fun _exn ->
         S.respond_not_found ())


  let redirect _c _request uri =
    let new_uri = Uri.with_scheme uri (Some "https") in
    let headers =
      Cohttp.Header.init_with "location" (Uri.to_string new_uri)
    in
    S.respond ~headers ~status:`Moved_permanently ~body:`Empty ()

  let serve c dispatch =
    let callback (_, cid) request _body =
      let uri = Cohttp.Request.uri request in
      let cid = Cohttp.Connection.to_string cid in
      log c "[%s] serving %s." cid (Uri.to_string uri);
      dispatch c request uri
    in
    let conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      log c "[%s] closing." cid
    in
    S.make ~conn_closed ~callback ()

end

(** Server boilerplate *)
module Make
    (C : CONSOLE) (S : STACKV4)
    (Clock : CLOCK)
    (DATA : KV_RO) (KEYS: KV_RO) =
struct

  module TCP  = S.TCPV4
  module TLS  = Tls_mirage.Make (TCP)
  module X509 = Tls_mirage.X509 (KEYS) (Clock)

  module Http  = Cohttp_mirage.Server(TCP)
  module Https = Cohttp_mirage.Server(TLS)

  module D  = Dispatch(C)(DATA)(Http)
  module DS = Dispatch(C)(DATA)(Https)

  let log c fmt = Printf.ksprintf (C.log c) fmt

  let with_http c = Http.listen @@ D.serve c D.redirect

  let with_tls c cfg tcp ~f =
    let peer, port = TCP.get_dest tcp in
    let log str = log c "[%s:%d] %s" (Ipaddr.V4.to_string peer) port str in
    TLS.server_of_flow cfg tcp >>= function
    | `Error _ -> log "TLS failed"; TCP.close tcp
    | `Ok tls  -> log "TLS ok"; f tls >>= fun () ->TLS.close tls
    | `Eof     -> log "TLS eof"; TCP.close tcp

  let tls_init kv =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    Lwt.return conf

  let start c stack _clock data keys =
    tls_init keys >>= fun cfg ->
    let callback = Https.listen @@ DS.serve c (DS.dispatcher data) in
    let https flow = with_tls c cfg flow ~f:callback in
    let http flow = with_http c flow in
    S.listen_tcpv4 stack ~port:443 https;
    S.listen_tcpv4 stack ~port:80 http;
    S.listen stack

end
