open Mirage

(** Custom bootvars *)

(** Default language *)
let bootvar_lang =
  let doc = Key.Doc.create
      ~doc:"Default language for the page served by the unikernel." ["lang"]
  in
  Key.create ~doc ~default:"en" "lang" Key.Desc.string

(** Consider headers *)
let bootvar_use_headers =
  let doc = Key.Doc.create
      ~doc:"Use headers to determine the language of the website visitor."
      ["use-header"]
  in
  Key.create ~doc ~default:true "use_headers" Key.Desc.bool

(* Network configuration *)

let stack = generic_stackv4 default_console tap0

(* storage configuration *)

let data = crunch "./static"
let keys = crunch "./secrets"

(* Dependencies *)

let server =
  foreign "Dispatch.Make"
    (console @-> clock @-> kv_ro @-> kv_ro @-> http @-> job)

let my_https =
  http_server @@ conduit_direct ~tls:true stack

let () =
  let libraries = [ "sequence" ; "containers" ; "tyxml" ] in
  let packages = [ "sequence" ; "containers" ; "tyxml" ] in
  register "No"
    ~libraries
    ~packages
    ~keys:[
      Key.hide bootvar_lang ;
      Key.hide bootvar_use_headers ;
    ]
    [ server $ default_console $ default_clock $ data $ keys $ my_https ]
