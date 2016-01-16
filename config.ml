open Mirage

(** Custom bootvars *)

(** Default language *)
let bootvar_lang =
  let i = Key.Arg.info
      ~doc:"Default language for the page served by the unikernel." ["lang"]
  in
  Key.create "lang" Key.Arg.(opt string "en" i)

(** Consider headers *)
let bootvar_use_headers =
  let i = Key.Arg.info
      ~doc:"Use headers to determine the language of the website visitor."
      ["use-header"]
  in
  Key.create "use_headers" Key.Arg.(opt bool true i)

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
      Key.abstract bootvar_lang ;
      Key.abstract bootvar_use_headers ;
    ]
    [ server $ default_console $ default_clock $ data $ keys $ my_https ]
