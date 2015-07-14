open Mirage

let red fmt = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let red_s = red "%s"
let err fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "%s %s\n%!" (red_s "[ERROR] ") str;
      exit 1
    ) fmt

let get ?doc ?default name f =
  try Unix.getenv name |> String.lowercase |> f
  with Not_found ->
    match default with
    | None   -> err "%s is not set" name
    | Some d -> d

let bool_of_env = function
  | "" | "0" | "false" -> false
  | _  -> true

(** Custom bootvars *)

(** Default language *)
let bootvar_lang =
  bootvar ~default:"en" "lang"

(** Consider headers *)
let bootvar_use_headers =
  bootvar ~default:"true" "use-headers"

(* Network configuration *)

let ip = Ipaddr.V4.of_string_exn
let address = get "ADDRESS" ~default:(ip "10.0.0.2") ip
let netmask = get "NETMASK" ~default:(ip "255.255.255.0") ip
let gateway = get "GATEWAY" ~default:(ip "10.0.0.1") ip
let address = { address; netmask; gateways = [gateway] }

let net =
  get "NET" ~default:`Socket  (function "socket" -> `Socket | _ -> `Direct)

let dhcp = get "DHCP" ~default:true bool_of_env

let stack =
  match net, dhcp with
  | `Direct, true  -> direct_stackv4_with_dhcp default_console tap0
  | `Direct, false -> direct_stackv4_with_static_ipv4 default_console tap0 address
  | `Socket, _     -> socket_stackv4 default_console [Ipaddr.V4.any]

(* storage configuration *)

let data = crunch "./static"
let keys = crunch "./secrets"

(* Dependencies *)

let https =
  let libraries = [ "tls.mirage"; "mirage-http" ] in
  let packages = [ "tls"; "mirage-http" ] in
  foreign ~libraries ~packages "Dispatch.Make"
    (console @-> stackv4 @-> clock @-> kv_ro @-> kv_ro @-> job)

let () =
  let ocamlfind = [ "sequence" ; "containers" ; "tyxml" ] in
  let opam = [ "sequence" ; "containers" ; "tyxml" ] in
  add_to_ocamlfind_libraries ocamlfind;
  add_to_opam_packages opam;
  register "No"
    ~bootvars:[
      bootvar_lang ;
      bootvar_use_headers ;
    ]
    [ https $ default_console $ stack $ default_clock $ data $ keys ]
