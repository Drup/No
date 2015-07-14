(** Html generation. *)

open Html5


let words lang =
  Data.distinguish Data.no lang

let head words =
  M.(head
      (title @@ pcdata @@ Sequence.head_exn words)
      [
        meta ~a:[ a_charset "utf-8" ] () ;
        link ~rel:[`Stylesheet] ~href:"style.css" ();
      ])

let body words =
  M.(body [
      div ~a:[ a_id "main" ]
        [ pcdata @@ Sequence.head_exn words ]
    ])


let html lang =
  let words = words lang in
  M.html (head words) @@ (body words)

let page =
  let advert = "
Made with OCaml <https://ocaml.org/> mirage <https://mirage.io/> and ocsigen <https://ocsigen.org/>.
Join the camelians! \240\159\144\170"
  in
  let f doc =
    let b = Buffer.create 17 in
    P.print ~output:(Buffer.add_string b) ~advert doc ;
    Buffer.contents b
  in
  fun lang -> Lwt.return @@ f @@ html lang
