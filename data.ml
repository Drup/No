(** The database containing the words. *)

let distinguish tbl l =
  let module S = Sequence in
  let front =
    l |> S.of_list
      |> S.uniq ~eq:CCString.equal
      |> S.filter_map (CCHashtbl.get tbl)
  in
  CCHashtbl.to_seq tbl
  |> S.filter (fun (x,_) -> not @@ List.mem x l)
  |> S.map snd
  |> S.to_array
  |> (fun a -> CCArray.shuffle a ; S.of_array a)
  |> S.append front


(** A list of [tag list * string]
    where tag is http://tools.ietf.org/html/bcp47
    See http://www.w3.org/International/articles/language-tags/ for a friendly explanation.
*)

(** No, in many languages. *)
let no = CCHashtbl.of_list [
  "af"     , "Neen."     ; (* Afrikaans *)
  "am"     , "لا"         ; (* Amharic *)
  "be"     , "Не."       ; (* Belarusian *)
  "ber"    , "Uhu."      ; (* Berber languages *)
  "bn"     , "না।"        ; (* Bengali *)
  "ca"     , "No."       ; (* Catalan *)
  "cmn"    , "不是。"     ; (* Mandarin Chinese *)
  "cs"     , "Ne."       ; (* Czech *)
  "cv"     , "Тӗрӗс мар" ; (* Chuvash *)
  "da"     , "Nej."      ; (* Danish *)
  "de"     , "Nein."     ; (* German *)
  "ekk"    , "Ei."       ; (* Standard Estonian *)
  "el"     , "Óχι"       ; (* Modern Greek *)
  "en"     , "No."       ; (* English *)
  "eo"     , "Ne."       ; (* Esperanto *)
  "es"     , "No."       ; (* Spanish, Castilian *)
  "fi"     , "Ei."       ; (* Finnish *)
  "fr"     , "Non."      ; (* French *)
  "gu"     , "ના."       ; (* Gujarati *)
  "he"     , "לא"        ; (* Hebrew *)
  "hi"     , "नहीं"       ; (* Hindi *)
  "hu"     , "Nem."      ; (* Hungarian *)
  "ia"     , "No."       ; (* Interlingua *)
  "id"     , "Tidak."    ; (* Indonesian *)
  "is"     , "Nei."      ; (* Icelandic *)
  "it"     , "No."       ; (* Italian *)
  "ja"     , "いいえ。"   ; (* Japanese *)
  "jbo"    , "na go'i"   ; (* Lojban *)
  "kk"     , "Жоқ."      ; (* Kazakh *)
  "kn"     , "ಇಲ್ಲ"       ; (* Kannada *)
  "la"     , "Non est."  ; (* Latin *)
  "lb"     , "Neen."     ; (* Luxembourgish, Letzeburgesch *)
  "lt"     , "Ne."       ; (* Lithuanian *)
  "lvs"    , "Nē."       ; (* Standard Latvian *)
  "lzh"    , "非也。"     ; (* Literary Chinese *)
  "mr"     , "नाही."      ; (* Marathi *)
  "mt"     , "Le."       ; (* Maltese *)
  "nds"    , "Nee."      ; (* Low German, Low Saxon *)
  "nl"     , "Neen."     ; (* Dutch, Flemish *)
  "nn"     , "Nei."      ; (* Norwegian Nynorsk *)
  "nb"     , "Nei."      ; (* Norwegian Bokmål *)
  "npi"    , "होईन"      ; (* Nepali *)
  "pa"     , "نیں"       ; (* Panjabi, Punjabi *)
  "pes"    , "نه"        ; (* Iranian Persian *)
  "prs"    , "نه"        ; (* Dari, Afghan Persian *)
  "pl"     , "Nie."      ; (* Polish *)
  "pt"     , "Não."      ; (* Portuguese *)
  "qya"    , "Lau."      ; (* Quenia *)
  "ro"     , "Nu."       ; (* Romanian, Moldavian, Moldovan *)
  "ro"     , "Nu."       ; (* Romania *)
  "ru"     , "Нет."      ; (* Russian *)
  "sd"     , "نا"        ; (* Sindhi *)
  "sv"     , "Nej."      ; (* Swedish *)
  "te"     , "No."       ; (* Telugu *)
  "tl"     , "Hindi."    ; (* Tagalog *)
  "tlh"    , "Qo'."      ; (* Klingon *)
  (* "tok" , "ala."      ; (* Toki Pona, NO LANGUAGE TAG *) *)
  "tpi"    , "Nogat."    ; (* Tok Pisin *)
  "tr"     , "Hayır."    ; (* Turkish *)
  "tt"     , "Юк."       ; (* Tatar *)
  "ug"     , "ياق"       ; (* Uighur, Uyghur *)
  "uk"     , "Ні."       ; (* Ukrainian *)
  "ur"     , "نہیں"      ; (* Urdu *)
  "yue"    , "唔係。"     ; (* Yue Chinese, Cantonese *)
  "zsm"    , "Tidak."    ; (* Standard Malay *)
]
