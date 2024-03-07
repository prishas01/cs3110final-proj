open Csv

type t = string list list

exception Invalid_Command of string

let read_csv str =
  try
    let rec help x =
      match x with
      | [] -> []
      | h :: t ->
          let rec row_help m =
            match m with
            | w :: z -> w :: row_help z
            | _ -> []
          in
          row_help h :: help t
    in
    str |> Csv.load |> help
  with _ -> raise (Invalid_Command "This command is invalid.")

let num_of_songs data =
  let rec helper data acc =
    match data with
    | [] -> acc
    | h :: t -> helper t (acc + 1)
  in
  helper data 0

let num_of_artists data =
  let rec helper data acc =
    match data with
    | [] -> acc
    | h :: t -> helper t (acc + 1)
  in
  helper data 0

let rec string_lt str =
  match str with
  | [] -> ""
  | h :: t -> h ^ " " ^ string_lt t

let rec get_data lst acc index =
  match lst with
  | [] -> raise (Invalid_Command "This command is invalid.")
  | h :: t -> if acc = index then h else get_data t (acc + 1) index

let read_row data name =
  match data with
  | [] -> raise (Invalid_Command "This command is invalid.")
  | h :: t -> get_data data 0 0

let all_info data =
  match data with
  | [] -> raise (Invalid_Command "This command is invalid.")
  | h :: t -> get_data data 0 0

let gen_data_ls array = List.flatten array

let song_by_album data input =
  let rec album_song_list data acc =
    match data with
    | [] -> acc
    | h :: t ->
        let album = List.nth h 2 in
        let rec find_album alb acc =
          match acc with
          | (some_album, song_list) :: t ->
              if some_album = alb then (alb, List.nth h 0 :: song_list) :: t
              else (some_album, song_list) :: find_album alb t
          | [] -> [ (alb, [ List.nth h 0 ]) ]
        in
        album_song_list t (find_album album acc)
  in
  let rec get_song pair_list input =
    match pair_list with
    | (some_album, its_song_list) :: t ->
        if String.lowercase_ascii some_album = String.lowercase_ascii input then
          let len = List.length its_song_list in
          let index = Random.int len in
          List.nth_opt its_song_list index
        else get_song t input
    | [] -> None
  in
  get_song (album_song_list data []) input

let song_by_artist data input =
  let rec artist_song_list data acc =
    match data with
    | [] -> acc
    | h :: t ->
        let artist = List.nth h 1 in
        let rec find_artist art acc =
          match acc with
          | (some_artist, song_list) :: t ->
              if some_artist = art then (art, List.nth h 0 :: song_list) :: t
              else (some_artist, song_list) :: find_artist art t
          | [] -> [ (art, [ List.nth h 0 ]) ]
        in
        artist_song_list t (find_artist artist acc)
  in
  let rec get_song pair_list input =
    match pair_list with
    | (some_artist, its_song_list) :: t ->
        if String.lowercase_ascii some_artist = String.lowercase_ascii input
        then
          let len = List.length its_song_list in
          let index = Random.int len in
          List.nth_opt its_song_list index
        else get_song t input
    | [] -> None
  in
  get_song (artist_song_list data []) input

let song_by_genre data input =
  let rec genre_song_list data acc =
    match data with
    | [] -> acc
    | h :: t ->
        let genre = List.nth h 4 in
        let rec find_genre gen acc =
          match acc with
          | (some_genre, song_list) :: t ->
              if some_genre = gen then (gen, List.nth h 0 :: song_list) :: t
              else (some_genre, song_list) :: find_genre gen t
          | [] -> [ (gen, [ List.nth h 0 ]) ]
        in
        genre_song_list t (find_genre genre acc)
  in
  let rec get_song pair_list input =
    match pair_list with
    | (some_genre, its_song_list) :: t ->
        if String.lowercase_ascii some_genre = String.lowercase_ascii input then
          let len = List.length its_song_list in
          let index = Random.int len in
          List.nth_opt its_song_list index
        else get_song t input
    | [] -> None
  in
  get_song (genre_song_list data []) input

let song_by_key data input =
  let rec key_song_list data acc =
    match data with
    | [] -> acc
    | h :: t ->
        let key = List.nth h 7 in
        let rec find_key k acc =
          match acc with
          | (some_key, song_list) :: t ->
              if some_key = k then (k, List.nth h 0 :: song_list) :: t
              else (some_key, song_list) :: find_key k t
          | [] -> [ (k, [ List.nth h 0 ]) ]
        in
        key_song_list t (find_key key acc)
  in
  let rec get_song pair_list input =
    match pair_list with
    | (some_key, its_song_list) :: t ->
        if String.lowercase_ascii some_key = String.lowercase_ascii input then
          let len = List.length its_song_list in
          let index = Random.int len in
          List.nth_opt its_song_list index
        else get_song t input
    | [] -> None
  in
  get_song (key_song_list data []) input

let song_by_year data input =
  let rec year_song_list data acc =
    match data with
    | [] -> acc
    | h :: t ->
        let year = List.nth h 3 in
        let rec find_year y acc =
          match acc with
          | (some_year, song_list) :: t ->
              if some_year = y then (y, List.nth h 0 :: song_list) :: t
              else (some_year, song_list) :: find_year y t
          | [] -> [ (y, [ List.nth h 0 ]) ]
        in
        year_song_list t (find_year year acc)
  in
  let rec get_song pair_list input =
    match pair_list with
    | (some_year, its_song_list) :: t ->
        if String.lowercase_ascii some_year = String.lowercase_ascii input then
          let len = List.length its_song_list in
          let index = Random.int len in
          List.nth_opt its_song_list index
        else get_song t input
    | [] -> None
  in
  get_song (year_song_list data []) input

let song_by_runtime data input =
  let rec runtime_song_list data acc =
    match data with
    | [] -> acc
    | h :: t ->
        let runtime = List.nth h 5 in
        let rec find_runtime r acc =
          match acc with
          | (some_runtime, song_list) :: t ->
              if some_runtime = r then (r, List.nth h 0 :: song_list) :: t
              else (some_runtime, song_list) :: find_runtime r t
          | [] -> [ (r, [ List.nth h 0 ]) ]
        in
        runtime_song_list t (find_runtime runtime acc)
  in
  let rec get_song pair_list input =
    match pair_list with
    | (some_runtime, its_song_list) :: t ->
        if String.lowercase_ascii some_runtime = String.lowercase_ascii input
        then
          let len = List.length its_song_list in
          let index = Random.int len in
          List.nth_opt its_song_list index
        else get_song t input
    | [] -> None
  in
  get_song (runtime_song_list data []) input

let song_by_tempo data input =
  let rec tempo_song_list data acc =
    match data with
    | [] -> acc
    | h :: t ->
        let tempo = List.nth h 6 in
        let rec find_tempo temp acc =
          match acc with
          | (some_tempo, song_list) :: t ->
              if some_tempo = temp then (temp, List.nth h 0 :: song_list) :: t
              else (some_tempo, song_list) :: find_tempo temp t
          | [] -> [ (temp, [ List.nth h 0 ]) ]
        in
        tempo_song_list t (find_tempo tempo acc)
  in
  let rec get_song pair_list input =
    match pair_list with
    | (some_tempo, its_song_list) :: t ->
        if String.lowercase_ascii some_tempo = String.lowercase_ascii input then
          let len = List.length its_song_list in
          let index = Random.int len in
          List.nth_opt its_song_list index
        else get_song t input
    | [] -> None
  in
  get_song (tempo_song_list data []) input

let get_all_artists data =
  let rec helper data acc =
    match data with
    | [] -> acc
    | h :: t ->
        let artist = List.nth h 1 in
        let find_artist a acc = if List.mem a acc then acc else a :: acc in
        helper t (find_artist artist acc)
  in
  let data' =
    match data with
    | h :: data_tail -> data_tail
    | _ -> []
  in
  helper data' []

let get_all_genres data =
  let rec helper data acc =
    match data with
    | [] -> acc
    | h :: t ->
        let genre = List.nth h 4 in
        let find_genre g acc = if List.mem g acc then acc else g :: acc in
        helper t (find_genre genre acc)
  in
  let data' =
    match data with
    | h :: data_tail -> data_tail
    | _ -> []
  in
  helper data' []

let get_all_songs data =
  let rec helper data acc =
    match data with
    | [] -> acc
    | h :: t ->
        let song = List.nth h 0 in
        let find_song s acc = if List.mem s acc then acc else s :: acc in
        helper t (find_song song acc)
  in
  let data' =
    match data with
    | h :: data_tail -> data_tail
    | _ -> []
  in
  helper data' []

let rec genre_of_song data input =
  match data with
  | h :: t ->
      if String.lowercase_ascii (List.hd h) = String.lowercase_ascii input then
        List.nth h 4
      else genre_of_song t input
  | [] -> ""

let rec artist_of_song data input =
  match data with
  | h :: t ->
      if String.lowercase_ascii (List.hd h) = String.lowercase_ascii input then
        List.nth h 1
      else artist_of_song t input
  | [] -> ""

let rec album_of_song data input =
  match data with
  | h :: t ->
      if String.lowercase_ascii (List.hd h) = String.lowercase_ascii input then
        List.nth h 2
      else album_of_song t input
  | [] -> ""

let rec year_of_song data input =
  match data with
  | h :: t ->
      if String.lowercase_ascii (List.hd h) = String.lowercase_ascii input then
        List.nth h 3
      else year_of_song t input
  | [] -> ""

let rec runtime_of_song data input =
  match data with
  | h :: t ->
      if String.lowercase_ascii (List.hd h) = String.lowercase_ascii input then
        List.nth h 5
      else runtime_of_song t input
  | [] -> ""

let rec tempo_of_song data input =
  match data with
  | h :: t ->
      if String.lowercase_ascii (List.hd h) = String.lowercase_ascii input then
        List.nth h 6
      else tempo_of_song t input
  | [] -> ""

let rec key_of_song data input =
  match data with
  | h :: t ->
      if String.lowercase_ascii (List.hd h) = String.lowercase_ascii input then
        List.nth h 7
      else key_of_song t input
  | [] -> ""

let rec composer_of_song data input =
  match data with
  | h :: t ->
      if String.lowercase_ascii (List.hd h) = String.lowercase_ascii input then
        List.nth h 1
      else composer_of_song t input
  | [] -> ""

let rec releaseyear_of_song data input =
  match data with
  | h :: t ->
      if String.lowercase_ascii (List.hd h) = String.lowercase_ascii input then
        List.nth h 2
      else releaseyear_of_song t input
  | [] -> ""

let rec classicalgenre_of_song data input =
  match data with
  | h :: t ->
      if String.lowercase_ascii (List.hd h) = String.lowercase_ascii input then
        List.nth h 3
      else classicalgenre_of_song t input
  | [] -> ""

let rec instrumentation_of_song data input =
  match data with
  | h :: t ->
      if String.lowercase_ascii (List.hd h) = String.lowercase_ascii input then
        List.nth h 4
      else instrumentation_of_song t input
  | [] -> ""

let song_by_classgenre data input =
  let rec genre_song_list data acc =
    match data with
    | [] -> acc
    | h :: t ->
        let genre = List.nth h 3 in
        let rec find_genre gen acc =
          match acc with
          | (some_genre, song_list) :: t ->
              if some_genre = gen then (gen, List.nth h 0 :: song_list) :: t
              else (some_genre, song_list) :: find_genre gen t
          | [] -> [ (gen, [ List.nth h 0 ]) ]
        in
        genre_song_list t (find_genre genre acc)
  in
  let rec get_song pair_list input =
    match pair_list with
    | (some_genre, its_song_list) :: t ->
        if String.lowercase_ascii some_genre = String.lowercase_ascii input then
          let len = List.length its_song_list in
          let index = Random.int len in
          List.nth_opt its_song_list index
        else get_song t input
    | [] -> None
  in
  if input = "fancy" then get_song (genre_song_list data []) "Baroque"
  else if input = "modern" then
    get_song (genre_song_list data []) "20th Century"
  else get_song (genre_song_list data []) input
