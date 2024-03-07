exception Invalid_Command of string

type obj_phrase = string list

type phrases =
  | Load of obj_phrase
  | Exit
  | Help
  | Nsongs
  | Nartists
  | Allinfo
  | SongByAlbum
  | SongByArtist
  | SongByGenre
  | SongByKey
  | SongByYear
  | SongByRuntime
  | SongByTempo
  | GetAllArtists
  | GetAllGenres
  | GetAllSongs
  | GenreOfSong
  | ArtistOfSong
  | AlbumOfSong
  | YearOfSong
  | RuntimeOfSong
  | TempoOfSong
  | KeyOfSong
  | CompOfSong
  | ReleaseOfSong
  | ClassicalGenreOfSong
  | InstrumentationOfSong
  | SongbyMood

let parsing_s input =
  input |> String.trim |> String.lowercase_ascii
  |> (fun x ->
       if x = "" then raise (Invalid_Command "Please type a command.") else x)
  |> String.split_on_char ' ' |> List.map String.trim
  |> List.filter (fun x -> not (x = ""))
  |> fun str ->
  match str with
  | cmd :: rest -> (
      match cmd with
      | "load" -> Load rest
      | "exit" -> Exit
      | "help" -> Help
      | "nsongs" -> Nsongs
      | "nartists" -> Nartists
      | "allinfo" -> Allinfo
      | "songalbum" -> SongByAlbum
      | "songartist" -> SongByArtist
      | "songgenre" -> SongByGenre
      | "songkey" -> SongByKey
      | "songyear" -> SongByYear
      | "songruntime" -> SongByRuntime
      | "songtempo" -> SongByTempo
      | "getallartists" -> GetAllArtists
      | "getallgenres" -> GetAllGenres
      | "getallsongs" -> GetAllSongs
      | "genreofsong" -> GenreOfSong
      | "artistofsong" -> ArtistOfSong
      | "albumofsong" -> AlbumOfSong
      | "yearofsong" -> YearOfSong
      | "runtimeofsong" -> RuntimeOfSong
      | "tempoofsong" -> TempoOfSong
      | "keyofsong" -> KeyOfSong
      | "songcomposer" -> CompOfSong
      | "releaseyear" -> ReleaseOfSong
      | "classicalgenre" -> ClassicalGenreOfSong
      | "instrumentation" -> InstrumentationOfSong
      | "pickasong" -> SongbyMood
      | _ -> raise (Invalid_Command "That command is invalid."))
  | _ -> raise (Invalid_Command "This command is invalid.")
