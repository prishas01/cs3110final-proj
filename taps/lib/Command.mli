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

val parsing_s : string -> phrases
(** Returns a phrase for a given input string.*)
