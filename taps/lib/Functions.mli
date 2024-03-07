type t

val read_csv : string -> t
(** Reads in the data from the csv file and outputs it as a type t.*)

val num_of_songs : t -> int
(** Returns an int for the number of songs that are in the inputted csv file.*)

val num_of_artists : t -> int
(** Returns an int for the number of artists that are in the inputted csv file.*)

val read_row : t -> string -> string list
(** Reads in the data from the csv file given a specified row and outputs it as
    a string list.*)

val all_info : t -> string list
(** Returns a string list for all the information that is provided in the
    inputted csv file.*)

val gen_data_ls : t -> string list
(** Returns a flattened array.*)

val string_lt : string list -> string
(** Returns a string for a given string list.*)

val song_by_album : t -> string -> string option
(** Returns a string option of a song in the inputted album. Returns None if the
    album is not found in the csv file. *)

val song_by_artist : t -> string -> string option
(** Returns a string option of a song by the inputted artist. Returns None if
    the artist is not found in the csv file. *)

val song_by_genre : t -> string -> string option
(** Returns a string option of a song of the inputted genre. Returns None if the
    genre is not found in the csv file. *)

val song_by_key : t -> string -> string option
(** Returns a string option of a song in the inputted key. Returns None if the
    key is not found in the csv file. *)

val song_by_year : t -> string -> string option
(** Returns a string option of a song from the inputted year. Returns None if
    the year is not found in the csv file. *)

val song_by_runtime : t -> string -> string option
(** Returns a string option of a song of the inputted runtime (in seconds).
    Returns None if the runtime is not found in the csv file. *)

val song_by_tempo : t -> string -> string option
(** Returns a string option of a song in the inputted tempo (in BPM). Returns
    None if the tempo is not found in the csv file. *)

val get_all_artists : t -> string list
(** Returns a string list of all artists in the csv file. *)

val get_all_genres : t -> string list
(** Returns a string list of all genres in the csv file. *)

val get_all_songs : t -> string list
(** Returns a string list of all songs in the csv file. *)

val genre_of_song : t -> string -> string
(** Returns the genre of a given song from the csv file, or an empty string if
    the song is not found. *)

val artist_of_song : t -> string -> string
(** Returns the artist of a given song from the csv file, or an empty string if
    the song is not found. *)

val album_of_song : t -> string -> string
(** Returns the album of a given song from the csv file, or an empty string if
    the song is not found. *)

val year_of_song : t -> string -> string
(** Returns the release year of a given song from the csv file, or an empty
    string if the song is not found. *)

val runtime_of_song : t -> string -> string
(** Returns the runtime (in seconds) of a given song from the csv file, or an
    empty string if the song is not found. *)

val tempo_of_song : t -> string -> string
(** Returns the tempo (in BPM) of a given song from the csv file, or an empty
    string if the song is not found. *)

val key_of_song : t -> string -> string
(** Returns the key of a given song from the csv file, or an empty string if the
    song is not found. *)

val composer_of_song : t -> string -> string
(* Returns the composer of the classical song from the given csv file.*)

val releaseyear_of_song : t -> string -> string
(* Returns the release year of the classical song from the given csv file.
   Returns an empty string if the song is not found.*)

val classicalgenre_of_song : t -> string -> string
(* Returns the genre of the classical song from the given csv file. Returns an
   empty string if the song is not found.*)

val instrumentation_of_song : t -> string -> string
(* Returns the instrumentation of the classical song from the given csv file.
   Returns an empty string if the song is not found.*)

val song_by_classgenre : t -> string -> string option
(** Returns a string of a song of the inputted mood. Returns None if the genre
    is not found in the csv file. *)
