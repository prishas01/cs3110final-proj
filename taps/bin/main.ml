open Printf
open Lib
open Command
open Functions
open Csv

(** Returns a string with all the possible Commands available for a given the
    input string "help".*)
let print_help () =
  "\n\
   List of Commands for music.csv:\n\
   'help': list of all commands\n\
   'exit' : exits the program\n\
   'nsongs' : number of songs in file\n\
   'nartists' : number of artists in file\n\
   'allinfo' : view all the categories of info we have\n\n\
   List of Commands for ourmusic.csv:\n\
   'songalbum': get a random song from the specified album\n\
   'songartist': get a random song from the specified artist\n\
   'songgenre': get a random song from the specified genre\n\
   'songkey': get a random song from the specified key\n\
   'songyear': get a random song from the specified year\n\
   'songruntime': get a random song from the specified runtime (in seconds)\n\
   'songtempo': get a random song from the specified tempo (in BPM)\n\
   'getallartists': get a list of all artists\n\
   'getallgenres': get a list of all genres\n\
   'getallsongs': get a list of all genres\n\
   'genreofsong': get the genre of a specified song\n\
   'artistofsong': get the artist of a specified song\n\
   'albumofsong': get the album of a specified song\n\
   'yearofsong': get the release year of a specified song\n\
   'runtimeofsong': get the runtime (in seconds) of a specified song\n\
   'tempoofsong': get the tempo (in BPM) of a specified song\n\
   'keyofsong': get the key of a specified song\n\n\
   List of Commands for classical.csv:\n\
   'songcomposer': get the composer of a specified song\n\
   'releaseyear': get the year of a specified song\n\
   'classicalgenre': get the classical genre of a specified song\n\
   'instrumentation': get the instrumentation of a specified song\n\
   'pickasong': let us pick a song based on your mood\n"

(** Outputs the value that the user asks for (string list, string options, or
    int). Matches the inputted command with the function needed to perform the
    request.*)
let rec comm_helper str data var_ls : unit =
  print_endline "\nPlease enter a command. Enter 'help' for assistance.";
  print_string ">>> ";
  try
    let inp = read_line () |> parsing_s in
    match inp with
    | Help ->
        print_string (print_help ());
        comm_helper str data var_ls
    | Exit ->
        print_string "Okay, Goodbye!\n";
        print_endline "\nHere are some of our top picks:";
        let rec gen_unique_nums set count =
          if count = 0 then set
          else
            let random_num = Random.int 52 in
            if List.mem random_num set then gen_unique_nums set count
            else gen_unique_nums (random_num :: set) (count - 1)
        in
        let print_song song_num =
          match song_num with
          | 0 -> print_endline "\"Chosen (feat. Ty Dolla $ign)\" -Blxst & Tyga"
          | 1 -> print_endline "\"3:15\" -Bazzi"
          | 2 -> print_endline "\"Drew Barrymore\" -Bryce Vine"
          | 3 -> print_endline "\"willow\" -Taylor Swift"
          | 4 -> print_endline "\"Let's Fall in Love for the Night\" -FINNEAS"
          | 5 -> print_endline "\"This Love\" -Maroon 5"
          | 6 -> print_endline "\"Paris\" -Taylor Swift"
          | 7 -> print_endline "\"I Hate U\" -SZA"
          | 8 -> print_endline "\"Lady of Namek\" -Tory Lanez"
          | 9 -> print_endline "\"Woman\" -Doja Cat"
          | 10 -> print_endline "\"The Hills\" -The Weeknd"
          | 11 -> print_endline "\"Falling Behind\" -Laufey"
          | 12 -> print_endline "\"Passionfruit\" -Drake"
          | 13 ->
              print_endline
                "\"TWIST & TURN (feat. Drake & PARTYNEXTDOOR)\" -Popcaan"
          | 14 -> print_endline "\"Blem\" -Drake"
          | 15 -> print_endline "\"The Light\" -Juice WRLD"
          | 16 -> print_endline "\"White Iverson\" -Post Malone"
          | 17 -> print_endline "\"LOVE. (FEAT. Zacari)\" -Kendrick Lamar"
          | 18 -> print_endline "\"Ballin'\" -Mustard & Roddy Ricch"
          | 19 -> print_endline "\"do u even miss me at all?\" -gianni & kyle"
          | 20 -> print_endline "\"Escapism.\" -RAYE & 070 Shake"
          | 21 -> print_endline "\"Saturday Nights\" -Khalid"
          | 22 -> print_endline "\"Midnight Rain\" -Taylor Swift"
          | 23 -> print_endline "\"golden hour\" -JVKE"
          | 24 -> print_endline "\"C'est La Vie\" -Yung Gravy"
          | 25 -> print_endline "\"Moonlight\" -Kali Uchis"
          | 26 -> print_endline "\"motive\" -Ariana Grande & Doja Cat"
          | 27 -> print_endline "\"ivy\" -Taylor Swift"
          | 28 -> print_endline "\"lie to me\" -Tate McRae & Ali Gatie"
          | 29 ->
              print_endline
                "\"I'll Have to Say I Love You In a Song\" -Jim Croce"
          | 30 -> print_endline "\"Happy Together\" -The Turtles"
          | 31 -> print_endline "\"Begin Again\" -Taylor Swift"
          | 32 -> print_endline "\"If I Ain't Got You\" -Alicia Keys"
          | 33 -> print_endline "\"Fear of Heights\" -Drake"
          | 34 -> print_endline "\"Best Mistake\" -Ariana Grande"
          | 35 -> print_endline "\"King of My Heart\" -Taylor Swift"
          | 36 -> print_endline "\"Mambo No. 5\" -Lou Bega"
          | 37 -> print_endline "\"Mrs.Worldwide\" -Yung Gravy"
          | 38 -> print_endline "\"Line Without a Hook\" -Ricky Montgomery"
          | 39 -> print_endline "\"traitor\" -Olivia Rodrigo"
          | 40 -> print_endline "\"Another Year\" -FINNEAS"
          | 41 -> print_endline "\"On the Low\" -jaash"
          | 42 -> print_endline "\"Hotel\" -Claire Rosinkranz"
          | 43 -> print_endline "\"Billie Bossa Nova\" -Billie Eilish"
          | 44 -> print_endline "\"This Guy's In Love With You\" -Frankie Valli"
          | 45 -> print_endline "\"Why\" -Bazzi"
          | 46 -> print_endline "\"Eye of the Tiger\" -Survivor"
          | 47 -> print_endline "\"Piano Man\" - Billy Joel"
          | 48 -> print_endline "\"Panini\" - Lil Nas X"
          | 49 -> print_endline "\"Lost Cause\" - Billie Eilish"
          | 50 -> print_endline "\"Story of My Life\" - One Direction"
          | 51 -> print_endline "\"Pray For Me\" -The Weeknd, Kendrick Lamar"
          | 52 -> print_endline "\"Sparks Fly\" -Taylor Swift"
          | _ -> print_endline "\"Drops of Jupiter (Tell Me)\" -Train"
        in
        let () = Random.self_init () in
        let top_picks = gen_unique_nums [] 5 in
        List.iter print_song top_picks;
        exit 0
    | Nsongs ->
        data |> num_of_songs |> string_of_int |> print_endline;
        comm_helper str data var_ls
    | Nartists ->
        data |> num_of_artists |> string_of_int |> print_endline;
        comm_helper str data var_ls
    | Allinfo ->
        print_endline (string_lt (all_info data));
        comm_helper str data var_ls
    | SongByAlbum ->
        print_endline "\nPlease enter an album.";
        print_string ">>> ";
        let input = read_line () in
        (match song_by_album data input with
        | Some song -> print_endline ("Random Song from Album: " ^ song)
        | None ->
            print_endline
              "No such album found. Please try something like: \"Thriller\"");
        comm_helper str data var_ls
    | SongByArtist ->
        print_endline "\nPlease enter an artist.";
        print_string ">>> ";
        let input = read_line () in
        (match song_by_artist data input with
        | Some song -> print_endline ("Random Song from Artist: " ^ song)
        | None ->
            print_endline
              "No such artist found. Please try something like: \"Harry \
               Styles\"");
        comm_helper str data var_ls
    | SongByGenre ->
        print_endline "\nPlease enter a genre.";
        print_string ">>> ";
        let input = read_line () in
        (match song_by_genre data input with
        | Some song -> print_endline ("Random Song from Genre: " ^ song)
        | None ->
            print_endline
              "No such genre found. Please try something like: \"Hip-Hop\"");
        comm_helper str data var_ls
    | SongByKey ->
        print_endline "\nPlease enter a key.";
        print_string ">>> ";
        let input = read_line () in
        (match song_by_key data input with
        | Some song -> print_endline ("Random Song from Key: " ^ song)
        | None ->
            print_endline "No such key found. Please try something like: \"C#\"");
        comm_helper str data var_ls
    | SongByYear ->
        print_endline "\nPlease enter a year.";
        print_string ">>> ";
        let input = read_line () in
        (match song_by_year data input with
        | Some song -> print_endline ("Random Song from Year: " ^ song)
        | None ->
            print_endline
              "No such year found. Please try something like: \"2018\"");
        comm_helper str data var_ls
    | SongByRuntime ->
        print_endline "\nPlease enter a runtime.";
        print_string ">>> ";
        let input = read_line () in
        (match song_by_runtime data input with
        | Some song -> print_endline ("Random Song from Runtime: " ^ song)
        | None ->
            print_endline
              "No such runtime found. Please try something like: \"216\"");
        comm_helper str data var_ls
    | SongByTempo ->
        print_endline "\nPlease enter a tempo.";
        print_string ">>> ";
        let input = read_line () in
        (match song_by_tempo data input with
        | Some song -> print_endline ("Random Song from Tempo: " ^ song)
        | None ->
            print_endline
              "No such tempo found. Please try something like: \"130\"");
        comm_helper str data var_ls
    | GetAllArtists ->
        print_endline
          ("All Artists: " ^ String.concat ", " (get_all_artists data));
        comm_helper str data var_ls
    | GetAllGenres ->
        print_endline ("All Genres: " ^ String.concat ", " (get_all_genres data));
        comm_helper str data var_ls
    | GetAllSongs ->
        print_endline ("All Songs: " ^ String.concat ", " (get_all_songs data));
        comm_helper str data var_ls
    | GenreOfSong ->
        print_endline "\nPlease enter a song.";
        print_string ">>> ";
        let input = read_line () in
        (match genre_of_song data input with
        | "" ->
            print_endline
              "No such song found. Please enter a song from \"getallsongs\""
        | genre -> print_endline ("Genre of the Song: " ^ genre));
        comm_helper str data var_ls
    | ArtistOfSong ->
        print_endline "\nPlease enter a song.";
        print_string ">>> ";
        let input = read_line () in
        (match artist_of_song data input with
        | "" ->
            print_endline
              "No such song found. Please enter a song from \"getallsongs\""
        | artist -> print_endline ("Artist of the Song: " ^ artist));
        comm_helper str data var_ls
    | AlbumOfSong ->
        print_endline "\nPlease enter a song.";
        print_string ">>> ";
        let input = read_line () in
        (match album_of_song data input with
        | "" ->
            print_endline
              "No such song found. Please enter a song from \"getallsongs\""
        | album -> print_endline ("Album of the Song: " ^ album));
        comm_helper str data var_ls
    | YearOfSong ->
        print_endline "\nPlease enter a song.";
        print_string ">>> ";
        let input = read_line () in
        (match year_of_song data input with
        | "" ->
            print_endline
              "No such song found. Please enter a song from \"getallsongs\""
        | year -> print_endline ("Release Year of the Song: " ^ year));
        comm_helper str data var_ls
    | RuntimeOfSong ->
        print_endline "\nPlease enter a song.";
        print_string ">>> ";
        let input = read_line () in
        (match runtime_of_song data input with
        | "" ->
            print_endline
              "No such song found. Please enter a song from \"getallsongs\""
        | runtime ->
            print_endline ("Runtime (in seconds) of the Song: " ^ runtime));
        comm_helper str data var_ls
    | TempoOfSong ->
        print_endline "\nPlease enter a song.";
        print_string ">>> ";
        let input = read_line () in
        (match tempo_of_song data input with
        | "" ->
            print_endline
              "No such song found. Please enter a song from \"getallsongs\""
        | tempo -> print_endline ("Tempo (in BPM) of the Song: " ^ tempo));
        comm_helper str data var_ls
    | KeyOfSong ->
        print_endline "\nPlease enter a song.";
        print_string ">>> ";
        let input = read_line () in
        (match key_of_song data input with
        | "" ->
            print_endline
              "No such song found. Please enter a song from \"getallsongs\""
        | key -> print_endline ("Key of the Song: " ^ key));
        comm_helper str data var_ls
    | CompOfSong ->
        print_endline "\nPlease enter a song.";
        print_string ">>> ";
        let input = read_line () in
        (match composer_of_song data input with
        | "" ->
            print_endline
              "No such song found. Please enter a song from \"getallsongs\""
        | comp -> print_endline ("Composer of the Song: " ^ comp));
        comm_helper str data var_ls
    | ReleaseOfSong ->
        print_endline "\nPlease enter a song.";
        print_string ">>> ";
        let input = read_line () in
        (match releaseyear_of_song data input with
        | "" ->
            print_endline
              "No such song found. Please enter a song from \"getallsongs\""
        | year -> print_endline ("Release year of the Song: " ^ year));
        comm_helper str data var_ls
    | ClassicalGenreOfSong ->
        print_endline "\nPlease enter a song.";
        print_string ">>> ";
        let input = read_line () in
        (match classicalgenre_of_song data input with
        | "" ->
            print_endline
              "No such song found. Please enter a song from \"getallsongs\""
        | genre -> print_endline ("Classical genre of the Song: " ^ genre));
        comm_helper str data var_ls
    | InstrumentationOfSong ->
        print_endline "\nPlease enter a song.";
        print_string ">>> ";
        let input = read_line () in
        (match instrumentation_of_song data input with
        | "" ->
            print_endline
              "No such song found. Please enter a song from \"getallsongs\""
        | inst -> print_endline ("Instrumentation of the Song: " ^ inst));
        comm_helper str data var_ls
    | SongbyMood ->
        print_endline
          "\n\
           Please enter a mood. Your choices are: romantic, fancy, modern, \
           classical";
        print_string ">>> ";
        let input = read_line () in
        (match song_by_classgenre data input with
        | Some song -> print_endline ("Your song: " ^ song)
        | None -> print_endline "No such mood found. Please try something else");
        comm_helper str data var_ls
    | _ ->
        print_string "Invalid. Exiting!\n";
        exit 0
  with Command.Invalid_Command s ->
    print_endline (s ^ " Type \"help\" for list of commands");
    comm_helper str data var_ls

let csv_import csv_str =
  List.map (fun name -> (name, Csv.load name)) [ csv_str ]

(** Returns values based on the user's inputs. This is the main function that
    initially interacts with the users.*)
let rec main () =
  print_string "\n\nWelcome to Our Song Database!\n";
  print_endline
    "We have three files you can choose from. \nDo you want to continue?\n";
  print_string "(yes/no): ";
  match read_line () with
  | "yes" -> (
      print_string "\n\nWhat file do you want to use?\n";
      print_string "(music.csv or ourmusic.csv or classical.csv): ";
      match read_line () with
      | "music.csv" ->
          print_string
            "\n\
             You have chosen the file: music.csv.\n\n\
             List of Commands for music.csv:\n\
             'help': list of all commands\n\
             'exit' : exits the program\n\
             'nsongs' : number of songs in file\n\
             'nartists' : number of artists in file\n\
             'allinfo' : view all the categories of info we have\n";
          comm_helper "lib/music.csv" (read_csv "lib/music.csv")
            [ ("lib/music.csv", gen_data_ls (read_csv "lib/music.csv")) ]
      | "ourmusic.csv" ->
          print_string
            "\n\
             You have chosen the file: ourmusic.csv.\n\n\
             List of Commands for ourmusic.csv:\n\
             'help': list of all commands\n\
             'exit' : exits the program\n\
             'songalbum': get a random song from the specified album\n\
             'songartist': get a random song from the specified artist\n\
             'songgenre': get a random song from the specified genre\n\
             'songkey': get a random song from the specified key\n\
             'songyear': get a random song from the specified year\n\
             'songruntime': get a random song from the specified runtime\n\
             'songtempo': get a random song from the specified tempo\n\
             'getallartists': get a list of all artists\n\
             'getallgenres': get a list of all genres\n\
             'getallsongs': get a list of all genres\n\
             'genreofsong': get the genre of a specified song\n\
             'artistofsong': get the artist of a specified song\n\
             'albumofsong': get the album of a specified song\n\
             'yearofsong': get the release year of a specified song\n\
             'runtimeofsong': get the runtime (in seconds) of a specified song\n\
             'tempoofsong': get the tempo (in BPM) of a specified song\n\
             'keyofsong': get the key of a specified song\n";

          comm_helper "lib/ourmusic.csv"
            (read_csv "lib/ourmusic.csv")
            [ ("lib/ourmusic.csv", gen_data_ls (read_csv "lib/ourmusic.csv")) ]
      | "classical.csv" ->
          print_string
            "\n\
             You have chosen the file: classical.csv.\n\n\
             List of Commands for classical.csv:\n\
             'help': list of all commands\n\
             'exit' : exits the program\n\
             'songcomposer': get the composer of a specified song\n\
             'releaseyear': get the year of a specified song\n\
             'classicalgenre': get the classical genre of a specified song\n\
             'instrumentation': get the instrumentation of a specified song\n\
             'getallartists': get a list of all artists\n\
             'getallsongs': get a list of all genres\n\
             'pickasong': let us pick a song based on your mood\n";

          comm_helper "lib/classical.csv"
            (read_csv "lib/classical.csv")
            [
              ("lib/classical.csv", gen_data_ls (read_csv "lib/classical.csv"));
            ]
      | _ ->
          print_string "Your input was invalid. Goodbye!\n";
          print_endline "\nHere are some of our top picks:";
          let rec gen_unique_nums set count =
            if count = 0 then set
            else
              let random_num = Random.int 52 in
              if List.mem random_num set then gen_unique_nums set count
              else gen_unique_nums (random_num :: set) (count - 1)
          in
          let print_song song_num =
            match song_num with
            | 0 -> print_endline "\"Shivers\" -Ed Sheeran"
            | 1 -> print_endline "\"I.F.L.Y\" -Bazzi"
            | 2 -> print_endline "\"Lights Down Low (feat. gnash)\" -MAX"
            | 3 -> print_endline "\"I Did Something Bad\" -Taylor Swift"
            | 4 -> print_endline "\"Mona Lisa, Mona Lisa\" -FINNEAS"
            | 5 -> print_endline "\"Killing Me Softly With His Song\" -Fugees"
            | 6 -> print_endline "\"Mastermind\" -Taylor Swift"
            | 7 -> print_endline "\"Snooze\" -SZA"
            | 8 -> print_endline "\"The Color Violet\" -Tory Lanez"
            | 9 -> print_endline "\"You Right\" -Doja Cat"
            | 10 -> print_endline "\"Often\" -The Weeknd"
            | 11 -> print_endline "\"Nonsense\" -Sabrina Carpenter"
            | 12 -> print_endline "\"moonlight\" -dhruv"
            | 13 -> print_endline "\"Moonlight in Atlanta\" -Russ"
            | 14 -> print_endline "\"AMAZING\" -Rex Orange County"
            | 15 -> print_endline "\"CUFF IT\" -Beyonce"
            | 16 -> print_endline "\"Chemical\" -Post Malone"
            | 17 -> print_endline "\"Trap Queen\" -Fetty Wap"
            | 18 ->
                print_endline
                  "\"See You Again (feat. Kali Uchis)\" -Tyler, The Creator"
            | 19 -> print_endline "\"One Dance\" -Drake"
            | 20 -> print_endline "\"3 Nights\" -Dominic Fike"
            | 21 -> print_endline "\"Bye Bye Bye\" -*NSYNC"
            | 22 -> print_endline "\"The Less I Know The Better\" -Tame Impala"
            | 23 -> print_endline "\"Classic\" -MKTO"
            | 24 -> print_endline "\"Circles\" -Post Malone"
            | 25 -> print_endline "\"Sky\" -Playboi Carti"
            | 26 -> print_endline "\"Dior\" -Pop Smoke"
            | 27 -> print_endline "\"Slow Motion (feat. Wizkid)\" -Don Toliver"
            | 28 -> print_endline "\"Cabo\" -Bankrol Hayden"
            | 29 -> print_endline "\"Look Back at It\" -A Boogie wit da Hoodie"
            | 30 -> print_endline "\"Lean Wit Me\" -Juice WRLD"
            | 31 -> print_endline "\"you broke me first\" -Tate McRae"
            | 32 -> print_endline "\"Wonderland\" -Taylor Swift"
            | 33 -> print_endline "\"She Will Be Loved\" -Maroon 5"
            | 34 -> print_endline "\"Stop\" -Anthony Ramos"
            | 35 -> print_endline "\"my future\" -Billie Eilish"
            | 36 -> print_endline "\"More Than A Woman\" -Bee Gees"
            | 37 -> print_endline "\"Walk It Talk It (feat. Drake)\" -Migos"
            | 38 -> print_endline "\"How Can I Forget\" -MKTO"
            | 39 -> print_endline "\"Never Gonna Give You Up\" -Rick Astley"
            | 40 -> print_endline "\"Honesty\" -Pink Sweat$"
            | 41 ->
                print_endline
                  "\"It's Been a Long, Long Time\" -Harry James and His \
                   Orchestra"
            | 42 -> print_endline "\"R.E.M.\" -Ariana Grande"
            | 43 -> print_endline "\"Play That Song\" -Train"
            | 44 -> print_endline "\"Sofia\" -Clario"
            | 45 -> print_endline "\"Move On\" -AJ Mitchell"
            | 46 -> print_endline "\"Sunflower\" -Rex Orange County"
            | 47 -> print_endline "\"Since U Been Gone\" -Kelly Clarkson"
            | 48 -> print_endline "\"Go Crazy\" -Chris Brown & Young Thug"
            | 49 -> print_endline "\"Hello\" -Lionel Richie"
            | 50 -> print_endline "\"Intentions (feat. Quavo)\" -Justin Bieber"
            | 51 -> print_endline "\"I'm Yours\" -Jason Mraz"
            | 52 -> print_endline "\"Somebody That I Used To Know\" -Gotye"
            | _ -> print_endline "\"Homage\" -Mild High Club"
          in
          let () = Random.self_init () in
          let top_picks = gen_unique_nums [] 5 in
          List.iter print_song top_picks;
          exit 0)
  | "no" ->
      print_string "Okay! Goodbye!\n";
      print_endline "\nHere are some of our top picks:";
      let rec gen_unique_nums set count =
        if count = 0 then set
        else
          let random_num = Random.int 52 in
          if List.mem random_num set then gen_unique_nums set count
          else gen_unique_nums (random_num :: set) (count - 1)
      in
      let print_song song_num =
        match song_num with
        | 0 -> print_endline "\"Vienna\" -Billy Joel"
        | 1 -> print_endline "\"Back To You\" -Selena Gomez"
        | 2 -> print_endline "\"Late At Night\" -Roddy Ricch"
        | 3 -> print_endline "\"After Party\" -Don Toliver"
        | 4 -> print_endline "\"Under The Influence\" -Chris Brown"
        | 5 -> print_endline "\"Night Changes\" -One Direction"
        | 6 -> print_endline "\"Karma\" -Taylor Swift"
        | 7 -> print_endline "\"Kill Bill\" -SZA"
        | 8 -> print_endline "\"In Your Eyes\" -The Weeknd"
        | 9 -> print_endline "\"I Don't Care\" -Ed Sheeran"
        | 10 -> print_endline "\"How Long\" -Charlie Puth"
        | 11 -> print_endline "\"Before He Cheats\" -Carrie Underwood"
        | 12 -> print_endline "\"Should've Said It\" -Camila Cabello"
        | 13 -> print_endline "\"Sucker for You\" -Matt Terry"
        | 14 -> print_endline "\"BBL Love (Interlude)\" -Drake"
        | 15 -> print_endline "\"Bahamas Promises\" -Drake"
        | 16 -> print_endline "\"At My Worst (feat. Kehlani)\" -Pink Sweat$"
        | 17 -> print_endline "\"There She Goes\" -The La's"
        | 18 -> print_endline "\"Therefore I Am\" -Billie Eilish"
        | 19 -> print_endline "\"Watermelon Sugar\" -Harry Styles"
        | 20 -> print_endline "\"Sugar\" -Maroon 5"
        | 21 ->
            print_endline
              "\"December, 1963 (Oh, What a Night)\" -The Four Seasons"
        | 22 -> print_endline "\"Begin Again\" -Taylor Swift"
        | 23 -> print_endline "\"Glimpse of Us\" -Joji"
        | 24 -> print_endline "\"Calling\" -Metro Boomin, Swae Lee & NAV"
        | 25 -> print_endline "\"Love on the Brain\" -Rihanna"
        | 26 -> print_endline "\"Flowers\" -Miley Cyrus"
        | 27 -> print_endline "\"The Motion\" -Drake"
        | 28 -> print_endline "\"STILL CHOSE YOU\" -The Kid LAROI"
        | 29 -> print_endline "\"Lost in Japan\" -Shawn Mendes"
        | 30 -> print_endline "\"this is what autumn feels like\" -JVKE"
        | 31 -> print_endline "\"Set Fire to the Rain\" -Adele"
        | 32 -> print_endline "\"Eastside\" -benny blanco, Halsey & Khalid"
        | 33 -> print_endline "\"prom dress\" -mxmtoon"
        | 34 -> print_endline "\"Back to You\" -Louis Tomlinson"
        | 35 -> print_endline "\"Don't Start Now\" -Dua Lipa"
        | 36 -> print_endline "\"I Forgot That You Existed\" -Taylor Swift"
        | 37 -> print_endline "\"Sugar\" -Maroon 5"
        | 38 -> print_endline "\"8\" -Billie Eilish"
        | 39 -> print_endline "\"Love the Way You Lie\" -Eminem"
        | 40 -> print_endline "\"Tearin' Up My Heart\" -*NSYNC"
        | 41 -> print_endline "\"Puppy Love\" -Paul Anka"
        | 42 -> print_endline "\"Marvins Room\" -Drake"
        | 43 -> print_endline "\"Dangerous Woman\" -Ariana Grande"
        | 44 -> print_endline "\"Sherry\" -The Four Seasons"
        | 45 -> print_endline "\"Heart on Ice\" -Rod Wave"
        | 46 -> print_endline "\"Treat You Better\" -Shawn Mendes"
        | 47 -> print_endline "\"Ghost in the Machine\" -SZA"
        | 48 -> print_endline "\"Hotline Bling\" -Drake"
        | 49 -> print_endline "\"Better\" -ZAYN"
        | 50 -> print_endline "\"Cake By the Ocean\" -DNCE"
        | 51 -> print_endline "\"ilomilo\" -Billie Eilish"
        | 52 ->
            print_endline "\"Swervin (feat. 6ix9ine)\" -A Boogie wit da Hoodie"
        | _ -> print_endline "\"Strange\" -Celeste"
      in
      let () = Random.self_init () in
      let top_picks = gen_unique_nums [] 5 in
      List.iter print_song top_picks;
      exit 0
  | _ ->
      print_string "Your input was invalid. Goodbye!\n";
      print_endline "\nHere are some of our top picks:";
      let rec gen_unique_nums set count =
        if count = 0 then set
        else
          let random_num = Random.int 52 in
          if List.mem random_num set then gen_unique_nums set count
          else gen_unique_nums (random_num :: set) (count - 1)
      in
      let print_song song_num =
        match song_num with
        | 0 -> print_endline "\"Finesse (Remix) [feat. Cardi B]\" -Bruno Mars"
        | 1 -> print_endline "\"Easily\" -Bruno Major"
        | 2 -> print_endline "\"In My Feelings\" -Drake"
        | 3 -> print_endline "\"Payphone\" -Maroon 5"
        | 4 ->
            print_endline
              "\"break up with your girlfriend, i'm bored\" -Ariana Grande"
        | 5 -> print_endline "\"Same Old Love\" -Selena Gomez"
        | 6 -> print_endline "\"Sweet Nothing\" -Taylor Swift"
        | 7 -> print_endline "\"Open Arms (feat. Travis Scott)\" -SZA"
        | 8 -> print_endline "\"Oui\" -Jeremih"
        | 9 -> print_endline "\"Out of Time\" -The Weeknd"
        | 10 -> print_endline "\"Personal\" -HRVY"
        | 11 -> print_endline "\"Photograph\" -Ed Sheeran"
        | 12 -> print_endline "\"Put Your Head On My Shoulder\" -Camila Cabello"
        | 13 ->
            print_endline "\"Raindrops (Insane)\" -Metro Boomin & Travis Scott"
        | 14 -> print_endline "\"Right Back\" -Khalid"
        | 15 -> print_endline "\"Righteous\" -Juice WRLD"
        | 16 -> print_endline "\"Rude\" -MAGIC!"
        | 17 -> print_endline "\"SAD!\" -XXXTENTACION"
        | 18 -> print_endline "\"Say It\" -Tory Lanez"
        | 19 -> print_endline "\"Until I Found You\" -Stephan Sanchez"
        | 20 -> print_endline "\"Valentino\" -24kGoldn"
        | 21 -> print_endline "\"Wake Me Up Before You Go Go\" -Wham!"
        | 22 -> print_endline "\"The Way I Loved You\" -Taylor Swift"
        | 23 -> print_endline "\"The Way You Look Tonight\" -Frank Sinatra"
        | 24 -> print_endline "\"Welcome Back (feat. Alessia Cara)\" -Ali Gatie"
        | 25 -> print_endline "\"Umbrella\" -Rihanna"
        | 26 -> print_endline "\"When I Was Your Man\" -Bruno Mars"
        | 27 -> print_endline "\"when the party's over\" -Billie Eilish"
        | 28 -> print_endline "\"When We Were Young\" -Adele"
        | 29 -> print_endline "\"When You're Smiling\" -Micheal Buble"
        | 30 -> print_endline "\"Wishful Thinking\" -BENEE"
        | 31 -> print_endline "\"Count on Me\" -Bruno Mars"
        | 32 -> print_endline "\"No Guidance (feat. Drake)\" -Chris Brown"
        | 33 -> print_endline "\"Sh-Boom\" -The Chords"
        | 34 ->
            print_endline
              "\"Boogie Wonderland\" -Earth, Wind & Fire, The Emotions"
        | 35 -> print_endline "\"Love Lies\" -Khalid & Normani"
        | 36 -> print_endline "\"In the Ghetto\" -Elvis Presley"
        | 37 -> print_endline "\"How It Ends\" -FINNEAS"
        | 38 -> print_endline "\"Skiing in Japan Freestyle\" -Yung Gravy"
        | 39 -> print_endline "\"Regent's Park\" -Bruno Major"
        | 40 -> print_endline "\"Renegades\" -X Ambassadors"
        | 41 ->
            print_endline
              "\"Baby I'm Yours\" -Arctice Monkeys & The Newell Octet"
        | 42 -> print_endline "\"Missin You Crazy\" -Russ"
        | 43 -> print_endline "\"Corduroy Dreams\" -Rex Orange County"
        | 44 -> print_endline "\"Here Comes The Sun\" -The Beatles"
        | 45 ->
            print_endline
              "\"We Don't Talk Anymore (feat. Selena Gomez)\" -Charlie Puth"
        | 46 -> print_endline "\"Le Festin\" -Michael Giacchino & Camille"
        | 47 -> print_endline "\"Breathe\" -Years & Years"
        | 48 -> print_endline "\"Trust Fund Baby\" -Why Don't We"
        | 49 -> print_endline "\"Memories\" -Maroon 5"
        | 50 ->
            print_endline "\"Rollin (feat. Future & Khalid)\" -Calvin Harris"
        | 51 -> print_endline "\"As It Was\" -Harry Styles"
        | 52 -> print_endline "\"Marlboro Nights\" -Lonely God"
        | _ -> print_endline "\"Cherry Wine\" -grentperez"
      in
      let () = Random.self_init () in
      let top_picks = gen_unique_nums [] 5 in
      List.iter print_song top_picks;
      exit 0

let () = main ()
