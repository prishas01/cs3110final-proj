(* In this test file, we test each of our functions to make sure that they are
   working as expected. We decided that it would be best to write our functions,
   and ensure that our “make play” is working properly, and then write our test
   cases. If our functions were not correct, then we would go back, fix the
   issue, and return to the test file to write more test cases. We repeated this
   process until we were certain that our functions were implemented correctly.
   To come up with test cases, we decided to use mainly glass box testing since
   we knew what each of the functions included. We decided that this would be
   the best way to ensure that we cover every branch and line of code for each
   function. Additionally, we wrote edge case test cases. For example, for
   strings, we manipulated the cases to test if the function was case-sensitive
   or not, and tested empty/nonempty strings that we knew would return None. We
   tried to have, on average, three tests per case to account for these
   possibilities. All functions were tested with OUnit2 EXCEPT for some
   functions in the main file (print_help, comm_helper, print_song, and
   csv_import, gen_unique_nums, top_picks, read_csv, read_row, gen_data_ls, and
   string_lt) which were all manually tested. These functions were either helper
   functions or functions that we used because the outputs were predictable. We
   knew what the outputs of the functions should be and how each of them should
   behave when called in other functions, so we thought it would be redundant to
   test these if we already knew they were working. For all the functions that
   we used the OUnit tests for, we knew this was the best approach and would
   make the most sense since it would be more efficient to test out different
   cases. Since the OUnit tests do everything for us, we were quickly able to
   write test cases and see which ones, if any, were failing. The organized
   structure of the testing suite also made it easy for us to quickly see which
   test cases we did and did not cover. *)

open OUnit2
open Lib
open Command
open Functions

exception Invalid_Command of string

let dat1 = read_csv "lib/music.csv"
let dat2 = read_csv "lib/ourmusic.csv"
let dat3 = read_csv "lib/classical.csv"

let num_of_songs_tests (expected_output : string) (nsongs : string) : test =
  "num_of_songs_tests" >:: fun _ -> assert_equal expected_output nsongs

let num_of_artists_tests (expected_output : string) (nartists : string) : test =
  "num_of_artists_tests" >:: fun _ -> assert_equal expected_output nartists

let read_row_tests (expected_output : string) (row : string) : test =
  "read_row_tests" >:: fun _ -> assert_equal expected_output row

let all_info_tests (expected_output : string) (allinfo : string) : test =
  "all_info_tests" >:: fun _ -> assert_equal expected_output allinfo

let gen_data_ls_tests (expected_output : string) (gen_data : string) : test =
  "gen_data_ls_tests" >:: fun _ -> assert_equal expected_output gen_data

let string_lt_tests (expected_output : string) (string_lt : string) : test =
  "string_lt_tests" >:: fun _ -> assert_equal expected_output string_lt

let song_by_album_tests (expected_output : string option)
    (some_song : string option) : test =
  "song_by_album_tests" >:: fun _ -> assert_equal expected_output some_song

let helper (album : string) =
  match song_by_album dat2 album with
  | Some x -> x
  | _ -> failwith "Wrong call"

let many_songs_by_album_tests (songs_list : string list) (album : string) : test
    =
  "many_song_by_album_tests" >:: fun _ ->
  assert_equal true (List.mem (helper album) songs_list)

let song_by_artist_tests (expected_output : string option)
    (some_song : string option) : test =
  "song_by_artist_tests" >:: fun _ -> assert_equal expected_output some_song

let helper (artist : string) =
  match song_by_artist dat2 artist with
  | Some x -> x
  | _ -> failwith "Wrong call"

let many_songs_by_artist_tests (songs_list : string list) (artist : string) :
    test =
  "many_song_by_artist_tests" >:: fun _ ->
  assert_equal true (List.mem (helper artist) songs_list)

let song_by_genre_tests (expected_output : string option)
    (some_song : string option) : test =
  "song_by_genre_tests" >:: fun _ -> assert_equal expected_output some_song

let helper (genre : string) =
  match song_by_genre dat2 genre with
  | Some x -> x
  | _ -> failwith "Wrong call"

let many_songs_by_genre_tests (songs_list : string list) (genre : string) : test
    =
  "many_song_by_genre_tests" >:: fun _ ->
  assert_equal true (List.mem (helper genre) songs_list)

let helper2 (genre : string) =
  match song_by_classgenre dat3 genre with
  | Some x -> x
  | _ -> failwith "Wrong call"

let sameclassgenre_tests (songs_list : string list) (genre : string) : test =
  "sameclassgenre_tests" >:: fun _ ->
  assert_equal true (List.mem (helper2 genre) songs_list)

let song_by_key_tests (expected_output : string option)
    (some_song : string option) : test =
  "song_by_key_tests" >:: fun _ -> assert_equal expected_output some_song

let helper (key : string) =
  match song_by_key dat2 key with
  | Some x -> x
  | _ -> failwith "Wrong call"

let many_songs_by_key_tests (songs_list : string list) (key : string) : test =
  "many_song_by_key_tests" >:: fun _ ->
  assert_equal true (List.mem (helper key) songs_list)

let song_by_year_tests (expected_output : string option)
    (some_song : string option) : test =
  "song_by_year_tests" >:: fun _ -> assert_equal expected_output some_song

let helper (year : string) =
  match song_by_year dat2 year with
  | Some x -> x
  | _ -> failwith "Wrong call"

let many_songs_by_year_tests (songs_list : string list) (year : string) : test =
  "many_song_by_year_tests" >:: fun _ ->
  assert_equal true (List.mem (helper year) songs_list)

let song_by_runtime_tests (expected_output : string option)
    (some_song : string option) : test =
  "song_by_runtime_tests" >:: fun _ -> assert_equal expected_output some_song

let helper (runtime : string) =
  match song_by_runtime dat2 runtime with
  | Some x -> x
  | _ -> failwith "Wrong call"

let many_songs_by_runtime_tests (songs_list : string list) (runtime : string) :
    test =
  "many_song_by_runtime_tests" >:: fun _ ->
  assert_equal true (List.mem (helper runtime) songs_list)

let song_by_tempo_tests (expected_output : string option)
    (some_song : string option) : test =
  "song_by_tempo_tests" >:: fun _ -> assert_equal expected_output some_song

let helper (tempo : string) =
  match song_by_tempo dat2 tempo with
  | Some x -> x
  | _ -> failwith "Wrong call"

let many_songs_by_tempo_tests (songs_list : string list) (tempo : string) : test
    =
  "many_song_by_tempo_tests" >:: fun _ ->
  assert_equal true (List.mem (helper tempo) songs_list)

let list_to_string (lst : string list) : string = String.concat ", " lst

let get_all_artists_tests (expected_output : string) (all_artists : string) :
    test =
  "get_all_artists_tests" >:: fun _ -> assert_equal expected_output all_artists

let get_all_genres_tests (expected_output : string) (all_genres : string) : test
    =
  "get_all_genres_tests" >:: fun _ -> assert_equal expected_output all_genres

let get_all_songs_tests (expected_output : string) (all_songs : string) : test =
  "get_all_songs_tests" >:: fun _ -> assert_equal expected_output all_songs

let genre_of_song_tests (expected_output : string) (genre : string) : test =
  "genre_of_song_tests" >:: fun _ -> assert_equal expected_output genre

let artist_of_song_tests (expected_output : string) (artist : string) : test =
  "artist_of_song_tests" >:: fun _ -> assert_equal expected_output artist

let album_of_song_tests (expected_output : string) (album : string) : test =
  "album_of_song_tests" >:: fun _ -> assert_equal expected_output album

let year_of_song_tests (expected_output : string) (year : string) : test =
  "year_of_song_tests" >:: fun _ -> assert_equal expected_output year

let runtime_of_song_tests (expected_output : string) (runtime : string) : test =
  "runtime_of_song_tests" >:: fun _ -> assert_equal expected_output runtime

let tempo_of_song_tests (expected_output : string) (tempo : string) : test =
  "tempo_of_song_tests" >:: fun _ -> assert_equal expected_output tempo

let key_of_song_tests (expected_output : string) (key : string) : test =
  "key_of_song_tests" >:: fun _ -> assert_equal expected_output key

let composer_of_song_tests (expected_output : string) (artist : string) : test =
  "composer_of_song_tests" >:: fun _ -> assert_equal expected_output artist

let instrumentation_of_song_tests (expected_output : string) (artist : string) :
    test =
  "instrumentation_of_song_tests" >:: fun _ ->
  assert_equal expected_output artist

let main_tests =
  [
    (* Num of Song Tests *)
    num_of_songs_tests "10001" (string_of_int (num_of_songs dat1));
    num_of_songs_tests "119" (string_of_int (num_of_songs dat2));
    (* Num of Artists Tests *)
    num_of_artists_tests "10001" (string_of_int (num_of_artists dat1));
    num_of_artists_tests "119" (string_of_int (num_of_artists dat2));
    (* Genre of Song Tests *)
    genre_of_song_tests "Hip-Hop" (genre_of_song dat2 "WAP");
    genre_of_song_tests "R&B" (genre_of_song dat2 "cAn't feel my face");
    genre_of_song_tests "" (genre_of_song dat2 "Savage");
    genre_of_song_tests "" (genre_of_song dat2 "daylight");
    genre_of_song_tests "Country Hip-Hop" (genre_of_song dat2 "Old Town Road");
    genre_of_song_tests "Pop" (genre_of_song dat2 "Watermelon Sugar");
    genre_of_song_tests "Pop" (genre_of_song dat2 "Firework");
    (* Artist of Song Tests *)
    artist_of_song_tests "Meghan Trainor"
      (artist_of_song dat2 "all about that bass");
    artist_of_song_tests "Taylor Swift" (artist_of_song dat2 "Shake It Off");
    artist_of_song_tests "" (artist_of_song dat2 "happy birthday");
    artist_of_song_tests "" (artist_of_song dat2 "Beat IT");
    artist_of_song_tests "Travis Scott" (artist_of_song dat2 "Sicko Mode");
    artist_of_song_tests "Jawsh 685 x Jason Derulo"
      (artist_of_song dat2 "Savage Love");
    artist_of_song_tests "Avicii" (artist_of_song dat2 "Wake Me Up");
    (* Album of Song Tests *)
    album_of_song_tests "WAP" (album_of_song dat2 "WAP");
    album_of_song_tests "Beauty Behind the Madness"
      (album_of_song dat2 "cAn't feel my face");
    album_of_song_tests "" (album_of_song dat2 "Savage");
    album_of_song_tests "" (album_of_song dat2 "daylight");
    album_of_song_tests "7 EP" (album_of_song dat2 "Old Town Road");
    album_of_song_tests "Fine Line" (album_of_song dat2 "Watermelon Sugar");
    album_of_song_tests "Teenage Dream" (album_of_song dat2 "Firework");
    (* Year of Song Tests *)
    year_of_song_tests "2014" (year_of_song dat2 "all about that bass");
    year_of_song_tests "2014" (year_of_song dat2 "Shake It Off");
    year_of_song_tests "" (year_of_song dat2 "happy birthday");
    year_of_song_tests "" (year_of_song dat2 "Beat IT");
    year_of_song_tests "2018" (year_of_song dat2 "Sicko Mode");
    year_of_song_tests "2020" (year_of_song dat2 "Savage Love");
    year_of_song_tests "2013" (year_of_song dat2 "Wake Me Up");
    (* Runtime of Song Tests *)
    runtime_of_song_tests "187" (runtime_of_song dat2 "WAP");
    runtime_of_song_tests "213" (runtime_of_song dat2 "cAn't feel my face");
    runtime_of_song_tests "" (runtime_of_song dat2 "Savage");
    runtime_of_song_tests "" (runtime_of_song dat2 "daylight");
    runtime_of_song_tests "174" (runtime_of_song dat2 "Watermelon Sugar");
    runtime_of_song_tests "312" (runtime_of_song dat2 "Sicko Mode");
    runtime_of_song_tests "246" (runtime_of_song dat2 "Wake Me Up");
    (* Tempo of Song Tests *)
    tempo_of_song_tests "134" (tempo_of_song dat2 "all about that bass");
    tempo_of_song_tests "160" (tempo_of_song dat2 "Shake It Off");
    tempo_of_song_tests "" (tempo_of_song dat2 "happy birthday");
    tempo_of_song_tests "" (tempo_of_song dat2 "Beat IT");
    tempo_of_song_tests "89" (tempo_of_song dat2 "Despacito");
    tempo_of_song_tests "150" (tempo_of_song dat2 "Savage Love");
    tempo_of_song_tests "124" (tempo_of_song dat2 "Wake Me Up");
    (* Key of Song Tests *)
    key_of_song_tests "A" (key_of_song dat2 "WAP");
    key_of_song_tests "Gm" (key_of_song dat2 "cAn't feel my face");
    key_of_song_tests "" (key_of_song dat2 "Savage");
    key_of_song_tests "" (key_of_song dat2 "daylight");
    key_of_song_tests "C#" (key_of_song dat2 "Sicko Mode");
    key_of_song_tests "Bm" (key_of_song dat2 "Radioactive");
    key_of_song_tests "B" (key_of_song dat2 "Old Town Road");
    (* Song by Artist Tests *)
    song_by_artist_tests (Some "Bang Bang") (song_by_artist dat2 "Jessie J");
    many_songs_by_artist_tests
      [ "Blinding Lights"; "The Hills"; "Starboy"; "Can't Feel My Face" ]
      "The Weeknd";
    many_songs_by_artist_tests
      [ "Blinding Lights"; "The Hills"; "Starboy"; "Can't Feel My Face" ]
      "thE weeknd";
    many_songs_by_artist_tests
      [ "Thunder"; "Radioactive"; "Believer" ]
      "IMAGINE DRAGONS";
    song_by_artist_tests None (song_by_artist dat2 "SZA");
    (* Song by Genre Tests *)
    song_by_genre_tests (Some "Take On Me") (song_by_genre dat2 "synth-pop");
    song_by_genre_tests None (song_by_genre dat2 "Reggae");
    many_songs_by_genre_tests
      [
        "Blinding Lights";
        "The Hills";
        "Starboy";
        "I Will Always Love You";
        "Can't Feel My Face";
        "Love on Top";
      ]
      "r&b";
    many_songs_by_genre_tests
      [ "Old Town Road"; "Need You Now"; "Dirt Road Anthem" ]
      "COUNTRY";
    song_by_genre_tests None (song_by_genre dat2 "folk");
    (* Song by Key Tests *)
    song_by_key_tests (Some "Green Light") (song_by_key dat2 "Bbm");
    song_by_key_tests (Some "Sorry") (song_by_key dat2 "G#m");
    many_songs_by_key_tests
      [ "Wonderwall"; "Truth Be Told"; "Dance Monkey" ]
      "F#";
    song_by_key_tests None (song_by_key dat2 "B#");
    many_songs_by_key_tests [ "Hello"; "Bang Bang"; "Happy" ] "Fm";
    (* Song by Year Tests *)
    song_by_year_tests (Some "Hotel California") (song_by_year dat2 "1976");
    song_by_year_tests None (song_by_year dat2 "1960");
    many_songs_by_year_tests
      [
        "Rolling in the Deep";
        "Never Say Never";
        "Firework";
        "Black and Yellow";
        "Memories";
        "Dirt Road Anthem";
      ]
      "2010";
    song_by_year_tests None (song_by_year dat2 "1989");
    song_by_year_tests (Some "I Will Always Love You")
      (song_by_year dat2 "1992");
    (* Song by Runtime Tests *)
    many_songs_by_runtime_tests [ "Rolling in the Deep"; "Despacito" ] "228";
    song_by_runtime_tests (Some "Green Light") (song_by_runtime dat2 "234");
    song_by_runtime_tests None (song_by_runtime dat2 "180");
    many_songs_by_runtime_tests [ "Take On Me"; "Havana"; "Applause" ] "217";
    (* Song by Tempo Tests *)
    many_songs_by_tempo_tests
      [ "Roses"; "Happier"; "Love Yourself"; "Attention"; "Sorry" ]
      "100";
    song_by_tempo_tests (Some "Levitating") (song_by_tempo dat2 "103");
    song_by_tempo_tests None (song_by_tempo dat2 "198");
    many_songs_by_tempo_tests [ "Someone Like You"; "Old Town Road" ] "67";
    (* Song by Album Tests *)
    many_songs_by_album_tests [ "Shake It Off"; "Bad Blood" ] "1989";
    song_by_album_tests (Some "The Man") (song_by_album dat2 "Lover");
    song_by_album_tests (Some "Shallow")
      (song_by_album dat2 "A Star Is Born Soundtrack");
    song_by_album_tests (Some "The Sound of Silence")
      (song_by_album dat2 "sounds Of silence");
    song_by_album_tests (Some "The Sound of Silence")
      (song_by_album dat2 "Sounds of Silence");
    song_by_album_tests None (song_by_album dat2 "For All the Dogs");
    song_by_album_tests (Some "Praying") (song_by_album dat2 "Rainbow");
    (* Get All Artists test*)
    get_all_artists_tests
      "Michael Jackson, Eagles, Simon & Garfunkel, a-ha, The Police, Guns N' \
       Roses, Dire Straits, Weezer, Jason Mraz, Oasis, Lorde, Jason Aldean, \
       Lady A, Carly Rae Jepsen, Rihanna ft. Drake, Coldplay, Queen, AC/DC, \
       Dua Lipa, David Guetta ft. Kid Cudi, Taylor Swift, Owl City, Kesha, Wiz \
       Khalifa, Beyoncé, Sia, Bruno Mars, Justin Bieber ft. Jaden Smith, \
       Rachel Platten, Idina Menzel, Taylor Swift ft. Kendrick Lamar, Whitney \
       Houston, Marshmello, Matthew West, Charlie Puth, Doja Cat, Vance Joy, \
       The Black Eyed Peas, BTS, Ariana Grande, Justin Bieber, Maroon 5, The \
       Weeknd ft. Daft Punk, Meghan Trainor, Maroon 5 ft. Cardi B, DNCE, \
       Marshmello ft. Bastille, Shawn Mendes, Justin Timberlake ft. Chris \
       Stapleton, Saint Jhn, Avicii, WALK THE MOON, Dean Martin, Miley Cyrus, \
       Drake, OneRepublic, Post Malone ft. 21 Savage, Jessie J, Ellie \
       Goulding, Magic!, Ella Henderson, Lizzo, Katy Perry ft. Juicy J, Panic! \
       At The Disco, Camila Cabello ft. Young Thug, The Chainsmokers, Lady \
       Gaga & Bradley Cooper, Hozier, Jonas Brothers, Cardi B ft. Megan Thee \
       Stallion, Tones and I, Adele, Post Malone & Swae Lee, Twenty One \
       Pilots, Macklemore & Ryan Lewis ft. Ray Dalton, Jawsh 685 x Jason \
       Derulo, Lady Gaga, Imagine Dragons, Katy Perry, Post Malone, Wiz \
       Khalifa ft. Charlie Puth, Harry Styles, Justin Timberlake, Pharrell \
       Williams, Billie Eilish, Travis Scott, Lewis Capaldi, Luis Fonsi ft. \
       Daddy Yankee, Mark Ronson ft. Bruno Mars, Lil Nas X, The Weeknd, Ed \
       Sheeran"
      (list_to_string (get_all_artists dat2));
    (* Get All Genres Test*)
    get_all_genres_tests
      "Folk Rock, Synth-pop, Country, Rock, Electronic, Christian, Indie Folk, \
       K-pop, Jazz, Reggae Fusion, Pop Rock, EDM, Soul, Alternative, Indie \
       Rock, Hip-Hop, Reggaeton, Funk, Country Hip-Hop, R&B, Pop"
      (list_to_string (get_all_genres dat2));
    (* Get All Songs Test*)
    get_all_songs_tests
      "Thriller, Hotel California, The Sound of Silence, Take On Me, Sweet \
       Child o' Mine, Every Breath You Take, November Rain, Sultans of Swing, \
       Bohemian Rhapsody, Say It Ain't So, I'm Yours, Where Is the Love?, \
       Wonderwall, Thunder, Green Light, Applause, Dirt Road Anthem, Rumor Has \
       It, Need You Now, Call Me Maybe, Work, Viva la Vida, Shake It Off, \
       Radio Ga Ga, Praying, Highway to Hell, Levitating, Juice, The Man, \
       Fireflies, Die Young, Black and Yellow, Love on Top, Someone Like You, \
       Chandelier, Can't Feel My Face, Locked Out of Heaven, Hello, Firework, \
       Wake Me Up, Mercy, Never Say Never, Fight Song, Let It Go, Sorry, Bad \
       Blood, I Will Always Love You, Alone, Truth Be Told, Attention, Say So, \
       Riptide, I Gotta Feeling, Perfect, Stitches, Dynamite, Rain On Me, 7 \
       Rings, Love Yourself, Memories, Starboy, All About That Bass, Girls \
       Like You, Cake by the Ocean, Happier, Senorita, Say Something, Roses, \
       Hey Brother, Shut Up and Dance, Party in the U.S.A., Believer, Sway, \
       Wrecking Ball, Hotline Bling, The Hills, Photograph, Counting Stars, \
       Rockstar, Bang Bang, Love Me Like You Do, Rude, Ghost, Truth Hurts, \
       Dark Horse, High Hopes, Havana, Sick Boy, Shallow, Take Me to Church, \
       Sucker, WAP, Dance Monkey, Rolling in the Deep, Sunflower, Stressed \
       Out, Can't Hold Us, Savage Love, Bad Romance, Radioactive, Roar, \
       Circles, See You Again, Watermelon Sugar, Can't Stop the Feeling!, \
       Happy, Bad Guy, Sicko Mode, Someone You Loved, Despacito, Uptown Funk, \
       Old Town Road, Blinding Lights, Shape of You"
      (list_to_string (get_all_songs dat2));
    (*classical csv tests*)

    (*composer year tests*)
    composer_of_song_tests "Pachelbel" (composer_of_song dat3 "Canon in D");
    composer_of_song_tests "" (composer_of_song dat3 "Love Story");
    composer_of_song_tests "Beethoven"
      (composer_of_song dat3 "Moonlight Sonata");
    composer_of_song_tests "Tchaikovsky" (composer_of_song dat3 "Swan Lake");
    composer_of_song_tests "Holst"
      (composer_of_song dat3 "The Planets - Jupiter");
    (*release year tests*)
    year_of_song_tests "1680" (releaseyear_of_song dat3 "Canon in D");
    year_of_song_tests "" (releaseyear_of_song dat3 "WAP");
    year_of_song_tests "1801" (releaseyear_of_song dat3 "Moonlight Sonata");
    year_of_song_tests "1876" (releaseyear_of_song dat3 "Swan Lake");
    year_of_song_tests "1916" (releaseyear_of_song dat3 "The Planets - Jupiter");
    (*genre tests*)
    genre_of_song_tests "Baroque" (classicalgenre_of_song dat3 "Canon in D");
    genre_of_song_tests "" (classicalgenre_of_song dat3 "Sicko Mode");
    genre_of_song_tests "Classical"
      (classicalgenre_of_song dat3 "Eine kleine Nachtmusik");
    genre_of_song_tests "Romantic"
      (classicalgenre_of_song dat3 "Symphony No. 9 in D minor");
    genre_of_song_tests "Impressionist"
      (classicalgenre_of_song dat3 "Clair de Lune");
    (*instrumentation tests*)
    instrumentation_of_song_tests "Strings"
      (instrumentation_of_song dat3 "Canon in D");
    instrumentation_of_song_tests ""
      (instrumentation_of_song dat3 "Ric Flair Drip");
    instrumentation_of_song_tests "Piano"
      (instrumentation_of_song dat3 "Moonlight Sonata");
    instrumentation_of_song_tests "Orchestra"
      (instrumentation_of_song dat3 "Swan Lake");
    instrumentation_of_song_tests "Orchestra"
      (instrumentation_of_song dat3 "Symphony No. 9 in D minor");
    (*pickasong tests*)
    sameclassgenre_tests
      [
        "Symphony No. 9 in D minor";
        "Swan Lake";
        "Ride of the Valkyries";
        "The Nutcracker Suite";
        "Carmen Suite No. 1";
        "Ave Maria";
        "Peer Gynt Suite No. 1";
        "Violin Concerto in D major";
        "Pavane";
        "Symphony No. 2 in D major";
        "La Traviata";
        "Pomp and Circumstance Elgar";
        "Symphony No. 8 in B minor";
        "Requiem";
        "Symphony No. 4 in E minor";
        "Concerto for Violin and Orchestra";
        "A Midsummer Night's Dream";
        "The Moldau";
        "Gymnopédie No. 1";
        "Erlkönig";
        "Symphony No. 6 in F major";
        "Danse Macabre";
        "Symphony No. 1 in C major";
        "Symphony No. 1 in D major";
        "Peer Gynt Suite No. 2";
        "La Campanella";
        "Symphony No. 2 in C minor";
        "Symphony No. 4 in G major";
        "Symphony No. 3 in F major";
        "Adagietto from Symphony No. 5";
        "Piano Concerto No. 2 in C minor";
        "Symphony No. 3 \"Scottish\"";
        "Carmen";
        "Symphony No. 9 \"From the New World\"";
        "Symphony No. 5 in C sharp minor";
        "Die Walküre";
        "Violin Concerto in D major";
        "Pictures at an Exhibition";
        "Scheherazade";
        "Symphony No. 6 \"Pathétique\"";
        "Aida";
        "Symphony No. 7 in E major";
        "Peer Gynt Suite No. 3";
        "Serenade for Strings in C major";
        "Piano Concerto No. 1";
        "La Bohème";
      ]
      "Romantic";
    sameclassgenre_tests
      [
        "Canon in D";
        "Four Seasons - Spring";
        "Brandenburg Concerto No. 3";
        "Concerto in D minor for Two Violins";
        "Water Music Suite No. 1 in F major";
        "Cello Suite No. 1";
        "Toccata and Fugue in D minor";
        "Mass in B minor";
        "Concerto for Two Violins in D minor";
        "Cello Suite No. 6 in D major";
        "The Four Seasons - Winter";
        "Toccata and Fugue in D minor";
        "Adagio in G minor";
        "Concerto for Flute";
        "Messiah";
      ]
      "fancy";
    sameclassgenre_tests
      [
        "The Planets - Jupiter";
        "Adagio for Strings";
        "Appalachian Spring";
        "The Firebird Suite";
        "Rhapsody in Blue,Gershwin";
        "Cello Concerto in E minor";
        "Agnus Dei";
        "String Quartet No. 8 in C minor";
        "Fantasia on a Theme by Thomas Tallis";
        "Rhapsody on a Theme of Paganini";
        "Symphony No. 1 \"Classical\"";
        "The Rite of Spring";
        "Violin Concerto No. 1 in D major";
      ]
      "modern";
    sameclassgenre_tests
      [
        "Moonlight Sonata";
        "Eine kleine Nachtmusik";
        "Symphony No. 40 in G minor";
        "Symphony No. 5 in C minor";
        "Ode to Joy";
        "Symphony No. 6 in F major";
        "Symphony No. 3 in E-flat major";
        "Symphony No. 41 \"Jupiter\"";
        "Piano Sonata No. 14 \"Moonlight\"";
        "Symphony No. 7 in A major";
        "Requiem";
        "Symphony No. 94 \"Surprise\"";
        "Missa Solemnis";
        "Concerto for Flute";
        "Piano Concerto No. 21 \"Elvira Madigan\"";
        "Symphony No. 5 in B-flat major";
        "The Marriage of Figaro";
        "The Magic Flute";
        "Symphony No. 7 in A major";
        "Piano Sonata No. 8 \"Pathétique\"";
        "Piano Sonata No. 17 \"Tempest\"";
        "The Barber of Seville";
        "Concerto for Clarinet and Orchestra";
        "Piano Concerto No. 23";
        "Piano Sonata No. 11 \"Alla Turca\"";
      ]
      "classical";
  ]

let suite = "test suite for Song Database" >::: List.flatten [ main_tests ]
let _ = run_test_tt_main suite
