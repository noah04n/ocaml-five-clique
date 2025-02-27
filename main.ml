let load_words () =
        let words_txt = "./words_alpha.txt" in
        let ic = open_in words_txt in

        let rec read_words acc =
                match input_line ic with
                | line -> read_words (line :: acc)
                | exception End_of_file ->
                                close_in ic;
                                List.rev acc
        in
        let words = read_words [] in
        Printf.printf "%d words loaded\n" (List.length words);
