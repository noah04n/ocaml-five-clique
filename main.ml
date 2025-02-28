let load_words () : string list =
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
        Printf.printf "%d words loaded\n" (List.length words); words

let print_words (words : string list) : unit =
        let rec read words =
                match words with
                | [] -> Printf.printf "Done\n" ; ()
                | x :: xs -> Printf.printf "%s\n" x ; read xs
        in
        read words


let is_n_letter (word : string) (n : int) : bool = 
        (String.length word) = n


(* make tail recursive later *)
let rec extract_n_letter_words (words : string list) (n : int) (p : string -> bool) : string list =
        match words with
        | [] -> []
        | x :: xs -> if p x then x :: extract_n_letter_words xs n p else extract_n_letter_words xs n p
