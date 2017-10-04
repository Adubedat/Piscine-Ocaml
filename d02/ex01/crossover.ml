(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   crossover.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/04 17:19:26 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/04 18:07:35 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let crossover lst1 lst2 =
    let rec loop l1 l2 final_list =
        match l1 with
            | [] -> final_list
            | h1 :: t1 -> (
                match l2 with
                    | h2 :: t2 when h1 = h2 -> loop t1 l2 (final_list @ [h1])
                    | h2 :: t2 -> loop l1 t2 final_list
                    | [] -> loop t1 lst2 final_list
            )
    in
    loop lst1 lst2 []

let rec print_int_list lst = match lst with
        | head :: tail -> print_string ( string_of_int head ^ " -> ");
                          print_int_list tail
        | [] -> print_string "[] \n"

let rec print_char_list lst = match lst with
        | head :: tail -> print_string ( (String.make 1 head) ^ " -> ");
                          print_char_list tail
        | [] -> print_string "[] \n"

let rec print_string_list lst = match lst with
        | head :: tail -> print_string ( head ^ " -> ");
                          print_string_list tail
        | [] -> print_string "[] \n"

let () =
    print_string_list (crossover [] ["aaa"; "lol2"; "bbb"; "ccc"; "ddd"]);
    print_string_list (crossover ["ddd"; "lol"; "sss"; "lol2"] []);
    print_char_list (crossover ['a'; 'b'; 'c'; 'd'; '9'] ['t'; 'e'; 'd'; 'a'; '9']);
    print_int_list (crossover [974; 4; 56; 76; 0] [45; 34; 654; 974; 0; 3; 4]);
    print_string_list (crossover ["ddd"; "lol"; "sss"; "lol2"; ""] ["aaa"; "lol2"; "bbb"; "ccc"; "ddd"; ""]);


                    

