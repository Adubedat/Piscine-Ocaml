(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/04 14:24:58 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/04 17:15:25 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let encode lst =
    let rec loop n lst final_list = match lst with
        | [] -> []
        | head1 :: tail1 -> (
            match tail1 with
            | head2 :: tail2 when head1 = head2 -> loop (n + 1) tail1 final_list
            | head2 :: tail2 -> loop 1 tail1 (final_list @ [(n, head1)])
            | [] -> final_list @ [(n, head1)]
        )
    in
    loop 1 lst []

let string_of_int_tuple tup  =
    let (x, y) = tup in
    let str = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")" in
    str

let string_of_char_tuple tup  =
    let (x, y) = tup in
    let str = "(" ^ string_of_int x ^ ", " ^ (String.make 1 y) ^ ")" in
    str

let string_of_string_tuple tup  =
    let (x, y) = tup in
    let str = "(" ^ string_of_int x ^ ", " ^ y ^ ")" in
    str

let rec print_char_tuple_list lst = match lst with
        | head :: tail -> print_string (string_of_char_tuple head ^ " -> ");
                          print_char_tuple_list tail
        | [] -> print_string "[] \n"

let rec print_int_tuple_list lst = match lst with
        | head :: tail -> print_string (string_of_int_tuple head ^ " -> ");
                          print_int_tuple_list tail
        | [] -> print_string "[] \n"

let rec print_string_tuple_list lst = match lst with
        | head :: tail -> print_string (string_of_string_tuple head ^ " -> ");
                          print_string_tuple_list tail
        | [] -> print_string "[] \n"

let () =
    print_int_tuple_list (encode [1; 1; 1; 2; 3]);
    print_string_tuple_list (encode ["lol"; "lol"; "test"; "bwah"; "bwah"; "bwah"]);
    print_char_tuple_list (encode ['a'; 'a'; 'g'; 'c'; 'c'; 'c'; 'z'; '5']);
    print_char_tuple_list (encode [])
    
