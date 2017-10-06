(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   cipher.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 15:30:07 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/06 18:02:22 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let is_alphamin c =
    c >= 'a' && c <= 'z'

let is_alphamaj c =
    c >= 'A' && c <= 'Z'

let rot42 str =
    let rot c =
        let c = int_of_char c in
        let amin = int_of_char 'a' in
        let amaj = int_of_char 'A' in
        if is_alphamin (char_of_int c) then
            char_of_int (amin + (c - amin + 42) mod 26)
        else if is_alphamaj (char_of_int c) then
            char_of_int (amaj + (c - amaj + 42) mod 26)
        else
            char_of_int c
    in
    String.map rot str

let ceasar n str =
    let rot c =
        let c = int_of_char c in
        let amin = int_of_char 'a' in
        let amaj = int_of_char 'A' in
        if is_alphamin (char_of_int c) then
            char_of_int (amin + (c - amin + n) mod 26)
        else if is_alphamaj (char_of_int c) then
            char_of_int (amaj + (c - amaj + n) mod 26)
        else
            char_of_int c
    in
    String.map rot str

let xor key str =
	let xor_key c =
		char_of_int ((int_of_char c) lxor key)
	in
	if key < 0 then str
	else String.map xor_key str

let ft_crypt str f_list =
    let rec loop lst str =
        match lst with
            | head :: tail -> loop tail (head str)
            | [] -> str
    in
    loop f_list str

let () =
    let str1 = rot42 "abcdefGHIJKLMNOPqrsTUVWXYZ" in
    print_endline str1;
	print_endline (Uncipher.unrot42 str1);
    let str2 = ceasar 20 "abcdefGHIJKLMNOPqrsTUVWXYZ" in
    print_endline str2;
    print_endline (Uncipher.unceasar 20 str2);
    let str = xor 4 "test" in
    print_endline str;
    print_endline (xor 4 str);
    let str3 = ft_crypt "test" [rot42; ceasar 8; xor 12] in
    print_endline str3;
    print_endline (Uncipher.ft_uncrypt str3 [xor 12; Uncipher.unceasar 8; Uncipher.unrot42])
