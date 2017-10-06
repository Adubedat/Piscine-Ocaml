(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   uncipher.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 16:46:29 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/06 17:57:08 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let is_alphamin c =
    c >= 'a' && c <= 'z'

let is_alphamaj c =
    c >= 'A' && c <= 'Z'

let unrot42 str =
    let rot c =
        let c = int_of_char c in
        let amin = int_of_char 'a' in
        let amaj = int_of_char 'A' in
        if is_alphamin (char_of_int c) then
            char_of_int (amin + 25 + (c - amin - 41) mod (-26))
        else if is_alphamaj (char_of_int c) then
            char_of_int (amaj + 25 + (c - amaj - 41) mod (-26))
        else
            char_of_int c
    in
    String.map rot str

let unceasar n str =
    let rot c =
        let c = int_of_char c in
        let amin = int_of_char 'a' in
        let amaj = int_of_char 'A' in
        if is_alphamin (char_of_int c) then
            let x = (c - amin - (n - 1)) in
            char_of_int (amin + 25 + (if x > 0 then (x mod 26) - 26 else x mod (-26)))
        else if is_alphamaj (char_of_int c) then
            let x = (c - amaj - (n - 1)) in
            char_of_int (amaj + 25 + (if x > 0 then (x mod 26) - 26 else x mod (-26)))
        else
            char_of_int c
    in
    String.map rot str

let ft_uncrypt str f_list =
    let rec loop lst str =
        match lst with
            | head :: tail -> loop tail (head str)
            | [] -> str
    in
    loop f_list str

let cool_to_str str =
    str ^ "cool"

let double_str str =
    str ^ str
