(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_string_all.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/02 17:21:49 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/02 18:32:44 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let is_digit c =
    c >= '0' && c <= '9'

let ft_string_all (is_digit: char -> bool) str =
    let len = String.length str in
    let rec loop str n =
	if len = 0 then false
        else if n = len then
            true
        else if is_digit (String.get str n) then
            loop str (n + 1)
        else
            false
    in
    loop str 0

let main () =
    print_endline (string_of_bool(ft_string_all is_digit ""));
    print_endline (string_of_bool(ft_string_all is_digit "0123456789"));
    print_endline (string_of_bool(ft_string_all is_digit "0123456789B"));
    print_endline (string_of_bool(ft_string_all is_digit "A0123456789"));
    print_endline (string_of_bool(ft_string_all is_digit "01234*56789"))

let () = main ()
