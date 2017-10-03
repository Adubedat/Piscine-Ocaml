(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_is_palindrome.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/02 18:30:24 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/02 18:47:42 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)
let is_equal a b =
    a = b

let ft_is_palindrome str =
    let len = String.length str in
    let rec loop str a b =
        if a >= b then
            true
        else if is_equal (String.get str a) (String.get str b) then
            loop str (a + 1) (b - 1)
        else
            false
    in
    loop str 0 (len - 1)

let main () =
    print_endline (string_of_bool(ft_is_palindrome "974"));
    print_endline (string_of_bool(ft_is_palindrome "65456"));
    print_endline (string_of_bool(ft_is_palindrome "a"));
    print_endline (string_of_bool(ft_is_palindrome "aa"));
    print_endline (string_of_bool(ft_is_palindrome "aaa"));
    print_endline (string_of_bool(ft_is_palindrome "radar"));
    print_endline (string_of_bool(ft_is_palindrome "madam"));
    print_endline (string_of_bool(ft_is_palindrome "car"));
    print_endline (string_of_bool(ft_is_palindrome ""))

let () = main ()
