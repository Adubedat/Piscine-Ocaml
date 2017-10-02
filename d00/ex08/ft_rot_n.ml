(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_rot_n.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/02 18:52:48 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/02 19:58:30 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let is_alphamin c =
    c >= 'a' && c <= 'z'

let is_alphamaj c =
    c >= 'A' && c <= 'Z'

let ft_rot_n n str =
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

let main () =
    print_string ((ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz")^"\n");
    print_string ((ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz")^"\n");
    print_string ((ft_rot_n 42 "0123456789")^"\n");
    print_string ((ft_rot_n 2 "OI2EAS67B9")^"\n");
    print_string ((ft_rot_n 0 "Damned !")^"\n");
    print_string ((ft_rot_n 42 "")^"\n");
    print_string ((ft_rot_n 1 "NBzlk qnbjr !")^"\n")


let () = main ()
