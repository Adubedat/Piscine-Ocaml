(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_alphabet.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/02 15:36:02 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/02 15:46:34 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_alphabet () =
    let ascii_of_z = int_of_char 'z' in
    let rec alpha_loop current_letter =
        if current_letter <= ascii_of_z then
            begin
                print_char (char_of_int current_letter);
                alpha_loop (current_letter + 1)
            end
    in
    alpha_loop (int_of_char 'a');
    print_char '\n'

let main () =
    ft_print_alphabet()

let () = main ()
