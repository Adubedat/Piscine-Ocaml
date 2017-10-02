(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/02 15:51:53 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/02 16:17:54 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb () =
    let rec loop_comb a b c =
        if a <> b && a <> c && b <> c && a < b && b < c then
            begin
                print_int a ; print_int b ; print_int c;
                if a <> 7 then
                    print_string ", "
            end;
        if c < 9 then
            loop_comb a b (c + 1)
        else if b < 9 then
            loop_comb a (b + 1) 0
        else if a < 9 then
            loop_comb (a + 1) 0 0
    in
    loop_comb 0 0 0;
    print_string "\n"

let main () =
    ft_print_comb()

let () = main ()
