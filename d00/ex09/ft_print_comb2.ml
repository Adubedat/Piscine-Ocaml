(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb2.ml                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/02 20:02:24 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/02 20:31:29 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb2 () =
    let rec loop_comb a b =
        if a <> b && a < b then
            begin
                    if a <= 9 then
                        begin print_int 0 ; print_int a; end
                    else
                        print_int a; 
                    print_char ' ';
                    if b <= 9 then
                        begin print_int 0 ; print_int b; end
                    else
                        print_int b;
                    if a <> 98 then
                        begin print_char ',' ; print_char ' '; end
            end;
        if b < 99 then
            loop_comb a (b + 1)
        else if a < 99 then
            loop_comb (a + 1) 0
    in
    loop_comb 0 0;
    print_char '\n'

let main () =
    ft_print_comb2 ()

let () = main ()
