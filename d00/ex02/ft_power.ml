(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_power.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/02 15:13:38 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/02 15:32:52 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_power x pow =
    if pow = 0 then
        1
    else
        x * (ft_power x (pow - 1))

let main () =
    print_int (ft_power 1 0) ; print_char '\n';
    print_int (ft_power 0 1) ; print_char '\n';
    print_int (ft_power 2 5) ; print_char '\n'

let () = main ()
