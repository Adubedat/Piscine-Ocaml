(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sum.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/09 22:15:09 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/09 22:20:03 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let sum x y = x +. y

let () =
    print_float (sum 0.0 0.0); print_char '\n';
    print_float (sum 1.875 (-15.0245)); print_char '\n';
    print_float (sum 14.0 0.78563202); print_char '\n';
    print_float (sum (-45.120) 0.0); print_char '\n';
    print_float (sum (-2.0) (-0.12546)); print_char '\n';
    print_float (sum 1.0 1.0); print_char '\n'
