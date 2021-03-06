(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_test_sign.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/02 14:27:35 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/02 14:52:03 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_test_sign x =
    if x >= 0 then
        print_endline "positive"
    else
        print_endline "negative"

let main () =
    ft_test_sign 214748467;
    ft_test_sign 0;
    ft_test_sign (-429987987987987)

let () = main ()
