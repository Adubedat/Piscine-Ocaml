(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/03 10:21:11 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/03 10:33:37 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_x n =
    if n < 0 then "Error"
    let rec x_loop str n =
        if n > 0 then
            x_loop (str ^ "x") (n -1)
        else
            str
    in
    x_loop "" n

let () =
    print_endline (repeat_x 5);
    print_endline (repeat_x 0);
    print_endline (repeat_x (-3))
