(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   army.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/12 22:03:04 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/12 22:39:38 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class ['a] army (lst: 'a list) =
object

    val mutable member = lst

    method add (elem: 'a) = member <- member @ [elem]

    method delete =
        try member <- List.tl member with
            | _ -> ()

    method print_army =
        let rec loop lst =
            match lst with
            | head::tail -> print_endline head#to_string; loop tail
            | [] -> ()
        in loop member

end
