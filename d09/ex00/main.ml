(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/14 15:54:35 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/14 16:15:18 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
    let hour5:Watchtower.Watchtower.hour = 5 in
    let hour9:Watchtower.Watchtower.hour = 9 in
    let new_hour1 = Watchtower.Watchtower.add hour5 hour9 in
    let new_hour2 = Watchtower.Watchtower.sub hour5 hour9 in
    let new_hour3 = Watchtower.Watchtower.sub hour9 hour5 in
    print_endline ("5 + 9 = " ^ (string_of_int new_hour1));
    print_endline ("5 - 9 = " ^ (string_of_int new_hour2));
    print_endline ("9 - 5 = " ^ (string_of_int new_hour3))

