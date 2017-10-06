(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 18:10:36 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/06 18:42:25 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec print_list lst =
    match lst with
        | head :: tail -> print_endline (Color.toString head); print_list tail
        | [] -> ()

let rec print_list_verbose lst =
    match lst with
        | head :: tail -> print_endline (Color.toStringVerbose head); print_list_verbose tail
        | [] -> ()

let () =
    print_list Color.all;
    print_list_verbose Color.all
