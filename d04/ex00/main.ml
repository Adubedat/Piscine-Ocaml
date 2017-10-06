(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 18:10:36 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/06 18:23:24 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec print_list lst =
    match lst with
        | head :: tail when head = Color.Spade -> print_endline (Color.toString head); print_list tail
        | head :: tail when head = Color.Heart -> print_endline (Color.toString head); print_list tail
        | head :: tail when head = Color.Diamond -> print_endline (Color.toString head); print_list tail
        | head :: tail when head = Color.Club -> print_endline (Color.toString head); print_list tail
        | [] | _ -> ()

let rec print_list_verbose lst =
    match lst with
        | head :: tail when head = Color.Spade -> print_endline (Color.toStringVerbose head); print_list_verbose tail
        | head :: tail when head = Color.Heart -> print_endline (Color.toStringVerbose head); print_list_verbose tail
        | head :: tail when head = Color.Diamond -> print_endline (Color.toStringVerbose head); print_list_verbose tail
        | head :: tail when head = Color.Club -> print_endline (Color.toStringVerbose head); print_list_verbose tail
        | [] | _ -> ()

let () =
    print_list Color.all;
    print_list_verbose Color.all
