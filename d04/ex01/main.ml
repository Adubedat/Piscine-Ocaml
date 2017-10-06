(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 18:38:05 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/06 18:51:56 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec print_list lst =
    match lst with
        | head :: tail -> print_endline (Value.toString head); print_list tail
        | [] -> ()

let rec print_list_verbose lst =
    match lst with
        | head :: tail -> print_endline (Value.toStringVerbose head); print_list_verbose tail
        | [] -> ()

let rec print_list_toint lst =
    match lst with
        | head :: tail -> print_int (Value.toInt head); print_char '\n'; print_list_toint tail
        | [] -> ()

let rec print_list_next lst =
    match lst with
        | head :: tail -> print_endline (Value.toStringVerbose (Value.next head)); print_list_next tail
        | [] -> ()

let rec print_list_previous lst =
    match lst with
        | head :: tail -> print_endline (Value.toStringVerbose (Value.previous head)); print_list_previous tail
        | [] -> ()

let () =
    print_endline "to string : \n";
    print_list Value.all;
    print_endline "\nto string verbose : \n";
    print_list_verbose Value.all;
    print_endline "\nto int : \n";
    print_list_toint Value.all;
 (*   print_endline "\nnext : \n";
    print_list_next Value.all;
    print_endline "\nprevious : \n";
    print_list_previous Value.all*)
