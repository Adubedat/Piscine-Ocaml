(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gray.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/04 18:26:07 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/04 21:12:47 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let int_to_binary_string i n =
    let highest = 1 lsl (n - 1) in
    let rec loop highest str =
        if highest = 0 then str
        else if highest land i <> 0 then loop (highest lsr 1) (str ^ "1")
        else loop (highest lsr 1) (str ^ "0")
    in
    loop highest ""

let gray n = 
    let max = int_of_float (float_of_int 2 ** float_of_int n) in
    let rec loop nb str =
        if nb < (max - 1) then loop (nb + 1) (str ^ int_to_binary_string (nb lxor (nb lsr 1)) n ^ " ")
        else if nb < max then loop (nb + 1) (str ^ int_to_binary_string (nb lxor (nb lsr 1)) n)
        else str
    in
    print_endline (loop 0 "")


let () =
    gray 1;
    gray 2;
    gray 3;
    gray 4;
    gray 0;
    gray (-1)

