(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sequence.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/04 21:13:52 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/04 21:54:24 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let new_sequence lst =
    let rec loop n lst final_list = match lst with
        | [] -> []
        | head1 :: tail1 -> (
            match tail1 with
            | head2 :: tail2 when head1 = head2 -> loop (n + 1) tail1 final_list
            | head2 :: tail2 -> loop 1 tail1 (final_list @ [n] @ [head1])
            | [] -> final_list @ [n] @ [head1]
        )
    in
    loop 1 lst []

let string_of_list lst =
    let rec loop lst str = match lst with
        | head :: tail -> loop tail (str ^ string_of_int head)
        | [] -> str
    in
    loop lst ""

let sequence n =
    let rec loop n l = match n with 
        | _ when n <= 0 -> ""
        | 1 -> string_of_list l
        | _ -> loop (n - 1) (new_sequence l)
    in
    loop n [1]

let () =
    print_endline (sequence (-1));
    print_endline (sequence 0);
    print_endline (sequence 1);
    print_endline (sequence 5);
    print_endline (sequence 15);
