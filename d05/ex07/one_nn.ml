(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   one_nn.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/10 22:05:58 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/10 23:08:07 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type radar = (float array * string)

let list_to_example str =
    let lst = String.split_on_char ',' str in
    let len = List.length lst in
    if len = 0 then ([||], "") else
    begin
        let coord_array = Array.make (len - 1) 0.0 in
        for i = 0 to (len - 2) do
            coord_array.(i) <- float_of_string (List.nth lst i)
        done;
        ((coord_array, List.nth lst (len - 1)): radar)
    end


let list_to_examples_list lst =
    let len = List.length lst in
    if len = 0 then [] else
    begin
        let examples_lst = ref [] in
        for i = 0 to (len -1) do
            examples_lst := List.append !examples_lst [(list_to_example (List.nth lst i))]
        done;
        !examples_lst
    end

let examples_of_file path =
    try
        let ic = open_in path in
        let lst = ref [] in
        try
            while true do
                lst := List.append !lst [input_line ic]
            done;
            []
        with End_of_file -> 
        begin
            close_in ic; list_to_examples_list !lst
        end
    with
    | exn -> []

let eu_dist a b =
    let alen = Array.length a in
    let blen = Array.length b in
    if alen <> blen || alen = 0 then 0.0 else
    begin
        let dist = ref 0.0 in
        for i = 0 to (alen - 1) do
            dist := !dist +. ((a.(i) -. b.(i)) ** 2.0)
        done;
        sqrt !dist
    end

let one_nn (lst: radar list) (radar:radar) =
    let len = List.length lst in
    let closest = ref (List.nth lst 0) in
    let (coord_radar, g_radar) = radar in
    for i = 0 to (len - 1) do
        let (coord1, g1) = !closest in
        let (coord2, g2) = List.nth lst i in
        if (eu_dist coord1 coord_radar) > (eu_dist coord2 coord_radar)
        then closest := List.nth lst i
    done;
    let (coord, str) = !closest in
    str

let print_elem elem =
    let (x, y) = elem in
    Array.iter (fun x -> print_string ((string_of_float x) ^ ", ")) x;
    print_endline y


let () =
    let lst = examples_of_file "test.csv" in
    let str = one_nn lst (([|1.5;1.0|], "g"):radar) in
    let str2 = one_nn lst (([|1.5;1.5|], "g"):radar) in
    let str3 = one_nn lst (([|1.9;1.9|], "g"):radar) in
    let str4 = one_nn lst (([|4.5;10.0|], "g"):radar) in
    print_endline str;
    print_endline str2;
    print_endline str3;
    print_endline str4;

