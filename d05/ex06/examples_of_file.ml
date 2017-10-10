(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_file.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/10 21:03:38 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/10 21:53:42 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let list_to_example str =
    let lst = String.split_on_char ',' str in
    let len = List.length lst in
    if len = 0 then ([||], "") else
    begin
        let coord_array = Array.make (len - 1) 0.0 in
        for i = 0 to (len - 2) do
            coord_array.(i) <- float_of_string (List.nth lst i)
        done;
        (coord_array, List.nth lst (len - 1))
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

let print_elem elem =
    let (x, y) = elem in
    Array.iter (fun x -> print_string ((string_of_float x) ^ ", ")) x;
    print_endline y

let () =
    let lst = examples_of_file "ionosphere.test.csv" in
    List.iter print_elem lst

