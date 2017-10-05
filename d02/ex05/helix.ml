(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   helix.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/05 09:44:24 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/05 10:57:28 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = {
    phosphate   : phosphate;
    deoxyribose : deoxyribose;
    nucleobase  : nucleobase
}

type helix = nucleotide list

let nucleobase c = match c with
    | 'A' -> A
    | 'T' -> T
    | 'C' -> C
    | 'G' -> G
    | _ -> None

let generate_nucleotide c = 
    {
        phosphate = "phosphate";
        deoxyribose = "deoxyribose";
        nucleobase = nucleobase c
    }

let generate_helix n =
    let rec loop n (lst:helix) =
        match n with
            | _ when n <= 0 -> lst
            | _ -> (
                match (Random.self_init (); Random.int 4) with
                    | 0 -> loop (n - 1) (lst @ [generate_nucleotide 'A'])
                    | 1 -> loop (n - 1) (lst @ [generate_nucleotide 'T'])
                    | 2 -> loop (n - 1) (lst @ [generate_nucleotide 'C'])
                    | 3 -> loop (n - 1) (lst @ [generate_nucleotide 'G'])
                    | _ -> loop (n - 1) (lst @ [generate_nucleotide 'X'])
            )
    in
    loop n []

let get_nucleobase nuc = match nuc with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | None -> "None"

let helix_to_string (lst:helix) =
    let rec loop str l =
        match l with
            | [] -> str
            | head :: tail -> loop (str ^ (get_nucleobase head.nucleobase)) tail
    in
    loop "" lst

let complementary_helix (helix:helix) =
    let rec loop helix (new_helix:helix) =
        match helix with
            | head :: tail when head.nucleobase = A -> loop tail (new_helix @ [generate_nucleotide 'T'])
            | head :: tail when head.nucleobase = T -> loop tail (new_helix @ [generate_nucleotide 'A'])
            | head :: tail when head.nucleobase = C -> loop tail (new_helix @ [generate_nucleotide 'G'])
            | head :: tail when head.nucleobase = G -> loop tail (new_helix @ [generate_nucleotide 'C'])
            | head :: tail -> loop tail (new_helix @ [generate_nucleotide 'X'])
            | [] -> new_helix
    in
    loop helix []

(* start of print and main functions *)

let () =
    let helix = generate_helix 10 in
    let helix2 = generate_helix 0 in
    let helix3 = generate_helix (-1) in
    let helix4 = generate_helix 42 in
    print_endline (helix_to_string helix);
    print_endline (helix_to_string (complementary_helix helix));
    print_endline (helix_to_string helix2);
    print_endline (helix_to_string (complementary_helix helix2));
    print_endline (helix_to_string helix3);
    print_endline (helix_to_string (complementary_helix helix3));
    print_endline (helix_to_string helix4);
    print_endline (helix_to_string (complementary_helix helix4))
