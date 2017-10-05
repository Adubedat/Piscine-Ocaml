(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   rna.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/05 11:01:29 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/05 11:19:25 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | U | None

type nucleotide = {
    phosphate   : phosphate;
    deoxyribose : deoxyribose;
    nucleobase  : nucleobase
}

type helix = nucleotide list

type rna = nucleobase list

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
    | _ -> "None"

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

let generate_rna (helix:helix) =
    let rec loop helix (rna:rna) =
        match helix with
            | head :: tail when head.nucleobase = A -> loop tail (rna @ [U])
            | head :: tail when head.nucleobase = T -> loop tail (rna @ [A])
            | head :: tail when head.nucleobase = C -> loop tail (rna @ [G])
            | head :: tail when head.nucleobase = G -> loop tail (rna @ [C])
            | head :: tail -> loop tail (rna @ [None])
            | [] -> rna
    in
    loop helix []

(* start of print and main functions *)
    let print_rna (rna:rna) =
        let rec loop rna =
            match rna with
                | head :: tail when head = A -> print_char 'A'; loop tail
                | head :: tail when head = T -> print_char 'T'; loop tail
                | head :: tail when head = C -> print_char 'C'; loop tail
                | head :: tail when head = G -> print_char 'G'; loop tail
                | head :: tail when head = U -> print_char 'U'; loop tail
                | head :: tail -> print_string "None"; loop tail
                | [] -> print_char '\n'
        in
        loop rna 

let () =
    let helix = generate_helix 10 in
    let helix2 = generate_helix 0 in
    let helix3 = generate_helix (-1) in
    let helix4 = generate_helix 42 in
    print_endline (helix_to_string helix);
    print_rna (generate_rna helix);
    print_endline (helix_to_string helix2);
    print_rna (generate_rna helix2);
    print_endline (helix_to_string helix3);
    print_rna (generate_rna helix3);
    print_endline (helix_to_string helix4);
    print_rna (generate_rna helix4)
