(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   rna.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/05 11:01:29 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/05 17:05:01 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | U | None

type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile | Leu |
                 Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val

type nucleotide = {
    phosphate   : phosphate;
    deoxyribose : deoxyribose;
    nucleobase  : nucleobase
}

type helix = nucleotide list

type rna = nucleobase list

type protein = aminoacid list

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
                    | _ -> loop (n - 1) (lst @ [generate_nucleotide ' '])
            )
    in
    loop n []

let get_nucleobase nuc = match nuc with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | U -> "U"
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

let generate_bases_triplets (rna:rna) =
    let rec loop rna lst =
        match rna with
            | head :: next :: next2 :: tail -> loop tail (lst @ [(head, next, next2)])
            | _ -> lst
     in
     loop rna []

let decode_arn (rna:rna) =
    let triplets_lst = generate_bases_triplets rna in
    let rec loop triplets_lst (prot:protein) =
        match triplets_lst with
            | head :: tail when head = (U,A,A) || head = (U,A,G) || head = (U,G,A) -> prot @ [Stop]
            | head :: tail when head = (G,C,A) || head = (G,C,C) || head = (G,C,G) ||
                                head = (G,C,U) -> loop tail (prot @ [Ala])
            | head :: tail when head = (A,G,A) || head = (A,G,G) || head = (C,G,A) ||
                                head = (C,G,C) || head = (C,G,G) || head = (C,G,U) -> loop tail (prot @ [Arg])
            | head :: tail when head = (A,A,C) || head = (A,A,U) -> loop tail (prot @ [Asn])
            | head :: tail when head = (G,A,C) || head = (G,A,U) -> loop tail (prot @ [Asp])
            | head :: tail when head = (U,G,C) || head = (U,G,U) -> loop tail (prot @ [Cys])
            | head :: tail when head = (C,A,A) || head = (C,A,G) -> loop tail (prot @ [Gln])
            | head :: tail when head = (G,A,A) || head = (G,A,G) -> loop tail (prot @ [Glu])
            | head :: tail when head = (G,G,A) || head = (G,G,C) || head = (G,G,G) ||
                                head = (G,G,U) -> loop tail (prot @ [Gly])
            | head :: tail when head = (C,A,C) || head = (C,A,U) -> loop tail (prot @ [His])
            | head :: tail when head = (A,U,A) || head = (A,U,C) || head = (A,U,U) -> loop tail (prot @ [Ile])
            | head :: tail when head = (C,U,A) || head = (C,U,C) || head = (C,U,G) ||
                                head = (C,U,U) || head = (U,U,A) || head = (U,U,G) -> loop tail (prot @ [Leu])
            | head :: tail when head = (A,A,A) || head = (A,A,G) -> loop tail (prot @ [Lys])
            | head :: tail when head = (A,U,G) -> loop tail (prot @ [Met])
            | head :: tail when head = (U,U,C) || head = (U,U,U) -> loop tail (prot @ [Phe])
            | head :: tail when head = (C,C,C) || head = (C,C,A) || head = (C,C,G) ||
                                head = (C,C,U) -> loop tail (prot @ [Pro])
            | head :: tail when head = (U,C,A) || head = (U,C,C) || head = (U,C,G) ||
                                head = (U,C,U) || head = (A,G,U) || head = (A,G,C) -> loop tail (prot @ [Ser])
            | head :: tail when head = (A,C,A) || head = (A,C,C) || head = (A,C,G) ||
                                head = (A,C,U) -> loop tail (prot @ [Thr])
            | head :: tail when head = (U,G,G) -> loop tail (prot @ [Trp])
            | head :: tail when head = (U,A,C) || head = (U,A,U) -> loop tail (prot @ [Tyr])
            | head :: tail -> loop tail (prot @ [Val])
            | [] -> prot
    in
    loop triplets_lst []

let string_of_protein (prot:protein) =
    let get_amino_str amino =
        match amino with
            | Stop -> "End of translation"
            | Ala -> "Alanine"
            | Arg -> "Arginine"
            | Asn -> "Asparagine"
            | Asp -> "Aspartique"
            | Cys -> "Cysteine"
            | Gln -> "Glutamine"
            | Glu -> "Glutamique"
            | Gly -> "Glycine"
            | His -> "Histidine"
            | Ile -> "Isoleucine"
            | Leu -> "Leucine"
            | Lys -> "Lysine"
            | Met -> "Methionine"
            | Phe -> "Phenylalanine"
            | Pro -> "Proline"
            | Ser -> "Serine"
            | Thr -> "Threonine"
            | Trp -> "Tryptophane"
            | Tyr -> "Tyrosine"
            | Val -> "Valine"
    in
    let rec loop prot str n =
        match prot with
            | head :: tail -> loop tail (str ^ (if n <> 0 then ", " else "") ^ (get_amino_str head)) (n + 1)
            | [] -> str
    in
    loop prot "" 0

(* start of print and main functions *)
let string_of_triplets tup =
    let (x, y, z) = tup in
    "(" ^ get_nucleobase x ^ "," ^ get_nucleobase y ^ "," ^ get_nucleobase z ^ ")"

let rec print_triplets_list lst = match lst with
    | head :: tail -> print_string (string_of_triplets head ^ " -> ");
                      print_triplets_list tail
    | [] -> print_string "[] \n"

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
    let helix4 = generate_helix 85 in
    let rna = generate_rna helix in
    let rna2 = generate_rna helix2 in
    let rna3 = generate_rna helix3 in
    let rna4 = generate_rna helix4 in
    print_rna rna; print_triplets_list (generate_bases_triplets rna); print_endline (string_of_protein(decode_arn rna));
    print_rna rna2; print_triplets_list (generate_bases_triplets rna2); print_endline (string_of_protein(decode_arn rna2));
    print_rna rna3; print_triplets_list (generate_bases_triplets rna3); print_endline (string_of_protein(decode_arn rna3));
    print_rna rna4; print_triplets_list (generate_bases_triplets rna4); print_endline (string_of_protein(decode_arn rna4));
