(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nucleotides.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/04 21:55:06 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/04 23:29:38 by adubedat         ###   ########.fr       *)
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

let get_nucleobase nuc = match nuc with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | None -> "None"

let print_nucleotide nuc =
    print_endline ("phosphate : " ^ nuc.phosphate);
    print_endline ("deoxyribose : " ^ nuc.deoxyribose);
    print_endline ("nucleobase : " ^ get_nucleobase nuc.nucleobase)

let () =
    print_nucleotide (generate_nucleotide 'A');
    print_nucleotide (generate_nucleotide 'T');
    print_nucleotide (generate_nucleotide 'C');
    print_nucleotide (generate_nucleotide 'G');
    print_nucleotide (generate_nucleotide 'S')
