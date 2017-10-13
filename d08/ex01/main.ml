(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/13 15:47:01 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/13 18:08:47 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
    let water = new Molecule.water in
    let carbon_dioxyde = new Molecule.carbon_dioxyde in
    let methane = new Molecule.methane in
    let azote_dioxyde = new Molecule.azote_dioxyde in
    let trinotrotoluene = new Molecule.trinotrotoluene in
    print_endline water#to_string;
    print_endline "\n----------------------\n";
    print_endline carbon_dioxyde#to_string;
    print_endline "\n----------------------\n";
    print_endline methane#to_string;
    print_endline "\n----------------------\n";
    print_endline azote_dioxyde#to_string;
    print_endline "\n----------------------\n";
    print_endline trinotrotoluene#to_string
