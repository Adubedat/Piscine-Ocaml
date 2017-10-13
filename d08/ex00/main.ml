(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/13 15:47:01 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/13 15:58:21 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
    let hydrogen = new Atom.hydrogen in
    let hydrogen2 = new Atom.hydrogen in
    let carbon = new Atom.carbon in
    let oxygen = new Atom.oxygen in
    let calcium = new Atom.calcium in
    let iron = new Atom.iron in
    let zinc = new Atom.zinc in
    let silver = new Atom.silver in
    let mercury = new Atom.mercury in
    let uranium = new Atom.uranium in
    let plutonium = new Atom.plutonium in
    let lithium = new Atom.lithium in
    print_endline hydrogen#to_string;
    print_endline "\n----------------------\n";
    print_endline carbon#to_string;
    print_endline "\n----------------------\n";
    print_endline oxygen#to_string;
    print_endline "\n----------------------\n";
    print_endline calcium#to_string;
    print_endline "\n----------------------\n";
    print_endline iron#to_string;
    print_endline "\n----------------------\n";
    print_endline zinc#to_string;
    print_endline "\n----------------------\n";
    print_endline silver#to_string;
    print_endline "\n----------------------\n";
    print_endline mercury#to_string;
    print_endline "\n----------------------\n";
    print_endline uranium#to_string;
    print_endline "\n----------------------\n";
    print_endline plutonium#to_string;
    print_endline "\n----------------------\n";
    print_endline (string_of_bool(hydrogen#equals carbon));
    print_endline "\n----------------------\n";
    print_endline (string_of_bool(hydrogen#equals hydrogen2));
    print_endline "\n----------------------\n";
    print_endline lithium#to_string
