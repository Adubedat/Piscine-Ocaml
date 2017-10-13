(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/13 15:47:01 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/13 18:54:19 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
    let methane = new Alkane.methane in
    let ethane = new Alkane.ethane in
    let pentane = new Alkane.pentane in
    let octane = new Alkane.octane in
    let dodecane = new Alkane.dodecane in
    print_endline methane#to_string;
    print_endline "\n----------------------\n";
    print_endline ethane#to_string;
    print_endline "\n----------------------\n";
    print_endline pentane#to_string;
    print_endline "\n----------------------\n";
    print_endline octane#to_string;
    print_endline "\n----------------------\n";
    print_endline dodecane#to_string
