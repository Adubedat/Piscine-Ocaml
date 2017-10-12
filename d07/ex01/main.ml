(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/12 17:41:34 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/12 17:57:20 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
    let bob = new People.people "bob" in
    let doctor = new Doctor.doctor "doc" 42 bob in
    print_endline doctor#to_string;
    doctor#travel_in_time 10 12;
    print_endline doctor#to_string;
    doctor#use_sonic_screw;
    doctor#regenerate
