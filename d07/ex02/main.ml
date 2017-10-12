(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/12 17:41:34 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/12 22:01:17 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
    let human = new People.people "human" in
    let doctor = new Doctor.doctor "doctor" 42 human in
    let dalek = new Dalek.dalek in
    print_endline human#to_string;
    human#talk;
    print_endline doctor#to_string;
    doctor#talk;
    print_endline dalek#to_string;
    dalek#talk;
    dalek#exterminate human;
    print_endline human#to_string;
    doctor#use_sonic_screw;
    dalek#die;
    print_endline dalek#to_string
