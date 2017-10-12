(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/12 17:41:34 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/12 22:44:04 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
    let human = new People.people "human" in
    let human_lst = [new People.people "human1"; new People.people "human2"; new People.people "human3"] in
    let doctor_lst = [new Doctor.doctor "doc1" 10 human; new Doctor.doctor "doc2" 45 human] in
    let dalek_lst = [new Dalek.dalek; new Dalek.dalek; new Dalek.dalek] in
    let army = new Army.army human_lst in
    let army2 = new Army.army doctor_lst in
    let army3 = new Army.army dalek_lst in
    army#print_army;
    print_endline "\nadd elem to army : \n";
    army#add (new People.people "human4");
    army#print_army;
    print_endline "\ndelete elem from army : \n";
    army#delete;
    army#print_army;
    print_endline "\ndoc army : \n";
    army2#print_army;
    print_endline "\ndalek army : \n";
    army3#print_army;
    army3#delete;
    army3#delete;
    army3#delete;
    army3#delete;
    print_endline "\ndalek army after destruction: \n";
    army3#print_army;

