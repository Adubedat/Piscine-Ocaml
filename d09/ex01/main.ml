(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/14 17:31:05 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/14 17:37:35 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let print_proj (x:App.App.project) =
    let (s, st, g) = x in
    print_endline ("project_name : " ^ s ^ "\nstatus : " ^ st ^ "\ngrade : " ^ (string_of_int g))

let () =
    let p1:App.App.project = ("project1", "success", 95) in
    let p2:App.App.project = ("project2", "failed", 64) in
    let p3 = App.App.combine p1 p2 in
    let p4 = App.App.fail p1 in
    let p5 = App.App.success p2 in
    print_proj p1;
    print_endline "\n-------------------\n";
    print_proj p2;
    print_endline "\n-------------------\n";
    print_proj p3;
    print_endline "\n-------------------\n";
    print_proj p4;
    print_endline "\n-------------------\n";
    print_proj p5

