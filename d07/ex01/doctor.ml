(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   doctor.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/12 17:37:22 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/12 17:56:42 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class doctor (name:string) (age:int) (sidekick:People.people) =
object

    val name = name
    val mutable age = age
    val sidekick = sidekick
    val mutable hp = 100

    initializer print_endline "An instance of doctor class has been created"

    method to_string = "name : " ^ name ^ ", age : " ^ (string_of_int age) ^
         ", hp : " ^ (string_of_int hp) ^ ", sidekick : " ^ sidekick#to_string

    method talk = print_endline "Hi! I'm the doctor!"

    method travel_in_time (start:int) (arrival:int) = print_endline ("
    ╔═══╩═══╗
    ╠╦╦╦═╦╦╦╣
    ╠╬╬╣ ╠╬╬╣
    ╠╩╩╣ ╠╩╩╣
    ║░░║ ║  ║
    ╠══╣ ╠══╣
    ║  ║ ║  ║
    ╩══╩═╩══╩");
    age <- age + (arrival - start)

    method use_sonic_screw = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

    method regenerate = hp <- 100
end

