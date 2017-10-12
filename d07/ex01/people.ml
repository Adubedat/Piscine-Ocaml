(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   people.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/12 17:18:52 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/12 17:31:03 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class people name =
    object
        val name:string = name
        val hp:int = 100
        
        initializer print_endline "An instance of people class has been created."

        method to_string = "name : " ^ name ^ ", hp : " ^ (string_of_int hp)

        method talk = print_endline ("I'm " ^ name ^ "! Do you know the Doctor?")

        method die = print_endline "Aaaarghh!"

    end
