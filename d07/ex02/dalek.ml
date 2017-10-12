(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   dalek.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/12 18:06:14 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/12 21:58:04 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class dalek =
object

    val name = 
        Random.self_init ();
        let a = int_of_char 'a' in
        let amaj = int_of_char 'A' in
        let letter_string = String.make 1 (char_of_int (a + (Random.int 26))) in
        let lettermaj_string = String.make 1 (char_of_int (amaj + (Random.int 26))) in
        let rec loop str nb =
            match nb with 
                | 3 -> str
                | 0 -> loop (str ^ lettermaj_string) (nb + 1)
                | _ -> loop (str ^ letter_string) (nb + 1)
        in
        loop "Dalek" 0

    val mutable hp = 100
    val mutable shield = true
    
    method to_string = "name : " ^ name ^ ", hp : " ^ (string_of_int hp) ^ ", shield : " ^ (string_of_bool shield)

    method talk = match (Random.int 3) with
        | 0 -> print_endline "Explain! Explain!"
        | 1 -> print_endline "Exterminate! Exterminate!"
        | 2 -> print_endline "I obey!"
        | _ -> print_endline "You are the Doctor! You are the enemy of the Daleks!"

    method exterminate (people:People.people) = people#die; if shield then shield <- false else shield <- true

    method die = print_endline "Emergency Temporal Shift!"; hp <- 0
end
