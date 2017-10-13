(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   molecule.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/13 16:28:18 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/13 18:35:21 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual molecule (name:string) (lst:Atom.atom list) =
object (self)

    method name = name
    method formula = 
        let carbon_list = List.filter (fun x -> x#symbol = "C") lst in
        let hydrogen_list = List.filter (fun x -> x#symbol = "H") lst in
        let other_list = List.filter (fun x -> x#symbol <> "C" && x#symbol <> "H") lst in
        let sorted_lst = List.sort (fun x y -> compare x#symbol y#symbol) other_list in
        let rec loop (str:string) (lst:Atom.atom list) count =
            match lst with
                | [] -> str
                | head :: tail -> begin
                    match tail with
                        | head2 :: tail2 when head#symbol = head2#symbol -> loop str tail (count + 1)
                        | head2 :: tail2 when count > 1 -> loop (str ^ head#symbol ^ (string_of_int count)) tail 1
                        | head2 :: tail2 -> loop (str ^ head#symbol) tail 1
                        | [] when count > 1 -> str ^ head#symbol ^ (string_of_int count)
                        | [] -> str ^ head#symbol
                end
        in
        (loop "" carbon_list 1) ^ (loop "" hydrogen_list 1) ^ (loop "" sorted_lst 1)

    method to_string = "name : " ^ self#name ^ "\nFormula : " ^ self#formula

end

class water =
object
    inherit molecule "Water" [new Atom.oxygen; new Atom.hydrogen; new Atom.hydrogen]
end

class carbon_dioxyde =
object
    inherit molecule "Carbone dioxyde" [new Atom.oxygen;new Atom.carbon; new Atom.oxygen]
end

class methane =
object
    inherit molecule "Methane" [new Atom.carbon; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen]
end

class azote_dioxyde =
object
    inherit molecule "Azote Dioxyde" [new Atom.oxygen; new Atom.oxygen; new Atom.nitrogen]
end

class trinotrotoluene =
object
    inherit molecule "Trinotrotoluene" [new Atom.oxygen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.nitrogen; new Atom.nitrogen; new Atom.nitrogen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon]
end
