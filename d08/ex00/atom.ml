(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   atom.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/13 15:14:40 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/13 15:55:44 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual atom (name:string) (symbol:string) (atomic_number:int) =
object (self)

   method name = name
   method symbol = symbol
   method atomic_number = atomic_number
   method to_string = "name : " ^ name ^"\nsymbol : " ^ symbol ^ "\natomic_number : " ^ (string_of_int atomic_number)
   method equals (other_atom:atom) = self#to_string = other_atom#to_string

end

class hydrogen =
object
    inherit atom "Hydrogen" "H" 1
end

class carbon =
object
    inherit atom "Carbon" "C" 6
end

class oxygen =
object
    inherit atom "Oxygen" "O" 8
end

class calcium =
object
    inherit atom "Calcium" "Ca" 20
end

class iron =
object
    inherit atom "Iron" "Fe" 26
end

class zinc =
object
    inherit atom "Zinc" "Zn" 30
end

class silver =
object
    inherit atom "Silver" "Ag" 47
end

class mercury =
object
    inherit atom "Mercury" "Hg" 80
end

class uranium =
object
    inherit atom "Uranium" "U" 92
end

class plutonium =
object
    inherit atom "Plutonium" "Pu" 94
end
