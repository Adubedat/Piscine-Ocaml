(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/13 18:41:42 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/13 18:55:22 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual alkane n =
object (self)

    method name = match n with
        | 1 -> "Methane"
        | 2 -> "Ethane"
        | 3 -> "Propane"
        | 4 -> "Butane"
        | 5 -> "Pentane"
        | 6 -> "Hexane"
        | 7 -> "Heptane"
        | 8 -> "Octane"
        | 9 -> "Nonane"
        | 10 -> "Decane"
        | 11 -> "Undecane"
        | 12 -> "Dodecane"
        | _ -> ""

    method formula = "C" ^ (string_of_int n) ^ "H" ^ (string_of_int (2 + 2 * n))

    method to_string = "name : " ^ self#name ^ "\nformula : " ^ self#formula

    method equals (elem:alkane) = self#to_string = elem#to_string

end

class methane =
object
    inherit alkane 1
end

class ethane =
object
    inherit alkane 2
end

class propane =
object
    inherit alkane 3
end

class butane =
object
    inherit alkane 4
end

class pentane =
object
    inherit alkane 5
end

class hexane =
object
    inherit alkane 6
end

class heptane =
object
    inherit alkane 7
end

class octane =
object
    inherit alkane 8
end

class nonane =
object
    inherit alkane 9
end

class decane =
object
    inherit alkane 10
end

class undecane =
object
    inherit alkane 11
end

class dodecane =
object
    inherit alkane 12
end
