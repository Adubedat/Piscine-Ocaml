(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   calc.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/14 18:22:05 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/14 19:36:23 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type MONOID =
    sig
        type element
        val zero1 : element
        val zero2 : element
        val mul : element -> element -> element
        val add : element -> element -> element
        val div : element -> element -> element
        val sub : element -> element -> element
    end

module Calc =
    functor (M : MONOID) ->
    struct

        let add x y = M.add x y
        let sub x y = M.sub x y
        let mul x y = M.mul x y
        let div x y = M.div x y
        let rec power x (pow:int) : M.element =
                if pow = 0 then M.zero1
                else (mul x (power x (pow - 1)))

        let fact (x:M.element) =
            let rec loop acc nb =
                if nb = M.zero1 then acc else loop (mul acc nb) (sub nb M.zero2)
            in
            loop M.zero2 x
    end

module INT =
struct
    type element = int
    let zero1 = 0
    let zero2 = 1
    let mul = ( * )
    let add = ( + )
    let div = ( / )
    let sub = ( - )
end

module FLOAT =
struct
    type element = float
    let zero1 = 0.0
    let zero2 = 1.0
    let mul = ( *. )
    let add = ( +. )
    let div = ( /. )
    let sub = ( -. )
end

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)
let () =
print_endline (string_of_int (Calc_int.power 3 3));
print_endline (string_of_float (Calc_float.power 3.0 3));
print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
Printf.printf "fact: %d %d\n" (Calc_int.fact 0) (Calc_int.fact 4);
Printf.printf "fact: %f %f\n" (Calc_float.fact 0.0) (Calc_float.fact 4.0);
Printf.printf "int : div (sub (8 4) sub (4 2)): %d \n" (Calc_int.div (Calc_int.sub 8 4) (Calc_int.sub 4 2));
Printf.printf "float : div (sub (8.0 4.0) add (4.2 2.1)) : %f \n" (Calc_float.div (Calc_float.sub 8.0 4.0) (Calc_float.add 4.2 2.1));
let num1 = Calc_int.add 1 4 in
let num2 = Calc_int.add 1 3 in
Printf.printf "zero1 case: %d %d\n" (Calc_int.add num1 INT.zero1) (Calc_int.sub num2 INT.zero1);
Printf.printf "zero2 case: %d %d\n" (Calc_int.mul num1 INT.zero2) (Calc_int.div num2 INT.zero2)
