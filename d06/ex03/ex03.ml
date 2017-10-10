(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex03.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/10 16:43:53 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/10 19:01:26 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type FIXED =
sig
    type t
    val of_float : float -> t
    val of_int : int -> t
    val to_float : t -> float
    val to_int : t -> int
    val to_string : t -> string
    val zero : t
    val one : t
    val succ : t -> t
    val pred : t -> t
    val min : t -> t -> t
    val max : t -> t -> t
(*    val gth : t -> t -> bool
    val lth : t -> t -> bool
    val gte : t -> t -> bool
    val lte : t -> t -> bool
    val eqp : t -> t -> bool (** physical equality *)
    val eqs : t -> t -> bool (** structural equality *)*)
    val add : t -> t -> t
 (*   val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t *)
    val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS =
sig
    val bits : int
end

module type MAKE =
    functor (Bits : FRACTIONNAL_BITS) ->
        FIXED

module Make : MAKE =
    functor (Bits : FRACTIONNAL_BITS) ->
    struct
        type t = int
        
        let of_float x = int_of_float (floor (x *. 2.0 ** (float_of_int Bits.bits) +. 0.5))
        let of_int x = x * (1 lsl Bits.bits)
        let to_float x = (float_of_int x) /. 2.0 ** (float_of_int Bits.bits)
        let to_int x = x / (1 lsl Bits.bits)
        let to_string x = string_of_float  (to_float x)
        let zero = 0
        let one = of_int 1
        let succ x = x + (to_int (of_int 1))
        let pred x = x - (to_int (of_int 1))
        let min x y = if x <= y then x else y
        let max x y = if x >= y then x else y
        let gth x y =
        let eqp x y = x = y
        let eqs x y = x == y
        let sub x y = x - y
        let add x y = x + y
        let mul x y = x * y
        let div x y = x / y
        let foreach x y f = 
            let rec loop x =
                if x <> y then (f x; loop (succ x))
                else f x
            in
            loop x

    end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)


let () =
    let x8 = Fixed8.of_int 21 in
    let y8 = Fixed8.of_float 21.42 in
    let r8 = Fixed8.add x8 y8 in
    print_endline (Fixed8.to_string r8);
    Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))
