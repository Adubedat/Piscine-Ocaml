(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex02.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/10 15:53:09 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/10 16:17:40 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type PAIR = sig val pair : (int * int) end
module type VAL = sig val x : int end

(* FIX ME !!! *)

module Pair : PAIR = struct let pair = ( 21, 42 ) end

module type MAKEPROJECTION =
    functor (Pair : PAIR) ->
        VAL

module MakeFst : MAKEPROJECTION =
    functor (Pair : PAIR) ->
    struct
        let x = match Pair.pair with
            | (fst, second) -> fst
    end

module MakeSnd : MAKEPROJECTION =
    functor (Pair : PAIR) ->
    struct
        let x = match Pair.pair with
            | (fst, snd) -> snd
    end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)


let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x

