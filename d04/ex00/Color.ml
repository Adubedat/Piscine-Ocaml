(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Color.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 18:07:53 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/06 18:21:53 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type t = Spade | Heart | Diamond | Club

let all = [Spade; Heart; Diamond; Club]

let toString (card:t) =
    match card with
        | Spade -> "S"
        | Heart -> "H"
        | Diamond -> "D"
        | Club -> "C"

let toStringVerbose (card:t) =
    match card with
        | Spade -> "Spade"
        | Heart -> "Heart"
        | Diamond -> "Diamond"
        | Club -> "Club" 
