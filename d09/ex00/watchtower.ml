(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   watchtower.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/14 15:52:10 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/14 16:28:32 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Watchtower =
struct
    type hour = int
    
    let (zero:hour) = 12

    let add (x:hour) (y:hour) : hour = (x + y) mod zero

    let sub (x:hour) (y:hour) : hour = add zero (x - y)

end
