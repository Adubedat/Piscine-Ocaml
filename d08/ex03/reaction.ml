(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   reaction.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/13 19:47:29 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/13 20:53:00 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class virtual reaction =
object

    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balances : bool

end
