(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   app.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/14 16:23:15 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/14 17:38:22 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module App =
struct
    type project = string * string * int

    let (zero : project) = ("", "", 0)

    let combine (x:project) (y:project) : project =
        let (s1, st1, g1) = x in
        let (s2, st2, g2) = y in
        let gnew = (g1 + g2) / 2 in
        let stnew = if gnew > 80 then "succeed" else "failed" in
        let snew = s1 ^ s2 in
        (snew, stnew, gnew)

    let fail (x:project) : project =
        let (s, st, g) = x in
        (s, "failed", 0)

    let success (x:project) : project =
        let (s, st, g) = x in
        (s, "succeed", 80)

end
