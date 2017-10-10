(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/10 14:54:48 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/10 15:51:06 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module StringHash =
    struct
        type t = string
        let equal x y = x = y
        let hash (str: t) =
            let len = String.length str in
            if len = 0 then 7 else
            begin
                let rec loop c hash count =
                    if count = (len - 1) then hash
                    else loop (String.get str (count + 1)) (hash * 33 + (int_of_char c)) (count + 1)
                in
                loop (String.get str 0) 5381 0
            end
    end

module StringHashtbl = Hashtbl.Make(StringHash)

let () =
    let ht = StringHashtbl.create 5 in
    let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
    let pairs = List.map (fun s -> (s, String.length s)) values in
    List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
    StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
