(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   micronap.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/09 15:25:35 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/09 22:12:28 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let main argv =
    try
        let ic = open_in argv.(1) in
        let lst = ref [] in
        try
            while true do
                lst := List.append !lst [input_line ic]
            done
        with End_of_file -> 
        begin
            close_in ic;
            let len = List.length !lst in
            let jokes = Array.make len "" in
            for i = 0 to (len - 1) do
                jokes.(i) <- List.nth !lst i
            done;
            print_endline jokes.(Random.self_init (); (Random.int len))
        end
    with
        | exn -> ()

let () =
    main Sys.argv
