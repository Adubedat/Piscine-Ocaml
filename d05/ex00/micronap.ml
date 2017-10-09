(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   micronap.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/09 15:25:35 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/09 15:45:35 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let my_sleep () = Unix.sleep 1

let main argv =
    begin try
            for i = 0 to ((int_of_string argv.(1)) - 1) do
                my_sleep ()
            done;
        with
            | _ -> ()
    end

let () =
    main Sys.argv
