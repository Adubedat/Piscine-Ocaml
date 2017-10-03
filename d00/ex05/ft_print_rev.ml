(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/02 17:11:44 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/02 17:19:14 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_rev str =
    let len = String.length str in
    let rec loop_strrev str n =
        if n >= 0 then
            begin
                print_char (String.get str n);
                loop_strrev str (n - 1)
            end
    in
    loop_strrev str (len -1);
    print_char '\n'

let main () =
    ft_print_rev "Hello world !";
    ft_print_rev "H";
    ft_print_rev "";
    ft_print_rev "Cake is a lie"

let () = main ()
