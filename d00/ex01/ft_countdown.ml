(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/02 15:00:00 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/02 15:09:28 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_countdown x =
    if x <= 0 then
        begin
            print_int 0;
            print_char '\n'
        end
    else
        begin
            print_int x;
            print_char '\n';
            ft_countdown (x - 1)
        end

let main() =
    ft_countdown 5;
    ft_countdown 0;
    ft_countdown (-3)

let () = main ()
