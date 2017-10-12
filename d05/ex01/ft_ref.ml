(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ref.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/09 16:00:46 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/09 19:36:59 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a ft_ref = {mutable content : 'a}

let return value = {content = value}

let get (ref: 'a ft_ref) = ref.content

let set (ref: 'a ft_ref) new_value = ref.content <- new_value

let bind (ref: 'a ft_ref) (f: 'a -> 'b ft_ref) = f (get ref)

let () =
    let my_ref = return 7 in
    print_int (get my_ref); print_char '\n';
    set my_ref 12; print_char '\n';
    print_int (get my_ref); print_char '\n';
    let second_ref = bind my_ref (fun x -> return (x * x)) in
    print_int (get second_ref); print_char '\n'
