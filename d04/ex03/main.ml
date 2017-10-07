(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 18:38:05 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/07 23:16:42 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec print_list lst =
    match lst with
        | head :: tail -> print_string (head ^ ", "); print_list tail
        | [] -> print_endline "\n"

(*let rec print_list_verbose lst =
    match lst with
        | head :: tail -> print_string ((Card.toStringVerbose head) ^ ", "); print_list_verbose tail
        | [] -> print_endline "\n"*)
(*
let rec print_list_toint lst =
    match lst with
        | head :: tail -> print_int (Card.toInt head); print_char '\n'; print_list_toint tail
        | [] -> ()

let rec print_list_next lst =
    match lst with
        | head :: tail -> print_endline (Card.toStringVerbose (Card.next head)); print_list_next tail
        | [] -> ()

let rec print_list_previous lst =
    match lst with
        | head :: tail -> print_endline (Card.toStringVerbose (Card.previous head)); print_list_previous tail
        | [] -> ()
*)
let print_card (card: Deck.Card.t) =
    print_endline (Deck.Card.toStringVerbose card)

let () =
    let deck = Deck.newDeck in
    let (card, deck2) = Deck.drawCard deck in
    print_list (Deck.toStringList deck);
    print_list (Deck.toStringListVerbose deck);
    print_list (Deck.toStringList deck2);
    print_list (Deck.toStringListVerbose deck2);
	print_card card
