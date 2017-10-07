(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 18:38:05 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/07 23:26:50 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec print_list lst =
    match lst with
        | head :: tail -> print_string (head ^ ", "); print_list tail
        | [] -> print_endline "\n"

let print_cards_list cards =
    let rec print_cards cards_lst nb =
        if nb mod 13 = 0 && nb > 0 then print_char '\n';
        match cards_lst with
            | head::tail -> Printf.printf "[%s] " (head);
                print_cards tail (nb + 1)
            | [] -> print_char '\n'
    in print_cards cards 0

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
    print_cards_list (Deck.toStringList deck);
    print_cards_list (Deck.toStringListVerbose deck);
    let (card, deck2) = Deck.drawCard deck in
    print_cards_list (Deck.toStringList deck2);
    print_cards_list (Deck.toStringListVerbose deck2);
	print_card card
