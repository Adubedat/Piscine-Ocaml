(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 18:38:05 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/07 13:37:07 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec print_list lst =
    match lst with
        | head :: tail -> print_string ((Card.toString head) ^ ", "); print_list tail
        | [] -> print_endline "\n"

let rec print_list_verbose lst =
    match lst with
        | head :: tail -> print_string ((Card.toStringVerbose head) ^ ", "); print_list_verbose tail
        | [] -> print_endline "\n"
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

let print_card (card: Card.t) =
    print_endline (Card.toStringVerbose card)

let () =
    let allspades = Card.allSpades in
    let allhearts = Card.allHearts in
    let alldiamonds = Card.allDiamonds in
    let allclubs = Card.allClubs in
    let all = Card.all in
    let king_heart = Card.newCard Card.Value.King Card.Color.Heart in
    let five_spade = Card.newCard Card.Value.T5 Card.Color.Spade in
    print_endline "to string spades: \n";
    print_list allspades;
    print_endline "to string hearts: \n";
    print_list allhearts;
    print_endline "to string diamond: \n";
    print_list alldiamonds;
    print_endline "to string club: \n";
    print_list allclubs;
    print_endline "to string all: \n";
    print_list all;
    print_endline "to string verbose all: \n";
    print_list_verbose all;
    print_endline "get value king of heart: \n";
    print_endline (Card.Value.toStringVerbose (Card.getValue king_heart)); print_char '\n';
    print_endline "get color king of heart: \n";
    print_endline (Card.Color.toStringVerbose (Card.getColor king_heart)); print_char '\n';
    print_endline "best of all: \n";
    print_card (Card.best all); print_char '\n';
    print_endline "min king_heart and five_spade : \n";
    print_card (Card.min king_heart five_spade); print_char '\n';
    print_endline "max king_heart and five_spade : \n";
    print_card (Card.max king_heart five_spade); print_char '\n';
    print_endline "compare king_heart and five_spade : \n";
    print_int (Card.compare king_heart five_spade); print_char '\n';print_char '\n';
    print_endline "compare five_spade and king_heart: \n";
    print_int (Card.compare five_spade king_heart); print_char '\n';print_char '\n';
    print_endline "king_heart if of heart : \n";
    print_endline (string_of_bool(Card.isOf king_heart Card.Color.Heart)); print_char '\n';
    print_endline "king_heart if of spade : \n";
    print_endline (string_of_bool(Card.isOf king_heart Card.Color.Spade)); print_char '\n';
    print_endline "iSpade king_heart : \n";
    print_endline (string_of_bool(Card.isSpade king_heart)); print_char '\n';
    print_endline "isDiamond king_heart : \n";
    print_endline (string_of_bool(Card.isDiamond king_heart)); print_char '\n';
    print_endline "isHeart king_heart : \n";
    print_endline (string_of_bool(Card.isHeart king_heart)); print_char '\n';
    print_endline "isClub king_heart : \n";
    print_endline (string_of_bool(Card.isClub king_heart)); print_char '\n'

