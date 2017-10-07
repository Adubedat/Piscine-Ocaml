(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Card.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/07 11:05:22 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/07 22:59:44 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Card =
struct
module Color =
    struct
		type t = Spade | Heart | Diamond | Club

		let all = [Spade; Heart; Diamond; Club]

		let toString (card:t) =
   			match card with
        		| Spade -> "S"
        		| Heart -> "H"
        		| Diamond -> "D"
        		| Club -> "C"

		let toStringVerbose (card:t) =
    		match card with
        		| Spade -> "Spade"
        		| Heart -> "Heart"
        		| Diamond -> "Diamond"
        		| Club -> "Club"
	end

module Value =
	struct
		type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

		let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

		let toInt = function
    		| T2 -> 1
    		| T3 -> 2
    		| T4 -> 3
    		| T5 -> 4
    		| T6 -> 5
    		| T7 -> 6
    		| T8 -> 7
    		| T9 -> 8
    		| T10 -> 9
    		| Jack -> 10
    		| Queen -> 11
    		| King -> 12
    		| As -> 13

		let toString = function
    		| T2 -> "2"
    		| T3 -> "3"
    		| T4 -> "4"
    		| T5 -> "5"
    		| T6 -> "6"
    		| T7 -> "7"
    		| T8 -> "8"
    		| T9 -> "9"
   			| T10 -> "10"
    		| Jack -> "J"
    		| Queen -> "Q"
    		| King -> "K"
    		| As -> "A"

		let toStringVerbose = function
    		| T2 -> "2"
    		| T3 -> "3"
    		| T4 -> "4"
    		| T5 -> "5"
    		| T6 -> "6"
    		| T7 -> "7"
    		| T8 -> "8"
    		| T9 -> "9"
    		| T10 -> "10"
    		| Jack -> "Jack"
    		| Queen -> "Queen"
    		| King -> "King"
    		| As -> "As"

		let next = function
    		| T2 -> T3
    		| T3 -> T4
    		| T4 -> T5
    		| T5 -> T6
    		| T6 -> T7
    		| T7 -> T8
    		| T8 -> T9
    		| T9 -> T10
    		| T10 -> Jack
    		| Jack -> Queen
    		| Queen -> King
    		| King -> As
    		| As -> invalid_arg "As does not have next"

		let previous = function
    		| T2 -> invalid_arg "T2 does not have previous"
    		| T3 -> T2
    		| T4 -> T3
    		| T5 -> T4
    		| T6 -> T5
    		| T7 -> T6
    		| T8 -> T7
    		| T9 -> T8
    		| T10 -> T9
    		| Jack -> T10
    		| Queen -> Jack
    		| King -> Queen
    		| As -> King
	end

type t = (Value.t * Color.t)

let newCard (value: Value.t) (color: Color.t) = 
    let (card: t) = (value, color) in
    card

let allSpades = 
    let rec loop lst =
        match lst with
            | head :: tail -> ((head, Color.Spade): t) :: loop tail
            | [] -> []
    in
    loop Value.all

let allHearts = 
    let rec loop lst =
        match lst with
            | head :: tail -> ((head, Color.Heart):t) :: loop tail
            | [] -> []
    in
    loop Value.all

let allDiamonds = 
    let rec loop lst =
        match lst with
            | head :: tail -> ((head, Color.Diamond): t) :: loop tail
            | [] -> []
    in
    loop Value.all

let allClubs = 
    let rec loop lst =
        match lst with
            | head :: tail -> ((head, Color.Club): t) :: loop tail
            | [] -> []
    in
    loop Value.all

let all =
    List.concat [allSpades; allHearts; allDiamonds; allClubs]

let getValue (card:t) =
    let (value,color) = card in value

let getColor (card:t) =
    let (value,color) = card in color

let toString (card:t) = 
    let (value,color) = card in
    Printf.sprintf ("%s%s") (Value.toString value) (Color.toString color)

let toStringVerbose (card:t) = 
    let (value,color) = card in
    Printf.sprintf ("Card(%s, %s)") (Value.toStringVerbose value) (Color.toStringVerbose color)

let compare ((value1, color1):t) ((value2, color2):t) =
    if (Value.toInt value1) > (Value.toInt value2) then 1
    else if (Value.toInt value1) = (Value.toInt value2) then 0
    else (-1)

let max ((value1, color1): t) ((value2, color2): t) =
    if (Value.toInt value1 >= Value.toInt value2) then ((value1, color1):t) else ((value2, color2):t)

let min ((value1, color1): t) ((value2, color2): t) =
    if (Value.toInt value1 <= Value.toInt value2) then ((value1, color1):t) else ((value2, color2):t)

let best (card: t list) = 
    match card with
        | [] -> invalid_arg "You can't use best with an empty list"
        | _ -> List.fold_left (fun a b -> if (compare a b) >= 0 then a else b) (List.hd card) card

let isOf ((value,color): t) (color2: Color.t) =
    if color = color2 then true
     else false

let isSpade ((value,color):t) =
    if color = Color.Spade then true
    else false

let isHeart ((value,color):t) =
    if color = Color.Heart then true
    else false

let isDiamond ((value,color):t) =
    if color = Color.Diamond then true
    else false

let isClub ((value,color):t) =
    if color = Color.Club then true
    else false;;
end

type t = Card.t list

let newDeck = 
    Random.self_init ();
    let rec loop (acc:t) nb =
        if nb = 52 then acc else (
            let (card:Card.t) = List.nth Card.all (Random.int 52) in
            if List.mem card acc then loop acc nb else loop (card :: acc) (nb + 1)
        )
    in
    loop [] 0

let toStringList (deck: t) =
    let rec loop lst deck =
        match deck with
            | head :: tail -> loop ((Card.toString head) :: lst) tail
            | [] -> lst
    in loop [] deck

let toStringListVerbose (deck: t) =
    let rec loop lst deck =
        match deck with
            | head :: tail -> loop ((Card.toStringVerbose head) :: lst) tail
            | [] -> lst
    in loop [] deck
 
let drawCard (deck: t) =
    ((List.hd deck), (List.tl deck))
