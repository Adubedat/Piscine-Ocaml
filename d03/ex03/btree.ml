(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   btree.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 20:26:47 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/06 23:34:42 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let is_bst (tree : 'a tree) =
    let min = min_int in
    let max = max_int in
    let rec iz_bst node min max =
        match node with
            | Node (v, left, right) -> if int_of_string v > min && int_of_string v < max &&
                    iz_bst left min (int_of_string v) && iz_bst right (int_of_string v) max then true else false
            | Nil -> true
    in
    iz_bst tree min max

let rec height (tree : 'a tree) =
    match tree with
        | Node (v, n1, n2) when n1 <> Nil || n2 <> Nil ->
                1 + (if (height n1) >= (height n2) then (height n1) else (height n2))
        | Node (v, n1, n2) -> 0
        | _ -> 0

let rec is_perfect (tree : 'a tree) =
    match tree with
        | Nil -> true
        | Node (v, Nil, Nil) -> true
        | Node (v, Node (v1, l1, r1), Node (v2, l2, r2)) -> if is_perfect (Node(v1,l1,r1)) &&
                is_perfect (Node(v2,l2,r2)) && height (Node(v1,l1,r1)) = height (Node(v2, l2, r2)) then true else false
        | _ -> false

let rec is_balanced (tree : 'a tree) =
    match tree with
        | Nil -> true
        | Node (v, l, r) -> is_balanced l && is_balanced r && abs ((height l) - (height r)) <= 1

let rec search_bst value (tree: 'a tree) =
    match tree with
        | Node (v, l, r) when v = value -> true
        | Nil -> false
        | Node (v, l, r) -> if search_bst value l = false && search_bst value r = false then false else true 

let rec add_bst value (tree: 'a tree) =
    match tree with
        | Node (v,l,r) -> if value > v then Node (v, l, add_bst value r)
                                       else Node (v, add_bst value l, r)
        | Nil -> Node (value, Nil, Nil)

(*let rec delete_bst value (tree: 'a tree) =
    match tree with
        | Nil -> Nil
        | Node (v, l, r) -> if value > v then Node (v, l, delete_bst value r)
                            else if value < v then Node (v, delete_bst value l, r)
                            else
                            match (l, r) with
                                | (Nil, Nil) -> Nil
                                | (Nil, r) -> r
                                | (l, Nil) -> l
                                | (_, _) -> let Node (vmax, _, _) = max l in
                                                Node (vmax, delete vmax l, Nil)*)

let rec size (tree : 'a tree) =
    match tree with
        | Nil -> 0
        | Node (v, n1, n2) -> 1 + (size n1) + (size n2)

let draw_square x y size =
    if size < 0 then ()
    else (
        Graphics.moveto (x - (size / 2)) (y - (size / 2));
        let rec loop x y nb =
            match nb with
                | 0 -> Graphics.lineto (x + size) y; loop (x + size) y (nb + 1);
                | 1 -> Graphics.lineto x (y + size); loop x (y + size) (nb + 1);
                | 2 -> Graphics.lineto (x - size) y; loop (x - size) y (nb + 1);
                | 3 -> Graphics.lineto x (y - size); loop x (y - size) (nb + 1);
                | _ -> ()
        in
        loop (x - (size / 2)) (y - (size / 2)) 0
    )

let draw_elem str x y =
    Graphics.moveto (x - 3) (y - 5);
    Graphics.draw_string str;
    draw_square x y 40

let link_elem x1 y1 x2 y2 =
    Graphics.moveto x1 (y1 - 20);
    Graphics.lineto x2 (y2 + 20)

let rec draw_tree (tree : 'a tree) =
    Graphics.open_graph " 800x600";
    let rec loop (node: 'a tree) x y dec =
        match node with
            | Nil -> draw_elem "Nil" x y
            | Node (v, n1, n2) ->   draw_elem v x y;
                                    link_elem x y (x - dec) (y - 100);
                                    loop n1 (x - dec) (y - 100) (dec / 2);
                                    link_elem x y (x + dec) (y - 100);
                                    loop n2 (x + dec) (y - 100) (dec / 2)
    in
    loop tree 400 550 200

let () =
	let node5 = Node ("4", Nil, Nil) in
	let node3 = Node ("2", Nil, Nil) in
	let node2 = Node ("7", Node ("6", Nil, Nil), Node ("9", Nil, Nil)) in
	let node1 = Node ("3", node3, node5) in
	let root = Node ("5", node1, node2) in
    print_endline ("is BST : " ^ string_of_bool (is_bst root));
    print_endline ("is perfect : " ^ string_of_bool (is_perfect root));
    print_endline ("is balanced : " ^ string_of_bool (is_balanced root));
    print_endline ("is there value 6 : " ^ string_of_bool (search_bst "6" root));
    print_endline ("is there value 0 : " ^ string_of_bool (search_bst "0" root));
    
    draw_tree root;
    (* draw_tree (add_bst "1" root); *)
    (* draw_tree (add_bst "5" root); *)
    (* draw_tree (add_bst "8" root); *)
	ignore(Graphics.read_key ())

