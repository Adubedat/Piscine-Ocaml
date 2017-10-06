(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gardening.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 14:01:55 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/06 15:24:58 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size (tree : 'a tree) =
    match tree with
        | Nil -> 0
        | Node (v, n1, n2) -> 1 + (size n1) + (size n2)

let rec height (tree : 'a tree) =
    match tree with
        | Node (v, n1, n2) when n1 <> Nil || n2 <> Nil -> 
                1 + (if (height n1) >= (height n2) then (height n1) else (height n2))
        | Node (v, n1, n2) -> 1
        | _ -> 0

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
    Graphics.moveto (x - 10) (y - 5);
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
                                    link_elem x y (x + dec) (y - 100);
                                    loop n1 (x + dec) (y - 100) (dec / 2);
                                    link_elem x y (x - dec) (y - 100);
                                    loop n2 (x - dec) (y - 100) (dec / 2)
    in
    loop tree 400 550 200

let () =
    let node5 = Node ("node5", Nil, Nil) in
    let node4 = Node ("node4", Nil, Nil) in
    let node3 = Node ("node3", Nil, node4) in
    let node2 = Node ("node2", node5, Nil) in
    let node1 = Node ("node1", node3, Nil) in
    let root = Node ("root", node1, node2) in
    print_endline (string_of_int (size root));
    print_endline (string_of_int (size node1));
    print_endline (string_of_int (size node2));
    print_endline (string_of_int (size node4));
    print_endline (string_of_int (size Nil));
    print_endline (string_of_int (height root));
    print_endline (string_of_int (height node1));
    print_endline (string_of_int (height node2));
    print_endline (string_of_int (height node4));
    print_endline (string_of_int (height Nil));
    draw_tree root;
    ignore(Graphics.read_key ())
