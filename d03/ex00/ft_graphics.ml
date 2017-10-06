(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_graphics.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: adubedat <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/06 11:57:27 by adubedat          #+#    #+#             *)
(*   Updated: 2017/10/06 13:56:27 by adubedat         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

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

let draw_nil x y = 
    Graphics.moveto (x - 7) (y - 5);
    Graphics.draw_string "Nil";
    draw_square x y 40

let draw_node value x y =
    Graphics.moveto (x - 10) (y - 5);
    Graphics.draw_string value;
    Graphics.moveto x y;
    draw_square x y 40;
    Graphics.moveto x (y - 20);
    Graphics.lineto (x - 100) (y - 100);
    draw_nil (x - 100) (y - 120);
    Graphics.moveto x (y - 20);
    Graphics.lineto (x + 100) (y - 100);
    draw_nil (x + 100) (y - 120)

let draw_tree_node (tree: 'a tree) =
    match tree with
        | Nil -> draw_nil 100 500
        | Node (value, nil1, nil2) -> draw_node value 400 400

let () =
    Graphics.open_graph " 800x600";
    draw_square 100 100 5;
    draw_square 700 100 (-1);
    draw_square 700 500 60;
    let nil = Nil in
    let node = Node ("lol", Nil, Nil) in
    draw_tree_node nil;
    draw_tree_node node;
    ignore(Graphics.read_key ())
