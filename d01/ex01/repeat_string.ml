let repeat_string ?(str="x") n =
	let rec loop_string str n acc =
		if n < 0 then "Error"
		else if n > 0 then
			loop_string str (n - 1) (acc ^ str)
		else
			acc
	in
	loop_string str n ""

let () =
	print_endline (repeat_string ~str:"what" 3);
	print_endline (repeat_string (-1));
	print_endline (repeat_string 0);
	print_endline (repeat_string ~str:"Toto" 1);
	print_endline (repeat_string 2);
	print_endline (repeat_string ~str:"a" 5)
