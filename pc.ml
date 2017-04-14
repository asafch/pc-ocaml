module PC = struct

	type 'a result =
		| Failure
		| Success of 'a;;

	let string_to_list str =
		let rec helper i limit = 
		if i = limit then
			[]
		else
			str.[i] :: (helper (i+1) limit)
		in
		helper 0 (String.length str)

	let list_to_string lst =
		let rec helper i lst =
			match lst with
			| [] -> String.make i '?'
			| head :: tail ->
				let result = helper (i + 1) tail in
				result.[i] <- head;
				result
		in
		helper 0 lst

	let run parser str =
		parser (string_to_list str)

	let const pred input =
		match input with
		| [] -> Failure
		| head :: tail -> 
			if pred head then
				Success (head, tail)
			else
				Failure

	let pchar to_match =
		const (fun ch -> to_match = ch)

	let pchar_ci to_match =
		const (fun ch -> Char.lowercase to_match = Char.lowercase ch)

	let or_else p1 p2 input =
		let res1 = p1 input in
		match res1 with
		| Failure -> p2 input
		| Success (matched, rest) -> res1

	let empty input = Success ([], input)

	let none input = Failure

	let always = 
		const (fun ch -> true)

	let map f parser input =
		let res = parser input in
		match res with
		| Success (matched, rest) -> Success (f matched, rest)
		| _ -> Failure

	let (|>>) x f = map f x

	let and_then parser1 parser2 input =
		let res1 = parser1 input in
		match res1 with
		| Failure -> Failure
		| Success (matched1, rest1) ->
			let res2 = parser2 rest1 in
			match res2 with
			| Failure -> Failure
			| Success (matched2, rest2) -> Success ((matched1, matched2), rest2)

	let (->>-) parser1 parser2 = and_then parser1 parser2

	let pstring to_match =
		(List.fold_right 
			(fun curr prev -> map (fun (x,y) -> x :: y) (curr ->>- prev)) 
			(List.map pchar (string_to_list to_match)) 
			empty)
		|>> list_to_string

	let pstring_ci to_match =
		(List.fold_right 
			(fun curr prev -> map (fun (x,y) -> x :: y) (curr ->>- prev)) 
			(List.map pchar_ci (string_to_list to_match)) 
			empty)
		|>> list_to_string

	let take_left parser1 parser2 =
		map (fun (x,y) -> x) (parser1 ->>- parser2)

	let (->>) parser1 parser2 = 
		take_left parser1 parser2

	let take_right parser1 parser2 =
		map (fun (x,y) -> y) (parser1 ->>- parser2)

	let (>>-) parser1 parser2 = 
		take_right parser1 parser2

	let between parser1 parser2 parser3 =
		parser1 >>- parser2 ->> parser3

	let maybe parser input =
		let result = parser input in
		match result with
		| Failure -> Success (None, input)
		| Success (matched, rest) -> Success(Some matched, rest)

	let any_of lst =
		List.fold_right (fun curr prev -> or_else curr prev) lst none

	let range low_end high_end =
		const (fun x -> low_end <= x && x <= high_end)

	let range_ci low_end high_end =
		let low_end = Char.lowercase low_end in
		let high_end = Char.lowercase high_end in
		const (fun x -> let x = Char.lowercase x in low_end <= x && x <= high_end)

	let rec star parser input =
		let result = parser input in
		match result with
		(* NOTE: star always returns a success, because it tries to match the parser 0 or more times *)
		| Failure -> Success ([], input)
		| Success (matched1, rest1) ->
			let result2 = star parser rest1 in
			match result2 with
			| Success (matched2, rest2) -> Success (matched1 :: matched2, rest2)

	let plus parser input =
		let result = parser input in
		match result with
		| Failure -> Failure
		| Success (matched1, rest1) -> 
			let result2 = star parser rest1 in
			match result2 with
			| Success (matched2, rest2) -> Success (matched1 :: matched2, rest2)

end;;
