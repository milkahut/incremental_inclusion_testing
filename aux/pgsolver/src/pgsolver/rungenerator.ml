let _ =
	let gen = ref (fun _ -> [||]) in
	Generators.enum_generators (fun f _ _ -> gen := f);
	Paritygame.print_game (!gen (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)))