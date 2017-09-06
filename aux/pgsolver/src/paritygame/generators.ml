open Paritygame;;
open Tcsset;;

let generatormap = ref StringMap.empty;;

let register_generator generator_func identifier description =
	if StringMap.mem identifier !generatormap
	then failwith ("generator `" ^ identifier ^ "' already registered!\n")
	else generatormap := StringMap.add identifier (generator_func, description) !generatormap;;
	
let mem_generator identifier = StringMap.mem identifier !generatormap;;

let find_generator identifier = StringMap.find identifier !generatormap;;

let enum_generators it = StringMap.iter (fun i (f, d) -> it f i d) !generatormap;;

let fold_generators fo b = StringMap.fold (fun i (f, d) x -> fo f i d x) !generatormap b;;