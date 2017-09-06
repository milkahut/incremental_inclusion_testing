let filename file = 
  let dotpos = String.rindex file '.' in
    String.sub file 0 (dotpos)

let convert file1 file2 =
  let aut1 = Nba.nba_of_ba_file file1 in
  let aut2 = Nba.nba_of_ba_file file2 in
  let a1,a2 = Nba.normalize_pair (aut1,aut2) in  
    Nba.gff_file_of_nba a1 ((filename file1)^".gff");
    Nba.gff_file_of_nba a2 ((filename file2)^".gff")

let revert file1 file2 =
  let aut1 = Nba.nba_of_gff_file file1 in
  let aut2 = Nba.nba_of_gff_file file2 in
  let a1,a2 = Nba.normalize_pair (aut1,aut2) in
    Nba.ba_file_of_nba a1 ((filename file1)^".ba");
    Nba.ba_file_of_nba a2 ((filename file2)^".ba")
 
let help_message = 
  "ba2gff: convert a pair of automata in .ba format to .gff format \n" ^
  "Usage: ba2gff file1 file2 \n" ^ 
  "Examples: ./ba2gff A34.ba B34.ba \n" ^
  "Effect: write files: A34.gff and B34.gff in the same directory \n" 

let toba = ref false

let sl = Arg.align [("-toba",Arg.Set(toba), "revert to ba files")]

let arg = ref []
  
let _ =
  Arg.parse sl (fun x -> arg := x::!arg) help_message;
  match !arg with
    | [file2; file1] -> if !toba then revert file1 file2 else convert file1 file2 
    | _ -> invalid_arg help_message
