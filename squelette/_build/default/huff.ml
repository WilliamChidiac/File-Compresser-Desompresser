(**gere [huff --help]*)
let help () = Printf.printf 
"run [huff file] where [file] is an ordinary file that you wish to compress. this will create a compressed file named [file.hf].\n
run [huff file.hf] where [file.hf] is a compressed file that you wish to decompress. this will create a decompressed file named [file].\n
run [huff --stats file] this fonctionnality is the same as [huff file] but it also writes down some stats about the transformation.\n"

(**gere [huff file]*)
let compress file = Huffman.compress file

(**gere [huff file.hf]*)
let decompress filehf = Huffman.decompress filehf

(**gere [huff --stats file]*)
let stats file = Huffman.storage file

(**apres une comande de la forme [huff filename] dans le terminal,
    determine si [filename] est compresse ou pas.
    si il ne l'est pas on le compresse 
    sinon on le decompresse*)
let compOrDecomp file = 
        try 
          let ishf = String.sub file (String.length file - 3) 3 in 
          if ishf = ".hf" then decompress file else compress file
        with 
          Invalid_argument "index out of bounds" -> compress file
          |Sys_error "" -> failwith "no such file or directory.\nrun [huff filename] where [filename] is a valid file name.\nrun [huff --help] to get some help"

let main () = 
  let command = Sys.argv in
  let ln = Array.length command in
  match ln with
   0 | 1 -> Printf.printf "run [huff --help] to get some help.\n"
  | 2 -> if command.(1) = "--help" then help () else compOrDecomp command.(1)
  | 3 -> if command.(1) = "--stats" then stats command.(2) else Printf.printf "run [huff --help] to get some help.\n"
  | _ -> Printf.printf "run [huff --help] to get some help.\n"

let () = main ()