


type arbre_huf = Noeud of (arbre_huf*arbre_huf) | Leaf of char


(**Rassemble de pair de (occurence * arbre_Huffman) a1 et a2 
  et rend le resultat*)
let add_element a1 a2 = 
  (fst a1+fst a2 , Noeud(((snd a1), (snd a2))))


(**
   ** **         ** **     *****   *****   *******   ********    *********     *****       *****     *********
 **     **     **     **   *** ** ** ***   ***   **  ***    **   ***          **    **    **    **   ***      
**            **       **  ***  ***  ***   ***   **  ***    **   ********       **          **       ********      
**            **       **  ***       ***   *******   ********    ********         **          **     ********       
 **     **     **     **   ***       ***   ***       ***  ***    ***          **    **    **    **   ***       
   ** **         ** **     ***       ***   ***       ***   ***   *********     ******      ******    ********* 
*)




(**
  compte le nombre d'occurence de chaque caractere
  et les places dans un tableau de taille 256
  chaque index du tableau represente un caractere
  et la valeur a chaque index represente le nombre d'occurence du caractere represente par l'index*)
let compte_octet  fichier = 
  let tab = Array.make 256 0 in 
  let rec aux tab fichier  =
    try 
      let entier = input_byte fichier in 
      let () = tab.(entier)<-tab.(entier)+1 in 
      aux tab fichier 
    with
      End_of_file -> tab
  in 
  aux tab fichier
(**prend en parametre un tableau de taille 256, les index du tableau representent leur caractere apres convertion de byte a char
    et la valeur a chaque case du tableau et le nombre d'occurence
    Retourne un tableau de pair (occurence * Leaf(caractere))*)
  let compte_occ element = 
    let element = Array.to_list element in 
    let rec aux element acc indice =
      match element with 
      [] -> acc 
      |e :: suite -> if e = 0 then aux suite acc (indice+1) else let acc = (e,Leaf(Char.chr indice)) ::acc in aux suite acc (indice+1)
    in 
    let res =Array.of_list (aux element [] 0) in 
    let () = Array.sort compare res in 
    res
  


(**cree un arbre de huffman en utilisant l'algo du cour*)
let cree_arbre tableau = 
let () = (Array.sort (fun a b -> compare (fst a) (fst b)) tableau) in
let tableau = Array.to_list tableau in
  let rec aux tab =
    match tab with 
    []-> failwith "Fichier vide"
    |[el]->el
    |e1::e2::suite -> let fusion = add_element e1 e2 in aux (Heap.add fusion suite)
  in snd (aux tableau)

(**[creer_tab_occ fichier] creer un arbre de huffman a partir du fichier [fichier]*)
let cree_arbre_from_file fichier = 
  let fst_tab = compte_octet fichier in 
  let fst_tab = compte_occ fst_tab
in cree_arbre (fst_tab)



(**[creer_code arbre] fonction qui prend en parametre un arbre de huffman et rend une pair de :
    une Array de liste de 0 et 1  de taille 256. Chaque index de la liste de la liste represente donc un caractere et la valeur de la liste a cette index 
        est une liste de 0 et de 1 representant le code de cette index dans l'arbre de huffman donne en parametre. 
        Si le caractre n'apparait pas dans l'arbre alors la valeur a l'index representant ce caractere sera le singleton [-1]
*)
let cree_code arbre = 
  let tableau = Array.make 256 [-1] in 
  let rec aux arbre codeCaractere =
    match arbre with 
    Noeud(gauche,droite) -> aux gauche (codeCaractere@[1]);
                            aux droite (codeCaractere@[0]);
    |Leaf(c) -> tableau.(Char.code c) <- codeCaractere;
  in aux arbre []; tableau 


let rec write_infile_from_BitList cfile lnum  = 
    match lnum with 
    [] -> ()
    |e :: l1 -> Bs.write_bit cfile e;
                write_infile_from_BitList cfile l1


(**[write_tree osfile arbre] ecrit dans [osfile] le parcour prefixe de [arbre] tel que chaque :
    Noeud(a1, a2) : est represente par un bit evalue a 1
    Leaf(c) : est represente par un bit evalue a 0 suivi du byte de c.*)
let rec write_tree osfile arbre = 
  match arbre with
  | Noeud(gauche, droit) -> Bs.write_bit osfile 1;
                            write_tree osfile gauche;
                            write_tree osfile droit
  | Leaf(c) -> Bs.write_bit osfile 0;
               Bs.write_byte osfile (Char.code c)


(**[readDecomp_writeComp file1 file2 codes] lit caractere par caractere dans [file1] qui est un fichier normal
  trouve le code associe au caractere lu grace a la Array [codes] 
  ensuite ecrit bit par bit dans [file2] le code du caractere lu !! [file2] est le fichier ".hf" que l'on compresse*)
let rec readDecomp_writeComp infile osfile intListArray = 
  let rec aux infile =
    try
      let entier = input_byte infile in 
      let listCode = intListArray.(entier) in
      write_infile_from_BitList osfile listCode;
      aux infile
    with 
      End_of_file -> () 
  in aux infile

(**[compress filename] prend un string associe au nom d'un fichier exsistant et
    le compresse dans le [fichier.hf]*)
let compress file = 
  let decomp_file = open_in file in
  let arbreH = cree_arbre_from_file decomp_file in 
  seek_in decomp_file 0;
  let arrCode = cree_code arbreH in
  let file_compresse = file^".hf" in let comp_file = open_out file_compresse in let osfile = Bs.of_out_channel comp_file in 
  write_tree osfile arbreH;
  readDecomp_writeComp decomp_file osfile arrCode;
  Bs.finalize osfile


(**
  ******      *********      ** **         ** **     *****   *****   *******   ********    *********     *****       *****     *********
  ***   **    ***          **     **     **     **   *** ** ** ***   ***   **  ***    **   ***          **    **    **    **   ***      
  ***    **   ********    **            **       **  ***  ***  ***   ***   **  ***    **   ********       **          **       ********      
  ***    **   ********    **            **       **  ***       ***   *******   ********    ********         **          **     ********       
  ***   **    ***          **     **     **     **   ***       ***   ***       ***  ***    ***          **    **    **    **   ***       
  ******      *********      ** **         ** **     ***       ***   ***       ***   ***   *********     ******      ******    ********* 
*)
(**[recreate_tree_from_bits istream] lis bit par bit dans [istream] et reconstruit l'arbre de huffman creer lors de la compression du fichier
  la fonction vu que l'arbre a ete ecrit par parcour prefixe la fonction s'arretera toute seul une fois que l'arbre sera totalement rempli
  et le prochain bit lu dans [istream] apres l'appel de la fonction sera donc le premier bit dans fichier avant compresion*)
let rec recreate_tree_from_bits isfile =
  try
  let n = Bs.read_bit isfile in 
  if n = 1 then
      let gauche = recreate_tree_from_bits isfile in
      let droit = recreate_tree_from_bits isfile in
      Noeud(gauche, droit)
  else 
      let c = Char.chr (Bs.read_byte isfile) in 
      Leaf(c)
  with 
    Bs.End_of_stream -> failwith "erreur : fichier compresse est incomplet"

    (**[readComp_writeDecomp file1 file2 codes] lit bit par bit dans [file1] qui est un fichier.hf compresse et 
  se deplace dans [initArbre] apres chaque lecture de bit, 
  un bit evalue a 1 equivalent a se deplace dans la partie gauche de l'arbre 
  un bit evalue a 0 est equivalent a se deplace dans la partie droite de l'arbre
  une fois qu'une feuille ateinte, 
  le caractere stocke dans cette feuille sera ecrit dans le fichier [file2] qui est donc le fichier normal, decompresse*)
let  readComp_writeDecomp isfile osfile initArbre = 
  let rec aux arbre =
      match arbre with
      | Noeud(gauche, droite) -> if Bs.read_bit isfile = 1 then 
                                    aux gauche
                                  else aux droite
      |Leaf(c) -> Bs.write_byte osfile (Char.code c);
                  aux initArbre
    in
  try 
    aux initArbre
  with
    Bs.End_of_stream -> ()


(**[decompress filename.hf] prend un string associe au nom d'un fichier.hf compresse exsistant et
    le decompresse dans le fichier [filename]*)
let decompress file =
    let newfilename = String.sub file 0 (String.length file - 3) in
    let isfile = Bs.of_in_channel (open_in file) in
    let arbre = recreate_tree_from_bits isfile in 
    let decomp_file = open_out newfilename in let osfile = Bs.of_out_channel decomp_file in 
    readComp_writeDecomp isfile osfile arbre
    

(**calcul le nombre de byte que le fichier occupe*)
let storage file = 
  let isfile = open_in file in
  let rec aux istream stor= 
    try 
      let _ = input_byte istream in
      aux istream (stor+1)
    with
        End_of_file -> Printf.printf "this file has a size of %d bytes\n" stor
  in aux isfile 0