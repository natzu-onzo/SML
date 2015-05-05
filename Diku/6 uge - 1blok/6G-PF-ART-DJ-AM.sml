(* Group assignment # 4:
 * - Philip Falck
 * - Alexander Roed Thorup
 * - David Jørgensen
 * - Asbjørn Michaelsen
 *)

(*******)
(* 6G1 *)
(*******)

local
    (* reads a file and then splits the list, char #"\n" in to string lists *)
    fun listLine file = let val instrm  = TextIO.openIn file
                            val allText = TextIO.inputAll instrm
                        in
                            String.fields(fn x => x = #"\n") allText
                            before
                            TextIO.closeIn instrm
                        end
in
    (* Reads a file and returns a char list list by mapping explode on the string list *)
    fun readSudoku fileName = map explode (listLine(fileName))
end;

(* unit testing *)
val test_readSudoku_01 =
    readSudoku("sudoku_eksempel_01.txt") =
        [[#"5", #"3", #"*", #"*", #"7", #"*", #"*", #"*", #"*"],
         [#"6", #"*", #"*", #"1", #"9", #"5", #"*", #"*", #"*"],
         [#"*", #"9", #"8", #"*", #"*", #"*", #"*", #"6", #"*"],
         [#"8", #"*", #"*", #"*", #"6", #"*", #"*", #"*", #"3"],
         [#"4", #"*", #"*", #"8", #"*", #"3", #"*", #"*", #"1"],
         [#"7", #"*", #"*", #"*", #"2", #"*", #"*", #"*", #"6"],
         [#"*", #"6", #"*", #"*", #"*", #"*", #"2", #"8", #"*"],
         [#"*", #"*", #"*", #"4", #"1", #"9", #"*", #"*", #"5"],
         [#"*", #"*", #"*", #"*", #"8", #"*", #"*", #"7", #"9"]];



(*******)
(* 6G2 *)
(*******)

(* Prints out a sudoku to the screen *)
fun showSudoku []      = print ""
  | showSudoku (x::xs) = (print(implode(x) ^ " \n"); showSudoku(xs))



(*******)
(* 6G3 *)
(*******)

local
    (* Edits the row at s'th position in the sudoku *)
    fun modifyRow ([])    (_,_) = []
      | modifyRow (x::xs) (0,c) = c :: xs
      | modifyRow (x::xs) (s,c) = x :: modifyRow (xs) (s - 1, c)
in
    (* Edits the line at r'th position in the sudoku *)
    fun modifySudoku ([])    (_, _, _) = []
      | modifySudoku (x::xs) (0, s, c) = modifyRow (x) (s,c) :: xs
      | modifySudoku (x::xs) (r, s, c) = x :: modifySudoku (xs) (r - 1, s, c)
end;



(*******)
(* 6G4 *)
(*******)

(* Prints a specific region of the sudoku *)
fun regionList (xs) (q) =
    [List.nth(List.nth(xs, q div 3 * 3 + 0), q mod 3 * 3 + 0),
     List.nth(List.nth(xs, q div 3 * 3 + 0), q mod 3 * 3 + 1),
     List.nth(List.nth(xs, q div 3 * 3 + 0), q mod 3 * 3 + 2),
     List.nth(List.nth(xs, q div 3 * 3 + 1), q mod 3 * 3 + 0),
     List.nth(List.nth(xs, q div 3 * 3 + 1), q mod 3 * 3 + 1),
     List.nth(List.nth(xs, q div 3 * 3 + 1), q mod 3 * 3 + 2),
     List.nth(List.nth(xs, q div 3 * 3 + 2), q mod 3 * 3 + 0),
     List.nth(List.nth(xs, q div 3 * 3 + 2), q mod 3 * 3 + 1),
     List.nth(List.nth(xs, q div 3 * 3 + 2), q mod 3 * 3 + 2)];

(* unit testing *)
val test_regionList_01 =
    regionList (readSudoku("sudoku_eksempel_01.txt")) 3 = 
        [#"8", #"*", #"*", #"4", #"*", #"*", #"7", #"*", #"*"]
