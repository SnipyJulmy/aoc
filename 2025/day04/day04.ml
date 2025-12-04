let filename = Sys.argv.(1)

let read_content (filename:string) : string Dynarray.t =
  let lines = Dynarray.create () in
  let input_channel = open_in filename in
  try
    while true do
      Dynarray.add_last lines @@ input_line input_channel
    done;
    lines
  with End_of_file ->
    close_in input_channel;
    lines

let is_paper (wildcard: char) (matrix: char Dynarray.t Dynarray.t) (r : int) (c : int) : bool =
  match Dynarray.get matrix r with
  | exception (Invalid_argument _) -> false
  | col -> match Dynarray.get col c with
    | exception(Invalid_argument _) -> false
    | symbol -> symbol == '@' || symbol == wildcard

let is_accessible (wildcard:char) (matrix: char Dynarray.t Dynarray.t) (r:int) (c:int) : bool =
  is_paper wildcard matrix r c &&
  let neighbours = 
    (if is_paper wildcard matrix (r-1) (c-1) then 1 else 0) +
    (if is_paper wildcard matrix (r-1) (c)   then 1 else 0) +
    (if is_paper wildcard matrix (r-1) (c+1) then 1 else 0) +
    (if is_paper wildcard matrix (r)   (c-1) then 1 else 0) +
    (if is_paper wildcard matrix (r)   (c+1) then 1 else 0) +
    (if is_paper wildcard matrix (r+1) (c-1) then 1 else 0) +
    (if is_paper wildcard matrix (r+1) (c)   then 1 else 0) +
    (if is_paper wildcard matrix (r+1) (c+1) then 1 else 0)
  in
  neighbours < 4

(* Remove the matrix of accessible papers and return the number of them removed *)
let clean_papers (wildcard:char) (matrix: char Dynarray.t Dynarray.t) : int =
  let count = ref 0 in
  Dynarray.iteri (fun row_idx row -> 
    Dynarray.iteri (fun col_idx symbol ->
      if is_accessible wildcard matrix row_idx col_idx
      then begin
        Dynarray.set row col_idx wildcard;
        incr count
      end else ()
    ) row
  ) matrix;
  !count

let input = read_content filename
let matrix = Dynarray.map (fun line -> Dynarray.init (String.length line) (String.get line)) input
let (_,score1) = Dynarray.fold_left (fun (row_idx, acc) row -> 
    let (_,col_count) = Dynarray.fold_left (fun (col_idx, count) col ->
      if is_paper '_' matrix row_idx col_idx && is_accessible '_' matrix row_idx col_idx 
      then (col_idx+1, count+1)
      else (col_idx+1, count)
    ) (0, 0) row in
    (row_idx + 1, acc + col_count)
  ) (0, 0) matrix

let score2 =
  let rec walk idx acc =
    let score = clean_papers (Char.chr idx) matrix in
    if score <= 0 then acc else walk (idx+1) (acc+score) in
  walk 0 0

let () = 
  Format.printf "Part 1: %i\n" score1;
  Format.printf "Part 2: %i\n" score2
