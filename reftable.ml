module NoteMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)
module IntMap = Map.Make(struct
  type t = int
  let compare x y = Pervasives.compare x y
end)

let maxStringInt = 95;;
let minStringInt = 0;;

let StringToIntMap = NoteMap.empty in
let StringToIntMap = NoteMap.add "~C" 0 StringToIntMap in
let StringToIntMap = NoteMap.add "~C#" 1 StringToIntMap in
let StringToIntMap = NoteMap.add "~Db" 1 StringToIntMap in
let StringToIntMap = NoteMap.add "~D" 2 StringToIntMap in
let StringToIntMap = NoteMap.add "~D#" 3 StringToIntMap in
let StringToIntMap = NoteMap.add "~Eb" 3 StringToIntMap in
let StringToIntMap = NoteMap.add "~E" 4 StringToIntMap in
let StringToIntMap = NoteMap.add "~F" 5 StringToIntMap in
let StringToIntMap = NoteMap.add "~F#" 6 StringToIntMap in
let StringToIntMap = NoteMap.add "~Gb" 6 StringToIntMap in
let StringToIntMap = NoteMap.add "~G" 7 StringToIntMap in
let StringToIntMap = NoteMap.add "~G#" 8 StringToIntMap in
let StringToIntMap = NoteMap.add "~Ab" 8 StringToIntMap in
let StringToIntMap = NoteMap.add "~A" 9 StringToIntMap in
let StringToIntMap = NoteMap.add "~A#" 10 StringToIntMap in
let StringToIntMap = NoteMap.add "~Bb" 10 StringToIntMap in
let StringToIntMap = NoteMap.add "~B" 11 StringToIntMap;;
let IntToNoteMap = IntMap.empty in
let IntToNoteMap = IntMap.add 0 "~C" IntToNoteMap in
let IntToNoteMap = IntMap.add 1 "~C#" IntToNoteMap in
let IntToNoteMap = IntMap.add 1 "~Db" IntToNoteMap in
let IntToNoteMap = IntMap.add 2 "~D" IntToNoteMap in
let IntToNoteMap = IntMap.add 3 "~D#" IntToNoteMap in
let IntToNoteMap = IntMap.add 3 "~Eb" IntToNoteMap in
let IntToNoteMap = IntMap.add 4 "~E" IntToNoteMap in
let IntToNoteMap = IntMap.add 5 "~F" IntToNoteMap in
let IntToNoteMap = IntMap.add 6 "~F#" IntToNoteMap in
let IntToNoteMap = IntMap.add 6 "~Gb" IntToNoteMap in
let IntToNoteMap = IntMap.add 7 "~G" IntToNoteMap in
let IntToNoteMap = IntMap.add 8 "~G#" IntToNoteMap in
let IntToNoteMap = IntMap.add 8 "~Ab" IntToNoteMap in
let IntToNoteMap = IntMap.add 9 "~A" IntToNoteMap in
let IntToNoteMap = IntMap.add 10 "~A#" IntToNoteMap in
let IntToNoteMap = IntMap.add 10 "~Bb" IntToNoteMap in
let IntToNoteMap = IntMap.add 11 "~B" IntToNoteMap;;

(*
let StringToDuration = fun x ­>
    if String.contains x '.' then
         Int_of_string(String.sub x ((String.index x '.')+1) ((String.length x) ­
((String.index x '.')+1)))
    else 4

let extractStringWithoutDuration = fun x ­>
    if String.contains x '.' then
         String.sub x 0 (String.index x '.')
    else x

let setStringDuration = fun x y ­>
      (extractStringWithoutDuration x) ^ "." ^ (string_of_int y)
*)

let StringToInt = fun x ­>
    let octave = String.get x ((String.length x)­1) in
	if ocave in ['1'-'7'] then
        let basicString = String.sub x 0 ((String.length x)­2) in
        ((NameMap.find basicString StringToIntMap) + ((int_of_char octave)­48) * 12)
	else (NameMap.find x StringToIntMap)

let IntToString = fun x ­>
    if x > maxStringInt then raise (Failure ("String higher than allowable reference threshold"))
      else if x < minStringInt then raise (Failure ("String lower than allowable reference threshold"))
      else if x > 11 then (IntMap.find (x­12*(x/12)) IntToNoteMap) ^ (string_of_int (x/12))
      else (IntMap.find x IntToNoteMap)
