module StringMap = Map.Make(struct
  type t = string
  let compare x y=Pervasives.compare x y
end)

module IntMap = Map.Make(struct
  type t = int
  let compare x y = Pervasives.compare x y
end)

let maxStringInt = 95;;
let minStringInt = 0;;

let str2int = StringMap.empty in
let str2int = StringMap.add "~C" 0 str2int in
let str2int = StringMap.add "~C#" 1 str2int in
let str2int = StringMap.add "~Db" 1 str2int in
let str2int = StringMap.add "~D" 2 str2int in
let str2int = StringMap.add "~D#" 3 str2int in
let str2int = StringMap.add "~Eb" 3 str2int in
let str2int = StringMap.add "~E" 4 str2int in
let str2int = StringMap.add "~F" 5 str2int in
let str2int = StringMap.add "~F#" 6 str2int in
let str2int = StringMap.add "~Gb" 6 str2int in
let str2int = StringMap.add "~G" 7 str2int in
let str2int = StringMap.add "~G#" 8 str2int in
let str2int = StringMap.add "~Ab" 8 str2int in
let str2int = StringMap.add "~A" 9 str2int in
let str2int = StringMap.add "~A#" 10 str2int in
let str2int = StringMap.add "~Bb" 10 str2int in
let str2int = StringMap.add "~B" 11 str2int in
let int2str = IntMap.empty in
let int2str = IntMap.add 0 "~C" int2str in
let int2str = IntMap.add 1 "~C#" int2str in
let int2str = IntMap.add 2 "~D" int2str in
let int2str = IntMap.add 3 "~D#" int2str in
let int2str = IntMap.add 4 "~E" int2str in
let int2str = IntMap.add 5 "~F" int2str in
let int2str = IntMap.add 6 "~F#" int2str in
let int2str = IntMap.add 7 "~G" int2str in
let int2str = IntMap.add 8 "~G#" int2str in
let int2str = IntMap.add 9 "~A" int2str in
let int2str = IntMap.add 10 "~A#" int2str in
let int2str = IntMap.add 11 "~B" int2str in


let mapstr2int = fun x ->
let octave = String.get x ((String.length x)-1) in
if (octave == '1')||(octave == '2')||(octave == '3')||(octave == '4')||(octave == '5')||(octave == '6')||(octave == '7') then
let basicString = String.sub x 0 ((String.length x) - 2) in
((StringMap.find basicString str2int) + ((int_of_char octave) - 48) * 12)
else (StringMap.find x str2int)
in

let mapint2str = fun x ->
if x > maxStringInt then raise (Failure ("String higher than allowable reference threshold"))
else if x < minStringInt then raise (Failure ("String lower than allowable reference threshold"))
else if x > 11 then (IntMap.find (x - 12*(x/12)) int2str) ^ (string_of_int (x/12))
else (IntMap.find x int2str)
