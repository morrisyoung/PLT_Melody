open Printf

let file = "example2.csv"

let () =
  (* Write message to file *)
  let input=([[105;4;2;1;1];[193;4;2;1;1]],[[[(34,2);(56,2);(12,2);(12,2)];[(55,1);(78,1)]];[[(34,2);(13,1);(88,2)];[(88,2);(81,2);(18,2);(22,2)]]]) in
  let(trackInfo,message)=input in
  (*let trackInfo=[[105;4;2;1;1];[193;4;2;1;1]] in*)
     (*let instrumentNo =  List.fold_left (fun s e -> s^string_of_int (List.nth e 0) ^",") "" trackInfo in*)
	 let instrumentNo = ( List.fold_left (fun s e -> let num = (List.nth e 0 ) in
							let num = (if num==0 then 193 else num) in
								s^string_of_int num ^",") "" trackInfo) in
		 let instrumentNo = String.sub instrumentNo 0 (String.length instrumentNo-1) in
			let instrumentNo = instrumentNo ^"\n" in
				let firstEle= List.nth trackInfo 0 in
					let basicbeat = List.nth firstEle 1 in
						let basicbeat = if basicbeat==0 then 4 else basicbeat in
  (*let message = [[[(34,2);(56,2);(12,2);(12,2)];[(55,1);(78,1)]];[[(34,2);(13,1);(88,2)];[(88,2);(81,2);(18,2);(22,2)]]]*)
	let list_of_tracks = List.map (fun e -> List.concat e) message in
		(*let  n_track= List.length list_of_tracks in*)
		(*let  basicbeat=4 in let count=0 in*)
		let rec makeStrList n pitch alist=   (*convert every note into a string in csv for n times*)
					  if n=0 then alist else
							(*(let astring = (string_of_int count) ^ "," ^ (string_of_int pitch) ^ ",90" in*)
							(let astring =  "," ^ (string_of_int pitch) ^ ",90" in
									(*let count=count+1 in*)  
										makeStrList (n-1)  pitch (astring::alist)) in
			let readTrack input=  (*concatenate all strings into one list for each track*)
				let str_track = List.map (fun (p,d) ->let n=basicbeat/d in  makeStrList n p []) input  in
					List.concat str_track
						(*in  List.map (fun e -> readTrack e) list_of_tracks*)
				in let list_of_strings = List.map (fun e -> readTrack e) list_of_tracks in (*do that to every track*)
					 let max_len = List.fold_left (fun max e-> if (List.length e)>max then (List.length e) else max ) 0 list_of_strings in

		  let oc = open_out file in    (* create or truncate file, return channel *)
		  (*fprintf oc "%s\n" message;   (* write something *)  *)
		  (*fprintf oc "%d\n" n_track;*)
		  fprintf oc "4\n";
		  fprintf oc "%s" instrumentNo;
		  (*fprintf oc "Instrument,105,Banjo,Instrument,114,Steel Drum\n";*)
			for count = 0 to max_len-1 do
						let (l, _) = (List.fold_left
						(fun (l, n) e ->if n<List.length e 
									then l^","^(string_of_int n)^(List.nth e n),n+0 
									else l^",,,",n+0) ("", count) list_of_strings)
						in let l= String.sub l 1 ((String.length l)-1) in
						let l = l^"\n"  
						in (fprintf oc "%s" l);
			done;
		  close_out oc;                (* flush and close the channel *)

  (* Read file and display the first line *)
  let ic = open_in file in
  try 
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    print_endline line;          (* write the result to stdout *)
    flush stdout;                (* write on the underlying device now *)
    close_in ic                  (* close the input channel *) 

  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    raise e 