/* the program starts from the main() function, and it ends when the main function ends */
function melody main(){
	bar b1; /* declare a bar variable */
/* assign value to the bar variable with [-b...] with notes*/
	b1=[-b (~C6;1),(~C6;1),(~G6;1),(~G6;1)]; 	
bar b2;
	b2=[-b (~A6;1),(~A6;1),(~G6;1),(~;1)];
	bar b3;
	b3=[-b (~F6;1),(~F6;1),(~E6;1),(~E6;1)];
	bar b4;
	b4=[-b (~D6;1),(~D6;1),(~C6;1),(~;1)];
/* declare a track variable attributes including instruments, fraction, beats per bar, beats per minute, and key pitch */
	track<<sax, 1,4,60,1>> t1; 
	t1=[-t b1,b2,b3,b4]; /* assign value to the track variable with [-t …] with bars */
	track<<sax, 1,4,60,0>> t2;
	t2=[-t b3,b4];
	track<<sax,1,4,60,0>> t3;
	t3=toneUp(t2,2); /* use built-in function to raise the tone of track t2 by one degree*/
	bar tmp;
/* use built-in function to get the first bar in track t3 and assign its value to bar tmp*/
	tmp=at(t3,0); 
	note tmpn1; /* declare a note variable tmpn1*/
            /* functions can be nested*/
	tmpn1=toneDown(at(tmp,2),1); 
	bar newb;
	/* assignment can be done with functions’ return value*/
newb=[-b at(tmp,0),at(tmp,1),tmpn1,tmpn1];
	t3=[-t newb,at(t3,1)];
	track<<sax,1,4,60,0>> t4;
	t4=t3+t3; /* tracks can be concatenated*/
	track<<sax,1,4,60,0>> tall;
tall=t1+t4+t1;
	track<<banjo,1,4,60,0>> tchor;
	track<<>> tchor1; /* tracks would be given default attributes if not specified when declared*/
	track<<>> tchor2;
	track<<>> tchor3;

	tchor1=[-t [-b (~C4;1),(~C5;1),(~E5;1),(~C5;1),(~F5;1),(~C5;1),(~E5;1),(~C5;1)]];
	tchor2=[-t [-b (~D5;1),(~B4;1),(~C5;1),(~A4;1),(~F4;1),(~G4;1),(~C5;1)]];
	tchor3=[-t [-b (~E5;1),(~G4;1),(~D5;1),(~G4;1),(~C5;1),(~G5;1),(~B5;1),(~G5;1)]];
	tchor=tchor1+tchor2+tchor3+tchor3+tchor1+tchor2;

	melody twinkle; /* declare melody variable*/
	twinkle=syn(tall,tchor); /* synthesize tracks into one melody*/
/* this returned melody value would be compiled and a .csv file would be generated*/
	return twinkle; 
}
/*self-defined function to synthesize tracks into melody*/
function melody syn(track t1, track t2){
	melody m1;
	m1=t1&t2;
	return m1;
}