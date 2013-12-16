

function melody main(){
	bar b1;
	b1=[-b (~C6;1),(~C6;1),(~G6;1),(~G6;1)];
	bar b2;
	b2=[-b (~A6;1),(~A6;1),(~G6;1),(~;1)];
	bar b3;
	b3=[-b (~F6;1),(~F6;1),(~E6;1),(~E6;1)];
	bar b4;
	b4=[-b (~D6;1),(~D6;1),(~C6;1),(~;1)];
	track<<sax, 1,4,60,1>> t1;
	t1=[-t b1,b2,b3,b4];



	track<<sax, 1,4,60,0>> t2;
	t2=[-t b3,b4];
	track<<sax,1,4,60,0>> t3;
	t3=toneUp(t2,2);
	track<<sax,1,4,60,0>> t4;
	t4=t3+t3;
	track<<sax,1,4,60,0>> tall;
	tall=t1+t4+t1;
	track<<banjo,1,4,60,0>> tchor;
	/*tchor=toneUp(tall,12);*/



	melody twinkle;
	twinkle=tall&tall;
	print(twinkle);
	return twinkle;
}