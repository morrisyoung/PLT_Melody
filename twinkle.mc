

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
	bar tmp;
	tmp=at(t3,0);
	note tmpn1;
	tmpn1=toneDown(at(tmp,2),1);
	bar newb;
	newb=[-b at(tmp,0),at(tmp,1),tmpn1,tmpn1];
	t3=[-t newb,at(t3,1)];
	track<<sax,1,4,60,0>> t4;
	t4=t3+t3;
	track<<sax,1,4,60,0>> tall;
	tall=t1+t4+t1;
	track<<banjo,1,4,60,0>> tchor;
	track<<>> tchor1;
	track<<>> tchor2;
	track<<>> tchor3;

	tchor1=[-t [-b (~C4;1),(~C5;1),(~E5;1),(~C5;1),(~F5;1),(~C5;1),(~E5;1),(~C5;1)]];
	tchor2=[-t [-b (~D5;1),(~B4;1),(~C5;1),(~A4;1),(~F4;1),(~G4;1),(~C5;1)]];
	tchor3=[-t [-b (~E5;1),(~G4;1),(~D5;1),(~G4;1),(~C5;1),(~G5;1),(~B5;1),(~G5;1)]];
	tchor=tchor1+tchor2+tchor3+tchor3+tchor1+tchor2;

	melody twinkle;
	twinkle=tall&tchor;
	print(twinkle);
	return twinkle;
}