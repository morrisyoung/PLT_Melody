function melody main(){
	bar b1;
	b1=[-b (~G6;1),(~C7;1),(~C7;1),(~D7;1)]; 
	bar b2;
	b2=[-b (~C7;1),(~B6;1),(~A6;1),(~A6;1)];
	bar b3;
	b3=[-b (~A6;1),(~D7;1),(~D7;1),(~E7;1)];
	bar b4;
	b4=[-b (~D7;1),(~C7;1),(~B6;1),(~G6;1),(~G6;1)];
	bar b5;
	b5=[-b (~E7;1),(~E7;1),(~F7;1),(~E7;1),(~D7;1)];
	bar b6;
	b6=[-b (~C7;1),(~A6;1),(~G6;1),(~G6;1),(~A6;1),(~D7;1),(~B6;1)];
	bar b7;
	b7=[-b (~C7;1),(~;1),(~G6;1)];
	bar b8;
	b8=[-b (~C7;1),(~C7;1),(~C7;1),(~B6;1),(~;1),(~B6;1)];
	bar b9;
	b9=[-b (~C7;1),(~B6;1),(~A6;1)];
	bar b10;
	b10=[-b (~G6;1),(~;1),(~D7;1),(~E7;1),(~D7;1),(~C7;1)];
	bar b11;
	b11=[-b (~G7;1),(~G6;1),(~G6;1),(~G6;1)];
	bar b12;
	b12=[-b (~A6;1),(~D7;1),(~B6;1)];
	bar b13;
	b13=[-b (~C7;1),(~;1),(~;1)];
	track<<banjo, 1,4,2,1>> t1;
	t1=[-t b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13];






	melody twinkle;
	twinkle=t1&t1;
	print(twinkle);
	return twinkle;
}