
function melody main(){
	bar b1;
	b1=[-b (~G6;1),(~C7;1),(~C7;1),(~D7;1)];
	bar b2;
	b2=[-b (~C7;1),(~B6;1),(~A6;1),(~A6;1)];
	bar b3;
	b3=[-b (~A6;1),(~D7;1),(~D7;1),(~E7;1)];
	bar b4;
	b4=[-b (~D7;1),(~C7;1),(~B6;1),(~G6;1)];
	track<<banjo, 1,4,2,1>> t1;
	t1=[-t b1,b2,b3,b4];






	melody twinkle;
	twinkle=t1&t1;
	print(twinkle);
	return twinkle;
}