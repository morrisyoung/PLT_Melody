/*
test case for built-in functions: 
print(element); 
at(bar/track,int); 
toneUp(pitch/note/bar/track,int); 
toneDown(pitch/note/bar/track,int);
length(bar/track);
*/

function melody main(){
	track<<>> t;
	t=[-t[-b(~A;1),(~B;2),(~C;3)],[-b(~A;1),(~B;2),(~C;3),(~D;4)]];
	bar<<>> b;
	note n;
	b=at(t,1);
	n=at(b,3);
	string sat;
	sat="-------------at";
	print(sat);
	print(b);
	print(n);
	print(sat);
	string sup;
	sup="-------------toneUp";
	/* for pitch-------------------*/
	pitch p;
	pitch pup;
	p=~D;
	print(sup);
	print(p);
	pup=toneUp(p,1);
	print(pup);
	/* for note-------------------*/
	note n;
	n=(p;5);
	note nup;
	nup=toneUp(n,2);
	print(n);
	print(nup);
	/* for bar-------------------*/
	bar<<>> b;
	b=[-b(n)];
	print(b);
	bar<<>> bup;
	bup=toneUp(b,3);
	print(bup);
	/* for track-------------------*/
	track<<>> t;
	t=[-tb];
	print(t);
	track<<>> tup;
	tup=toneUp(t,4);
	print(tup);
	print(sup);
	sup="-------------toneDown";
	/* for pitch-------------------*/
	p=~B7;
	print(sup);
	print(p);
	pup=toneDown(p,1);
	print(pup);
	/* for note-------------------*/
	n=(p;5);
	nup=toneDown(n,2);
	print(n);
	print(nup);
	/* for bar-------------------*/
	b=[-b n,n,n,n,n,n];
	print(b);
	bup=toneDown(b,3);
	print(bup);
	/* for track-------------------*/
	t=[-tb];
	print(t);
	tup=toneDown(t,4);
	print(tup);
	print(sup);

	string slength;
	slength="-------------length";
	print(slength);
	int i;
	i=length(b);
	int j;
	j=length(t);
	print(i);
	print(j);
}