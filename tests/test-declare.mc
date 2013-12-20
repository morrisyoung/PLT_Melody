pitch p1;
note n1;
bar b1;
function pitch test (note n2, track t1)
{
	pitch p2;
	p2 = ~D;
	print(p2);
	pitch p3;
	p3 = ~E;
	print(p3);
	return p3;
}
function melody main ()
{

	p1=~C;
	print(p1);
	n1 = (p1;8);
	print(n1);
	b1=[-b n1,n1,n1,n1];
	print(b1);
	track<<violin, 1,2,3,4>> track1;
	track1=[-t [-b (~C;4)]];
	print(track1);
	track<<piano, 1,2,3,4>> track2;
	track2=[-t [-b (~D;4)]];
	print(track2);
	melody m1;
	m1=track1&track2;
	print(m1);
	return m1;
}
