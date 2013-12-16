/*
test case for concatination '<-'
*/

function melody main(){
	melody m1;
	note n1;
	track<<>> t1;
	bar b1;
	n1=(~A;6);
	b1=[-b (n1)];
	
	t1=[-t b1];
	m1=t1&t1;
	b1=b1<-n1;
	print(b1);
	t1=t1<-b1;
	print(t1);
	return m1;
}