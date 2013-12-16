/*test case for self-defined function*/

function melody main(){
	track<<>> t;
	t = [-t[-b(~A;4),(~B;5)]];
	t = test(2,t);
	print(t);
	melody m;
	m=t&t;
	return m;
}


function track test(int i,track t){
	track<<>> tr;
	print(t);
	tr = toneUp(t,i);
	return tr;
}