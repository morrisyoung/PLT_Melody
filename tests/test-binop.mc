/*
test case for binary operation:
+,*,&,==,!=,<,<=,>,>=,&&,||
*/

function melody main(){
/*Add*/
	int i;
	int j;
	int r1;
	int r2;
	note n1;
	note n2;
	n1=(~B;4);
	n2=(~B;4);
	string s1;
	string s2;
	string s;
	pitch p;
	p=~C;
	track<<>> t1;
	track<<>> t2;
	track<<>> t;
	t1=[-t [-b (~A;1)]];
	t2=[-t [-b (~B;2)]];
	s1="hello ";
	s2="world!";
	i=2;
	j=3;
	r1=i+j;
	print(r1);
/*Multiply*/
	r2=i*j;
	print(r2);
	s=s1+s2;
	print(s);
	t=t1+t2;
	print(t);
	melody m;
	m=t1&t2&t1;
	print(m);
	string seq;
	seq="== ok";
	string sneq;
	sneq="!= ok";
	string bigger;
	string less;
	string geq;
	string leq;
	if(1==1&&~A==~A&&n1==n2&&"1"=="1"){
		print(seq);
	}
	if(1!=2||p!=~A||s1!=s2||n1!=n2){
		print(sneq);
	}
	if(11>2){
		
		bigger="> ok";
		print(bigger);
	}
	if(1<2){
		
		less="< ok";
		print(less);
	}
	if(4>=4){
		
		geq=">= ok";
		print(geq);
	}
	if(55<=100){
		
		leq="<= ok";
		print(leq);
	}
	return m;
}
