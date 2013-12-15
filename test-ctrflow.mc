pitch p1;


function melody main(){
	melody m1;
	int j;
	int i;
	track<<>> t1;
	
	track<<>> t2;
	bool flag;
	p1=~B;
	t1=[-t [-b (p1;4)]];
	t2=[-t [-b (~A;16)]];
	
	
	m1=t1&t2;
	j=0;
	i=0;
	flag=false;
	if(flag){
		for(i;i<5;i=i+1){
			if(j==0){
				print(m1);
				for(i;i<5;i=i+1){
					print(i);
				}
			}
		}
	}
	else{
		while(j<3){
			j=j+1;
			print(j);
		}
	}
	return m1;
}
