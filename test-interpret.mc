
/*
pitch p1;
note b;
melody a;
int i;

function melody main (){
a=[-b (~A;4),(~D;3),(~B;4),(~E;2)];
bool flag;
string s;
s= "tian_xia_tai_ping";
flag = true;
i=1;

p1=
b=(~A;4);

if(flag){
a=([-t [-b b],[-b b],[-b b],[-b (~B;4)] ] <- [-b (~D;3)])&[-t [-b (~A;6)]];
}

}
*/

note n1;
note n2;

function note hehe (pitch p1) {
pitch p2;
return (p1;3);
}

function melody main () {
pitch p1;
note n3;
melody m1;
p1=~B;
n1=(~A;4);
n2=(~D;3);
n3=hehe(p1);
print(n1,n2,n3);
}
