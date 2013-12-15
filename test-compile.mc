
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
/*
function note hehe (pitch p1) {
pitch p2;
return (p1;3);
}
*/
function melody main () {
pitch p1;
pitch p2;
track<<>> t1;
n1=(~A;4);
t1=[-t [-b n1]];
melody m1;
p1=~B1;
n1=(~A;4);
n2=(~D;3);
note n3;/*to test the local-declaration and statement mixture problem*/
n3=hehe(p1);
print(n3);
print(n1,n2,n3);
m1=t1&[-t [-b n3]];
melody m2;
m2=[-t [-b (p1;2),(~D;2),(p1;2),(~A;2)],[-b (~C;1),(p1;1),(~C;1)]]&[-t [-b (p1;2),(~E;1),(p1;2)],[-b (~B;2),(p1;2),(~F;2),(p1;2)]];
print m2;
/*return m2;*/
}
