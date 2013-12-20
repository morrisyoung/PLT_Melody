function melody main (){
pitch p1;
p1=~D;
bar b1;
b1=[-b (p1;2)];
print(b1);
bar b2;
rhythm r1;
r1=[-r 1,2,3,4];
print(r1);
b2=[-b [-r 1,2,3,4];(p1,p1,p1,p1)];
print(b2);
track<<>>t1;
t1=[-t b1];
melody m;
m=t1&t1;
return m;
}
