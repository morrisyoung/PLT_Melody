function melody main (){
pitch p1;
p1=~G4;
pitch p2;
p2=~A4;
pitch p3;
p3=~B4;
pitch p4;
p4=~C5;
pitch p5;
p5=~D5;
pitch p6;
p6=~E5;
pitch p7;
p7=~F5;
pitch p8;
p8=~G5;
pitch p9;
p9=~A5;
pitch p10;
p10=~B5;
pitch p11;
p11=~C6;
track<<piano,16,8,120,1>>t1;
bar b1;
b1=[-b (p4;8),(~;16),(p6;16),(p8;8),(p8;8)];
bar b2;
b2=[-b (p9;4),(p8;4)];
bar b3;
b3=[-b (p6;8),(~;16),(p4;16),(p8;8),(p8;16),(p8;16)];
bar b4;
b4=[-b (p6;4),(p4;4)];
bar b5;
b5=[-b (p1;16),(p1;16),(p1;8),(p1;8),(p1;16),(p1;16)];
bar b6;
b6=[-b (p4;4),(p4;8),(~;8)];
t1=[-t b1,b2,b3,b4,b5,b6];
melody m;
m=t1&t1;
return m;
}