function melody main (){
p1=~G4;
p2=~A4;
p3=~B4;
p4=~C5;
p5=~D5;
p6=~E5;
p7=~F5;
p8=~G5;
p9=~A5;
p10=~B5;
p11=~C6;
track<<piano,16,8,120,1>>=t1;
bar b1;
b1=[-b (p4;8),(~;16),(p6;16),(p8;8),(p8;8)];
bar b2;
b2=[-b (p9;4),(p8;4)];
bar b3;
b3=[-b (;),(;)];

}
