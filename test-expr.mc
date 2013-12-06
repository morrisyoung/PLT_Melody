pitch p1=~C;
pitch p2=~Db4;
pitch p3=toneUp(p2,1);
pitch p4=toneDown(~Db2,1);
note n1=(p1;8);/*the fraction of a note should be a literal*/
note n2=(p1;16);
int i=12;
int j=60;
int k=i+j;
int k1=i*j;
bool b1=true;
bool b2=false;
note n3=(~G;8);
note n4=toneUp(n3,0);
note n5=toneDown((~G;8),1);
string s="helloworld123hehe";/*here the string can only contain upper and lower letter and the digits*/
string s1="getong";
string s2="jingsi";
string s3=s1+s2;
bar<<~C&~E&~G>>b1=[n1*4,n2,(p1;4),(~C;8)^n1];
bar<<p1&p2&~D>>b2=[n1,n2*2,n3^n3]<-n1;
bar<<p1&p2&~D>>b3=[n1,n2*2,n3^n3]<-(p1;8);
rhythm r1=[4,8,8,4];
rhythm r2=[i,j,j,i];
bar<<>>b4=[r1;(p1,p2,p2*2)];
bar<<>>b5=[[4,8,8,4];(p1,p2,p2*2)];
bar<<>>b6=[n1]^[n2,n3*2];/*here are some problems with "^"*/
bar<<>>b7=b4^[(~D;8),n1];
bar<<>>b8=toneUp([r1;(p1,p2,p2*2)],3);
int m=length([r1;(p1,p2,p2*2)]);
note n6=at(b6,i);
track<<"piano",4,4,60,~D>>t1=[b1*2,b2,b3,b4]<-b5;
track<<s,i,i,j,p1>>t2=[b1*2,b2,b3,b4];
track<<>>t3=[b1*4];
track<<>>t4=t1+t2;
track<<>>t5=toneDown([b1*2,b2,b3,b4],5);
int n=length(t5);
bar<<>>b9=at(t1,i);
melody m1=t1&t2;
melody m2=t1+[b1,b2,b3,b4]+[b1*2,[n1*2,n2,(p1;4),(~C;4)]];
