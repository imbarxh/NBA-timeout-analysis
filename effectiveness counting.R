data=read.csv("/users/rui/downloads/DataTimeout.csv")
data1=subset(data,X<142)
diff=data$Point1-data$Point2
diff1=data.frame(data$SecondsRemaining[-1],diff(diff))

index0 = which(diff1[,2]==0)

length(index0)


deal <-function(x,y){
  e=c()
for (i in y)
{
  a=x[(i-1),2]
  b=(a>0)
  t=2
 while (b==(x[(i-t),2]>0))
 {
   a=a+x[(i-t),2];
   t=t+1;
 }
 e=c(e,a)
}
e
}
cum1 = deal(diff1,index0)

index6 = which(abs(cum1)>=6)
next_num = diff1[index0[index6]+1,2]
TF_num = (next_num*(cum1[index6])<0)
  table(TF_num)/length(index0)

