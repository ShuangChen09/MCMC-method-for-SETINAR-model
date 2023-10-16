## the mentioned below true values are estimates from gibbs sampling ##
## this step is to estimate latent variable ##
mhsample=function(data,r,truealpha1,truealpha2,truelambda){
  y1=numeric(length(data))
  y2=numeric(length(data))
  z=numeric(length(data))
  for (i in 2:length(data)){
    y11=rbinom(1,bb1(data,r)[i-1],truealpha1)
    y22=rbinom(1,bb2(data,r)[i-1],truealpha2)
    while (data[i]<y11+y22){
      y11=rbinom(1,bb1(data,r)[i],truealpha1)
      y22=rbinom(1,bb2(data,r)[i],truealpha2)
    }
    y1[i]=y11
    y2[i]=y22
    zz=data[i]-y11-y22
    u=runif(1)
    A=(factorial(z[i-1])*(truelambda^(zz-z[i-1])))/(factorial(zz))
    acceptance=min(1,A)
    if(acceptance>=u){
      z[i]=zz
    }else{
      z[i]=z[i-1]
    }
  }
  result=c(y1[1:length(data)],y2[1:length(data)],z[1:length(data)])
  return(result)
}
