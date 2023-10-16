## Gibbs Sampling##
examplegibbs<-function(t,burn_in,data,a,b,r,truealpha1,truealpha2){
  alpha1=numeric(t+1+burn_in)
  alpha2=numeric(t+1+burn_in)
  lambda=numeric(t+1+burn_in)
  alpha1[1]=sample(0:1,1,replace=TRUE)
  alpha2[1]=sample(0:1,1,replace=TRUE)
  for (i in 2:(t+1+burn_in)){
    alpha1[i]=rbeta(1,1+sum(yy1(r,truealpha1,data)),1+sum(bb1(data,r))-sum(yy1(r,truealpha1,data)))
    alpha2[i]=rbeta(1,1+sum(yy2(r,truealpha2,data)),1+sum(bb2(data,r))-sum(yy2(r,truealpha2,data)))
    c1=a+sum(data)-sum(yy1(r,truealpha1,data))-sum(yy2(r,truealpha2,data))
    c2=length(data)+b
    lambda[i]=rgamma(1,shape=c1,rate=c2)
  }
  alpha1=alpha1[(2):(t+1+burn_in)]
  alpha2=alpha2[(2):(t+1+burn_in)]
  lambda=lambda[(2):(t+1+burn_in)]
  result=c(alpha1,alpha2,lambda)
  return(result)
}
