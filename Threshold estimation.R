## under the Maximum A Posteriori (MAP) criterion ##
mmp=function(data,q,t,burn_in,a,b,truealpha1,truealpha2,truelambda){
  mp=1
  mpmp1=rep(1,length(data))
  mpmp2=rep(1,length(data))
  mpmp3=rep(1,length(data))
  for(r in rlowerupper(data,q)){
    gibbsresult=examplegibbs(t,burn_in,data1_100,a,b,h,truealpha1,truealpha2)
    alpha1hat=mean(gibbsresult[(burn_in+1):(t+burn_in)])
    alpha2hat=mean(gibbsresult[(t+2*burn_in+1):(2*t+2*burn_in)])
    lambdahat=mean(gibbsresult[(2*t+3*burn_in+1):(3*t+3*burn_in)])
    mhresult=mhsample(data,r,alpha1hat,alpha2hat,lambdahat)
    y1hat=mhresult[1:length(data)]
    y2hat=mhresult[(length(data)+1):(2*length(data))]
    zhat=mhresult[(2*length(data)+1):(3*length(data))]
    for (i in 1:length(data)){
      mpmp1[i]=choose(bb1(data,r)[i],y1hat[i])
    }
    for (i in 1:length(data)){
      mpmp2[i]=choose(bb2(data,r)[i],y2hat[i])
    }
    for (i in 1:length(data)){
      mpmp3=factorial(zhat[i])
    }
    mpmp1<-ifelse(mpmp1==0,1,mpmp1)
    mpmp2<-ifelse(mpmp2==0,1,mpmp2)
    mpmp4=mpmp1*mpmp2/mpmp3
    mpmp=log(mpmp4)
    mpone=sum(mpmp)+
      sum(y1hat)*log(alpha1hat)+sum(y2hat)*log(alpha2hat)+
      sum(bb1(data,r))*log(1-alpha1hat)+sum(bb2(data,r))*log(1-alpha2hat)-
      sum(y1hat)*log(1-alpha1hat)-sum(y2hat)*log(1-alpha2hat)+
      sum(zhat)*log(lambdahat)+(a-1)*log(lambdahat)-length(data)*lambdahat-b*lambdahat
    mp=append(mp,mpone)
  }
  prer=rlowerupper(data,q)
  mp=mp[-1]
  location=which.max(mp)
  result=prer[location]
  return(result)
}
