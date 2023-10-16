shixing<-function(x,r) {
  if (x<=r) {
    return(1)
  } else {
    return(0)
  }
}


## functions for sampling##
bb1=function(data,r){
  b=rep(0,length(data)+1)
  for (i in 2:(length(data)+1)){
    b[i]=shixing(data[i-1],r)*data[i-1]
  }
  bb=b[2:(length(data)+1)]
  return(bb)
}


bb2=function(data,r){
  b=rep(0,length(data)+1)
  for (i in 2:(length(data)+1)){
    b[i]=(1-shixing(data[i-1],r))*data[i-1]
  }
  bb=b[2:(length(data)+1)]
  return(bb)
}


yy1=function(r,alpha1,data){
  y=rep(0,length(data)+1)
  for (i in (2:(length(data)+1))){
    y[i]=bto(alpha1,data[i-1])*shixing(data[i-1],r)
  }
  y=y[2:(length(data)+1)]
  return(y)
}


yy2=function(r,alpha2,data){
  y=rep(0,length(data)+1)
  for (i in (2:(length(data)+1))){
    y[i]=bto(alpha2,data[i-1])*(1-shixing(data[i-1],r))
  }
  y=y[2:(length(data)+1)]
  return(y)
}


rlowerupper=function(data,q){
  sorted_data=sort(data)
  rmax=sorted_data[ceiling((1-q)* length(sorted_data))]
  rmin=sorted_data[floor(q * length(sorted_data))]
  result=rmin:rmax
  return(result)
}

## functions for results discussions ##
interval=function(data,q){
  sample_mean<-mean(data)
  standard_error<-sqrt(var(data))
  confidence_level<-q
  z_value<-abs(qnorm((1-confidence_level) / 2))
  tt1=sample_mean-z_value*standard_error
  tt2=sample_mean+z_value*standard_error
  lower_bound=quantile(data,0.025)
  upper_bound=quantile(data,0.975)
  in_interval <- data>=tt1&data<=tt2
  count_in_interval<-sum(in_interval)
  cp=count_in_interval/length(data)
  rr=c(tt1,tt2,cp)
  return(rr)
}


discussion=function(result,rawdata,t,burn_in,truealpha1,truealpha2,truelambda){
  alp1=result[(burn_in+1):(t+burn_in)]
  alp2=result[(t+2*burn_in+1):(2*t+2*burn_in)]
  lambda=result[(2*t+3*burn_in+1):(3*t+3*burn_in)]
  resl=c(mean(alp1),mean(alp2),mean(lambda))
  bias=c(mean(alp1)-truealpha1,mean(alp2)-truealpha2,mean(lambda)-truelambda)
  mse=c(mean((alp1-truealpha1)^2),mean((alp2-truealpha2)^2),mean((lambda-truelambda)^2))
  interval_95=c(interval(alp1,0.95),interval(alp2,0.95),interval(lambda,0.95))
  ac=acf(rawdata)
  acs=(2*sum(ac$acf)+1)
  acs1=sqrt(var(alp1)*acs/length(rawdata))
  acs2=sqrt(var(alp2)*acs/length(rawdata))
  acs3=sqrt(var(lambda)*acs/length(rawdata))
  acs4=c(acs1,acs2,acs3)
  rrr=list(name_result=resl,name_bias=bias,name_mse=mse,name_95interval=interval_95,name_MCerrors=acs4)
  return(rrr)
}


rdiscussion=function(rdata,realr){
  result1=mean(rdata)
  result2=sd(rdata)
  result3=median(rdata)
  result4=sqrt(((rdata-result3)^2)/length(rdata))
  result5=sum(rdata==realr)/length(rdata)
  result6=list(name_mean=result1,name_meansd=result2,
               name_median=result3,name_mediansd=result4,
               name_percentages=result5)
  return(result6)
}


resultextract=function(results,burn_in,t){
  num=length(results)/(3*(burn_in+t))
  alpha11=numeric(num)
  alpha22=numeric(num)
  lambdaaa=numeric(num)
  for (i in 1:num){
    aa=results[(3*(i-1)*(burn_in+t)+1):(3*i*(burn_in+t))]
    alpha11[i]=mean(aa[(burn_in+1):(burn_in+t)])
    alpha22[i]=mean(aa[(2*burn_in+t+1):(2*burn_in+2*t)])
    lambdaaa[i]=mean(aa[(3*burn_in+2*t+1):(3*burn_in+3*t)])
  }
  resulttt=c(alpha11,alpha22,lambdaaa)
  return(resulttt)
}
