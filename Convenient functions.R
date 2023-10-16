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
