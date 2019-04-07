library(progress)
y=cif
frequency=12
rows= dim(y)[1]
col = dim(y)[2]
pb <- progress_bar$new(total = rows)
per = 0.25
stat_length = as.integer((col-3)*per) #for cif
#stat_length = as.integer((col)*per)
final_comb = matrix(,nrow=1,ncol=(col-3-stat_length)) #-----change
for (i in 1:rows) {
  
  
  data = y[i,3:col] #----change
  tb=table(is.na(data))
  length = tb[["FALSE"]]
  data = data[,1:length]
  d_l = dim(data)[2]
  s_l =as.integer(d_l*per)
  
  data_fit  = data[,s_l:(2*s_l)]
  data_nn = data[,(2*s_l+1):d_l]
  
  horizon = 1
  out = FourTheta(ts(t(data_fit),frequency=frequency),horizon) #----change
  temp = t(as.numeric(out$mean))
  len = length(data_nn)
  #mat = matrix(,nrow=1,ncol=len)
  
  loop = len%/%horizon
  rem = len%%horizon
  for (z in 1:(loop)){
    data_fit = data[,1:s_l+(z*horizon)]
    out=FourTheta(ts(t(data_fit),frequency=frequency),horizon) #-----change
    temp = cbind(temp,t(as.numeric(out$mean)))
    
  }
  final = t(temp[1:len])
  
  # final = cbind(t(fitted),t(forecast))
  #final=t(fitted)
  temp_1 = length(final)
  tmp1 = col-3-temp_1-stat_length#--------change
  if (tmp1 != 0){
    mat = matrix(,nrow=1,ncol=tmp1)
    final1 = cbind(final,mat)
  } else {
    final1 = final
  }
  #mat = matrix(,nrow=1,ncol=tmp1)
  # final1 = cbind(final,mat)
  final_comb = rbind(final_comb,final1)
  #print(i)
  pb$tick()
}
write.csv(final_comb,"/home/chatta/ICML/CIF2016/theta_50_h1.csv") #need to add path
