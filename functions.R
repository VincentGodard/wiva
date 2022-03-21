
# convert valley lines segments to center points with width 
process_lines <- function(ll){
for (i in 1:length(ll)){
#  print(i)
  tmp2 = geom(ll[i],df=T)
  if (nrow(tmp2)!=2){stop(paste("number of vertexes not equal to 2, element #",i))}
  ll$X[i] = mean(tmp2$x)
  ll$Y[i] = mean(tmp2$y)
  ll$w[i] = sqrt((tmp2$x[1]-tmp2$x[2])^2+(tmp2$y[1]-tmp2$y[2])^2)
}
tmp3 =as.data.frame(ll)
tmp4 = vect(cbind(tmp3$X,tmp3$Y),atts=tmp3,crs=crs(ll))
return(tmp4)
}


get_st_values <-function(pts,df_st){
for (i in 1:length(pts)){
pt = as.data.frame(geom(pts[i]))
df_st$d = sqrt((pt$x-df_st$x)^2+(pt$y-df_st$y)^2)
df_st_min = df_st[which.min(df_st$d),]
df_st_min$i = i
if(i==1){df_st_pts=df_st_min}else{df_st_pts=rbind(df_st_pts,df_st_min)}
}
colnames(df_st_pts)[which(names(df_st_pts) == "x")] <- "x_st"
colnames(df_st_pts)[which(names(df_st_pts) == "y")] <- "y_st"
tmp2 = cbind(as.data.frame(pts),df_st_pts)
pts2 = vect(cbind(tmp2$X,tmp2$Y),type="points",atts=tmp2,crs=crs(pts))
return(pts2)
}


get_qq_threshold <-function(rast,fact_red=10,plot=FALSE){
  data = na.omit(as.vector(rast))
  data = sort(data) # ordering
  data_sp = data[seq_along(data)%%fact_red==0] # undersampling
  qq = qqnorm(data,type="l",lwd=3,plot.it = FALSE)
  qq_sp = qqnorm(data_sp,type="l",lwd=3,plot.it = FALSE)
  
  #
  probs = c(0.25,0.75)
  vp <- quantile(data, probs = probs)
  lm1 <- lm(vp~qnorm(probs))
  # difference
  b = qq$x*lm1$coefficients[2]+lm1$coefficients[1]
  diff = abs((b-qq$y)/b)
  #first value
  iflag = which(diff <0.01)[1]
  val = qq$y[iflag]
  #
  if (plot){
    plot(qq_sp$x,qq_sp$y,type="l",lwd=3,
         main=paste("Threshold=",round(val,2)),
         xlab="Theoretical quantiles",ylab="Samples quantiles")
    grid()
    abline(lm1,col="red",lty=2)
    abline(v=qq$x[iflag],col="blue")
    points(qnorm(probs),vp,pch=21,bg="white")
  }
  return(val)
} 


