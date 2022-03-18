
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


