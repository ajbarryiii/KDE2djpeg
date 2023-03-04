
library(stats)
library(jpeg)

#read image into r
img<-readJPEG()
#2d euclidean distance from x for every item in vector y
d2d<-function(x,y){
  distance<-NULL
  for(i in 1:dim(y)[1]){
    distance[i]<- sqrt(sum((x-y[i,])^2))
  }
  return(distance)
}

#function for image smoothing, im<-image file, h<-smoothing parameter, b<-radius of the block (wrt l infinty norm) 
smimg<-function(im,h,b){
  #initialize coordinates
  d<-dim(im)[1]
  f<-matrix(nrow = d, ncol = d)
  
  for(i in 1:d){
    for(l in 1:d){
      #generate x,y in block size b
      X<-expand.grid(one = 1, x = c(max(1, i-b):min(d,i+b)), y = c(max(1, l-b):min(d,l+b)))
      Y<-NULL
      for(j in 1:length(X[,1])){
        #load in the y values
        Y[j]<-im[X[j,2], X[j,3]]
      }
      X<-as.matrix(X)
      dis<-d2d(c(i,l), X[,2:3])
      #make matrix w
      W<-diag(dnorm(dis/h))
      #compute the predicted value of the image at the i,l coordinate
      f[i,l]<-(X%*%solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%Y)[which((X[,2]==i)&(X[,3]==l))]
      
    }}
  return(f)