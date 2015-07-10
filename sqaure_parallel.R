library(cluster)
library(kernlab)
library(foreach)
library(doParallel)
clusts <-sort(unique(sm8))
sigma_1<-0
sigma_2<-0
a<-rep(0,1)
b<-rep(0,1)
c<-rep(0,1)
d<-0
sigma<-0.00782
sum<-rep(-2,length(clusts))
cluster_sum<-rep(-2,length(clusts))
totalsum<-rep(-2,length(clusts))
square_error<-rep(-2,length(clusts))
cluster_size<-size(sm8)
cl<-makeCluster(8)
stopCluster(cl)
registerDoParallel(cl)
var1<-foreach(i = 1:length(sm8),.combine=c) %dopar% my(i)

  my<-function(i)
  {
  a<-0
  for(j in 1:length(sm8))
  {
    if(i!=j)
    {
      if((sm8[i]==1)&&(sm8[j]==1))
      {
        a=a+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==2)&&(sm8[j]==2))
      {
        a=a+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==3)&&(sm8[j]==3))
      {
        a=a+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==4)&&(sm8[j]==4))
      {
        a=a+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==5)&&(sm8[j]==5))
      {
        a=a+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==6)&&(sm8[j]==6))
      {
        a=a+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==7)&&(sm8[j]==7))
      {
        a=a+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==8)&&(sm8[j]==8))
      {
        a=a+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==9)&&(sm8[j]==9))
      {
        a=a+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
    }
  }
  return (a)
}
var2<-foreach(i = 1:10000,.combine=c) %dopar% m(i)
    m<-function(i)
    {
      b<-0
      for(j in 1:length(sm8))
      {
      if((sm8[i]==sm8[j])&&(i!=j))
      {
        b<-b+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      
    }
    return (b)
  }
var3<-foreach(i = 1:10000,.combine=c) %dopar% n(i)

n<-function(i)
{
  c<-exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[i,]))/sigma*sigma)
  return (c)
}
for(i in 1:10000)
{
sum[sm8[i]]<-sum[sm8[i]]+var1[i]
 cluster_sum[sm8[j]]<-cluster_sum[sm8[j]]+var2[i]
 totalsum[sm8[i]]<-totalsum[sm8[i]]+var3[i]
}
for(count in 1:9)
{
  sum[count]<-sum[count]/cluster_size[i]
  cluster_sum[count]<-cluster_sum[count]/(cluster_size[i]*cluster_size[i])
}
for(count in 1:9)
  square_error[count]<-totalsum[count]-2*sum[count]+cluster_sum[count]
plot(square_error)
rm(square_error)
