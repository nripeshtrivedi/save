library(foreach)
library(doParallel)
library(ROCR)
library(cluster)
library(kernlab)
cluster_size<-size(sm6)
clusts <-sort(unique(sm6))
sigma_1<-0
sigma_2<-0
sigma<-0.00782


s<-rep(-2,length(sm6))
cl<-makeCluster(8)
stopCluster(cl)
registerDoParallel(cl)
values<-foreach(i = 1:1000,.combine=c) %dopar% my(i)

my<-function(i)
{
  print(i)
  library(kernlab)
  cluster_size<-size(sm6)
  clusts <-sort(unique(sm6))
  sigma_1<-0
  sigma_2<-0
  sigma<-0.00782
  temp<-rep(200000000,length(clusts)-1)
  for(j in 1:length(sm6))
  {
    if((sm6[j]==sm6[i])&&(i!=j))
    {
      sigma_1<-sigma_1-2*exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      sigma_2<-sigma_2+exp(-dist(rbind(Allpara_nazero[j,],Allpara_nazero[j,]))/sigma*sigma)
    }
  }   
  a<-(exp(-(dist(rbind(Allpara_nazero[i,],Allpara_nazero[i,]))/sigma*sigma))+sigma_1+sigma_2)/cluster_size[sm6[i]]
  sigma_1<-0
  sigma_2<-0
  for(k in 1:length(clusts))
  {
    if((k==sm6[i]))
    {
      next();
    }
    else
    {
      for(j in 1:length(sm6))
      {
        if(sm6[j]==k)
        {
          sigma_1<-sigma_1-2*exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
          sigma_2<-sigma_2+exp(-dist(rbind(Allpara_nazero[j,],Allpara_nazero[j,]))/sigma*sigma) 
        }
      }
    }
    
    if(!(k==sm6[i]))
      temp[k]<-(exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[i,]))/sigma*sigma)+sigma_1+sigma_2)/cluster_size[k]
    sigma_1<-0
    sigma_2<-0
  }
  b<-min(temp)
  return(b-a[1])/max(b,a[1])
}



