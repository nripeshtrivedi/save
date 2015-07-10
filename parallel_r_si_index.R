library(foreach)
library(doMC)
library(doParallel)
library(ROCR)
library(cluster)
library(kernlab)
cluster_size<-size(of_sm2_1_0.009)
clusts <-sort(unique(of_sm2_1_0.009))
sigma_1<-0
sigma_2<-0
sigma<-0.009
s<-rep(-2,length(of_sm2_1_0.009))
stopCluster(cl)
cl<-makeCluster(37)
registerDoParallel(cl)
nvalues1<-foreach(i = 1:length(of_sm2_1_0.009),.packages='kernlab') %dopar% my(i)

my<-function(i)
{
  cluster_size<-size(of_sm2_1_0.009)
  clusts <-sort(unique(of_sm2_1_0.009))
  sigma_1<-0
  sigma_2<-0
  sigma<-0.009
  temp<-rep(200000000000,length(clusts)-1)
  for(j in 1:length(of_sm2_1_0.009))
  {
    if((of_sm2_1_0.009[j]==of_sm2_1_0.009[i])&&(i!=j))
    {
      sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
      sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma)
    }
  }   
  a<-(exp(-(dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma))+sigma_1+sigma_2)/cluster_size[of_sm2_1_0.009[i]]
  sigma_1<-0
  sigma_2<-0
  for(k in 1:length(clusts))
  {
    if((k==of_sm2_1_0.009[i]))
    {
      next();
    }
    else
    {
      for(j in 1:length(of_sm2_1_0.009))
      {
        if(of_sm2_1_0.009[j]==k)
        {
          sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
          sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma) 
        }
      }
    }
    
    if(!(k==of_sm2_1_0.009[i]))
      temp[k]<-(exp(-dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma)+sigma_1+sigma_2)/cluster_size[k]
    sigma_1<-0
    sigma_2<-0
  }
  b<-min(temp)
  return(b-a[1])/max(b,a[1])
}



registerDoSEQ()


