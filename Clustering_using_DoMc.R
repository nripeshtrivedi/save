library(foreach)
library(doMC)
library(doParallel)
library(ROCR)
library(cluster)
library(kernlab)
cluster_size<-size(of_sm2_1_0.001)
clusts <-sort(unique(of_sm2_1_0.001))
sigma_1<-0
sigma_2<-0
sigma<-0.001
s<-rep(-2,length(of_sm2_1_0.001))
stopCluster(cl)
cl<-makeCluster(37)
registerDoParallel(cl)
nvalues<-foreach(i = 1:length(of_sm2_1_0.001),.packages='kernlab') %dopar% my(i)

my<-function(i)
{
  cluster_size<-size(of_sm2_1_0.001)
  clusts <-sort(unique(of_sm2_1_0.001))
  sigma_1<-0
  sigma_2<-0
  sigma<-0.001
  temp<-rep(200000000000,length(clusts)-1)
  for(j in 1:length(of_sm2_1_0.001))
  {
    if((of_sm2_1_0.001[j]==of_sm2_1_0.001[i])&&(i!=j))
    {
      sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
      sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma)
    }
  }   
  a<-(exp(-(dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma))+sigma_1+sigma_2)/cluster_size[of_sm2_1_0.001[i]]
  sigma_1<-0
  sigma_2<-0
  for(k in 1:length(clusts))
  {
    if((k==of_sm2_1_0.001[i]))
    {
      next();
    }
    else
    {
      for(j in 1:length(of_sm2_1_0.001))
      {
        if(of_sm2_1_0.001[j]==k)
        {
          sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
          sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma) 
        }
      }
    }
    
    if(!(k==of_sm2_1_0.001[i]))
      temp[k]<-(exp(-dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma)+sigma_1+sigma_2)/cluster_size[k]
    sigma_1<-0
    sigma_2<-0
  }
  b<-min(temp)
  return(b-a[1])/max(b,a[1])
}






library(foreach)
library(doMC)
library(doParallel)
library(ROCR)
library(cluster)
library(kernlab)
cluster_size<-size(of_sm5_1_0.001)
clusts <-sort(unique(of_sm5_1_0.001))
sigma_1<-0
sigma_2<-0
sigma<-0.001
s<-rep(-2,length(of_sm5_1_0.001))
stopCluster(cl)
cl<-makeCluster(37)
registerDoParallel(cl)
nvalues3<-foreach(i = 1:length(of_sm5_1_0.001),.packages='kernlab') %dopar% mys(i)

mys<-function(i)
{
  cluster_size<-size(of_sm5_1_0.001)
  clusts <-sort(unique(of_sm5_1_0.001))
  sigma_1<-0
  sigma_2<-0
  sigma<-0.001
  temp<-rep(200000000000,length(clusts)-1)
  for(j in 1:length(of_sm5_1_0.001))
  {
    if((of_sm5_1_0.001[j]==of_sm5_1_0.001[i])&&(i!=j))
    {
      sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
      sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma)
    }
  }   
  a<-(exp(-(dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma))+sigma_1+sigma_2)/cluster_size[of_sm5_1_0.001[i]]
  sigma_1<-0
  sigma_2<-0
  for(k in 1:length(clusts))
  {
    if((k==of_sm5_1_0.001[i]))
    {
      next();
    }
    else
    {
      for(j in 1:length(of_sm5_1_0.001))
      {
        if(of_sm5_1_0.001[j]==k)
        {
          sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
          sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma) 
        }
      }
    }
    
    if(!(k==of_sm5_1_0.001[i]))
      temp[k]<-(exp(-dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma)+sigma_1+sigma_2)/cluster_size[k]
    sigma_1<-0
    sigma_2<-0
  }
  b<-min(temp)
  return(b-a[1])/max(b,a[1])
}



registerDoSEQ()

library(foreach)
library(doMC)
library(doParallel)
library(ROCR)
library(cluster)
library(kernlab)
cluster_size<-size(of_sm6_1_0.001)
clusts <-sort(unique(of_sm6_1_0.001))
sigma_1<-0
sigma_2<-0
sigma<-0.001
s<-rep(-2,length(of_sm6_1_0.001))
stopCluster(cl)
cl<-makeCluster(37)
registerDoParallel(cl)
nvalues5<-foreach(i = 1:length(of_sm6_1_0.001),.packages='kernlab') %dopar% myb(i)

myb<-function(i)
{
  cluster_size<-size(of_sm6_1_0.001)
  clusts <-sort(unique(of_sm6_1_0.001))
  sigma_1<-0
  sigma_2<-0
  sigma<-0.001
  temp<-rep(200000000000,length(clusts)-1)
  for(j in 1:length(of_sm6_1_0.001))
  {
    if((of_sm6_1_0.001[j]==of_sm6_1_0.001[i])&&(i!=j))
    {
      sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
      sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma)
    }
  }   
  a<-(exp(-(dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma))+sigma_1+sigma_2)/cluster_size[of_sm6_1_0.001[i]]
  sigma_1<-0
  sigma_2<-0
  for(k in 1:length(clusts))
  {
    if((k==of_sm6_1_0.001[i]))
    {
      next();
    }
    else
    {
      for(j in 1:length(of_sm6_1_0.001))
      {
        if(of_sm6_1_0.001[j]==k)
        {
          sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
          sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma) 
        }
      }
    }
    
    if(!(k==of_sm6_1_0.001[i]))
      temp[k]<-(exp(-dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma)+sigma_1+sigma_2)/cluster_size[k]
    sigma_1<-0
    sigma_2<-0
  }
  b<-min(temp)
  return(b-a[1])/max(b,a[1])
}



registerDoSEQ()

library(foreach)
library(doMC)
library(doParallel)
library(ROCR)
library(cluster)
library(kernlab)
cluster_size<-size(of_sm_2_0.001)
clusts <-sort(unique(of_sm_2_0.001))
sigma_1<-0
sigma_2<-0
sigma<-0.001
s<-rep(-2,length(of_sm_2_0.001))
stopCluster(cl)
cl<-makeCluster(37)
registerDoParallel(cl)
nvalues6<-foreach(i = 1:length(of_sm_2_0.001),.packages='kernlab') %dopar% mya(i)

mya<-function(i)
{
  cluster_size<-size(of_sm_2_0.001)
  clusts <-sort(unique(of_sm_2_0.001))
  sigma_1<-0
  sigma_2<-0
  sigma<-0.001
  temp<-rep(200000000000,length(clusts)-1)
  for(j in 1:length(of_sm_2_0.001))
  {
    if((of_sm_2_0.001[j]==of_sm_2_0.001[i])&&(i!=j))
    {
      sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
      sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma)
    }
  }   
  a<-(exp(-(dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma))+sigma_1+sigma_2)/cluster_size[of_sm_2_0.001[i]]
  sigma_1<-0
  sigma_2<-0
  for(k in 1:length(clusts))
  {
    if((k==of_sm_2_0.001[i]))
    {
      next();
    }
    else
    {
      for(j in 1:length(of_sm_2_0.001))
      {
        if(of_sm_2_0.001[j]==k)
        {
          sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
          sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma) 
        }
      }
    }
    
    if(!(k==of_sm_2_0.001[i]))
      temp[k]<-(exp(-dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma)+sigma_1+sigma_2)/cluster_size[k]
    sigma_1<-0
    sigma_2<-0
  }
  b<-min(temp)
  return(b-a[1])/max(b,a[1])
}



registerDoSEQ()
registerDoSEQ()

library(foreach)
library(doMC)
library(doParallel)
library(ROCR)
library(cluster)
library(kernlab)
cluster_size<-size(of_sm3_1_0.001)
clusts <-sort(unique(of_sm3_1_0.001))
sigma_1<-0
sigma_2<-0
sigma<-0.001
s<-rep(-2,length(of_sm3_1_0.001))
stopCluster(cl)
cl<-makeCluster(37)
registerDoParallel(cl)
nvalues1<-foreach(i = 1:length(of_sm3_1_0.001),.packages='kernlab') %dopar% myq(i)

myq<-function(i)
{
  cluster_size<-size(of_sm3_1_0.001)
  clusts <-sort(unique(of_sm3_1_0.001))
  sigma_1<-0
  sigma_2<-0
  sigma<-0.001
  temp<-rep(200000000000,length(clusts)-1)
  for(j in 1:length(of_sm3_1_0.001))
  {
    if((of_sm3_1_0.001[j]==of_sm3_1_0.001[i])&&(i!=j))
    {
      sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
      sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma)
    }
  }   
  a<-(exp(-(dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma))+sigma_1+sigma_2)/cluster_size[of_sm3_1_0.001[i]]
  sigma_1<-0
  sigma_2<-0
  for(k in 1:length(clusts))
  {
    if((k==of_sm3_1_0.001[i]))
    {
      next();
    }
    else
    {
      for(j in 1:length(of_sm3_1_0.001))
      {
        if(of_sm3_1_0.001[j]==k)
        {
          sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
          sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma) 
        }
      }
    }
    
    if(!(k==of_sm3_1_0.001[i]))
      temp[k]<-(exp(-dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma)+sigma_1+sigma_2)/cluster_size[k]
    sigma_1<-0
    sigma_2<-0
  }
  b<-min(temp)
  return(b-a[1])/max(b,a[1])
}



registerDoSEQ()






library(foreach)
library(doMC)
library(doParallel)
library(ROCR)
library(cluster)
library(kernlab)
cluster_size<-size(of_sm4_1_0.001)
clusts <-sort(unique(of_sm4_1_0.001))
sigma_1<-0
sigma_2<-0
sigma<-0.001
s<-rep(-2,length(of_sm4_1_0.001))
stopCluster(cl)
cl<-makeCluster(37)
registerDoParallel(cl)
nvalues2<-foreach(i = 1:length(of_sm4_1_0.001),.packages='kernlab') %dopar% myp(i)

myp<-function(i)
{
  cluster_size<-size(of_sm4_1_0.001)
  clusts <-sort(unique(of_sm4_1_0.001))
  sigma_1<-0
  sigma_2<-0
  sigma<-0.001
  temp<-rep(200000000000,length(clusts)-1)
  for(j in 1:length(of_sm4_1_0.001))
  {
    if((of_sm4_1_0.001[j]==of_sm4_1_0.001[i])&&(i!=j))
    {
      sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
      sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma)
    }
  }   
  a<-(exp(-(dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma))+sigma_1+sigma_2)/cluster_size[of_sm4_1_0.001[i]]
  sigma_1<-0
  sigma_2<-0
  for(k in 1:length(clusts))
  {
    if((k==of_sm4_1_0.001[i]))
    {
      next();
    }
    else
    {
      for(j in 1:length(of_sm4_1_0.001))
      {
        if(of_sm4_1_0.001[j]==k)
        {
          sigma_1<-sigma_1-2*exp(-dist(rbind(Part_matrix[i,],Part_matrix[j,]))/sigma*sigma)
          sigma_2<-sigma_2+exp(-dist(rbind(Part_matrix[j,],Part_matrix[j,]))/sigma*sigma) 
        }
      }
    }
    
    if(!(k==of_sm4_1_0.001[i]))
      temp[k]<-(exp(-dist(rbind(Part_matrix[i,],Part_matrix[i,]))/sigma*sigma)+sigma_1+sigma_2)/cluster_size[k]
    sigma_1<-0
    sigma_2<-0
  }
  b<-min(temp)
  return(b-a[1])/max(b,a[1])
}



registerDoSEQ()

