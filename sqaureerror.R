library(cluster)
library(kernlab)
library(foreach)
clusts <-sort(unique(sm8))
sigma_1<-0
sigma_2<-0
sigma<-0.00782
sum<-rep(-2,length(clusts))
cluster_sum<-rep(-2,length(clusts))
totalsum<-rep(-2,length(clusts))
square_error<-rep(-2,length(clusts))
cluster_size<-size(sm8)
for(i in 1:length(sm8))
{
  print(i)
  for(j in 1:length(sm8))
  {
    if(i!=j)
    {
      if((sm8[i]==1)&&(sm8[j]==1))
      {
        sum[1]=sum[1]+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==2)&&(sm8[j]==2))
      {
        sum[2]=sum[2]+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==3)&&(sm8[j]==3))
      {
        sum[3]=sum[3]+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==4)&&(sm8[j]==4))
      {
        sum[4]=sum[4]+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==5)&&(sm8[j]==5))
      {
        sum[5]=sum[5]+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==6)&&(sm8[j]==6))
      {
        sum[6]=sum[6]+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==7)&&(sm8[j]==7))
      {
        sum[7]=sum[7]+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==8)&&(sm8[j]==8))
      {
        sum[8]=sum[8]+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
      if((sm8[i]==9)&&(sm8[j]==9))
      {
        sum[9]=sum[9]+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }
     
      
      if((sm8[i]==sm8[j])&&(i!=j))
      {
        cluster_sum[sm8[j]]<-cluster_sum[sm8[j]]+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      }

}
}
totalsum[sm8[i]]<-totalsum[sm8[i]]+exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[i,]))/sigma*sigma)
}
for(count in 1:9)
  square_error[count]<-totalsum[count]+sum[count]+cluster_sum[count]
plot(square_error)

