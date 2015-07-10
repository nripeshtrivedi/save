clusts <-sort(unique(nsm6))
sigma_1<-0
sigma_2<-0
sigma<-0.0055
s<-rep(-2,length(nsm6))
cluster_size<-size(nsm6)
for(i in 1:length(nsm6))
{
  print(i)
  temp<-rep(200000000,length(clusts)-1)
  for(j in 1:length(nsm6))
  {
    if((nsm6[j]==nsm6[i])&&(i!=j))
    {
      sigma_1<-sigma_1-2*exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      sigma_2<-sigma_2+exp(-dist(rbind(Allpara_nazero[j,],Allpara_nazero[j,]))/sigma*sigma)
    }
  }   
  a<-(exp(-(dist(rbind(Allpara_nazero[i,],Allpara_nazero[i,]))/sigma*sigma))+sigma_1+sigma_2)/cluster_size[nsm6[i]]
  sigma_1<-0
  sigma_2<-0
  for(k in 1:length(clusts))
  {
    if((k==nsm6[i]))
    {
      next();
    }
    else
    {
      for(j in 1:length(nsm6))
      {
        if(nsm6[j]==k)
        {
          sigma_1<-sigma_1-2*exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
          sigma_2<-sigma_2+exp(-dist(rbind(Allpara_nazero[j,],Allpara_nazero[j,]))/sigma*sigma) 
        }
      }
    }
    
    if(!(k==nsm6[i]))
      temp[k]<-(exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[i,]))/sigma*sigma)+sigma_1+sigma_2)/cluster_size[k]
    sigma_1<-0
    sigma_2<-0
  }
  b<-min(temp)
  s[i]<-(b-a[1])/max(b,a[1])
  
}



