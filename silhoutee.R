library(ggplot2)
library(cluster)
nc<-10
  rm(coeffs,clusts)
  coeffs<-0
  clusts <- sort(unique(sm_1))
  
 
  for(i in 1:length(sm_1)){
    cdist <- rep(0,length(clusts))
    for(j in clusts){
      cdist[j] <- dist(rbind((centers(sm_1)[j,]),Allpara_nazero[i,]))
    }
    a <- cdist[sm_1[i]]
    b <- min(cdist[clusts != sm_1[i]])
    coeffs[i] <- (b-a) / max(b,a)
    
  
    
  }
  meanCoeff[1]<-mean(coeffs)
  rm(coeffs,clusts)
  coeffs<-0
  clusts <- sort(unique(sm2_1))
 
  for(i in 1:length(sm2_1)){
    cdist <- rep(0,length(clusts))
    for(j in clusts){
      cdist[j] <- dist(rbind((centers(sm2_1)[j,]),Allpara_nazero[i,]))
    }
    a <- cdist[sm2_1[i]]
    b <- min(cdist[clusts != sm2_1[i]])
    coeffs[i] <- (b-a) / max(b,a)
  
  }
  
  
  meanCoeff[2]<-mean(coeffs)
  rm(coeffs,clusts)
  coeffs<-0
 clusts <- sort(unique(sm3_1))

  for(i in 1:length(sm3_1)){
    cdist <- rep(0,length(clusts))
    for(j in clusts){
      cdist[j] <- dist(rbind((centers(sm3_1)[j,]),Allpara_nazero[i,]))
    }
    a<- cdist[sm3_1[i]]
    b <- min(cdist[clusts != sm3_1[i]])
    coeffs[i] <- (b-a) / max(b,a)

  }
  
  meanCoeff[3]<-mean(coeffs)
  rm(coeffs,clusts)
  coeffs<-0
  clusts <- sort(unique(sm4_1))
 
  for(i in 1:length(sm4_1)){
    cdist <- rep(0,length(clusts))
    for(j in clusts){
      cdist[j] <- dist(rbind((centers(sm4_1)[j,]),Allpara_nazero[i,]))
    }
    a <- cdist[sm4_1[i]]
    b <- min(cdist[clusts != sm4_1[i]])
    coeffs[i] <- (b-a) / max(b,a)

  }
  meanCoeff[5]<-mean(coeffs)
  rm(coeffs,clusts)
  coeffs<-0
  clusts <- sort(unique(sm5_1))
  
  for(i in 1:length(sm5_1)){
    cdist <- rep(0,length(clusts))
    for(j in clusts){
      cdist[j] <- dist(rbind((centers(sm5_1)[j,]),Allpara_nazero[i,]))
    }
    a <- cdist[sm5_1[i]]
    b <- min(cdist[clusts != sm5_1[i]])
    coeffs[i] <- (b-a) / max(b,a)
 
    
  }
  meanCoeff[6]<-mean(coeffs)
  rm(coeffs,clusts)
  coeffs<-0
  clusts <- sort(unique(sm6_1))
 
  for(i in 1:length(sm6_1)){
    cdist <- rep(0,length(clusts))
    for(j in clusts){
      cdist[j] <- dist(rbind((centers(sm6_1)[j,]),Allpara_nazero[i,]))
    }
    a <- cdist[sm6_1[i]]
    b <- min(cdist[clusts != sm6_1[i]])
    coeffs[i] <- (b-a) / max(b,a)
 
    
  }
  meanCoeff[7]<-mean(coeffs)
  rm(coeffs,clusts)
  coeffs<-0
  clusts <- sort(unique(sm7_1))
 
  for(i in 1:length(sm7_1)){
    cdist <- rep(0,length(clusts))
    for(j in clusts){
      cdist[j] <- dist(rbind((centers(sm7_1)[j,]),Allpara_nazero[i,]))
    }
    a <- cdist[sm7_1[i]]
    b <- min(cdist[clusts != sm7_1[i]])
    coeffs[i] <- (b-a) / max(b,a)

  }
  meanCoeff[8]<-mean(coeffs)
  rm(coeffs,clusts)
  coeffs<-0
  clusts <- sort(unique(sm8_1))
 
  for(i in 1:length(sm8_1)){
    cdist <- rep(0,length(clusts))
    for(j in clusts){
      cdist[j] <- dist(rbind((centers(sm8_1)[j,]),Allpara_nazero[i,]))
    }
    a <- cdist[sm8_1[i]]
    b <- min(cdist[clusts != sm8_1[i]])
    coeffs[i] <- (b-a) / max(b,a)
 
  
  }
  meanCoeff[9]<-mean(coeffs)
  mean_mean_Coeff<-rep(-2,length(c(1:6)))
 mean_mean_Coeff[1]<-mean(meanCoeff)
 
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm_0.5))
 
 
 for(i in 1:length(sm_0.5)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm_0.5)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm_0.5[i]]
   b <- min(cdist[clusts != sm_0.5[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
   
 }
 meanCoeff[1]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm2_0.5))
 
 for(i in 1:length(sm2_0.5)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm2_0.5)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm2_0.5[i]]
   b <- min(cdist[clusts != sm2_0.5[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 
 
 meanCoeff[2]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm3_0.5))
 
 for(i in 1:length(sm3_0.5)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm3_0.5)[j,]),Allpara_nazero[i,]))
   }
   a<- cdist[sm3_0.5[i]]
   b <- min(cdist[clusts != sm3_0.5[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 
 meanCoeff[3]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm4_0.5))
 
 for(i in 1:length(sm4_0.5)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm4_0.5)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm4_0.5[i]]
   b <- min(cdist[clusts != sm4_0.5[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 meanCoeff[5]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm5_0.5))
 
 for(i in 1:length(sm5_0.5)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm5_0.5)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm5_0.5[i]]
   b <- min(cdist[clusts != sm5_0.5[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[6]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm6_0.5))
 
 for(i in 1:length(sm6_0.5)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm6_0.5)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm6_0.5[i]]
   b <- min(cdist[clusts != sm6_0.5[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[7]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm7_0.5))
 
 for(i in 1:length(sm7_0.5)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm7_0.5)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm7_0.5[i]]
   b <- min(cdist[clusts != sm7_0.5[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 meanCoeff[8]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm8_0.5))
 
 for(i in 1:length(sm8_0.5)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm8_0.5)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm8_0.5[i]]
   b <- min(cdist[clusts != sm8_0.5[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[9]<-mean(coeffs)
 mean_mean_Coeff[2]<-mean(meanCoeff)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm_0.1))
 
 
 for(i in 1:length(sm_0.1)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm_0.1)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm_0.1[i]]
   b <- min(cdist[clusts != sm_0.1[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
   
 }
 meanCoeff[1]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm2_0.1))
 
 for(i in 1:length(sm2_0.1)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm2_0.1)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm2_0.1[i]]
   b <- min(cdist[clusts != sm2_0.1[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 
 
 meanCoeff[2]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm3_0.1))
 
 for(i in 1:length(sm3_0.1)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm3_0.1)[j,]),Allpara_nazero[i,]))
   }
   a<- cdist[sm3_0.1[i]]
   b <- min(cdist[clusts != sm3_0.1[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 
 meanCoeff[3]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm4_0.1))
 
 for(i in 1:length(sm4_0.1)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm4_0.1)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm4_0.1[i]]
   b <- min(cdist[clusts != sm4_0.1[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 meanCoeff[5]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm5_0.1))
 
 for(i in 1:length(sm5_0.1)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm5_0.1)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm5_0.1[i]]
   b <- min(cdist[clusts != sm5_0.1[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[6]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm6_0.1))
 
 for(i in 1:length(sm6_0.1)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm6_0.1)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm6_0.1[i]]
   b <- min(cdist[clusts != sm6_0.1[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[7]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm7_0.1))
 
 for(i in 1:length(sm7_0.1)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm7_0.1)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm7_0.1[i]]
   b <- min(cdist[clusts != sm7_0.1[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 meanCoeff[8]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm8_0.1))
 
 for(i in 1:length(sm8_0.1)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm8_0.1)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm8_0.1[i]]
   b <- min(cdist[clusts != sm8_0.1[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[9]<-mean(coeffs)
 mean_mean_Coeff[3]<-mean(meanCoeff)
 
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm_0.05))
 
 
 for(i in 1:length(sm_0.05)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm_0.05)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm_0.05[i]]
   b <- min(cdist[clusts != sm_0.05[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
   
 }
 meanCoeff[1]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm2_0.05))
 
 for(i in 1:length(sm2_0.05)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm2_0.05)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm2_0.05[i]]
   b <- min(cdist[clusts != sm2_0.05[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 
 
 meanCoeff[2]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm3_0.05))
 
 for(i in 1:length(sm3_0.05)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm3_0.05)[j,]),Allpara_nazero[i,]))
   }
   a<- cdist[sm3_0.05[i]]
   b <- min(cdist[clusts != sm3_0.05[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 
 meanCoeff[3]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm4_0.05))
 
 for(i in 1:length(sm4_0.05)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm4_0.05)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm4_0.05[i]]
   b <- min(cdist[clusts != sm4_0.05[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 meanCoeff[5]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm5_0.05))
 
 for(i in 1:length(sm5_0.05)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm5_0.05)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm5_0.05[i]]
   b <- min(cdist[clusts != sm5_0.05[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[6]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm6_0.05))
 
 for(i in 1:length(sm6_0.05)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm6_0.05)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm6_0.05[i]]
   b <- min(cdist[clusts != sm6_0.05[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[7]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm7_0.05))
 
 for(i in 1:length(sm7_0.05)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm7_0.05)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm7_0.05[i]]
   b <- min(cdist[clusts != sm7_0.05[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 meanCoeff[8]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm8_0.05))
 
 for(i in 1:length(sm8_0.05)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm8_0.05)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm8_0.05[i]]
   b <- min(cdist[clusts != sm8_0.05[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[9]<-mean(coeffs)
 mean_mean_Coeff[4]<-mean(meanCoeff)
 
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(nsm))
 
 
 for(i in 1:length(nsm)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(nsm)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[nsm[i]]
   b <- min(cdist[clusts != nsm[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
   
 }
 meanCoeff[1]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(nsm2))
 
 for(i in 1:length(nsm2)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(nsm2)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[nsm2[i]]
   b <- min(cdist[clusts != nsm2[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 
 
 meanCoeff[2]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(nsm3))
 
 for(i in 1:length(nsm3)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(nsm3)[j,]),Allpara_nazero[i,]))
   }
   a<- cdist[nsm3[i]]
   b <- min(cdist[clusts != nsm3[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 
 meanCoeff[3]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(nsm4))
 
 for(i in 1:length(nsm4)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(nsm4)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[nsm4[i]]
   b <- min(cdist[clusts != nsm4[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 meanCoeff[5]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(nsm5))
 
 for(i in 1:length(nsm5)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(nsm5)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[nsm5[i]]
   b <- min(cdist[clusts != nsm5[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[6]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(nsm6))
 
 for(i in 1:length(nsm6)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(nsm6)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[nsm6[i]]
   b <- min(cdist[clusts != nsm6[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[7]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(nsm7))
 
 for(i in 1:length(nsm7)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(nsm7)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[nsm7[i]]
   b <- min(cdist[clusts != nsm7[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 meanCoeff[8]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(nsm8))
 
 for(i in 1:length(nsm8)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(nsm8)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[nsm8[i]]
   b <- min(cdist[clusts != nsm8[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[9]<-mean(coeffs)
 mean_mean_Coeff[6]<-mean(meanCoeff)
 
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm_0.001))
 
 
 for(i in 1:length(sm_0.001)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm_0.001)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm_0.001[i]]
   b <- min(cdist[clusts != sm_0.001[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
   
 }
 meanCoeff[1]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm2_0.001))
 
 for(i in 1:length(sm2_0.001)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm2_0.001)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm2_0.001[i]]
   b <- min(cdist[clusts != sm2_0.001[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 
 
 meanCoeff[2]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm3_0.001))
 
 for(i in 1:length(sm3_0.001)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm3_0.001)[j,]),Allpara_nazero[i,]))
   }
   a<- cdist[sm3_0.001[i]]
   b <- min(cdist[clusts != sm3_0.001[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 
 meanCoeff[3]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm4_0.001))
 
 for(i in 1:length(sm4_0.001)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm4_0.001)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm4_0.001[i]]
   b <- min(cdist[clusts != sm4_0.001[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 meanCoeff[5]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm5_0.001))
 
 for(i in 1:length(sm5_0.001)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm5_0.001)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm5_0.001[i]]
   b <- min(cdist[clusts != sm5_0.001[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[6]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm6_0.001))
 
 for(i in 1:length(sm6_0.001)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm6_0.001)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm6_0.001[i]]
   b <- min(cdist[clusts != sm6_0.001[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanCoeff[7]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm7_0.001))
 
 for(i in 1:length(sm7_0.001)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm7_0.001)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[sm7_0.001[i]]
   b <- min(cdist[clusts != sm7_0.001[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
 }
 meanCoeff[8]<-mean(coeffs)
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(sm_0.001))
 
 for(i in 1:length(sm_0.001)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(sm_0.001)[j,]),Allpara_nazero[i,]))
   }
   a <- exp(-cdist[sm_0.001[i]])
   b <- min(cdist[clusts != sm_0.001[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
 }
 meanmeanCoeff[9]<-mean(coeffs)
 mean_mean_Coeff[7]<-mean(meanCoeff)
 
 rm(coeffs,clusts)
 coeffs<-0
 clusts <- sort(unique(nsm5))
 
 
 for(i in 1:length(nsm5)){
   cdist <- rep(0,length(clusts))
   for(j in clusts){
     cdist[j] <- dist(rbind((centers(nsm5)[j,]),Allpara_nazero[i,]))
   }
   a <- cdist[nsm5[i]]
   b <- min(cdist[clusts != nsm5[i]])
   coeffs[i] <- (b-a) / max(b,a)
   
   
   
 }
 meameanCoeff[1]<-mean(coeffs)
 
 
 
 
 
 



  
  
  plot(2:5, ameanCoeff[2:5], type="b", xlab="Number of Clusters",ylab="Sum of Squares Error", col="blue", lty=6)
  