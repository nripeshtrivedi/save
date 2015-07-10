library("kernlab")
Allpara<-cbind(members$numLogins,members$numForumPosts ,members$numMsgUser,members$convRequests,members$forumViews,members$helpViews,members$pageViewsWeb, members$pageViewsApp, members$activedays)
Allpara_nazero<- Allpara[ apply(Allpara!=0, 1, any), , drop=FALSE] 
rcorr(Allpara_nazero, type="pearson")

nc<-13
seed<-1234
wss <- (nrow(data)-1)*sum(apply(Allpara_nazero,2,var))
set.seed(seed)
c<-1
part_matrix = matrix( , nrow=nrow(Allpara_nazero)/4, ncol=9) 
for(i in 1:67084)
{
  if(i%%4==0)
  {
    for(j in 1:9)
    part_matrix[c,j]<-Allpara_nazero[i,j]
  }
  c<-c+1
}
sm<-kkmeans(Allpara_nazero, centers=2,kpar = list(sigma=0.0051))
sm2<-kkmeans(Allpara_nazero, centers=3,kpar = list(sigma=0.0051))
sm3<-kkmeans(Allpara_nazero, centers=4,kpar = list(sigma=0.0051)
sm6<-kkmeans(Allpara_nazero, centers=7,kpar = list(sigma=0.0051))
sm7<-kkmeans(Allpara_nazero, centers=6,kpar = list(sigma=0.003))
sm8<-kkmeans(Allpara_nazero, centers=7,kpar = list(sigma=0.003))
smvalue<-kkmeans(Allpara_nazero, centers=8,kpar = list(sigma=0.003))
sm10<-kkmeans(Allpara_nazero, centers=11, kpar = list(sigma=0.0051))
sm11<-kkmeans(Allpara_nazero, centers=10,kpar = list(sigma=0.0045))
sm12<-kkmeans(Allpara_nazero, centers=11,kpar = list(sigma=0.003))
sm13<-kkmeans(Allpara_nazero, centers=12,kpar = list(sigma=0.0045))
sm14<-kkmeans(Allpara_nazero, centers=13,kpar = list(sigma=0.0045))
nsm10<-kkmeans(Allpara_nazero, centers=11,kpar = list(sigma=0.0055))
wss[1]<-sum(withinss(sm))
wss[2]<-sum(withinss(sm2))
wss[3]<-sum(withinss(sm3))
wss[4]<-sum(withinss(sm4))
wss[5]<-sum(withinss(sm5))
wss[6]<-sum(withinss(sm6))
wss[7]<-sum(withinss(sm7))
wss[8]<-sum(withinss(sm8))
wss[9]<-sum(withinss(sm9))
wss[10]<-sum(withinss(sm10))
wss[11]<-sum(withinss(sm11))
plot(2:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

a.cols <-rainbow(6, alpha=1)[nsm5]

X_for_column<-Allpara_nazero
X_for_column[, c(1,2,3,4,5,6,7,8,9)]<-log(X_for_column[, c(1,2,3,4,5,6,7,8,9)]+1)

plot(Allpara_nazero[,c(3,1)], xlab="Interaction", ylab="Intiation", col=a.cols,log="xy")
plot(Allpara_nazero[,c(3,9)], xlab="Interaction", ylab="Loyality", col=a.cols,log="xy")
plot(X_for_column[,c(4,8)], xlab="Initiation", ylab="Loyality", col=a.cols)
plot(Allpara_nazero[,c(1,2)], col=a.cols,log="xy")
plot(Allpara_nazero[,c(2,3)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(3,4)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(4,5)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(5,6)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(6,7)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(1,3)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(3,9)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(1,5)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(1,6)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(1,7)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(2,3)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(2,4)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(2,5)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(2,6)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(2,7)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(3,5)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(3,6)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(3,7)], log="xy")
plot(X_for_column[,c(4,6)], col=a.cols)
plot(X_for_column[,c(4,7)], col=a.cols)
plot(X_for_column[,c(4,8)], col=a.cols)
plot(X_for_column[,c(4,9)], col=a.cols)
plot(X_for_column[,c(1,6)], col=a.cols)
plot(X_for_column[,c(1,7)], col=a.cols)
plot(X_for_column[,c(1,8)], col=a.cols)
plot(X_for_column[,c(1,9)])
plot(Allpara_nazero[,c(5,6)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(5,7)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(6,7)], col=a.cols, log="xy")
plot(Allpara_nazero[,c(1,3)], col=a.cols, log="xy")

sm0.001<-kkmeans(Allpara_na
                 zero, centers=7,kpar = list(sigma=0.001))
sm0.0009<-kkmeans(Allpara_nazero, centers=7,kpar = list(sigma=0.0009))
sm0.0008<-kkmeans(Allpara_nazero, centers=7,kpar = list(sigma=0.0008))
sm0.0015<-kkmeans(Allpara_nazero, centers=7,kpar = list(sigma=0.0015))
#fiiting power law distribution
power.law.fit(Allpara_nazero[,3],xmin=NULL, start=2,force.continuous = TRUE)

