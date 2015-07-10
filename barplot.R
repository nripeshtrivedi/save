
Allpara<-cbind(members$numLogins,members$numForumPosts ,members$numMsgUser,members$convRequests,members$forumViews,members$helpViews,members$pageViewsWeb, members$pageViewsApp, members$activedays)
Allpara_nazero<- Allpara[ apply(Allpara!=0, 1, any), , drop=FALSE] 
rcorr(Allpara_nazero, type="pearson")

#plot of users
pieplot<-c(9497, 3579 , 5563, 976, 2568, 7405, 14639,  22858)
lbls <- c("I", " II ", "III ", " IV", " V  ","VI", "VII","VIII") 
pie(pieplot, labels = lbls, main="Cluster of Users")


#interaction




pieplot2<-c(0.05479346 ,0.05730722, 0.07649877,6.17530411,0.26762625)
lbls2 <- c("1st ", "2nd", "3rd", "4th Cluster", "5th"pie(pieplot2, labels = lbls2, main="Visits to the website")
           
           pieplot3<-c(17.847334,34.905631 , 56.224561 ,374.972734 ,1.121493)
           lbls3 <- c("1st ", "2nd", "3rd", "4th Cluster", "5th")
           pie(pieplot3, labels = lbls3, main="Message exchanges")
           
           pieplot4<-c(0.06327065 ,0.06814913, 0.09638554,10.03121414,0.36954135)
           lbls4 <- c("----------------------1st", "----2nd", "----------3rd", "4th Cluster", "5th")
           pie(pieplot4, labels = lbls4, main="Active Days")
           #interaction plot
           barplot(centers(sm4),horiz=TRUE, names.arg=c("I","II","III","IV","V","VI","VII" ), cex.names=0.4)
           #initiation plot
           variance<-c(3.294835,3.322186,3.346879,3.749639, 4.041171,10.4248,71.82049)
           barplot(variance, horiz=TRUE,names.arg=c("I","II","III","IV","V","VI","VII" ), cex.names=0.4, col="blue", main="percent initation from users in each clusters")
            #interaction plot
           variance<-c(5.447653,2.884053,10.91309,14.18299,2.441814,0.1090743,64.02133)
           barplot(variance, horiz=TRUE,names.arg=c("I","II","III","IV","V","VI","VII" ), cex.names=0.4, col="blue", main="percent interaction from users in each clusters")
           #loyality plot
           variance<-c(0.3116359,0.3274162,0.377282,0.4083127,0.6218019,1.972732,95.98082)
           barplot(variance, horiz=TRUE,names.arg=c("I","II","III","IV","V","VI","VII" ), cex.names=0.4, col="blue", main="percent loyality from users in each clusters")
                   library(corrplot)
                   M <- cor(Allpara_nazero)
                   corrplot(M, method = "circle")
                   c<-as.data(Allpara_nazero)
                   library(scatterplot3d)
                   attach(Allpara_nazero)
                   scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
                   type="h", main="3D Scatterplot") 
                   persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
                   persp3d(Allpara_nazero[,1], Allpara_nazero[,3], Allpara_nazero[,9], aspect=c(1, 1, 0.5), col = "lightblue",
                   xlab = "X", ylab = "Y", zlab = "Sinc( r )")
                   colnames(Allpara_nazero) = c("A", "B", "C","D","E","F","G","H","I")
                   alldata<-as.data.frame(Allpara_nazero)
                   library(scatterplot3d)
                   alldata[,c("A")]
                   attach(alldata)
                   a.cols <- rainbow(4,alpha=1)[sm3]
                   plot3d(Allpara_nazero[,c(1,3,9)],col=a.cols)
                   s3d<-scatterplot3d(A,I,C,pch=16, highlight.3d=TRUE,
                   type="h",col=a.cols, main="3D Scatterplot")
                   library(Rcmdr)
                   fit <- lm(C ~ A+I) 
                   s3d$plane3d(fit)
                   scatter3d(A,I,C)
                   