library(foreach)
library(doParallel)
library(ROCR)
library(cluster)
library(kernlab)
values<-foreach(i = 1:7,.packages='kernlab',.compact=c) %dopar% my(i)
#For sigma=1
my<-function(i)
{
if(i==1)
{
of_sm_1<-kkmeans(part_matrix, centers=2)
return(of_sm_1)
}
if(i==2)
{
of_sm2_1<-kkmeans(part_matrix, centers=3)
return(of_sm2_1)
}
if(i==3)
{
of_sm3_1<-kkmeans(part_matrix, centers=4)
return(of_sm3_1)
}
if(i==4)
{
of_sm4_1<-kkmeans(part_matrix, centers=5)
return(of_sm4_1)
}
if(i==5)
{
of_sm5_1<-kkmeans(part_matrix, centers=6)
return(of_sm5_1)
}
if(i==6)
{
of_sm6_1<-kkmeans(part_matrix, centers=7)
return(of_sm6_1)
}
if(i==7)
{
of_sm7_1<-kkmeans(part_matrix, centers=8)
return(of_sm7_1)
}
}

#For sigma=0.5
sm_0.5<-kkmeans(Part_matrix, centers=2,kpar = list(sigma=0.5))
sm2_0.5<-kkmeans(Part_matrix, centers=3,kpar = list(sigma=0.5))
sm3_0.5<-kkmeans(Part_matrix, centers=4,kpar = list(sigma=0.5))
sm4_0.5<-kkmeans(Part_matrix, centers=5,kpar = list(sigma=0.5))
sm5_0.5<-kkmeans(Part_matrix, centers=6,kpar = list(sigma=0.5))
sm6_0.5<-kkmeans(Part_matrix, centers=7,kpar = list(sigma=0.5))
sm7_0.5<-kkmeans(Part_matrix, centers=8,kpar = list(sigma=0.5))
sm8_0.5<-kkmeans(Part_matrix, centers=9,kpar = list(sigma=0.5))
sm9_0.5<-kkmeans(Part_matrix, centers=10,kpar = list(sigma=0.5))

#For sigma=0.1
sm_0.1<-kkmeans(Part_matrix, centers=2,kpar = list(sigma=0.1))
sm2_0.1<-kkmeans(Part_matrix, centers=3,kpar = list(sigma=0.1))
sm3_0.1<-kkmeans(Part_matrix, centers=4,kpar = list(sigma=0.1))
sm4_0.1<-kkmeans(Part_matrix, centers=5,kpar = list(sigma=0.1))
sm5_0.1<-kkmeans(Part_matrix, centers=6,kpar = list(sigma=0.1))
sm6_0.1<-kkmeans(Part_matrix, centers=7,kpar = list(sigma=0.1))
sm7_0.1<-kkmeans(Part_matrix, centers=8,kpar = list(sigma=0.1))
sm8_0.1<-kkmeans(Part_matrix, centers=9,kpar = list(sigma=0.1))
sm9_0.1<-kkmeans(Part_matrix, centers=10,kpar = list(sigma=0.1))

#For sigma=0.05
sm_0.05<-kkmeans(Part_matrix, centers=2,kpar = list(sigma=0.05))
sm2_0.05<-kkmeans(Part_matrix, centers=3,kpar = list(sigma=0.05))
sm3_0.05<-kkmeans(Part_matrix, centers=4,kpar = list(sigma=0.05))
sm4_0.05<-kkmeans(Part_matrix, centers=5,kpar = list(sigma=0.05))
sm5_0.05<-kkmeans(Part_matrix, centers=6,kpar = list(sigma=0.05))
sm6_0.05<-kkmeans(Part_matrix, centers=7,kpar = list(sigma=0.05))
sm7_0.05<-kkmeans(Part_matrix, centers=8,kpar = list(sigma=0.05))
sm8_0.05<-kkmeans(Part_matrix, centers=9,kpar = list(sigma=0.05))
sm9_0.05<-kkmeans(Part_matrix, centers=10,kpar = list(sigma=0.05))

#For sigma=0.01
sm_0.01<-kkmeans(Part_matrix, centers=2,kpar = list(sigma=0.01))
sm2_0.01<-kkmeans(Part_matrix, centers=3,kpar = list(sigma=0.01))
sm3_0.01<-kkmeans(Part_matrix, centers=4,kpar = list(sigma=0.01))
sm4_0.01<-kkmeans(Part_matrix, centers=5,kpar = list(sigma=0.01))
sm5_0.01<-kkmeans(Part_matrix, centers=6,kpar = list(sigma=0.01))
sm6_0.01<-kkmeans(Part_matrix, centers=7,kpar = list(sigma=0.01))
sm7_0.01<-kkmeans(Part_matrix, centers=8,kpar = list(sigma=0.01))
sm8_0.01<-kkmeans(Part_matrix, centers=9,kpar = list(sigma=0.01))
sm9_0.01<-kkmeans(Part_matrix, centers=10,kpar = list(sigma=0.01))

#For sigma=0.005
sm_0.005<-kkmeans(Part_matrix, centers=2,kpar = list(sigma=0.005))
sm2_0.005<-kkmeans(Part_matrix, centers=3,kpar = list(sigma=0.005))
sm3_0.005<-kkmeans(Part_matrix, centers=4,kpar = list(sigma=0.005)
                   sm4_0.005<-kkmeans(Part_matrix, centers=5,kpar = list(sigma=0.005)
                                      sm5_0.005<-kkmeans(Part_matrix, centers=6,kpar = list(sigma=0.005)
                                                         sm6_0.005<-kkmeans(Part_matrix, centers=7,kpar = list(sigma=0.005))
                                                         sm7_0.005<-kkmeans(Part_matrix, centers=8,kpar = list(sigma=0.005))
                                                         sm8_0.005<-kkmeans(Part_matrix, centers=9,kpar = list(sigma=0.005))
                                                         sm9_0.005<-kkmeans(Part_matrix, centers=10,kpar = list(sigma=0.005))
                                                         
                                                         #For sigma=0.001
                                                         sm_0.001<-kkmeans(Part_matrix, centers=2,kpar = list(sigma=0.002))
                                                         sm2_0.001<-kkmeans(Part_matrix, centers=3,kpar = list(sigma=0.002))
                                                         sm3_0.001<-kkmeans(Part_matrix, centers=4,kpar = list(sigma=0.002))
                                                         sm4_0.001<-kkmeans(Part_matrix, centers=5,kpar = list(sigma=0.002))
                                                         sm5_0.001<-kkmeans(Part_matrix, centers=6,kpar = list(sigma=0.002))
                                                         sm6_0.001<-kkmeans(Part_matrix, centers=7,kpar = list(sigma=0.002))
                                                         sm7_0.001<-kkmeans(Part_matrix, centers=8,kpar = list(sigma=0.002))
                                                         sm8_0.001<-kkmeans(Part_matrix, centers=9,kpar = list(sigma=0.002))
                                                         sm9_0.001<-kkmeans(Part_matrix, centers=10,kpar = list(sigma=0.002))
                                                         
                                                         #For sigma=0.0001
                                                         sm_0.0001<-kkmeans(Part_matrix, centers=2,kpar = list(sigma=0.0001))
                                                         sm2_0.0001<-kkmeans(Part_matrix, centers=3,kpar = list(sigma=0.0001))
                                                         sm3_0.0001<-kkmeans(Part_matrix, centers=4,kpar = list(sigma=0.0001))
                                                         sm4_0.0001<-kkmeans(Part_matrix, centers=5,kpar = list(sigma=0.0001))
                                                         sm5_0.0001<-kkmeans(Part_matrix, centers=6,kpar = list(sigma=0.0001))
                                                         sm6_0.0001<-kkmeans(Part_matrix, centers=7,kpar = list(sigma=0.0001))
                                                         sm7_0.0001<-kkmeans(Part_matrix, centers=8,kpar = list(sigma=0.0001))
                                                         sm8_0.0001<-kkmeans(Part_matrix, centers=9,kpar = list(sigma=0.0001))
                                                         sm9_0.0001<-kkmeans(Part_matrix, centers=10,kpar = list(sigma=0.0001))
                                                         
                                                         