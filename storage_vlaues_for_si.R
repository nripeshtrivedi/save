v<-rep(0,16771)
for(i in 1:16771)
{
v[i]<-values[i]
}
z<-unlist(values1)
mean(z)
#for sigma =0.005
v_sm_of_sm_2_0.005<-mean(z)
v_sm_of_sm3_1_0.005<-mean(z) # do again
v_sm_of_sm4_1_0.005<-mean(unlist(nvalues))
#for sigma =0.009
v_sm_of_sm_2_0.009<-mean(unlist(nvalues))
v_sm2_1_0.009<-mean(unlist(nvalues1))
