MMK.PB<-function(time){
  lambda=100
  mu1=2
  mu2=3
  mu3=6
  t = 0
  C1_hist = 0
  C2_hist = 0
  C3_hist = 0
  s = 0
  
  ##generate the first time with rate lambda 
  T1 = rexp(1,rate=lambda)
  C1=50
  C2=33
  C3=17
  event_times = T1
  interval_times=T1
  t = T1
  num_event = 0
  
  
  ##queueing agorithm
  while (t<time) {
    num_event = num_event+1
    if ((C1+C2+C3)>100){
      while (t<time){
        num_event = num_event+1
        if (max(C1,C2,C3)==C1&min(C1,C2,C3)==C3&C1>100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = ifelse(p<lambda/(lambda+100*mu1),C1+1,C1-1)
          C2=C2
          C3=C3
        }else if (max(C1,C2,C3)==C1&min(C1,C2,C3)==C3&C1+C2>100&C1<=100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 =ifelse(p<lambda/(lambda+C1*mu1),C1+1,C1-1)
          C2=ifelse(p<lambda/(lambda+(100-C1)*mu2),C2+1,C2-1)
          C3=C3
        }
        else if (max(C1,C2,C3)==C1&min(C1,C2,C3)==C3&(C1+C2)<=100&((C1+C2+C3>100))){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = ifelse(p<lambda/(lambda+C1*mu1),C1+1,C1-1)
          C2=  ifelse(p<lambda/(lambda+C2*mu2),C2+1,C2-1)
          C3=  ifelse(p<lambda/(lambda+(100-C1-C2)*mu3),C3+1,C3-1)
        }else if (max(C1,C2,C3)==C1&min(C1,C2,C3)==C2&C1>100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = ifelse(p<lambda/(lambda+100*mu1),C1+1,C1-1)
          C2=C2
          C3=C3
        }else if (max(C1,C2,C3)==C1&min(C1,C2,C3)==C2&C1+C3>100&C1<=100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 =ifelse(p<lambda/(lambda+C1*mu1),C1+1,C1-1)
          C2=C2
          C3=ifelse(p<lambda/(lambda+(100-C1)*mu3),C3+1,C3-1)
        }
        else if (max(C1,C2,C3)==C1&min(C1,C2,C3)==C2&(C1+C3)<=100&((C1+C2+C3>100))){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = ifelse(p<lambda/(lambda+C1*mu1),C1+1,C1-1)
          C2=  ifelse(p<lambda/(lambda+(100-C1-C3)*mu2),C2+1,C2-1)
          C3=  ifelse(p<lambda/(lambda+C3*mu3),C3+1,C3-1)
        }else if (max(C1,C2,C3)==C2&min(C1,C2,C3)==C3&C2>100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = C1
          C2=ifelse(p<lambda/(lambda+100*mu2),C2+1,C2-1)
          C3=C3
        }else if (max(C1,C2,C3)==C2&min(C1,C2,C3)==C3&C2+C1>100&C2<=100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 =ifelse(p<lambda/(lambda+(100-C2)*mu1),C1+1,C1-1)
          C2=ifelse(p<lambda/(lambda+C2*mu2),C2+1,C2-1)
          C3=C3
        }
        else if (max(C1,C2,C3)==C2&min(C1,C2,C3)==C3&(C1+C2)<=100&((C1+C2+C3>100))){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = ifelse(p<lambda/(lambda+C1*mu1),C1+1,C1-1)
          C2=  ifelse(p<lambda/(lambda+C2*mu2),C2+1,C2-1)
          C3=  ifelse(p<lambda/(lambda+(100-C1-C2)*mu3),C3+1,C3-1)
        }else if (max(C1,C2,C3)==C2&min(C1,C2,C3)==C1&C2>100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = C1
          C2=ifelse(p<lambda/(lambda+100*mu2),C2+1,C2-1)
          C3=C3
        }else if (max(C1,C2,C3)==C2&min(C1,C2,C3)==C1&C2+C3>100&C2<=100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 =C1
          C2=ifelse(p<lambda/(lambda+C2*mu2),C2+1,C2-1)
          C3=ifelse(p<lambda/(lambda+(100-C2)*mu3),C3+1,C3-1)
        }
        else if (max(C1,C2,C3)==C2&min(C1,C2,C3)==C1&(C3+C2)<=100&((C1+C2+C3>100))){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = ifelse(p<lambda/(lambda+(100-C2-C3)*mu1),C1+1,C1-1)
          C2=  ifelse(p<lambda/(lambda+C2*mu2),C2+1,C2-1)
          C3=  ifelse(p<lambda/(lambda+C3*mu3),C3+1,C3-1)
        }else if (max(C1,C2,C3)==C3&min(C1,C2,C3)==C1&C3>100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = C1
          C2=C2
          C3=ifelse(p<lambda/(lambda+100*mu3),C3+1,C3-1)
        }else if (max(C1,C2,C3)==C3&min(C1,C2,C3)==C1&C2+C3>100&C3<=100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 =C1
          C2=ifelse(p<lambda/(lambda+(100-C2)*mu2),C2+1,C2-1)
          C3=ifelse(p<lambda/(lambda+C3*mu3),C3+1,C3-1)
        }
        else if (max(C1,C2,C3)==C3&min(C1,C2,C3)==C1&(C3+C2)<=100&((C1+C2+C3>100))){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = ifelse(p<lambda/(lambda+(100-C2-C3)*mu1),C1+1,C1-1)
          C2=  ifelse(p<lambda/(lambda+C2*mu2),C2+1,C2-1)
          C3=  ifelse(p<lambda/(lambda+C3*mu3),C3+1,C3-1)
        }else if (max(C1,C2,C3)==C3&min(C1,C2,C3)==C2&C3>100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = C1
          C2=C2
          C3=ifelse(p<lambda/(lambda+100*mu3),C3+1,C3-1)
        }else if (max(C1,C2,C3)==C3&min(C1,C2,C3)==C2&C1+C3>100&C3<=100){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 =ifelse(p<lambda/(lambda+(100-C3)*mu1),C1+1,C1-1)
          C2=C2
          C3=ifelse(p<lambda/(lambda+C3*mu3),C3+1,C3-1)
        }
        else if (max(C1,C2,C3)==C3&min(C1,C2,C3)==C2&(C1+C3)<=100&((C1+C2+C3>100))){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = ifelse(p<lambda/(lambda+C1*mu1),C1+1,C1-1)
          C2=  ifelse(p<lambda/(lambda+(100-C1-C3)*mu2),C2+1,C2-1)
          C3=  ifelse(p<lambda/(lambda+C3*mu3),C3+1,C3-1)
        }
        else if((C1+C2+C3)<=100){
          break
        }
        t = t+T1
        event_times[num_event] = t
        interval_times[num_event]=T1
      }
      Q_hist<-matrix(c(C1_hist,C2_hist,C3_hist,event_times,interval_times), ,5) 
    }
    else if ((C1+C2+C3)<=100&(C1+C2+C3)!=0){
      while (t<time){
        num_event = num_event+1
        if ((C1+C2+C3)<=100&(C1+C2+C3!=0)){
          T1 = rexp(1,rate=lambda+mu1)
          p = runif(1,0,1)
          C1_hist[num_event] = C1
          C2_hist[num_event] = C2
          C3_hist[num_event] = C3
          C1 = ifelse(p<lambda/(lambda+C1*mu1),C1+1,C1-1)
          C2=  ifelse(p<lambda/(lambda+C2*mu2),C2+1,C2-1)
          C3=  ifelse(p<lambda/(lambda+C3*mu3),C3+1,C3-1)
        }else {
          break
        }
        t = t+T1
        event_times[num_event] =t
        interval_times[num_event]=T1
      }
      Q_hist<-matrix(c(C1_hist,C2_hist,C3_hist,num_event,interval_times), ,5)
    }
    else if ((C1+C2+C3)==0){
      while (t<time){
        num_event = num_event+1
        # here, the queue was empty, so only arrivals are possible
        T1 = rexp(1,rate=lambda)
        C1_hist[num_event] = C1
        C2_hist[num_event] = C2
        C3_hist[num_event] = C3
        C1=1
        C2=1
        C3=1
        t = t+T1
        event_times[num_event] = t
        interval_times[num_event]=T1
      }
      Q_hist<-matrix(c(C1_hist,C2_hist,C3_hist,event_times,interval_times), ,5)
    }
  }
  out<-Q_hist
}


Q_histB<-MMK.PB(time=100)
Q_histB<-Q_histB[-1,]
Q_histB<-na.omit(Q_histB)

Q_histB<-as.data.frame(Q_histB)
library(ggplot2)
ggplot(Q_histB, aes(x=V4)) + 
  geom_line(aes(y = V1), color = "darkred") + 
  geom_line(aes(y = V2), color="steelblue")+
  geom_line(aes(y = V3), color = "orange")+
  labs(x="time", y="queue length")+
  theme_minimal()


