# Real Data
##############################
# Step 1 Read Data 
#        and Global Setting      
##############################
x1 = read.csv('20-09-01.csv')[,5]
x2 = read.csv('20-09-02.csv')[,5]
x3 = read.csv('20-09-03.csv')[,5]
x4 = read.csv('20-09-04.csv')[,5]
x5 = read.csv('20-09-05.csv')[,5]
x6 = read.csv('20-09-06.csv')[,5]

x1 = x1[1:42842]
x2 = x2[1:42842]
x3 = x3[1:42842]
x4 = x4[1:42842]
x5 = x5[1:42842]
x6 = x6[1:42842]

reward.matrix.true = rbind(x1,x2,x3,x4,x5,x6)

# number of arms
K = 6
# number of time steps
T.times = 42842

pt = c(1000*c(1:(42842/1000)),T.times)
  
plot(pt, x1[pt], type='s', lwd=2, ylim = c(min(reward.matrix),max(reward.matrix)), col=1, pch=1,
     xlab = 'Time',ylab='Expected Reward of Each Arm')
lines(pt, x2[pt], type='s',col=2, lty=2, lwd=2)
lines(pt, x3[pt], type='s',col=3, lty=3,lwd=2)
lines(pt, x4[pt], type='s',col=4, lty=4,lwd=2)
lines(pt, x5[pt], type='s',col=5, lty=5,lwd=2)
lines(pt, x6[pt], type='s',col=6, lty=6,lwd=2)

legend('topleft',c('Arm 1','Arm 2','Arm 3','Arm 4','Arm 5', 'Arm 6'),lwd=c(2,2,2,2,2,2),lty = 1:6, col=1:6, cex=0.7)


pt = c(25000:35000)

plot(pt, x1[pt], type='l', ylim = c(min(reward.matrix),max(reward.matrix)), col=1, pch=1)
lines(pt, x2[pt], type='l',col=2, pch=2)
lines(pt, x3[pt], type='l',col=3, pch=3)
lines(pt, x4[pt], type='l',col=4, pch=4)
lines(pt, x5[pt], type='l',col=5, pch=5)
lines(pt, x6[pt], type='l',col=6, pch=6)

plot(1:T.times, x1, type='l',col=1, lty=1)
lines(1:T.times, x2, type='l',col=2, lty=2)
lines(1:T.times, x3, type='l',col=3, lty=3)
lines(1:T.times, x4, type='l',col=4, lty=4)
lines(1:T.times, x5, type='l',col=5, lty=5)
lines(1:T.times, x6, type='l',col=6,lty=6)


mu.all <- reward.matrix.true
# best arm means vector 
mu.best <- apply(mu.all, 2, max)

R = 1

arm.selected.UCB.matrix <- matrix(0, nrow=R, ncol=T.times)
reward.UCB.matrix <- matrix(0, nrow=R, ncol=T.times)
regret.UCB.matrix <- matrix(0, nrow=R, ncol=T.times)
cum.regret.UCB.matrix <-  matrix(0, nrow=R, ncol=T.times)

arm.selected.M_UCB.matrix <- matrix(0, nrow=R, ncol=T.times)
reward.M_UCB.matrix <- matrix(0, nrow=R, ncol=T.times)
regret.M_UCB.matrix <- matrix(0, nrow=R, ncol=T.times)
cum.regret.M_UCB.matrix <-  matrix(0, nrow=R, ncol=T.times)
tau.log.M_UCB.matrix <- matrix(0, nrow=R, ncol=T.times)

arm.selected.RM_UCB1.matrix <- matrix(0, nrow=R, ncol=T.times)
reward.RM_UCB1.matrix <- matrix(0, nrow=R, ncol=T.times)
regret.RM_UCB1.matrix <- matrix(0, nrow=R, ncol=T.times)
cum.regret.RM_UCB1.matrix <-  matrix(0, nrow=R, ncol=T.times)
tau.log.RM_UCB1.matrix <- matrix(0, nrow=R, ncol=T.times)

arm.selected.RM_UCB2.matrix <- matrix(0, nrow=R, ncol=T.times)
reward.RM_UCB2.matrix <- matrix(0, nrow=R, ncol=T.times)
regret.RM_UCB2.matrix <- matrix(0, nrow=R, ncol=T.times)
cum.regret.RM_UCB2.matrix <-  matrix(0, nrow=R, ncol=T.times)
tau.log.RM_UCB2.matrix <- matrix(0, nrow=R, ncol=T.times)

arm.selected.RM_UCB3.matrix <- matrix(0, nrow=R, ncol=T.times)
reward.RM_UCB3.matrix <- matrix(0, nrow=R, ncol=T.times)
regret.RM_UCB3.matrix <- matrix(0, nrow=R, ncol=T.times)
cum.regret.RM_UCB3.matrix <-  matrix(0, nrow=R, ncol=T.times)
tau.log.RM_UCB3.matrix <- matrix(0, nrow=R, ncol=T.times)

arm.selected.RM_UCB4.matrix <- matrix(0, nrow=R, ncol=T.times)
reward.RM_UCB4.matrix <- matrix(0, nrow=R, ncol=T.times)
regret.RM_UCB4.matrix <- matrix(0, nrow=R, ncol=T.times)
cum.regret.RM_UCB4.matrix <-  matrix(0, nrow=R, ncol=T.times)
tau.log.RM_UCB4.matrix <- matrix(0, nrow=R, ncol=T.times)

arm.selected.dUCB.matrix <- matrix(0, nrow=R, ncol=T.times)
reward.dUCB.matrix <- matrix(0, nrow=R, ncol=T.times)
regret.dUCB.matrix <- matrix(0, nrow=R, ncol=T.times)
cum.regret.dUCB.matrix <-  matrix(0, nrow=R, ncol=T.times)

arm.selected.SW.matrix <- matrix(0, nrow=R, ncol=T.times)
reward.SW.matrix <- matrix(0, nrow=R, ncol=T.times)
regret.SW.matrix <- matrix(0, nrow=R, ncol=T.times)
cum.regret.SW.matrix <-  matrix(0, nrow=R, ncol=T.times)

for(ii in 1:R){
  # reward matrix
  # add noise
  #noise.index <- sample(1:T.times, 0.01*T.times)
  noise.index <- c(read.table('noise.txt'))
  #write.table(noise.index, 'noise.txt')
  noise.index <- noise.index$x
  reward.matrix <- mu.all <- reward.matrix.true
  # best arm means vector 
  reward.matrix[ ,c(noise.index)] <- 0
  mu.best <- apply(mu.all, 2, max)
  
  
  
  ##############################
  # Step 2.1 UCB 
  ##############################
 
    
  # simulate arms by UCB
  arm.selected.UCB <- c()
  num.selections.UCB = rep(0,K)
  sums_of_reward.UCB = rep(0,K)
  reward.UCB <- c()
  regret.UCB <- c()

system.time(
  
  for(t in 1:T.times){
    ad = 0
    max_upper_bound = 0
    for(i in 1:K){
      if(num.selections.UCB[i] > 0){
        average_reward = sums_of_reward.UCB[i] / num.selections.UCB[i]
        delta_i = sqrt(2 * log(t) / num.selections.UCB[i])
        upper_bound = average_reward + delta_i
      }else{
        upper_bound = 10^400
      }
      if(upper_bound > max_upper_bound){
        max_upper_bound = upper_bound
        ad = i
      }  
    }
    arm.selected.UCB <- c(arm.selected.UCB, ad)
    num.selections.UCB[ad] = num.selections.UCB[ad] + 1
    sums_of_reward.UCB[ad] = sums_of_reward.UCB[ad] + reward.matrix[ad,t]
    reward.UCB <- c(reward.UCB, reward.matrix[ad,t])
    regret.UCB <- c(regret.UCB, mu.best[t] - mu.all[ad,t])
  }

)
  #sums_of_reward.UCB
  #num.selections.UCB
  
  cum.regret.UCB <- cumsum(regret.UCB)
  cum.regret.UCB[T.times]
  reward.UCB[T.times]
  cumsum(reward.UCB)[T.times]
  
   
  arm.selected.UCB.matrix[ii,] <- arm.selected.UCB
  reward.UCB.matrix[ii,] <- reward.UCB
  regret.UCB.matrix[ii,] <- regret.UCB
  cum.regret.UCB.matrix[ii,] <- cum.regret.UCB
    
  
  ##############################
  # Step 2.2 M-UCB 
  ##############################
  
  
  # simulate arms by M_UCB
  arm.selected.M_UCB <- c()
  num.selections.M_UCB = rep(0,K)
  sums_of_reward.M_UCB = rep(0,K)
  reward.M_UCB <- c()
  regret.M_UCB <- c()
  
  # the last change point time
  tau <- 0
  tau.log.M_UCB <- c(tau)
  
  #delta =5
  #w = (4/delta^2)*( sqrt(log(2*K*T.times^2)) + sqrt(log(2*T.times)) )^2
  w = 200
  #b = sqrt(w*log(2*K*T.times^2)/2)
  b = 80
  #gamma = sqrt( (M-1)*K*(2*b+3*sqrt(w))/2/T.times )
  #gamma = 0.01*K
  gamma = 0.01

system.time(
  
  for(t in 1:T.times){
    ad = 0
    A <- (t-tau)%%floor(K/gamma)
    if(A <= (K-1)){
      ad = A+1
    }else{
      max_upper_bound = 0
      for(i in 1:K){
        if(num.selections.M_UCB[i] > 0){
          average_reward = sums_of_reward.M_UCB[i] / num.selections.M_UCB[i]
          delta_i = sqrt(2 * log(t) / num.selections.M_UCB[i])
          upper_bound = average_reward + delta_i
        }else{
          upper_bound = 10^400
        }
        if(upper_bound > max_upper_bound){
          max_upper_bound = upper_bound
          ad = i
        }  
      }
    }
    
    arm.selected.M_UCB <- c(arm.selected.M_UCB, ad)
    num.selections.M_UCB[ad] = num.selections.M_UCB[ad] + 1
    sums_of_reward.M_UCB[ad] = sums_of_reward.M_UCB[ad] + reward.matrix[ad,t]
    reward.M_UCB <- c(reward.M_UCB, reward.matrix[ad,t])
    regret.M_UCB <- c(regret.M_UCB, mu.best[t] - mu.all[ad,t])
    
    #change detection
    if(num.selections.M_UCB[ad] >= w){
      arm = ad
      arm.current.M_UCB = arm.selected.M_UCB[tau:t]
      reward.current.M_UCB = reward.M_UCB[tau:t]
      data_y = reward.current.M_UCB[arm.current.M_UCB == arm]
      data_y = data_y[(length(data_y)-w+1):length(data_y)]
      sum_first_half = sum(data_y[1:(w/2)])
      sum_second_half = sum(data_y[(w/2+1):w])
      has_detected = abs(sum_first_half - sum_second_half) > b
      if(has_detected){
        tau = t
        tau.log.M_UCB = c(tau.log.M_UCB,tau)
        num.selections.M_UCB = rep(0,K)
        sums_of_reward.M_UCB = rep(0,K)
      }
    }
    
  }
)

  sums_of_reward.M_UCB
  num.selections.M_UCB
  tau.log.M_UCB
  cum.regret.M_UCB <- cumsum(regret.M_UCB)
  cum.regret.M_UCB[T.times]
  reward.M_UCB[T.times]
  cumsum(reward.M_UCB)[T.times]
  
  
  arm.selected.M_UCB.matrix[ii,] <- arm.selected.M_UCB
  reward.M_UCB.matrix[ii,] <- reward.M_UCB
  regret.M_UCB.matrix[ii,] <- regret.M_UCB
  cum.regret.M_UCB.matrix[ii,] <- cum.regret.M_UCB
  if(length(tau.log.M_UCB)>1) tau.log.M_UCB.matrix[ii,tau.log.M_UCB[-1]] <- 1
  
  ##############################
  # Step 2.3 RM-UCB
  ##############################
  
  # alpha = 0.01
  # simulate arms by RM_UCB
  arm.selected.RM_UCB <- c()
  num.selections.RM_UCB = rep(0,K)
  sums_of_reward.RM_UCB = rep(0,K)
  reward.RM_UCB <- c()
  regret.RM_UCB <- c()
  
  # the last change point time
  tau <- 0
  tau.log.RM_UCB <- c(tau)
  for(i in 1:K){
    assign(paste("mathcalA", i, sep=""), c(0))
  }
  
  #delta = 0.05
  #w = (4/delta^2)*( sqrt(log(2*K*T.times^2)) + sqrt(log(2*T.times)) )^2
  w = 200
  #b = sqrt(w*log(2*K*T.times^2)/2)
  b = 80
  #gamma = sqrt( (M-1)*K*(2*b+3*sqrt(w))/2/T.times )
  gamma = 0.01*K
  gamma = 0.01
  alpha = 0.01

system.time(
  
  for(t in 1:T.times){
    ad = 0
    A <- (t-tau)%%floor(K/gamma)
    if(A <= (K-1)){
      ad = A+1
    }else{
      max_upper_bound = 0
      for(i in 1:K){
        if(num.selections.RM_UCB[i] > 0){
          average_reward = sums_of_reward.RM_UCB[i] / num.selections.RM_UCB[i]
          delta_i = sqrt(2 * log(t) / num.selections.RM_UCB[i])
          upper_bound = average_reward + delta_i
        }else{
          upper_bound = 10^400
        }
        if(upper_bound > max_upper_bound){
          max_upper_bound = upper_bound
          ad = i
        }  
      }
    }
    
    arm.selected.RM_UCB <- c(arm.selected.RM_UCB, ad)
    num.selections.RM_UCB[ad] = num.selections.RM_UCB[ad] + 1
    sums_of_reward.RM_UCB[ad] = sums_of_reward.RM_UCB[ad] + reward.matrix[ad,t]
    reward.RM_UCB <- c(reward.RM_UCB, reward.matrix[ad,t])
    regret.RM_UCB <- c(regret.RM_UCB, mu.best[t] - mu.all[ad,t])
    
    #change detection
    if(num.selections.RM_UCB[ad] >= w){
      arm = ad
      arm.current.RM_UCB = arm.selected.RM_UCB[tau:t]
      reward.current.RM_UCB = reward.RM_UCB[tau:t]
      data_y = reward.current.RM_UCB[arm.current.RM_UCB == arm]
      data_y = data_y[(length(data_y)-w+1):length(data_y)]
      sum_first_half = sum(data_y[1:(w/2)])
      sum_second_half = sum(data_y[(w/2+1):w])
      
      D <- 50
      if(length(get(paste("mathcalA", arm, sep=""))) >= D){
        a <-  quantile(get(paste("mathcalA", arm, sep="")), 1-alpha)
      }
      
      if(abs(sum_first_half - sum_second_half) < a){
        xx = abs(sum_first_half - sum_second_half)
      }else{
        xx = a
      }
      
      has_detected = xx**2 > b**2
      
      if(has_detected){
        tau = t
        tau.log.RM_UCB = c(tau.log.RM_UCB,tau)
        num.selections.RM_UCB = rep(0,K)
        sums_of_reward.RM_UCB = rep(0,K)
        for(i in 1:K){
          assign(paste("mathcalA", i, sep=""), c(0))
        }
      }else{
        newmclA = c(get(paste("mathcalA", arm, sep="")), abs(sum_first_half - sum_second_half))
        assign(paste("mathcalA", arm, sep=""), newmclA)
      }
    }
    
  }
) 
  
  sums_of_reward.RM_UCB
  num.selections.RM_UCB
  tau.log.RM_UCB
  cum.regret.RM_UCB <- cumsum(regret.RM_UCB)
  cum.regret.RM_UCB[T.times]
  reward.RM_UCB[T.times]
  cumsum(reward.RM_UCB)[T.times]
  
  arm.selected.RM_UCB1.matrix[ii,] <- arm.selected.RM_UCB
  reward.RM_UCB1.matrix[ii,] <- reward.RM_UCB
  regret.RM_UCB1.matrix[ii,] <- regret.RM_UCB
  cum.regret.RM_UCB1.matrix[ii,] <- cum.regret.RM_UCB
  if(length(tau.log.RM_UCB)>1) tau.log.RM_UCB1.matrix[ii,tau.log.RM_UCB[-1]] <- 1
  
  
  # alpha = 0.025
  # simulate arms by RM_UCB
  arm.selected.RM_UCB <- c()
  num.selections.RM_UCB = rep(0,K)
  sums_of_reward.RM_UCB = rep(0,K)
  reward.RM_UCB <- c()
  regret.RM_UCB <- c()
  
  # the last change point time
  tau <- 0
  tau.log.RM_UCB <- c(tau)
  for(i in 1:K){
    assign(paste("mathcalA", i, sep=""), c(0))
  }
  
  #delta = 0.05
  #w = (4/delta^2)*( sqrt(log(2*K*T.times^2)) + sqrt(log(2*T.times)) )^2
  w = 200
  #b = sqrt(w*log(2*K*T.times^2)/2)
  b = 78
  #gamma = sqrt( (M-1)*K*(2*b+3*sqrt(w))/2/T.times )
  gamma = 0.01*K
  gamma = 0.005
  alpha = 0.025
  
  for(t in 1:T.times){
    ad = 0
    A <- (t-tau)%%floor(K/gamma)
    if(A <= (K-1)){
      ad = A+1
    }else{
      max_upper_bound = 0
      for(i in 1:K){
        if(num.selections.RM_UCB[i] > 0){
          average_reward = sums_of_reward.RM_UCB[i] / num.selections.RM_UCB[i]
          delta_i = sqrt(2 * log(t) / num.selections.RM_UCB[i])
          upper_bound = average_reward + delta_i
        }else{
          upper_bound = 10^400
        }
        if(upper_bound > max_upper_bound){
          max_upper_bound = upper_bound
          ad = i
        }  
      }
    }
    
    arm.selected.RM_UCB <- c(arm.selected.RM_UCB, ad)
    num.selections.RM_UCB[ad] = num.selections.RM_UCB[ad] + 1
    sums_of_reward.RM_UCB[ad] = sums_of_reward.RM_UCB[ad] + reward.matrix[ad,t]
    reward.RM_UCB <- c(reward.RM_UCB, reward.matrix[ad,t])
    regret.RM_UCB <- c(regret.RM_UCB, mu.best[t] - mu.all[ad,t])
    
    #change detection
    if(num.selections.RM_UCB[ad] >= w){
      arm = ad
      arm.current.RM_UCB = arm.selected.RM_UCB[tau:t]
      reward.current.RM_UCB = reward.RM_UCB[tau:t]
      data_y = reward.current.RM_UCB[arm.current.RM_UCB == arm]
      data_y = data_y[(length(data_y)-w+1):length(data_y)]
      sum_first_half = sum(data_y[1:(w/2)])
      sum_second_half = sum(data_y[(w/2+1):w])
      
      D <- 20
      if(length(get(paste("mathcalA", arm, sep=""))) >= D){
        a <-  quantile(get(paste("mathcalA", arm, sep="")), 1-alpha)
      }
      
      if(abs(sum_first_half - sum_second_half) < a){
        xx = abs(sum_first_half - sum_second_half)
      }else{
        xx = a
      }
      
      has_detected = xx**2 > b**2
      
      if(has_detected){
        tau = t
        tau.log.RM_UCB = c(tau.log.RM_UCB,tau)
        num.selections.RM_UCB = rep(0,K)
        sums_of_reward.RM_UCB = rep(0,K)
        for(i in 1:K){
          assign(paste("mathcalA", i, sep=""), c(0))
        }
      }else{
        newmclA = c(get(paste("mathcalA", arm, sep="")), abs(sum_first_half - sum_second_half))
        assign(paste("mathcalA", arm, sep=""), newmclA)
      }
    }
    
  }
  
  
  #sums_of_reward.RM_UCB
  #num.selections.RM_UCB
  tau.log.RM_UCB
  cum.regret.RM_UCB <- cumsum(regret.RM_UCB)
  cum.regret.RM_UCB[T.times]
  reward.RM_UCB[T.times]
  cumsum(reward.RM_UCB)[T.times]
  
  
  arm.selected.RM_UCB2.matrix[ii,] <- arm.selected.RM_UCB
  reward.RM_UCB2.matrix[ii,] <- reward.RM_UCB
  regret.RM_UCB2.matrix[ii,] <- regret.RM_UCB
  cum.regret.RM_UCB2.matrix[ii,] <- cum.regret.RM_UCB
  if(length(tau.log.RM_UCB)>1) tau.log.RM_UCB2.matrix[ii,tau.log.RM_UCB[-1]] <- 1
  
  
  # alpha = 0.05
  # simulate arms by RM_UCB
  arm.selected.RM_UCB <- c()
  num.selections.RM_UCB = rep(0,K)
  sums_of_reward.RM_UCB = rep(0,K)
  reward.RM_UCB <- c()
  regret.RM_UCB <- c()
  
  # the last change point time
  tau <- 0
  tau.log.RM_UCB <- c(tau)
  for(i in 1:K){
    assign(paste("mathcalA", i, sep=""), c(0))
  }
  
  #delta = 0.05
  #w = (4/delta^2)*( sqrt(log(2*K*T.times^2)) + sqrt(log(2*T.times)) )^2
  w = 200
  #b = sqrt(w*log(2*K*T.times^2)/2)
  b = 80
  #gamma = sqrt( (M-1)*K*(2*b+3*sqrt(w))/2/T.times )
  gamma = 0.01*K
  gamma = 0.01
  alpha = 0.05
  
  for(t in 1:T.times){
    ad = 0
    A <- (t-tau)%%floor(K/gamma)
    if(A <= (K-1)){
      ad = A+1
    }else{
      max_upper_bound = 0
      for(i in 1:K){
        if(num.selections.RM_UCB[i] > 0){
          average_reward = sums_of_reward.RM_UCB[i] / num.selections.RM_UCB[i]
          delta_i = sqrt(2 * log(t) / num.selections.RM_UCB[i])
          upper_bound = average_reward + delta_i
        }else{
          upper_bound = 10^400
        }
        if(upper_bound > max_upper_bound){
          max_upper_bound = upper_bound
          ad = i
        }  
      }
    }
    
    arm.selected.RM_UCB <- c(arm.selected.RM_UCB, ad)
    num.selections.RM_UCB[ad] = num.selections.RM_UCB[ad] + 1
    sums_of_reward.RM_UCB[ad] = sums_of_reward.RM_UCB[ad] + reward.matrix[ad,t]
    reward.RM_UCB <- c(reward.RM_UCB, reward.matrix[ad,t])
    regret.RM_UCB <- c(regret.RM_UCB, mu.best[t] - mu.all[ad,t])
    
    #change detection
    if(num.selections.RM_UCB[ad] >= w){
      arm = ad
      arm.current.RM_UCB = arm.selected.RM_UCB[tau:t]
      reward.current.RM_UCB = reward.RM_UCB[tau:t]
      data_y = reward.current.RM_UCB[arm.current.RM_UCB == arm]
      data_y = data_y[(length(data_y)-w+1):length(data_y)]
      sum_first_half = sum(data_y[1:(w/2)])
      sum_second_half = sum(data_y[(w/2+1):w])
      
      D <- 20
      if(length(get(paste("mathcalA", arm, sep=""))) >= D){
        a <-  quantile(get(paste("mathcalA", arm, sep="")), 1-alpha)
      }
      
      if(abs(sum_first_half - sum_second_half) < a){
        xx = abs(sum_first_half - sum_second_half)
      }else{
        xx = a
      }
      
      has_detected = xx**2 > b**2
      
      if(has_detected){
        tau = t
        tau.log.RM_UCB = c(tau.log.RM_UCB,tau)
        num.selections.RM_UCB = rep(0,K)
        sums_of_reward.RM_UCB = rep(0,K)
        for(i in 1:K){
          assign(paste("mathcalA", i, sep=""), c(0))
        }
      }else{
        newmclA = c(get(paste("mathcalA", arm, sep="")), abs(sum_first_half - sum_second_half))
        assign(paste("mathcalA", arm, sep=""), newmclA)
      }
    }
    
  }
  
  
  #sums_of_reward.RM_UCB
  #num.selections.RM_UCB
  tau.log.RM_UCB
  cum.regret.RM_UCB <- cumsum(regret.RM_UCB)
  cum.regret.RM_UCB[T.times]
  reward.RM_UCB[T.times]
  cumsum(reward.RM_UCB)[T.times]
  
  arm.selected.RM_UCB3.matrix[ii,] <- arm.selected.RM_UCB
  reward.RM_UCB3.matrix[ii,] <- reward.RM_UCB
  regret.RM_UCB3.matrix[ii,] <- regret.RM_UCB
  cum.regret.RM_UCB3.matrix[ii,] <- cum.regret.RM_UCB
  if(length(tau.log.RM_UCB)>1) tau.log.RM_UCB3.matrix[ii,tau.log.RM_UCB[-1]] <- 1
  
  
  # alpha = 0.1
  # simulate arms by RM_UCB
  arm.selected.RM_UCB <- c()
  num.selections.RM_UCB = rep(0,K)
  sums_of_reward.RM_UCB = rep(0,K)
  reward.RM_UCB <- c()
  regret.RM_UCB <- c()
  
  # the last change point time
  tau <- 0
  tau.log.RM_UCB <- c(tau)
  for(i in 1:K){
    assign(paste("mathcalA", i, sep=""), c(0))
  }
  
  #delta = 0.05
  #w = (4/delta^2)*( sqrt(log(2*K*T.times^2)) + sqrt(log(2*T.times)) )^2
  w = 200
  #b = sqrt(w*log(2*K*T.times^2)/2)
  b = 80
  #gamma = sqrt( (M-1)*K*(2*b+3*sqrt(w))/2/T.times )
  gamma = 0.01*K
  gamma = 0.01
  alpha = 0.1
  
  for(t in 1:T.times){
    ad = 0
    A <- (t-tau)%%floor(K/gamma)
    if(A <= (K-1)){
      ad = A+1
    }else{
      max_upper_bound = 0
      for(i in 1:K){
        if(num.selections.RM_UCB[i] > 0){
          average_reward = sums_of_reward.RM_UCB[i] / num.selections.RM_UCB[i]
          delta_i = sqrt(2 * log(t) / num.selections.RM_UCB[i])
          upper_bound = average_reward + delta_i
        }else{
          upper_bound = 10^400
        }
        if(upper_bound > max_upper_bound){
          max_upper_bound = upper_bound
          ad = i
        }  
      }
    }
    
    arm.selected.RM_UCB <- c(arm.selected.RM_UCB, ad)
    num.selections.RM_UCB[ad] = num.selections.RM_UCB[ad] + 1
    sums_of_reward.RM_UCB[ad] = sums_of_reward.RM_UCB[ad] + reward.matrix[ad,t]
    reward.RM_UCB <- c(reward.RM_UCB, reward.matrix[ad,t])
    regret.RM_UCB <- c(regret.RM_UCB, mu.best[t] - mu.all[ad,t])
    
    #change detection
    if(num.selections.RM_UCB[ad] >= w){
      arm = ad
      arm.current.RM_UCB = arm.selected.RM_UCB[tau:t]
      reward.current.RM_UCB = reward.RM_UCB[tau:t]
      data_y = reward.current.RM_UCB[arm.current.RM_UCB == arm]
      data_y = data_y[(length(data_y)-w+1):length(data_y)]
      sum_first_half = sum(data_y[1:(w/2)])
      sum_second_half = sum(data_y[(w/2+1):w])
      
      D <- 50
      if(length(get(paste("mathcalA", arm, sep=""))) >= D){
        a <-  quantile(get(paste("mathcalA", arm, sep="")), 1-alpha)
      }
      
      if(abs(sum_first_half - sum_second_half) < a){
        xx = abs(sum_first_half - sum_second_half)
      }else{
        xx = a
      }
      
      has_detected = xx**2 > b**2
      
      if(has_detected){
        tau = t
        tau.log.RM_UCB = c(tau.log.RM_UCB,tau)
        num.selections.RM_UCB = rep(0,K)
        sums_of_reward.RM_UCB = rep(0,K)
        for(i in 1:K){
          assign(paste("mathcalA", i, sep=""), c(0))
        }
      }else{
        newmclA = c(get(paste("mathcalA", arm, sep="")), abs(sum_first_half - sum_second_half))
        assign(paste("mathcalA", arm, sep=""), newmclA)
      }
    }
    
  }
  
  
  #sums_of_reward.RM_UCB
  #num.selections.RM_UCB
  tau.log.RM_UCB
  cum.regret.RM_UCB <- cumsum(regret.RM_UCB)
  cum.regret.RM_UCB[T.times]
  reward.RM_UCB[T.times]
  cumsum(reward.RM_UCB)[T.times]
  
  arm.selected.RM_UCB4.matrix[ii,] <- arm.selected.RM_UCB
  reward.RM_UCB4.matrix[ii,] <- reward.RM_UCB
  regret.RM_UCB4.matrix[ii,] <- regret.RM_UCB
  cum.regret.RM_UCB4.matrix[ii,] <- cum.regret.RM_UCB
  if(length(tau.log.RM_UCB)>1) tau.log.RM_UCB4.matrix[ii,tau.log.RM_UCB[-1]] <- 1
  
  ##############################
  # Step 2.4 D-UCB 
  ##############################
  
  #: Default parameter for gamma.
  #gamma.d = 1-0.25*sqrt((M-1)/T.times)
  gamma.d = 0.9
  
  # simulate arms by D-UCB
  arm.selected.dUCB <- c()
  num.selections.dUCB = rep(0,K)
  sums_of_reward.dUCB = rep(0,K)
  reward.dUCB <- c()
  regret.dUCB <- c()
  
system.time(
  
  for(t in 1:T.times){
    ad = 0
    max_upper_bound = 0
    
    n_t_gamma <- sum(num.selections.dUCB)
    
    for(i in 1:K){
      if(num.selections.dUCB[i] > 0){
        average_reward = sums_of_reward.dUCB[i] / num.selections.dUCB[i]
        delta_i = sqrt(0.5 * log(n_t_gamma) / num.selections.dUCB[i])
        upper_bound = average_reward + delta_i
      }else{
        upper_bound = 10^400
      }
      if(upper_bound > max_upper_bound){
        max_upper_bound = upper_bound
        ad = i
      }  
    }
    arm.selected.dUCB <- c(arm.selected.dUCB, ad)
    
    num.selections.dUCB = num.selections.dUCB*gamma.d
    sums_of_reward.dUCB = sums_of_reward.dUCB*gamma.d
    
    num.selections.dUCB[ad] = num.selections.dUCB[ad] + 1
    sums_of_reward.dUCB[ad] = sums_of_reward.dUCB[ad] + reward.matrix[ad,t]
    
    reward.dUCB <- c(reward.dUCB, reward.matrix[ad,t])
    regret.dUCB <- c(regret.dUCB, mu.best[t] - mu.all[ad,t])
  }
)

  #sums_of_reward.dUCB
  #num.selections.dUCB
  
  cum.regret.dUCB <- cumsum(regret.dUCB)
  cum.regret.dUCB[T.times]
  reward.dUCB[T.times]
  cumsum(reward.dUCB)[T.times]
  
  #plot(1:T.times, cum.regret.dUCB, type='l')
  #plot(1:T.times, cumsum(reward.dUCB), type='l')
  
  arm.selected.dUCB.matrix[ii,] <- arm.selected.dUCB
  reward.dUCB.matrix[ii,] <- reward.dUCB
  regret.dUCB.matrix[ii,] <- regret.dUCB
  cum.regret.dUCB.matrix[ii,] <- cum.regret.dUCB
  
  ##############################
  # Step 2.5 SW-UCB 
  ##############################
  #tau.sw = round(2*sqrt(T.times*log(T.times)/(M-1)))
  #tau.sw = 1000
  tau.sw = 2000
  # simulate arms by SW
  arm.selected.SW <- c()
  reward.SW <- c()
  regret.SW <- c()

  system.time(
    
  for(t in 1:T.times){
    ad = 0
    max_upper_bound = 0
    now = length(arm.selected.SW)
    for(i in 1:K){
      num.selections.SW = 0
      num.selections.SW = sum(arm.selected.SW[(max(now-tau.sw,0)+1):now] == i)
      if(num.selections.SW > 0){
        sums_of_reward.SW = sum((reward.SW[(max(now-tau.sw,0)+1):now])[arm.selected.SW[(max(now-tau.sw,0)+1):now] == i])
        average_reward = sums_of_reward.SW / num.selections.SW
        delta_i = sqrt( log(min(t,tau.sw)) / num.selections.SW)
        upper_bound = average_reward + delta_i
      }else{
        upper_bound = 10^400
      }
      if(upper_bound > max_upper_bound){
        max_upper_bound = upper_bound
        ad = i
      }  
    }
    arm.selected.SW <- c(arm.selected.SW, ad)
    reward.SW <- c(reward.SW, reward.matrix[ad,t])
    regret.SW <- c(regret.SW, mu.best[t] - mu.all[ad,t])
  }
  )  
  #table(arm.selected.SW)
  cum.regret.SW <- cumsum(regret.SW)
  cum.regret.SW[T.times]
  reward.SW[T.times]
  cumsum(reward.SW)[T.times]
  
  arm.selected.SW.matrix[ii,] <- arm.selected.SW
  reward.SW.matrix[ii,] <- reward.SW
  regret.SW.matrix[ii,] <- regret.SW
  cum.regret.SW.matrix[ii,] <- cum.regret.SW
}


##############################
# Step Result and Plot
##############################
# plot
y1 =apply(cum.regret.UCB.matrix,2,mean)
y2 =apply(cum.regret.M_UCB.matrix,2,mean)
y3 =apply(cum.regret.RM_UCB3.matrix,2,mean)
y4 =apply(cum.regret.dUCB.matrix,2,mean)
y5 =apply(cum.regret.SW.matrix,2,mean)

y1[T.times]
y2[T.times]
y3[T.times]
y4[T.times]
y5[T.times]

plot(1:T.times, y2, type='l',col=1, xlab='Time',
     ylab='Cumulative Regret', 
     ylim=c(0,max(c(y2,y3,y4,y5))))
lines(1:T.times, y2, type='l',col=1, lwd=2, lty=1)
lines(1:T.times, y3, type='l',col=2, lwd=2, lty=2)
lines(1:T.times, y4, type='l',col=3, lwd=3, lty=3)
lines(1:T.times, y5, type='l',col=4, lwd=4, lty=4)
legend('topleft',c('M-UCB','RCD-UCB','D-UCB','SW-UCB'),lwd=c(2,2,2,2),lty = 1:4, col=1:4, cex=0.8)

