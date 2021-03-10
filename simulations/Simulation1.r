# Simulation 1
##############################
# Step 1 Global Setting      
##############################
# number of arms
K = 3
# number of time steps
T.times = 10000
# number of stationary segments
M = 5

# mean of each arm
# mu is all the means of the K arms in M stationary periods
mu1 = c(0.35, 0.05, 0.1, 0.6, 0.25)
mean(mu1)
mu2 = c(0.1, 0.55, 0.15, 0.15, 0.35)
mean(mu2)
mu3 = c(0.5, 0.15, 0.1, 0.05, 0.7)
mean(mu3)
#mu4 = c(0.25, 0.35, 0.25, 0.35)
#mu <- matrix(round(0.7*runif(M*K),3),nrow=K,ncol=M)
mu <- rbind(mu1,mu2,mu3)
mu

mu.all <- matrix(0,nrow=K, ncol=T.times)
for(i in 0:(M-1)){
  mu.all[,(T.times*i/M + 1):(T.times*(i+1)/M)] <- mu[,i+1]
}
#mu.all <- matrix(0,nrow=K, ncol=T.times)
#mu.all[,1:500] <- mu[,1]
#mu.all[,501:1200] <- mu[,2]
#mu.all[,1201:1800] <- mu[,3]
#mu.all[,1801:2800] <- mu[,4]
#mu.all[,2801:5000] <- mu[,5]

# best arm means vector 
mu.best <- apply(mu.all, 2, max)

sd = 0.1

# plot the mu.all
plot(1:T.times, mu.all[1,], type='l', ylim=c(0,0.8), xlab='Time', 
     ylab='Expected Reward of Each Arm', lty=1, axes=F)
axis(side=1)
axis(side=2, at=0.1*c(1:10)); box()
abline(v=(1:(M-1))/M*T.times, col = 'grey')
for(i in 1:K) lines(1:T.times, mu.all[i,], type='l',col=c(1,2,4)[i], lty=i, lwd=2)
legend('topleft',c('Arm 1','Arm 2','Arm 3'),lwd=c(2,2,2),lty = 1:3, col=c(1,2,4), cex=0.8)

set.seed(1)
# 100 replicates
R = 100

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

system.time(
for(ii in 1:R){
  if(ii%%10==0) cat(ii)
  # reward matrix
  reward.matrix <- matrix(nrow=K, ncol=T.times)
  for(t in 1:T.times) reward.matrix[,t] <- mu.all[,t] + sd*rnorm(K)
  
  # add noise
  noise.index <- sample(1:T.times, 0.1*T.times)
  #noise.index <- read.table('noise.index.txt')
  reward.matrix[,noise.index] <- 2 + sd*rnorm(0.025*T.times*K)
  
  
  ##############################
  # Step 2.1 UCB 
  ##############################
  
  # simulate arms by UCB
  arm.selected.UCB <- c()
  num.selections.UCB = rep(0,K)
  sums_of_reward.UCB = rep(0,K)
  reward.UCB <- c()
  regret.UCB <- c()
  
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
  
  #sums_of_reward.UCB
  #num.selections.UCB
  
  cum.regret.UCB <- cumsum(regret.UCB)
  #cum.regret.UCB[T.times]
  
  arm.selected.UCB.matrix[ii,] <- arm.selected.UCB
  reward.UCB.matrix[ii,] <- reward.UCB
  regret.UCB.matrix[ii,] <- regret.UCB
  cum.regret.UCB.matrix[ii,] <- cum.regret.UCB
}
)
  
  ##############################
  # Step 2.2 M-UCB 
  ##############################
  
system.time(
  for(ii in 1:R){
    if(ii%%10==0) cat(ii)  
  # simulate arms by M_UCB
  arm.selected.M_UCB <- c()
  num.selections.M_UCB = rep(0,K)
  sums_of_reward.M_UCB = rep(0,K)
  reward.M_UCB <- c()
  regret.M_UCB <- c()
  
  # the last change point time
  tau <- 0
  tau.log.M_UCB <- c(tau)
  
  w = 100
  #b = sqrt(w*log(2*K*T.times^2)/2)
  b = 15
  gamma = sqrt( (M-1)*K*(2*b+3*sqrt(w))/2/T.times )
  #gamma = 0.01*K
  #gamma = 0.01
  
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
  
  #sums_of_reward.M_UCB
  #num.selections.M_UCB
  #tau.log.M_UCB
  cum.regret.M_UCB <- cumsum(regret.M_UCB)
  #cum.regret.M_UCB[T.times]
  
  arm.selected.M_UCB.matrix[ii,] <- arm.selected.M_UCB
  reward.M_UCB.matrix[ii,] <- reward.M_UCB
  regret.M_UCB.matrix[ii,] <- regret.M_UCB
  cum.regret.M_UCB.matrix[ii,] <- cum.regret.M_UCB
  if(length(tau.log.M_UCB)>1) tau.log.M_UCB.matrix[ii,tau.log.M_UCB[-1]] <- 1
  }  
)
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
  
  w = 100
  #b = sqrt(w*log(2*K*T.times^2)/2)
  b = 15
  #gamma = sqrt( (M-1)*K*(2*b+3*sqrt(w))/2/T.times )
  gamma = 0.01*K
  gamma = 0.01
  alpha = 0.01
  
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
  #tau.log.RM_UCB
  cum.regret.RM_UCB <- cumsum(regret.RM_UCB)
  #cum.regret.RM_UCB[T.times]
  
  arm.selected.RM_UCB1.matrix[ii,] <- arm.selected.RM_UCB
  reward.RM_UCB1.matrix[ii,] <- reward.RM_UCB
  regret.RM_UCB1.matrix[ii,] <- regret.RM_UCB
  cum.regret.RM_UCB1.matrix[ii,] <- cum.regret.RM_UCB
  if(length(tau.log.RM_UCB)>1) tau.log.RM_UCB1.matrix[ii,tau.log.RM_UCB[-1]] <- 1
  
system.time(
    for(ii in 1:R){
      if(ii%%10==0) cat(ii)    
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
  
  w = 100
  #b = sqrt(w*log(2*K*T.times^2)/2)
  b = 15
  #gamma = sqrt( (M-1)*K*(2*b+3*sqrt(w))/2/T.times )
  gamma = 0.01*K
  gamma = 0.01
  alpha = 0.025
  
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
    
  })
  
  
  #sums_of_reward.RM_UCB
  #num.selections.RM_UCB
  #tau.log.RM_UCB
  cum.regret.RM_UCB <- cumsum(regret.RM_UCB)
  #cum.regret.RM_UCB[T.times]
  
  arm.selected.RM_UCB2.matrix[ii,] <- arm.selected.RM_UCB
  reward.RM_UCB2.matrix[ii,] <- reward.RM_UCB
  regret.RM_UCB2.matrix[ii,] <- regret.RM_UCB
  cum.regret.RM_UCB2.matrix[ii,] <- cum.regret.RM_UCB
  if(length(tau.log.RM_UCB)>1) tau.log.RM_UCB2.matrix[ii,tau.log.RM_UCB[-1]] <- 1
    }
) 
  
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
  
  w = 100
  #b = sqrt(w*log(2*K*T.times^2)/2)
  b = 15
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
  #tau.log.RM_UCB
  cum.regret.RM_UCB <- cumsum(regret.RM_UCB)
  #cum.regret.RM_UCB[T.times]
  
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
  
  w = 100
  #b = sqrt(w*log(2*K*T.times^2)/2)
  b = 15
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
  #tau.log.RM_UCB
  cum.regret.RM_UCB <- cumsum(regret.RM_UCB)
  #cum.regret.RM_UCB[T.times]
  
  arm.selected.RM_UCB4.matrix[ii,] <- arm.selected.RM_UCB
  reward.RM_UCB4.matrix[ii,] <- reward.RM_UCB
  regret.RM_UCB4.matrix[ii,] <- regret.RM_UCB
  cum.regret.RM_UCB4.matrix[ii,] <- cum.regret.RM_UCB
  if(length(tau.log.RM_UCB)>1) tau.log.RM_UCB4.matrix[ii,tau.log.RM_UCB[-1]] <- 1
  
  ##############################
  # Step 2.4 D-UCB 
  ##############################
system.time(
    for(ii in 1:R){
      if(ii%%10==0) cat(ii)    
  #: Default parameter for gamma.
  gamma.d = 1-0.25*sqrt((M-1)/T.times)
  #gamma.d = 0.99
  
  # simulate arms by D-UCB
  arm.selected.dUCB <- c()
  num.selections.dUCB = rep(0,K)
  sums_of_reward.dUCB = rep(0,K)
  reward.dUCB <- c()
  regret.dUCB <- c()
  
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
  
  #sums_of_reward.dUCB
  #num.selections.dUCB
  
  cum.regret.dUCB <- cumsum(regret.dUCB)
  #cum.regret.dUCB[T.times]
  
  #plot(1:T.times, cum.regret.dUCB, type='l')
  #plot(1:T.times, cumsum(reward.dUCB), type='l')
  
  arm.selected.dUCB.matrix[ii,] <- arm.selected.dUCB
  reward.dUCB.matrix[ii,] <- reward.dUCB
  regret.dUCB.matrix[ii,] <- regret.dUCB
  cum.regret.dUCB.matrix[ii,] <- cum.regret.dUCB
    }
)
  ##############################
  # Step 2.5 SW-UCB 
  ##############################
system.time(
    for(ii in 1:R){
      if(ii%%10==0) cat(ii)    
  tau.sw = round(2*sqrt(T.times*log(T.times)/(M-1)))
  #tau.sw = 1000
  #tau.sw = 150
  #tau.sw=tau.sw/2
  # simulate arms by SW
  arm.selected.SW <- c()
  reward.SW <- c()
  regret.SW <- c()
  
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
  
  #table(arm.selected.SW)
  cum.regret.SW <- cumsum(regret.SW)
  #cum.regret.SW[T.times]
  
  arm.selected.SW.matrix[ii,] <- arm.selected.SW
  reward.SW.matrix[ii,] <- reward.SW
  regret.SW.matrix[ii,] <- regret.SW
  cum.regret.SW.matrix[ii,] <- cum.regret.SW
}
)

  
##############################
# Step Result and Plot
##############################

# plot
y1 =apply(cum.regret.M_UCB.matrix,2, mean)
y2 =apply(cum.regret.RM_UCB1.matrix,2,mean)
y3 =apply(cum.regret.RM_UCB2.matrix,2,mean)
y4 =apply(cum.regret.RM_UCB3.matrix,2,mean)
y5 =apply(cum.regret.RM_UCB4.matrix,2,mean)

y1[T.times]
y2[T.times]
y3[T.times]
y4[T.times]
y5[T.times]

plot(1:T.times, y1, type='l',col=1, xlab='Time',
     ylab='Expected Cumulative Regret', 
     ylim=c(0,max(c(y1,y2,y3,y4,y5))))
lines(1:T.times, y1, type='l',col=1, lwd=2, lty=1)
lines(1:T.times, y2, type='l',col=2, lwd=2, lty=2)
lines(1:T.times, y3, type='l',col=3, lwd=3, lty=3)
lines(1:T.times, y4, type='l',col=4, lwd=4, lty=4)
lines(1:T.times, y5, type='l',col=5, lwd=5, lty=5)

legend('topleft',c('M-UCB','RCD-UCB.S1','RCD-UCB.S2','RCD-UCB.S3','RCD-UCB.S4'),lwd=c(2,2,2,2),lty = 1:5, col=1:5, cex=0.8)


# change point

mean(apply(tau.log.M_UCB.matrix,1,sum))
sd(apply(tau.log.M_UCB.matrix,1,sum))
mean(apply(tau.log.RM_UCB1.matrix,1,sum))
sd(apply(tau.log.RM_UCB1.matrix,1,sum))
mean(apply(tau.log.RM_UCB2.matrix,1,sum))
sd(apply(tau.log.RM_UCB2.matrix,1,sum))
mean(apply(tau.log.RM_UCB3.matrix,1,sum))
sd(apply(tau.log.RM_UCB3.matrix,1,sum))
mean(apply(tau.log.RM_UCB4.matrix,1,sum))
sd(apply(tau.log.RM_UCB4.matrix,1,sum))

# compare with others
mean(cum.regret.UCB.matrix[,T.times])
sd(cum.regret.UCB.matrix[,T.times])
mean(cum.regret.M_UCB.matrix[,T.times])
sd(cum.regret.M_UCB.matrix[,T.times])
mean(cum.regret.RM_UCB2.matrix[,T.times])
sd(cum.regret.RM_UCB2.matrix[,T.times])
mean(cum.regret.dUCB.matrix[,T.times])
sd(cum.regret.dUCB.matrix[,T.times])
mean(cum.regret.SW.matrix[,T.times])
sd(cum.regret.SW.matrix[,T.times])
