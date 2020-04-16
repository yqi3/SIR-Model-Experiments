# Alice Qi
# MATH 2240 (ODE) SIR Model Simulation
# 4/17/2020

# Initial conditions and parameters. This part should be run before starting or re-running any of the following experiments. ----
n <- 100
s0 <- 90
i0 <- 10
r0 <- 0

s <- c(s0)
i <- c(i0)
r <- c(r0)

recov <- 0.05

# 1) Experiment 1: One infected individual makes one susceptible individual sick upon direct contact. Recovery is not yet considered. Therefore, each random draw consists of 2 individuals. Balanced number of s & i - greatest speed of infection. ----
for (j in 1:500) {
  if (tail(s, n=1) < 0) {
    break
  }
  
  s_r <- tail(s, n=1) / n
  
  a <- runif(2)
  
  if (a[1] < s_r & a[2] < s_r) {
    s <- c(s, tail(s, n=1))
    i <- c(i, tail(i, n=1))
  }
  
  else if (a[1] > s_r & a[2] > s_r) {
    s <- c(s, tail(s, n=1))
    i <- c(i, tail(i, n=1))
  }
  
  else {
    b <- rbinom(1, 1, 0.5)
    if (b == 0) {
      s <- c(s, tail(s, n=1))
      i <- c(i, tail(i, n=1))
    }
    
    else {
      s <- c(s, tail(s, n=1) - 1)
      i <- c(i, tail(i, n=1) + 1)
    }
  }
}
plot(i, cex = 0.5)


# 2) Experiment 2: Instead of one infected object making one susceptible sick, one infected makes 2 susceptible sick. Recovery is not yet considered. Therefore, each random draw consists of 3 individuals. Greater speed of infection. ----
for (j in 1:500) {
  if (tail(s, n=1) < 0) {
    break
  }
  
  s_r <- tail(s, n=1) / n
  
  a <- runif(3)
  
  if (a[1] < s_r & a[2] < s_r & a[3] < s_r) {
    s <- c(s, tail(s, n=1))
    i <- c(i, tail(i, n=1))
  }
  
  else if (a[1] > s_r & a[2] > s_r & a[3] > s_r) {
    s <- c(s, tail(s, n=1))
    i <- c(i, tail(i, n=1))
  }
  
  else if ((a[1] < s_r & a[2] < s_r & a[3] > s_r) | (a[1] < s_r & a[2] > s_r & a[3] < s_r) | (a[1] > s_r & a[2] < s_r & a[3] < s_r)) {
    b <- rbinom(2, 1, 0.5)
    if (b[1] == 0 & b[2] == 0) {
      s <- c(s, tail(s, n=1))
      i <- c(i, tail(i, n=1))
    }
    else if (b[1] == 1 & b[2] == 1) {
      s <- c(s, tail(s, n=1)-2)
      i <- c(i, tail(i, n=1)+2)
    }
    else {
      s <- c(s, tail(s, n=1)-1)
      i <- c(i, tail(i, n=1)+1)
    }
  }
  
  else {
    b <- rbinom(1, 1, 0.5)
    if (b == 0) {
      s <- c(s, tail(s, n=1))
      i <- c(i, tail(i, n=1))
    }
    else {
      s <- c(s, tail(s, n=1)-1)
      i <- c(i, tail(i, n=1)+1)
    }
  }
}
plot(i, cex = 0.5)


# 3) Experiment 3 builds upon experiment 2 by adding an average length of recovery. Much less speed of infection if number of ppl contacted is small, probability of reappearance. ----
for (j in 1:500) {
  if (tail(s, n=1) < 0) {
    break
  }
  
  r <- c(r, tail(r, n = 1) + tail(i, n=1) * recov)
  s_r <- tail(s, n=1) / (n - tail(r, n=1))
  
  a <- runif(3, 0, (n - tail(r, n = 1)/n))
  
  if (a[1] < s_r & a[2] < s_r & a[3] < s_r) {
    s <- c(s, tail(s, n=1))
    i <- c(i, tail(i, n=1) * (1 - recov))
  }
  
  else if (a[1] > s_r & a[2] > s_r & a[3] > s_r) {
    s <- c(s, tail(s, n=1))
    i <- c(i, tail(i, n=1) * (1 - recov))
  }
  
  else if ((a[1] < s_r & a[2] < s_r & a[3] > s_r) | (a[1] < s_r & a[2] > s_r & a[3] < s_r) | (a[1] > s_r & a[2] < s_r & a[3] < s_r)) {
    b <- rbinom(2, 1, 0.5)
    if (b[1] == 0 & b[2] == 0) {
      s <- c(s, tail(s, n=1))
      i <- c(i, tail(i, n=1) * (1 - recov))
    }
    else if (b[1] == 1 & b[2] == 1) {
      s <- c(s, tail(s, n=1)-2)
      i <- c(i, tail(i, n=1) * (1 - recov) + 2)
    }
    else {
      s <- c(s, tail(s, n=1)-1)
      i <- c(i, tail(i, n=1) * (1 - recov) + 1)
    }
  }
  
  else {
    b <- rbinom(1, 1, 0.5)
    if (b == 0) {
      s <- c(s, tail(s, n=1))
      i <- c(i, tail(i, n=1) * (1 - recov))
    }
    else {
      s <- c(s, tail(s, n=1)-1)
      i <- c(i, tail(i, n=1) * (1 - recov) + 1)
    }
  }
}
plot(i, cex = 0.5)