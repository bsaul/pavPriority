#------------------------------------------------------------------------------#
# A simple simulator to test a not-so-simple idea
#------------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(ggplot2)

#------------------------------------------------------------------------------#
# Causal model functions ####
#------------------------------------------------------------------------------#

generate_obs <- function(n, z0, y0, a, delta, theta){
  data_frame(
    a = a,
    z = z0 + delta * a,
    y = y0 + theta * z
  )
}

causal_model <- function(data, delta_hyp, theta_hyp){
  data %>% mutate_(
    z_o =~ z - delta_hyp * a,
    y_o =~ y - theta_hyp * z,
    y_o2 =~ y - theta_hyp * z_o
  )
}

# from: https://stackoverflow.com/questions/28368072/create-combinations-of-a-binary-vector
make_Omega <- function(n, m) {
  ind <- combn(seq_len(n), m)
  ind <- t(ind) + (seq_len(ncol(ind)) - 1) * n
  res <- rep(0, nrow(ind) * n)
  res[ind] <- 1
  matrix(res, ncol = n, nrow = nrow(ind), byrow = TRUE)
}

ts_dist <- function(O, z){
  as.numeric(O %*% z)
}

ts_obs <- function(a, z){
  as.numeric(t(a) %*% z)
}

pval1 <- function(O, a, z){
  mean(abs(ts_obs(a, z)) <= abs(ts_dist(O, z)))
}

pval2 <- function(z, y){
  cor.test(z, y, exact = TRUE)$p.value
}


hyp_tester_proposed <- function(data, O, delta_hyp, theta_hyp){
  new_data <- causal_model(data, delta_hyp, theta_hyp)
  
  p1 <- pval1(O, new_data$a, new_data$z_o)
  p2 <- pval2(new_data$z, new_data$y_o)
  
  ts <- -2 * sum(log(c(p1, p2)))
  pchisq(ts, df = 2 * 2, lower.tail = FALSE)
}

hyp_tester_old <- function(data, delta_hyp, theta_hyp){
  new_data <- causal_model(data, delta_hyp, theta_hyp)
  
  p1 <- pval2(new_data$a, new_data$z_o)
  p2 <- pval2(new_data$z, new_data$y_o)
  
  ts <- -2 * sum(log(c(p1, p2)))
  pchisq(ts, df = 2 * 2, lower.tail = FALSE)
}

O <- make_Omega(10, 10/2)

simulator <- function(seed, n, O, delta_hyp, theta_hyp){
  set.seed(seed)
  z <- runif(n, 0, 1)
  y <- rnorm(n)  
  a_obs <- O[sample(nrow(O), 1), ]
  obs_data <- generate_obs(n, z, y, a_obs, 1, 1)

  c(
    new = hyp_tester_proposed(obs_data, O, delta_hyp, theta_hyp),
    old = hyp_tester_old(obs_data, delta_hyp, theta_hyp)
  )
}

x <- t(replicate(1000, simulator(sample(1:10000000, 1), 10, O, 1, 1)))
uu <- runif(1000)
qqplot(uu, x[ , 1])
points(sort(uu), sort(x[ , 2]), col = 'red')
lines(x = c(0, 1), y= c(0, 1))

z <- runif(10, 0, 1)
y <- rnorm(10)  
a_obs <- O[sample(nrow(O), 1), ]
obs_data <- generate_obs(10, z, y, a_obs, 1, 1)

hyps <- expand.grid(delta = seq(-1, 3, by = .1), theta = seq(-1, 3, by = .1))


hyps$p_newmethod <- apply(hyps, 1, function(x) {hyp_tester_proposed(obs_data, O, x[1], x[2]) })
hyps$p_oldmethod <- apply(hyps, 1, function(x) {hyp_tester_old(obs_data, x[1], x[2]) })

hyps$powernew <- apply(hyps, 1, function(x){
  mean(replicate(100, simulator(sample(1:10000000, 1), 10, O, x[1], x[2]))[1, ] <= 0.05)
})

hyps$powerold <- apply(hyps, 1, function(x){
  mean(replicate(100, simulator(sample(1:10000000, 1), 10, O, x[1], x[2]))[2, ] <= 0.05)
})

ggplot(
  data = hyps,
  aes(x = delta, y = theta)
  ) + 
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) + 
  geom_point(aes(fill  = powernew), shape = 21, size = 2, color = 'white') + 
  scale_fill_gradient(low = "red", high = "white", space = "Lab",
                      na.value = "grey50", guide = "none") + 
  theme_classic()
