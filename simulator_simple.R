#------------------------------------------------------------------------------#
# A simple simulator to test a not-so-simple idea
#------------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(ggplot2)

#------------------------------------------------------------------------------#
# Causal model functions ####
#------------------------------------------------------------------------------#

start_frame <- function(n, t){
  data_frame(id = rep(1:n, each = t),
             t  = rep(1:t, times = n))
}

add_uniformity <- function(frame,
                           distr = list(rfun = runif, options = list()),
                           delta){
  n <- max(frame$id)
  t <- max(frame$t)
  frame$x <- do.call(distr$rfun, args = append(list(n = n), distr$options)) %>%
    rep(each = t) %>%
    {. + (delta * 0:(t - 1))}
  frame
}

start_frame(12, 3) %>%
  add_uniformity(delta = 4)

generate_uniformity <- function(data, delta){
  x <- c(replicate(max(data$id), 
                   generate_uniformity2(runif(1, min = .2, max = .3), 
                                        delta, max(data$t)), simplify = TRUE))
  data %>%
    # group_by_(~id) %>%
    mutate_(
      x = ~x
      #x =~ x + (delta * (t - 1))
    )
}



#### OLD --> 

generate_uniformity2 <- function(x0, delta, t){
  x0 + delta * 0:t
}

#### Comparison
n <- 12
t <- 3


microbenchmark::microbenchmark(
  start_frame(12, 3) %>%
    add_uniformity(delta = 4),
 data_frame(id = rep(1:n, each = t),
            t  = rep(1:t, times = n),
              x = c(replicate(12, 
            generate_uniformity2(runif(1, min = .2, max = .3), 
                                 4, 2), simplify = TRUE)))
)

##### ---- ####


generate_obs <- function(x0, a, theta){
  stopifnot(length(x0) == length(a))
  t <- length(a)
  out <- numeric(t)
  
  out <- x0 + (cumsum(a) * theta)

  data_frame(x = out, a = a, t = 1:t, lag_x = lag(x))
}

unwind_trial <- function(x, a, delta_hyp, theta_hyp){
  t <- length(a)
  out <- numeric(t)
  for(i in 1:t){
    if(i == 1){
      out[1] <- x[1] - (a[1] * theta_hyp)
    } else if (i == 2) {
      out[i] <- x[i] - (delta_hyp + theta_hyp * (a[1] + a[2]))
    } else if (i > 2) {
      stop('Not implemented')
    }
  }
  data_frame(x = out, a = a, t = 1:t, lag_x = lag(x))
  
}

#------------------------------------------------------------------------------#
# Generate Omega ####
#------------------------------------------------------------------------------#

A <- matrix(
  c(1, 1,
    1, 0,
    0, 0), nrow = 3, ncol = 2, byrow = TRUE
)


library(gtools)
O <- permutations(9, 9, set=TRUE, repeats.allowed=FALSE)
O

gps <- list(1:3, 4:6, 7:9)
get.col    <- function(x, j) x[, j]
is.ordered <- function(x) !colSums(diff(t(x)) < 0)
is.valid   <- Reduce(`&`, Map(is.ordered, Map(get.col, list(O),  gps)))
O <- O[is.valid, ]
O[O %in% c(1:3)] <- 'A'
O[O %in% c(4:6)] <- 'B'
O[O %in% c(7:9)] <- 'C'
O
O_star <- O[sample.int(nrow(O), 100), ]
O_star <- O
#------------------------------------------------------------------------------#
# Observe data ####
#------------------------------------------------------------------------------#

n <- 9
t <- 2
delta_tru <- 5
theta_tru <- 2

ts1 <- function(x, a){
  1/abs(cov(x, a))
}

#### Simulator #### 

compute_pvalue <- function(obs_data, delta_hyp, theta_hyp, test_statistic){

  obs_unwind <- lapply(split(obs_data, obs_data$id), function(dt){
    unwind_trial(dt$x, dt$a, delta_hyp = delta_hyp, theta_hyp = theta_hyp)
  }) %>% bind_rows

  obs_unwind$id  <- obs_data$id
  obs_unwind$trt <- obs_data$trt
  
  obs_ts <- test_statistic(obs_unwind$x, obs_unwind$a)
  ptm <- proc.time()
  apply(O_star, 1, function(x) {
    lapply(seq_along(x), function(j){
      dt <- filter_(obs_data, ~id == j)$x
      a  <- A[which(x[j] == LETTERS), ]
      unwind_trial(x = dt,
                   a = a,
                   delta_hyp = delta_hyp,
                   theta_hyp = theta_hyp)
      
    }) %>% bind_rows() -> new_dt
    test_statistic(new_dt$x, new_dt$x)
  }) -> ts_dist
  
  mean(ts_dist > obs_ts)
}

do_1_simulation <- function(uniformity, hypotheses){
  # Observe data
  O_obs <- O[sample(1:nrow(O), 1), ]
  
  lapply(seq_along(1:n), function(i){
    index <- (t * (i - 1)) + 1
    generate_obs(x0    = uniformity[index:(index + 1)], 
                 a     = A[which(O_obs[i] == LETTERS), ], 
                 theta = theta_tru) %>%
      mutate_(id =~ i,
              trt   =~ O_obs[i])
  }) %>% bind_rows() ->
    obs_data

  apply(hypotheses, 1, function(x){
    compute_pvalue(obs_data = obs_data, x[1], x[2], test_statistic = ts1)
  })
}


hyps <- expand.grid(delta = seq(4, 6, by = .2), theta = seq(1, 3, by = .2))




onesim <- do_1_simulation(uniformity, hyps)

hyps$p <- onesim

ggplot(
  data = hyps,
  aes(x = delta, y = theta)
) + 
  geom_hline(yintercept = 2) +
  geom_vline(xintercept = 5) + 
  geom_point(aes(fill  = p), shape = 21, size = 2, color = 'white') + 
  scale_fill_gradient(low = "white", high = "red", space = "Lab",
                      na.value = "grey50", guide = "none") + 
  theme_classic()


## Check Type 1 error #### 

check_type1 <- function(times){
  replicate(times, {
  uniformity <- c(replicate(n, generate_uniformity(runif(1, min = .2, max = .3), 
                                                   delta_tru, t - 1), simplify = TRUE))
  # Observe data
  O_obs <- O[sample(1:nrow(O), 1), ]
  
  lapply(seq_along(1:n), function(i){
    index <- (t * (i - 1)) + 1
    generate_obs(x0    = uniformity[index:(index + 1)], 
                 a     = A[which(O_obs[i] == LETTERS), ], 
                 theta = theta_tru) %>%
      mutate_(id =~ i,
              trt   =~ O_obs[i])
  }) %>% bind_rows() ->
    obs_data
  compute_pvalue(obs_data, delta_hyp = delta_tru, theta_hyp = theta_tru, test_statistic = ts1)
  })
}

type1_check <- check_type1(50)

qqplot(type1_check, runif(100))

#### ETC ####
pair_diff <- function(x){
  m <- (outer(x, x, '-'))^2
  sum(m[lower.tri(m)])
}

test_dt <- obs_unwind %>% 
  group_by(trt, id) %>%
  summarise(diff = pair_diff(x))

glm(a ~ x, data = obs_unwind) %>% summary()





