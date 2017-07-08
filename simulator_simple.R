#------------------------------------------------------------------------------#
# A simple simulator to test a not-so-simple idea
#------------------------------------------------------------------------------#

library(dplyr)
library(tidyr)

#------------------------------------------------------------------------------#
# Causal model functions ####
#------------------------------------------------------------------------------#

generate_obs <- function(x0, a, delta, theta){
  t <- length(a)
  out <- numeric(t)
  
  for(i in 1:t){
    if(i == 1){
      out[1] <- x0 + (a[1] * theta)
    } else {
      out[i] <- out[i - 1] * delta +  a[i] * theta
    }
  }
  data_frame(x = out, a = a, t = 1:t, lag_x = lag(x))
}

unwind_trial <- function(x, a, delta_hyp, theta_hyp){
  t <- length(a)
  out <- numeric(t)
  for(i in 1:t){
    if(i == 1){
      out[1] <- x[1] - (a[1] * theta_hyp)
    } else if (i == 2) {
      out[i] <- (x[i] - (a[1] * theta_hyp * delta_hyp + a[2] * theta_hyp))/delta_hyp
    } else if (i == 3) {
      out[i] <- (x[i] - theta_hyp * (delta_hyp^2 * a[1] + delta_hyp * a[2] + a[3]))/delta_hyp^2
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
O <- permutations(6, 6, set=TRUE, repeats.allowed=FALSE)
O

gps <- list(1:3, 4:6)
get.col    <- function(x, j) x[, j]
is.ordered <- function(x) !colSums(diff(t(x)) < 0)
is.valid   <- Reduce(`&`, Map(is.ordered, Map(get.col, list(O),  gps)))
O <- O[is.valid, ]
O[O %in% c(1:2)] <- 'A'
O[O %in% c(3:4)] <- 'B'
O[O %in% c(5:6)] <- 'C'


#------------------------------------------------------------------------------#
# Observe data ####
#------------------------------------------------------------------------------#

# Generate starting values
x0 <- runif(6, min = .2, max = .3)

# Observe data
O_obs <- O[sample(1:nrow(O), 1), ]

delta_tru <- 5
theta_tru <- 2
delta <- 5
theta <- 2

lapply(seq_along(1:length(x0)), function(i){
  generate_obs(x0    = x0[i], 
               a     = A[which(O_obs[i] == LETTERS), ], 
               delta = delta_tru, 
               theta = theta_tru) %>%
    mutate_(id =~ i,
            trt   =~ O_obs[i])
}) %>% bind_rows() ->
  obs_data

obs_unwind <- lapply(split(obs_data, obs_data$id), function(dt){
  unwind_trial(dt$x, dt$a, delta_hyp = delta, theta_hyp = theta)
}) %>% bind_rows

obs_unwind$id <- obs_data$id
obs_unwind$trt <- obs_data$trt

pair_diff <- function(x){
  m <- (outer(x, x, '-'))^2
  sum(m[lower.tri(m)])
}

test_dt <- obs_unwind %>% 
  group_by(trt, id) %>%
  summarise(diff = pair_diff(x))

glm(a ~ x, data = obs_unwind) %>% summary()


ts1 <- function(x, a){
  1/abs(cov(x, a))
}

obs_ts <- ts1(obs_unwind$x, obs_unwind$a)
  
apply(O, 1, function(x) {
  lapply(seq_along(x), function(j){
    dt <- filter_(obs_data, ~id == j)$x
    a  <- A[which(x[j] == LETTERS), ]
    unwind_trial(x = dt,
                 a = a,
                 delta_hyp = delta,
                 theta_hyp = theta)

  }) %>% bind_rows() -> new_dt
  ts1(new_dt$x, new_dt$x)
}) -> ts_dist

mean(ts_dist > obs_ts)

z <- tidyr::spread(obs_data %>% select(x, t, id, trt), key = t, value = x) 
nonpartest(diff~trt, test_dt, permreps=1000)




