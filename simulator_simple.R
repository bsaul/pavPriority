#------------------------------------------------------------------------------#
# A simple simulator to test a not-so-simple idea
#------------------------------------------------------------------------------#

library(iterpc)

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

z <- generate_obs(.25, c(1, 1, 0), .3, -.02)
z$x
z2 <- unwind_trial(z$x, c(1, 1, 0), .3, -.02)

Omega <- matrix(
  c(1, 1, 1,
    1, 1, 0,
    1, 0, 0,
    0, 0, 0), nrow = 4, ncol = 3, byrow = TRUE
)

ts1 <- function(x){
  m <- (outer(x, x, '-'))^2
  sum(m[lower.tri(m)])
}

apply(Omega, 1, function(a) {
  r <- unwind_trial(z$x, a, .3, -.02)
  ts1(r$x)
})


I <- iterpc(c(2, 2, 2), labels = c('a', 'b', 'c'), replace = TRUE)
O <- getall(I)

gps <- list(1:2, 3:4, 5:6, 7:8)

library(gtools)
dat=permutations(8, 8, set=TRUE, repeats.allowed=FALSE)

get.col    <- function(x, j) x[, j]
is.ordered <- function(x) !colSums(diff(t(x)) < 0)
is.valid   <- Reduce(`&`, Map(is.ordered, Map(get.col, list(dat),  gps)))

dat <- dat[is.valid, ]

dat[dat %in% c(1:4)] <- 'A'
dat[dat %in% c(5:8)] <- 'B'
dat[dat %in% c(9:12)] <- 'C'
dat[dat %in% c(13:16)] <- 'D'

O <- dat

# Generate starting values
x0 <- runif(8, min = .2, max = .3)

# Observe data
O_obs <- O[sample(1:nrow(O), 1), ]
lapply(seq_along(1:length(x0)), function(i){
  generate_obs(x     = x0[i], 
               a     = Omega[which(O_obs[i] == LETTERS), ], 
               delta = 0.3, 
               theta = -0.02) %>% 
    mutate_(id =~ i)
}) %>% bind_rows() ->
  obs_data

obs_ts0 <- lapply(split(obs_data, obs_data$id), function(dt){
  r <- unwind_trial(dt$x, dt$a, delta =0.3, theta = -0.02)
  ts1(r$x)
}) %>% unlist()

obs_ts <- summary(aov(obs_ts0 ~ O_obs))[[1]]['F value'][1, ]
  
apply(O, 1, function(x) {
  lapply(seq_along(x), function(j){
    dt <- filter_(obs_data, ~id == j)$x
    a  <- Omega[which(x[j] == LETTERS), ]
    r <- unwind_trial(x = dt,
                      a = a,
                      delta = 1,
                      theta = 0)
    ts1(r$x)
  }) %>% unlist() -> z
  summary(aov(z ~ x))[[1]]['F value'][1, ]
}) -> ts_dist

mean(obs_ts <= ts_dist)

kruskal.test(obs_ts0, g = as.factor(O_obs), exact = TRUE)
