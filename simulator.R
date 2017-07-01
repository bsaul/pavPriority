# simulator
library(dplyr)

# simulator

theta <- .5
delta_x <- .3
phi_x <- 0

delta_y <- .3
phi_y <- 0

x_1 <- .25
y_1 <- .25

make_trial1 <- function(x0, y0, t, a, theta){
  out <- matrix(NA, ncol = 2, nrow = t)
  
  for(i in 1:t){
    if(i == 1){
      out[1, ] <- c(x0, y0) + (a[1] * theta)
    } else {
      out[i, ] <- c(
        out[i - 1, 1] * delta_x + phi_x * out[i - 1, 2] + a[i] * theta,
        out[i - 1, 2] * delta_y + phi_y * out[i - 1, 1] + a[i] * theta)
    }
  }
  out
}

make_trial2 <- function(x0, t, a, theta){
  out <- numeric(t)
  
  for(i in 1:t){
    if(i == 1){
      out[1] <- x0 + (a[1] * theta)
    } else {
      out[i] <- out[i - 1] * delta_x +  a[i] * theta
    }
  }
  data_frame(x = out, a = a, t = 1:t, lag_x = lag(x))
}


z <- make_trial2(.25, 3, c(1, 1, 1), -.02)
z
lm(x ~ -1 + lag_x +  a, data = z)
