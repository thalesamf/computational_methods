#####################################################
#Population ecology: using deSolve to solve ODEs in R
#####################################################

#Packages------------------------------------------------------------
library(deSolve)
library(ggplot2) # because we will plot things
library(tidyr) # because we will manipulate some data

#1) Logistic Growth Mode---------------------------------------------


# Creating a function for logistic growth
logGrowth <- function(t, y, p) {
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N * (1 - a * N)
    return(list(dN.dt))
  })
}


# named vector with parameters
p <- c(r = 1, a = 0.001)
# initial condition
y0 <- c(N = 10)
# time steps
t <- 1:20

# ODE
out_log <- ode(y = y0, times = t, func = logGrowth, parms = p)

head(out_log) #Output = Estimation of population size

#Plotting
df_log <- as.data.frame(out_log)
ggplot(df_log) +
  geom_line(aes(x = time, y = N)) +
  theme_classic()+
  geom_hline(yintercept = 1000, colour = "red", size = 0.7, linetype = "dotted")

#2) Lotka-Volterra competition model ------------------------------------------

# Creating a function for Lotka-Volterra competition model
LVComp <- function(t, y, p) {
  N <- y
  with(as.list(p), {
    dN1.dt <- r[1] * N[1] * (1 - a[1, 1] * N[1] - a[1, 2] * N[2])
    dN2.dt <- r[2] * N[2] * (1 - a[2, 1] * N[1] - a[2, 2] * N[2])
    return(list(c(dN1.dt, dN2.dt)))
  })
}

#Parameters
a <- matrix(c(0.02, 0.01, 0.01, 0.03), nrow = 2)
r <- c(1, 1)
p2 <- list(r, a)
N0 <- c(10, 10)
t2 <- c(1:100)

#ODE
out_lv <- ode(y = N0, times = t2, func = LVComp, parms = p2)
head(out_lv) #Output = Estimation population size

#Plotting
df_lv <- pivot_longer(as.data.frame(out_lv), cols = 2:3) #Representing every variable in a column
View(df_lv)

ggplot(df_lv) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()

#(?) Whci kind of
ggplot(df_lv)+
  geom_line (aes(x = 1, y = 2, color = name)) +
  labs (x = "Population 1", y = "Population 2", color = "Species") +
  theme_classic()
