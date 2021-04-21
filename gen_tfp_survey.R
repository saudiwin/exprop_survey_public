# test for how to generate random polynomials

int <- -.5
cov1 <- .7
cov2 <- -.01

X <- 0:100

Y <- int + cov1*X + cov2*(X^2)

Y_trans <- exp(int + cov1*X + cov2*(X^2))

plot(X,Y)

plot(X,Y_trans)

int <- -.2
cov1 <- .01
cov2 <- -.001
X_out <- int + cov1*X + cov2*(X^2)
de_X_out <- X_out/sd(X_out)
Y_trans2 <- 2*plogis(de_X_out)
Y_trans3 <- 2*plogis(X_out)
plot(Y_trans2,X)
plot(Y_trans3,X)

# simulate some of these bad boys
require(dplyr)
all_out <- lapply(1:1000, function(i) {
  int <- runif(1,-.3,.3)
  cov1 <- runif(1,-.3,.3)
  cov2 <- runif(1,-.01,.01)
  
  X_out <- int + cov1*X + cov2*(X^2)
  X_out <- X_out/sd(X_out)
  return(tibble(X=X,
                Y=2*plogis(X_out),
                iter=i))
}) %>% bind_rows

require(ggplot2)

all_out %>% 
  ggplot(aes(y=Y,x=X)) +
  geom_line(aes(group=iter),
            alpha=0.5) +
  xlab("Time") + 
  ylab("Time-Varying TFP") +
  theme(panel.background = element_blank())

ggsave("random_poly.png")
