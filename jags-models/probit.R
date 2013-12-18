##------------------------------------------------------------------------------
##
##    Probit
##    Andreas Beger
##    17 December 2013
##
##------------------------------------------------------------------------------

library(R2jags)

##    Probit data-generating process

X <- matrix(c(rep(1, 1000), rnorm(2000)), ncol=3)
colnames(X) <- c("const", "x1", "x2")

b <- matrix(c(-0.5, 1, 0), ncol=1)
rownames(b) <- c("const", "x1", "x2")

y.star <- X %*% b + rnorm(1000)
y <- as.numeric(pnorm(y.star) > 0.5)

table(y)


##    MLE model

mle.model <- glm(y ~ X[, 2:3], family=binomial(link="probit"))
summary(mle.model)


##    JAGS model

modelstring <-
"model {
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    probit(p[i]) <- b[1] + b[2]*x1[i] + b[3]*x2[i]      
  }
  ## Priors
  for (i in 1:3) {
    b[i] ~ dnorm(0, 0.01)
  }
}"
writeLines(modelstring, con="~/Desktop/jags-model.txt")

jags.model <- jags(
  data=list(N=1000, y=y, x1=X[, 2], x2=X[, 3]),
  parameters.to.save=c("b"),
  model.file="~/Desktop/jags-model.txt",
  n.chains=1,
  n.iter=10500,
  n.burnin=500,
  n.thin=5,
  jags.seed=124)

jags.model$BUGSoutput
