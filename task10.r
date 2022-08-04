test_dat <- read.csv('var_17.csv')
head(test_dat)
library(countreg)
library(RMKdiscrete)
zeroinfl.p.fit<- zeroinfl(Output ~ Predictor.Count|Predictor.Zero,data = test_dat,dist= "poisson")
summary(zeroinfl.p.fit)
zeroinfl.nb.fit<- zeroinfl(Output ~ Predictor.Count|Predictor.Zero,data = test_dat,dist= "negbin")
summary(zeroinfl.nb.fit)
AIC(zeroinfl.p.fit)
AIC(zeroinfl.nb.fit)
predicted.prob.zero.component <- predict(zeroinfl.nb.fit,type = "zero")


plot(test_dat$Predictor.Zero,predicted.prob.zero.component,
     main = "Predicted Zero Component Probability Equals Zero")
predicted.prob.count.density <- predict(zeroinfl.nb.fit,type = "prob")

plot(test_dat$Predictor.Count,predicted.prob.count.density[,1],
     main = "Predicted Probability Count Component = 0")
Probability.Zero <- predicted.prob.count.density[,1]
head(Probability.Zero)
plot(test_dat$Predictor.Count,predicted.prob.count.density[,2],
     main = "Predicted Probability Count Component = 1")
head(predicted.prob.count.density[2])
coeff.count<-zeroinfl.nb.fit$coefficients$count
theta <-zeroinfl.nb.fit$theta
theta
mu<-exp(coeff.count[1]+coeff.count[2]*test_dat$Predictor.Count)
p <- 1/(1+mu/theta)
predicted.prob.count.component<-dnegbin(0,p = p,mu=mu)

plot(test_dat$Predictor.Count,predicted.prob.count.component, main = "Predicted Count Component Probability Equals Zero")
head(predicted.prob.count.component)
probCheck<-(predict(zeroinfl.nb.fit,type="prob")[,1]-predict(zeroinfl.nb.fit,type = "zero"))/
  (1-predict(zeroinfl.nb.fit,type = "zero"))
head(cbind(predicted.prob.count.component,probCheck))
rootogram(zeroinfl.nb.fit)
zeroinfl.nb.fit$coefficients$count[1] <-3
zeroinfl.nb.fit$coefficients$count[2] <-2
rootogram(zeroinfl.nb.fit)
rootogram(zeroinfl.p.fit)

#predicted.prob.total = cbind(Probability.Zero, test_dat$Predictor.Count, test_dat$Predictor.Zero)

Probability.Zero <- c( Probability.Zero )
Predictor.Count <- c(test_dat$Predictor.Count)
Predictor.Zero <- c( test_dat$Predictor.Zero)
predicted.prob.total <- cbind(Probability.Zero, Predictor.Count, Predictor.Zero)

res <- list(predicted.prob.zero.component=predicted.prob.zero.component,
            predicted.prob.count.component = predicted.prob.count.component,
            predicted.prob.total = predicted.prob.total,
            theta = theta)

saveRDS(res, file = 'result.rds')
