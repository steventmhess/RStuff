# Mediation in R
# https://library.virginia.edu/data/articles/introduction-to-mediation-analysis
# https://cran.ism.ac.jp/web/packages/mediation/vignettes/mediation-old.pdf

# Test Data
myData <- read.csv('http://static.lib.virginia.edu/statlab/materials/data/mediationData.csv')

#Build all three models (X is treatment, M is mediator, Y is observed)
model.SH0 <- lm(Y~X, data=myData)
summary(model.SH0) # Make sure X, Y are related, Y=b0+b1X+e
model.SHM <- lm(M~X, data=myData)
summary(model.SHM) # M=b0+b2X+e, X must affect M to continue
model.SHY <- lm(Y~X+M, data=myData)
# summary(model.SHY) # Y=b0+b4X+b3M+e, M must affect Y, X affect on Y must weaken (partial med) or vanish (full med) to continue

# Bootstrap, ### iters, test to see whether mediation is stat sig different than 0
library(mediation) # Load the library
results <- mediate(model.SHM, model.SHY, sims=1500, treat="X" , mediator="M")
summary(results)
plot(results)
