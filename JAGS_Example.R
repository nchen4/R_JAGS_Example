library(rjags)

#################################################################################################### .
# An hierarchical model example using JAGS                                                      ####
# For R group tutorial                                                                          ####
#
#################################################################################################### .

ModelBHM <- function()
{
    modelString <-
    "model
    {
        for (i in 1:numGroups)
        {
            y[i] ~ dbin(p[i],n[i]);
            logit(p[i]) <- eta[i];
            eta[i] ~ dnorm(mu, tau1)
        }

    #Priors
    mu ~ dnorm(mu0, tau2)
    tau1 ~ dgamma(alpha, beta)

    }"
    return(modelString)
}


nResponse      <- c(4, 5, 6, 7)
nTotal         <- rep(25, 4)
dPriorCenter   <- 0
dPriorPrecision<- 1e-8
dAlpha         <- 1.0
dBeta          <- 1.0
nGroups        <- length(nResponse)

lData<-list(y = nResponse, n = nTotal, mu0 = dPriorCenter, tau2 = dPriorPrecision, alpha = dAlpha, beta = dBeta, numGroups = nGroups)

cat("Start running MCMC...\n")
jagsModel<- jags.model(file = textConnection(ModelBHM()), data = lData, n.chains = 4, n.adapt = 1000)
update(jagsModel, n.iter = 1000)
jagsOut  <- coda.samples(model = jagsModel, variable.names = c("p" , "mu"), n.iter = 10000, thin = 1)


# Trace plot of the sampling
plot(jagsOut)

# Summary
result   <- summary(jagsOut)
print(result)

# Gelman Rubin Diagnostic
gelman.diag(jagsOut)

# Combine the data from different chains
mPostData <- do.call(rbind, jagsOut)
head(mPostData)
dim(mPostData)

# Mean and Median values
apply(mPostData, MARGIN = 2, FUN = function(x){c(mean = mean(x), median = median(x))})

# Invervals
library(boa)
apply(mPostData, MARGIN = 2, FUN = function(x){boa.hpd(x, 0.1)})
apply(mPostData, MARGIN = 2, FUN = function(x){quantile(x, c(0.05, 0.95))})


#################################################################################################### .
# Parallel Sampling                                                                            ####
#################################################################################################### .

library(R2jags)


jags.parallel(data = lData, model.file = "BHM.txt", n.chains = 2, n.iter = 11000, n.burnin = 1000, n.thin = 1,
              parameters.to.save = c("p" , "mu"))

