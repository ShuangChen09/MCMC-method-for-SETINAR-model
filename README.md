# MCMC-method-for-SETINAR-model
Used bayesian MCMC method to estimate the parameters of self-exciting threshold integer-valued autoregressive model model.


This is a paper I came across while researching data cloning methods for estimating time series models, ([paper link](https://www.sciencedirect.com/science/article/pii/S0167947321002449)). I found it particularly interesting, so I reproduced its code.

### step1
establish the setinar(2,1) model
### step2
claculate the likelihood
### step3
set the priors and get the posteriors
### step4
gibbs sampling to estimate the parameters of lambda, alpha_1, and alpha_2
### step5
estimate the threshold based on MPP criteria and MH sampling on latent variables
