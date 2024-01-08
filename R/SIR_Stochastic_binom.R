## Original Author is Bellan et al., (2012)
## How to make epidemiological training infectious by Bellan et al. 2012 PLoS Biology
##
## NOTE: This file can be converted to an R script by replacing the .txt suffix with a .R 
## suffix. Individual R files, additional exercises, and future code updates are available
## for download from the MMED website at:	http://lalashan.mcmaster.ca/theobio/mmed/index.php/MMF

## By manipulating N, R0, and infectious period quantities, organizers
## can experiment with how they think their outbreak may proceed. We
## assume a latent period of 0 as participants can infect individuals
## as soon as they are infected as we performed the exercise but we
## leave it up to others to add latent periods.

## Note that the following stochastic simulation uses a chain binomial
## algorithm to simulate the epidemics while the model for Figure 3 is
## produced using the Gillespie algorithm.  These both are tenable
## ways to model an outbreak and differ primarily in that the former
## considers time in discrete units and the latter considers time to
## be continuous.

#-------------------------------------------------------------#
# Original code slightly modified by Ashok Krishnamurthy, PhD #
#-------------------------------------------------------------#

num.sims <- 50                               # number of simulations
N <- 1000                                    # number participants
vacc.prop <- 0 #0.35                         # immune proportion at the beginning
init.immune <- round(vacc.prop*N)            # initial immune individuals
init.infect <- 4
R0 <- 3.5                                    # R0
infper <- 1                                  # infectious period in days
## latper <- 0                               # latent period in days
### R0 = beta*N/(1/infper) = beta*N*infper
### beta = R0/(N*infper)
beta <- R0/(N*infper)                        # transmission coefficient
step <- 0.1                                  # time steps (in days)
timeseq <- seq(0, 10, by = step)             # discrete time intervals to simulate

plot(0,0, type ="n", xlim = c(min(timeseq), max(timeseq)),
     ylim = c(0, N+10), bty = "n", xlab = "time (days)", ylab = "# hosts", lwd = 1)

cum.incidence <- rep(init.infect,num.sims)

for(ss in 1:num.sims)              # do num.sims outbreaks
{
     sim <- data.frame(S = N - init.infect - init.immune, I = init.infect, R = init.immune)
     
     for(ii in 2:length(timeseq))                        # run through time series
     {
          p.trans <- 1 - exp(-beta*step*sim$I[ii-1])     # probability of S -> I per unit S
          new.inf <- rbinom(1, sim$S[ii-1], p.trans)     # number new infections
          
          p.recov <- 1 - exp(-1/infper*step)             # probability of I -> R
          new.recov <- rbinom(1, sim$I[ii-1], p.recov)   # number new recoveries
          
          temp.S <- sim$S[ii-1] - new.inf
          temp.I <- sim$I[ii-1] + new.inf - new.recov
          temp.R <- sim$R[ii-1] + new.recov
          
          sim <- rbind(sim, c(temp.S, temp.I, temp.R))
          cum.incidence[ss] <- cum.incidence[ss] + new.inf
     }
     lines(timeseq, sim[,"S"],col = "blue")
     lines(timeseq, sim[,"I"],col = "red")
     lines(timeseq, sim[,"R"],col = "green")
}
