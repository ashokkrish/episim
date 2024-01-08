#------------------------------------------------------#
# Stochastic SIR epidemic model in a closed population #
#------------------------------------------------------#

library(ggplot2)

# grow vectors by this many elements at a time
chunksize = 50

# initial conditions at time t = 0
S0 = 254
I0 = 7

# parameter values
alpha = 2.73
beta = 0.0178

# vectorlengths keeps track of how long are state vectors are
vectorlengths = chunksize

# Create empty vectors.
S = rep(0,chunksize)
I = rep(0,chunksize)
R = rep(0,chunksize)
et = rep(0,chunksize)

# initialize vectors
S[1] = S0
I[1] = I0
R[1] = 0
et[1] = 0
i = 1

while (I[i] > 0) 
{ 
	# while there are still infectious individuals
	print(sprintf('event number %d, t=%g, I = %d', i, et[i], I[i]))

	rSI = beta*S[i]*I[i] # total rate of S->I
	rIR = alpha*I[i] # total rate of I->R
	totalRate = rSI + rIR # total rate of all combined events
	et[i+1] = et[i] + rexp(1,totalRate) # time of next event

	if (runif(1) < rSI/totalRate)
	{
	# a susceptible individual becomes infectious
	S[i+1] = S[i]-1
	I[i+1] = I[i]+1
	R[i+1] = R[i]
	} 
	else 
	{
	# an infectious individual recovers
	S[i+1] = S[i]
	I[i+1] = I[i]-1
	R[i+1] = R[i]+1
	}

	i = i + 1

	# If our vectors have reached the length we've pre-allocated
	# space for, then "grow" them. Setting the length this way
	# pads the extra elements with "NA" values.

	if (i == vectorlengths) 
	{
	vectorlengths = vectorlengths + chunksize

	length(et) = vectorlengths
	length(S) = vectorlengths
	length(I) = vectorlengths
	length(R) = vectorlengths

	# instead you could pad with zeros, by doing e.g.
	# S = c(S, rep(0,chunksize))
	# and similarly for I, R, and et
	}
}

# when we're all done, strip off any extra elements remaining at
# the ends of the vectors, which we didn't use

length(et) = i
length(S) = i
length(I) = i
length(R) = i

# 	# plot S, I, and R versus time
# 	matplot(et,cbind(S,I,R),type='l',col=c('blue', 'red','green'), lty=c(1,1,1), xlab = "Time", ylab = "Compartment Size", main = "Stochastic SIR Epidemic Model")
# 	legend('topright', legend = c('S(t)', 'I(t)', 'R(t)'),
# 	col=c('blue', 'red','green'), lty = c(1,1,1))
# 	#abline(v = 0, h = 0)
# 	
# 	# also plot the SI-phase plane in a new figure window
# 	dev.new()
# 	plot(S, I, type ='l', main = 'SI Phase Plane', xlab ='S(t)', ylab ='I(t)')
# 	#abline(v = 0, h = 0)

  # Combine data into a data frame
  df <- data.frame(Time = et[1:i], S = S[1:i], I = I[1:i], R = R[1:i])
  
  # Plot S, I, and R versus time
  ggplot(df, aes(x = Time)) +
    geom_line(aes(y = S, color = "S(t)"), linetype = "solid", linewidth = 1) +
    geom_line(aes(y = I, color = "I(t)"), linetype = "solid", linewidth = 1) +
    geom_line(aes(y = R, color = "R(t)"), linetype = "solid", linewidth = 1) +
    labs(x = "Time", y = "Number of individuals") +
    ggtitle("Stochastic SIR Epidemic Model") +
    scale_color_manual(name = "Compartment",
                       values = c("S(t)" = "blue", "I(t)" = "red", "R(t)" = "green"),
                       breaks = c("S(t)", "I(t)", "R(t)")) +
    theme_minimal() +
    theme(panel.grid = element_blank(), legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    geom_vline(xintercept = 0, linetype = "solid") +
    geom_hline(yintercept = 0, linetype = "solid")
  
  # # Plot the SI-phase plane
  # ggplot(df, aes(x = S, y = I)) +
  #   geom_line(linetype = "solid") +
  #   labs(x = "S(t)", y = "I(t)", title = "SI Phase Plane") +
  #   theme_minimal()