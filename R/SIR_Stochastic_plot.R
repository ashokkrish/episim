sir_stochastic_plot <- function() {
  expr <- quote({
    equal <- .Primitive("==")
    validation_string <- "Enter a valid value for parameters"
    output$plotSIR_Stoc <- renderPlot({
      isolate({
        validate(
          need(input$stochasticSIR > 0, message = validation_string),
          need(input$betaSIR_Stoc >= 0, message = validation_string),
          need(input$gammaSIR_Stoc >= 0, message = validation_string),
          need(input$populationSIR_Stoc > 0, message = validation_string),
          need(input$susceptibleSIR_Stoc > 0, message = validation_string),
          need(input$infectedSIR_Stoc >= 0, message = validation_string),
          need(input$recoveredSIR_Stoc >= 0, message = validation_string),
          need(input$timesteps > 0, message = validation_string),
          sum(
            input$susceptibleSIR_Stoc,
            input$infectedSIR_Stoc,
            input$recoveredSIR_Stoc
          ) |>
            equal(input$populationSIR_Stoc) |>
            need(message = gsub("\n", "", "Number of persons living in all \
epidemic compartments must sum to the Population Count (population_size)"))
        )

        number_simulations <- input$stochasticSIR # TODO: change the inputId.
        population_size <- input$populationSIR_Stoc
        proportion_vaccinated <- 0
        inititial_immune <- input$recoveredSIR_Stoc
        initial_infected <- input$infectedSIR_Stoc
        R0 <- 3.5
        infper <- 1 # infectious period in days
        beta <- R0 / (1000) # transmission coefficient
        step <- 0.1 # time steps (in days)
        timeseq <- seq(0, 10, by = step) # discrete time intervals to simulate

        plot(
          x = 0,
          y = 0,
          type = "n",
          xlim = c(min(timeseq), max(timeseq)),
          ylim = c(0, population_size + 10),
          bty = "n",
          xlab = "time (days)",
          ylab = "# hosts",
          lwd = 1
        )

        cumulative_incidence <- rep(initial_infected, number_simulations)

        ## TODO: vectorize this loop with the use of lead and lag. NOTE: "do
        ## number_simulations outbreaks" was the original comment for this outer
        ## loop. Is this a misnomer? It seems to me there should be _one_
        ## outbreak and that number_simulations is actually the previous
        ## iteration's value, i.e. the prior value of the differential equation.
        ## This needs to be reviewed in persion with Dr. Krishnamurthy.
        for (ss in 1:number_simulations) {
          sim <- data.frame(
            S = population_size - initial_infected - inititial_immune,
            I = initial_infected,
            R = inititial_immune
          )

          ## run through time series
          for (ii in 2:length(timeseq)) {
            ## probability of S -> I per unit S
            probability_of_transmission <- 1 - exp(-beta * step * sim$I[ii - 1])

            ## number new infections
            new_infections <-
              rbinom(1, sim$S[ii - 1], probability_of_transmission)

            ## probability of I -> R
            probability_of_recovery <- 1 - exp(-1 / infper * step)

            ## number new recoveries
            new_recovered <- rbinom(1, sim$I[ii - 1], probability_of_recovery)

            temp_s <- sim$S[ii - 1] - new_infections
            temp_i <- sim$I[ii - 1] + new_infections - new_recovered
            temp_r <- sim$R[ii - 1] + new_recovered

            sim <- rbind(sim, c(temp_s, temp_i, temp_r))
            cumulative_incidence[ss] <-
              cumulative_incidence[ss] + new_infections
          }

          lines(timeseq, sim[, "S"], col = "blue")
          lines(timeseq, sim[, "I"], col = "red")
          lines(timeseq, sim[, "R"], col = "green")
        }
      })
    })
  })

  ## Evaluate the above qouted expression in the parent frame (the calling
  ## environment).
  eval.parent(expr)
}
