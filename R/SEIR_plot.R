seir_plot <- function() {
  expr <- quote({
    #############################################
    #####            PLOT - SEIR            #####
    #############################################
    seir_equations <- function(time, variables, parameters) {
      S <- variables[1]
      E <- variables[2]
      I <- variables[3]
      R <- variables[4]
      N <- variables[5]
      q <- variables[6]
      dS <- (input$muBirth * N) - (input$muDeath * S) - (input$beta * ((S * I) / (N^q)))
      dE <- (input$beta * ((S * I) / (N^q))) - (input$gamma * E) - (input$muDeath * E)
      dI <- (input$gamma * E) - (I * input$sigma) - (input$muDeath * I)
      dR <- (I * input$sigma) - (input$muDeath * R)
      dN <- dS + dE + dI + dR
      list(c(dS, dE, dI, dR, dN, q))
    }

    seir_values <- reactive({
      req(input$timesteps, input$beta, input$gamma, input$muBirth, input$muDeath)

      validate(
        need(input$beta >= 0, "Enter a valid value for parameters"),
        need(input$gamma >= 0, "Enter a valid value for parameters"),
        need(input$sigma >= 0, "Enter a valid value for parameters"),
        need(input$population > 0, "Enter a valid value for parameters"),
        need(input$exposed >= 0, "Enter a valid value for parameters"),
        need(input$susceptible > 0, "Enter a valid value for parameters"),
        need(input$infected >= 0, "Enter a valid value for parameters"),
        need(input$recovered >= 0, "Enter a valid value for parameters"),
        need(input$timesteps > 0, "Enter a valid value for parameters."),
        need(input$population == (input$exposed + input$susceptible + input$infected + input$recovered), "Number of persons living in all epidemic compartments must sum to the Population Count (N)")
      )

      ode(
        y = c(
          S = input$susceptible,
          E = input$exposed,
          I = input$infected,
          R = input$recovered,
          N = input$population,
          q = as.integer(input$qValue)
        ),
        times = seq(0, input$timesteps, by = 1),
        func = seir_equations,
        parms = c(
          beta = input$beta,
          gamma = input$gamma,
          muB = input$muBirth,
          muD = input$muDeath
        )
      )
    })

    output$plotSEIR <- renderPlot({
      input$go
      isolate({
        val <- as.data.frame(seir_values())

        ggplot(val, aes(x = time)) +
          ggtitle("SEIR Epidemic Model") +
          theme(
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold")
          ) +
          theme(legend.position = "bottom") +
          theme(plot.title = element_text(size = 22, face = "bold"), axis.text = element_text(size = 14)) +
          ylab("Number of People") +
          xlab("Time") +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          #
          geom_line(aes(x = time, y = S, color = "Blue"), linewidth = 1.5) +
          geom_line(aes(x = time, y = E, color = "Brown"), linewidth = 1.5) +
          geom_line(aes(x = time, y = I, color = "Red"), linewidth = 1.5) +
          geom_line(aes(x = time, y = R, color = "Green"), linewidth = 1.5) +
          scale_color_identity(
            name = "SEIR", breaks = c("Blue", "Brown", "Red", "Green"),
            labels = c("Susceptible", "Exposed", "Infected", "Recovered"), guide = "legend"
          )
      })
    })

    output$SEIRPhasePlane <- renderPlot({
      input$go
      isolate({
        val <- as.data.frame(seir_values())
        ggplot(val, aes(x = S)) +

          ## MAYBE FIXME: linewidth is unused, apparently. See if removing it
          ## doesn't break anything.
          geom_line(aes(y = I, color = "Blue")) +
          theme(
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold")
          ) +
          ggtitle("SEIR Phase Plane") +
          theme(plot.title = element_text(size = 22, face = "bold")) +
          theme(legend.position = "bottom") +
          ylab("Infected (I)") +
          xlab("Susceptible (S)") +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_color_identity(
            breaks = "Blue",
            labels = "Susceptible"
          )
      })
    })

    output$tableSEIR <- renderTable({
      input$go
      isolate({
        valSEIR <- as.data.frame(seir_values())
        valSEIR <- valSEIR[-c(7)]
        return(valSEIR)
      })
    })
  })

  ## Evaluate the above qouted expression in the parent frame (the calling
  ## environment).
  eval.parent(expr)
}
