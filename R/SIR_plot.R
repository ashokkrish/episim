sir_plot <- function() {
  expr <- quote({
    sir_equations <- function(time, variables, parameters) {
      S <- variables[1]
      I <- variables[2]
      R <- variables[3]
      N <- variables[4]
      q <- variables[5]
      dS <- (input$muBirth * N) - (input$muDeath * S) - (input$betaSIR * ((S * I) / (N^q)))
      dI <- (input$betaSIR * ((S * I) / (N^q))) - (input$gammaSIR * I) - (input$muDeath * I)
      dR <- (input$gammaSIR * I) - (input$muDeath * R)
      dN <- dS + dI + dR
      list(c(dS, dI, dR, dN, q))
    }

    sir_values <- reactive({
      req(input$timesteps, input$betaSIR, input$gammaSIR, input$muBirth, input$muDeath)


      validate(
        need(input$betaSIR >= 0, "Enter a valid value for parameters"),
        need(input$gammaSIR >= 0, "Enter a valid value for parameters"),
        need(input$populationSIR > 0, "Enter a valid value for parameters"),
        need(input$susceptibleSIR > 0, "Enter a valid value for parameters"),
        need(input$infectedSIR >= 0, "Enter a valid value for parameters"),
        need(input$recoveredSIR >= 0, "Enter a valid value for parameters"),
        need(input$timesteps > 0, "Enter a valid value for parameters"),
        need(input$populationSIR == (input$susceptibleSIR + input$infectedSIR + input$recoveredSIR), "Number of persons living in all epidemic compartments must sum to the Population Count (N)")
      )


      ode(
        y = c(
          S = input$susceptibleSIR,
          I = input$infectedSIR,
          R = input$recoveredSIR,
          N = input$populationSIR,
          q = as.integer(input$qValue)
        ),
        times = seq(0, input$timesteps, by = 1),
        func = sir_equations,
        parms = c(
          beta = input$betaSIR,
          gamma = input$gammaSIR,
          muB = input$muBirth,
          muD = input$muDeath
        )
      )
    })


    output$plotSIR <- renderPlot({
      input$go
      isolate({
        val <- as.data.frame(sir_values())
        ggplot(val, aes(x = time)) +
          theme(
            axis.line = element_line(color = "black"), axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold")
          ) +
          ggtitle("SIR Epidemic Model") +
          theme(plot.title = element_text(size = 22, face = "bold")) +
          theme(legend.position = "bottom") +
          ylab("Number of People") +
          xlab("Time") +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          geom_line(aes(y = S, color = "Blue"), linewidth = 1.5) +
          geom_line(aes(y = I, color = "Red"), linewidth = 1.5) +
          geom_line(aes(y = R, color = "Green"), linewidth = 1.5) +
          scale_color_identity(
            name = "SIR", breaks = c("Blue", "Red", "Green"),
            labels = c("Susceptible", "Infected", "Recovered"), guide = "legend"
          )
      })
    })

    output$SIRPhasePlane <- renderPlot({
      input$go
      isolate({
        val <- as.data.frame(sir_values())
        ggplot(val, aes(x = S)) +
          geom_line(aes(y = I, color = "Blue"), linewidth = 1.5) +
          theme(
            axis.line = element_line(color = "black"), axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold")
          ) +
          theme(legend.position = "bottom") +
          ggtitle("SI Phase Plane") +
          theme(plot.title = element_text(size = 22, face = "bold")) +
          ylab("Infected (I)") +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          xlab("Susceptible (S)") +
          scale_color_identity(
            breaks = "Blue",
            labels = "Susceptible"
          )
      })
    })


    output$tableSIR <- renderTable({
      input$go
      isolate({
        val <- as.data.frame(sir_values())
        val <- val[-c(6)]
        return(val)
      })
    })
  })

  ## Evaluate the above qouted expression in the parent frame (the calling
  ## environment).
  eval.parent(expr)
}
