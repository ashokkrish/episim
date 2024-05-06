sird_plot <- function() {
  expr <- quote({
    sird_equations <- function(time, variables, parameters) {
      S <- variables[1]
      I <- variables[2]
      R <- variables[3]
      D <- variables[4]
      N <- variables[5]
      q <- variables[6]
      dS <- (input$muBirth * N) - (input$muDeath * S) - (input$betaSIRD * ((S * I) / (N^q)))
      dI <- (input$betaSIRD * ((S * I) / (N^q))) - (input$gammaSIRD * I) - (input$deltaSIRD * I) - (input$muDeath * I)
      dR <- (input$gammaSIRD * I) - (input$muDeath * R)
      dD <- (input$deltaSIRD * I)
      dN <- dS + dI + dR
      list(c(dS, dI, dR, dD, dN, q))
    }

    sird_values <- reactive({
      req(input$timesteps, input$betaSIRD, input$gammaSIRD, input$deltaSIRD, input$muBirth, input$muDeath)

      validate(
        need(input$betaSIRD >= 0, "Enter a valid value for parameters"),
        need(input$gammaSIRD >= 0, "Enter a valid value for parameters"),
        need(input$deltaSIRD >= 0, "Enter a valid value for parameters"),
        need(input$populationSIRD > 0, "Enter a valid value for parameters"),
        need(input$susceptibleSIRD > 0, "Enter a valid value for parameters"),
        need(input$infectedSIRD >= 0, "Enter a valid value for parameters"),
        need(input$recoveredSIRD >= 0, "Enter a valid value for parameters"),
        need(input$deadSIRD >= 0, "Enter a valid value for parameters"),
        need(input$timesteps > 0, "Timesteps must be greater than 0."),
        need(input$populationSIRD == (input$susceptibleSIRD + input$infectedSIRD + input$recoveredSIRD), "Number of persons living in all epidemic compartments must sum to the Population Count (N)")
      )

      ode(
        y = c(
          S = input$susceptibleSIRD,
          I = input$infectedSIRD,
          R = input$recoveredSIRD,
          D = input$deadSIRD,
          N = input$populationSIRD,
          q = as.integer(input$qValue)
        ),
        times = seq(0, input$timesteps, by = 1),
        func = sird_equations,
        parms = c(
          beta = input$betaSIRD,
          gamma = input$gammaSIRD,
          delta = input$deltaSIRD,
          muB = input$muBirth,
          muD = input$muDeath
        )
      )
    })

    output$plotSIRD <- renderPlot({
      input$go
      isolate({
        val <- as.data.frame(sird_values())
        ggplot(val, aes(x = time)) +
          theme(
            axis.line = element_line(color = "black"), axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold")
          ) +
          ggtitle("SIRD Epidemic Model") +
          theme(plot.title = element_text(size = 22, face = "bold")) +
          theme(legend.position = "bottom") +
          ylab("Number of People") +
          xlab("Time") +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          geom_line(aes(y = S, color = "Blue"), linewidth = 1.5) +
          geom_line(aes(y = I, color = "Red"), linewidth = 1.5) +
          geom_line(aes(y = R, color = "Green"), linewidth = 1.5) +
          geom_line(aes(y = D, color = "Orange"), linewidth = 1.5) +
          scale_color_identity(
            name = "SIRD", breaks = c("Blue", "Red", "Green", "Orange"),
            labels = c("Susceptible", "Infected", "Recovered", "Dead"), guide = "legend"
          )
      })
    })

    output$SIRDPhasePlane <- renderPlot({
      input$go
      isolate({
        val <- as.data.frame(sird_values())
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

    output$tableSIRD <- renderTable({
      input$go
      isolate({
        val <- as.data.frame(sird_values())
        val <- val[-c(7)]
        return(val)
      })
    })
  })

  ## Evaluate the above qouted expression in the parent frame (the calling
  ## environment).
  eval.parent(expr)
}
