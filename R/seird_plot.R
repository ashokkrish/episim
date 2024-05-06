seird_plot <- function() {
  expr <- quote({
    #############################################
    #####            PLOT - SEIRD          #####
    #############################################
    seird_equations <- function(time, variables, parameters) {
      S <- variables[1]
      E <- variables[2]
      I <- variables[3]
      R <- variables[4]
      D <- variables[5]
      N <- variables[6]
      q <- variables[7]
      dS <- (input$muBirth * N) - (input$muDeath * S) - (input$betaSEIRD * ((S * I) / (N^q)))
      dE <- (input$beta * ((S * I) / (N^q))) - (input$gammaSEIRD * E) - (input$muDeath * E)
      dI <- (input$gammaSEIRD * E) - (I * input$sigmaSEIRD) - (input$deltaSEIRD * I) - (input$muDeath * I)
      dR <- (I * input$sigma) - (input$muDeath * R)
      dD <- (input$deltaSEIRD * I)
      dN <- dS + dE + dI + dR
      list(c(dS, dE, dI, dR, dD, dN, q))
    }

    seird_values <- reactive({
      req(input$timesteps, input$betaSEIRD, input$gammaSEIRD, input$muBirth, input$muDeath)

      validate(
        need(input$betaSEIRD >= 0, "Enter a valid value for parameters"),
        need(input$gammaSEIRD >= 0, "Enter a valid value for parameters"),
        need(input$sigmaSEIRD >= 0, "Enter a valid value for parameters"),
        need(input$deltaSEIRD >= 0, "Enter a valid value for parameters"),
        need(input$populationSEIRD > 0, "Enter a valid value for parameters"),
        need(input$exposedSEIRD >= 0, "Enter a valid value for parameters"),
        need(input$susceptibleSEIRD > 0, "Enter a valid value for parameters"),
        need(input$infectedSEIRD >= 0, "Enter a valid value for parameters"),
        need(input$recoveredSEIRD >= 0, "Enter a valid value for parameters"),
        need(input$deadSEIRD >= 0, "Enter a valid value for parameters"),
        need(input$timesteps > 0, "Enter a valid value for parameters"),
        need(input$populationSEIRD == (input$exposedSEIRD + input$susceptibleSEIRD + input$infectedSEIRD + input$recoveredSEIRD), "Number of persons living in all epidemic compartments must sum to the Population Count (N)")
      )

      ode(
        y = c(
          S = input$susceptibleSEIRD,
          E = input$exposedSEIRD,
          I = input$infectedSEIRD,
          R = input$recoveredSEIRD,
          D = input$deadSEIRD,
          N = input$populationSEIRD,
          q = as.integer(input$qValue)
        ),
        times = seq(0, input$timesteps, by = 1),
        func = seird_equations,
        parms = c(
          beta = input$betaSEIRD,
          gamma = input$gammaSEIRD,
          sigma = input$sigmaSEIRD,
          delta = input$deltaSEIRD,
          muB = input$muBirth,
          muD = input$muDeath
        )
      )
    })

    output$plotSEIRD <- renderPlot({
      input$go
      isolate({
        val <- as.data.frame(seird_values())
        ggplot(val, aes(x = time)) +
          ggtitle("SEIRD Epidemic Model") +
          theme(
            axis.line = element_line(color = "black"), axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold")
          ) +
          theme(plot.title = element_text(size = 22, face = "bold")) +
          theme(legend.position = "bottom") +
          ylab("Number of People") +
          xlab("Time") +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          geom_line(aes(x = time, y = S, color = "Blue"), linewidth = 1.5) +
          geom_line(aes(x = time, y = E, color = "Brown"), linewidth = 1.5) +
          geom_line(aes(x = time, y = I, color = "Red"), linewidth = 1.5) +
          geom_line(aes(x = time, y = R, color = "Green"), linewidth = 1.5) +
          geom_line(aes(x = time, y = D, color = "Orange"), linewidth = 1.5) +
          scale_color_identity(
            name = "SEIRD", breaks = c("Blue", "Brown", "Red", "Green", "Orange"),
            labels = c("Susceptible", "Exposed", "Infected", "Recovered", "Dead"), guide = "legend"
          )
      })
    })

    output$SEIRDPhasePlane <- renderPlot({
      input$go
      isolate({
        val <- as.data.frame(seird_values())
        ggplot(val, aes(x = S)) +
          geom_line(aes(y = I, color = "Blue"), linewidth = 1.5) +
          theme(
            axis.line = element_line(color = "black"), axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(size = 16, face = "bold")
          ) +
          theme(legend.position = "bottom") +
          ggtitle("SEIRD Phase Plane") +
          theme(plot.title = element_text(size = 22, face = "bold")) +
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

    output$tableSEIRD <- renderTable({
      input$go
      isolate({
        valSEIRD <- as.data.frame(seird_values())
        valSEIRD <- valSEIRD[-c(8)]
        return(valSEIRD)
      })
    })
  })

  ## Evaluate the above qouted expression in the parent frame (the calling
  ## environment).
  eval.parent(expr)
}
