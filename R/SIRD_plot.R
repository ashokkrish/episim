library(deSolve)
library(ggplot2)

equationsSIRD <- function(time, variables, parameters) {
    S <- variables[1]
    I <- variables[2]
    R <- variables[3]
    D <- variables[4]
    N <- variables[5]
    q <- variables[6]
    dS <- (parameters["muB"] * N) - (parameters["muD"] * S) - (parameters["beta"] * ((S * I) / (N^q)))
    dI <- (parameters["beta"] * ((S * I) / (N^q))) - (parameters["gamma"] * I) - (parameters["delta"] * I) - (parameters["muD"] * I)
    dR <- (parameters["gamma"] * I) - (parameters["muD"] * R)
    dD <- (parameters["delta"] * I)
    dN <- dS + dI + dR
    list(c(dS, dI, dR, dD, dN, q))
}

defaultParams <- list(
    beta = 0.001,
    gamma = 0.1,
    delta = 0.05,
    muB = 0,
    muD = 0,
    population = 500,
    susceptible = 499,
    infected = 1,
    recovered = 0,
    dead = 0,
    timesteps = 50,
    q = 0
)

# For standalone usage
# TODO: find out if there's a better way to check if the script is being 
# run standalone or as part of a shiny app
solveSIRD <- function() {
    params <- defaultParams

    source <- if (!shiny::isRunning()) params else input

    timesteps <- source$timesteps
    beta <- source$beta
    gamma <- source$gamma
    delta <- source$delta
    muB <- source$muB
    muD <- source$muD
    population <- source$population
    susceptible <- source$susceptible
    infected <- source$infected
    recovered <- source$recovered
    dead <- source$dead
    q <- source$q

    # Using lsoda
    solnsSIRD <- lsoda(
        y = c(
            S = susceptible,
            I = infected,
            R = recovered,
            D = dead,
            N = population,
            q = q
        ),
        times = seq(0, timesteps, by = 1),
        func = equationsSIRD,
        parms = c(
            beta = beta,
            gamma = gamma,
            delta = delta,
            muB = muB,
            muD = muD
        )
    )

    as.data.frame(solnsSIRD)
}

plotSIRD <- function() {
    valuesSIRD <- solveSIRD()
    ggplot(valuesSIRD, aes(x = time)) +
        theme(
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 14),
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
}


plotPhasePlaneSIRD <- function() {
    valuesSIRD <- solveSIRD()
    ggplot(valuesSIRD, aes(x = S)) +
        geom_line(aes(y = I, color = "Blue"), linewidth = 1.5) +
        theme(
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 14),
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
            name = "SIRD", breaks = c("Blue", "Red", "Green", "Orange"),
            labels = c("Susceptible", "Infected", "Recovered", "Dead"), guide = "legend"
        )
}

# no longer need to do shiny::isRunning() check since this function is only called
# from server.R
solveAndRenderSIRD <- function() {
    expr <- quote({
        output$plotSIRD <- renderPlot({
            plotSIRD()
        })
        output$SIRDPhasePlane <- renderPlot({
            plotPhasePlaneSIRD()
        })
        output$tableSIRD <- renderTable({
            return(solveSIRD()[-c(6)])
        })
    })

    eval.parent(expr)
}

# For standalone usage
# TODO: find out if there's a better way to check if the script is being 
# run standalone or as part of a shiny app
if (!shiny::isRunning()) {
    plot <- plotSIRD()
    print(plot)
    ggsave("sird_plot.png", plot)

    phasePlanePlot <- plotPhasePlaneSIRD()
    print(phasePlanePlot)
    ggsave("sird_phase_plane_plot.png", phasePlanePlot)
}