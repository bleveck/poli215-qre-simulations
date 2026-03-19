library(shiny)

# ── Core functions ───────────────────────────────────────────────────
logit <- function(x) 1 / (1 + exp(-x))

qre_2x2 <- function(A, B, lambda, start = c(0.5, 0.5)) {
  obj <- function(z) {
    p <- logit(z[1])
    q <- logit(z[2])
    ur1 <- A[1,1] * q + A[1,2] * (1 - q)
    ur2 <- A[2,1] * q + A[2,2] * (1 - q)
    uc1 <- B[1,1] * p + B[2,1] * (1 - p)
    uc2 <- B[1,2] * p + B[2,2] * (1 - p)
    br_p <- logit(lambda * (ur1 - ur2))
    br_q <- logit(lambda * (uc1 - uc2))
    (p - br_p)^2 + (q - br_q)^2
  }
  fit <- optim(qlogis(start), obj, method = "Nelder-Mead")
  c(p = logit(fit$par[1]), q = logit(fit$par[2]))
}

simulate_sfp_2x2 <- function(A, B, lambda, alpha, T, N, p0, q0) {
  phat <- p0
  qhat <- q0
  out <- data.frame(t = 1:T, p_play = NA_real_, q_play = NA_real_,
                    phat = NA_real_, qhat = NA_real_)
  for (t in 1:T) {
    ur1 <- A[1,1] * qhat + A[1,2] * (1 - qhat)
    ur2 <- A[2,1] * qhat + A[2,2] * (1 - qhat)
    uc1 <- B[1,1] * phat + B[2,1] * (1 - phat)
    uc2 <- B[1,2] * phat + B[2,2] * (1 - phat)
    pr <- logit(lambda * (ur1 - ur2))
    pc <- logit(lambda * (uc1 - uc2))
    p_play <- mean(rbinom(N, 1, pr))
    q_play <- mean(rbinom(N, 1, pc))
    phat <- (1 - alpha) * phat + alpha * p_play
    qhat <- (1 - alpha) * qhat + alpha * q_play
    out[t, ] <- c(t, p_play, q_play, phat, qhat)
  }
  out$run_p <- cumsum(out$p_play) / out$t
  out$run_q <- cumsum(out$q_play) / out$t
  out
}

# ── Preset games ─────────────────────────────────────────────────────
games <- list(
  "Asymmetric Matching Pennies" = list(
    A = matrix(c(2, -2, -1, 1), 2, byrow = TRUE),
    B = matrix(c(-2, 2, 1, -1), 2, byrow = TRUE),
    nash_p = 1/3, nash_q = 1/2
  ),
  "Symmetric Matching Pennies" = list(
    A = matrix(c(1, -1, -1, 1), 2, byrow = TRUE),
    B = matrix(c(-1, 1, 1, -1), 2, byrow = TRUE),
    nash_p = 0.5, nash_q = 0.5
  ),
  "Matching Pennies (9,0)" = list(
    A = matrix(c(9, 0, 0, 1), 2, byrow = TRUE),
    B = matrix(c(0, 1, 1, 0), 2, byrow = TRUE),
    nash_p = 0.5, nash_q = 0.1
  ),
  "Chicken" = list(
    A = matrix(c(0, 5, 2, 4), 2, byrow = TRUE),
    B = matrix(c(0, 2, 5, 4), 2, byrow = TRUE),
    nash_p = 1/3, nash_q = 1/3
  )
)

# ── UI ───────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("QRE and Noisy Learning Explorer — POLI 215"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("game", "Game:", choices = names(games)),

      hr(),
      h4("QRE / Learning Parameters"),
      sliderInput("lambda", HTML("&lambda; (precision):"),
                  min = 0.1, max = 20, value = 2, step = 0.1),
      sliderInput("alpha", HTML("&alpha; (learning rate):"),
                  min = 0.01, max = 0.5, value = 0.05, step = 0.01),

      hr(),
      h4("Simulation Settings"),
      sliderInput("T_rounds", "Rounds (T):", min = 100, max = 5000,
                  value = 3000, step = 100),
      sliderInput("N_pop", "Population size (N):", min = 10, max = 1000,
                  value = 400, step = 10),
      sliderInput("p0", HTML("Starting belief p&#770;<sub>0</sub> (about Row):"),
                  min = 0.01, max = 0.99, value = 0.9, step = 0.01),
      sliderInput("q0", HTML("Starting belief q&#770;<sub>0</sub> (about Column):"),
                  min = 0.01, max = 0.99, value = 0.1, step = 0.01),

      hr(),
      actionButton("run", "Run Simulation", class = "btn-primary", width = "100%"),

      hr(),
      h4("Game Matrices"),
      tableOutput("payoff_table")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(6, h4("QRE Response Function"),
               plotOutput("qrf_plot", height = "350px")),
        column(6, h4("QRE vs Nash"),
               verbatimTextOutput("qre_info"))
      ),
      hr(),
      h4("Smooth Fictitious Play — Convergence"),
      plotOutput("convergence_plot", height = "400px"),
      hr(),
      h4("Belief Dynamics"),
      plotOutput("belief_plot", height = "350px")
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────
server <- function(input, output, session) {

  game_data <- reactive({
    games[[input$game]]
  })

  qre_vals <- reactive({
    g <- game_data()
    qre_2x2(g$A, g$B, lambda = input$lambda)
  })

  sim_result <- eventReactive(input$run, {
    g <- game_data()
    set.seed(sample.int(10000, 1))
    simulate_sfp_2x2(g$A, g$B,
                     lambda = input$lambda,
                     alpha  = input$alpha,
                     T      = input$T_rounds,
                     N      = input$N_pop,
                     p0     = input$p0,
                     q0     = input$q0)
  })

  output$payoff_table <- renderTable({
    g <- game_data()
    df <- data.frame(
      ` ` = c("R1", "R2"),
      C1 = sprintf("(%g, %g)", g$A[,1], g$B[,1]),
      C2 = sprintf("(%g, %g)", g$A[,2], g$B[,2]),
      check.names = FALSE
    )
    df
  }, align = "c", striped = TRUE)

  output$qrf_plot <- renderPlot({
    lam <- input$lambda
    x <- seq(-5, 5, length.out = 500)
    plot(x, logit(lam * x), type = "l", lwd = 3, col = "steelblue",
         xlab = "Eu(A) - Eu(B)", ylab = "Pr(A)",
         main = bquote("Logit QRF," ~ lambda == .(lam)),
         ylim = c(0, 1))
    # Best response for reference
    lines(x, ifelse(x > 0, 1, ifelse(x < 0, 0, 0.5)),
          lty = 2, col = "gray50", lwd = 1.5)
    abline(h = 0.5, lty = 3, col = "gray80")
    abline(v = 0,   lty = 3, col = "gray80")
    legend("bottomright",
           legend = c(bquote("QRF (" * lambda == .(lam) * ")"), "Best response"),
           col = c("steelblue", "gray50"), lty = c(1, 2), lwd = c(3, 1.5),
           bty = "n")
  })

  output$qre_info <- renderText({
    g <- game_data()
    qre <- qre_vals()
    paste0(
      "Game: ", input$game, "\n",
      "lambda = ", input$lambda, "\n\n",
      "--- Nash Equilibrium ---\n",
      sprintf("  p (Row plays R1): %.4f\n", g$nash_p),
      sprintf("  q (Col plays C1): %.4f\n\n", g$nash_q),
      "--- QRE ---\n",
      sprintf("  p (Row plays R1): %.4f\n", qre["p"]),
      sprintf("  q (Col plays C1): %.4f\n\n", qre["q"]),
      "--- Difference ---\n",
      sprintf("  p: QRE - Nash = %+.4f\n", qre["p"] - g$nash_p),
      sprintf("  q: QRE - Nash = %+.4f\n", qre["q"] - g$nash_q)
    )
  })

  output$convergence_plot <- renderPlot({
    req(sim_result())
    sim <- sim_result()
    g   <- game_data()
    qre <- qre_vals()

    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

    # Row
    plot(sim$t, sim$run_p, type = "l", ylim = c(0, 1),
         col = "steelblue", lwd = 2,
         xlab = "Round", ylab = expression("Row: Pr(" * R[1] * ")"),
         main = "Row Player")
    abline(h = qre["p"],   lty = 2, col = "red",    lwd = 1.5)
    abline(h = g$nash_p,   lty = 3, col = "gray50", lwd = 1.5)
    legend("topright",
           legend = c("Running avg",
                      sprintf("QRE (%.3f)", qre["p"]),
                      sprintf("Nash (%.3f)", g$nash_p)),
           col = c("steelblue", "red", "gray50"),
           lty = c(1, 2, 3), lwd = c(2, 1.5, 1.5), bty = "n", cex = 0.85)

    # Column
    plot(sim$t, sim$run_q, type = "l", ylim = c(0, 1),
         col = "steelblue", lwd = 2,
         xlab = "Round", ylab = expression("Column: Pr(" * C[1] * ")"),
         main = "Column Player")
    abline(h = qre["q"],   lty = 2, col = "red",    lwd = 1.5)
    abline(h = g$nash_q,   lty = 3, col = "gray50", lwd = 1.5)
    legend("topright",
           legend = c("Running avg",
                      sprintf("QRE (%.3f)", qre["q"]),
                      sprintf("Nash (%.3f)", g$nash_q)),
           col = c("steelblue", "red", "gray50"),
           lty = c(1, 2, 3), lwd = c(2, 1.5, 1.5), bty = "n", cex = 0.85)
  })

  output$belief_plot <- renderPlot({
    req(sim_result())
    sim <- sim_result()
    g   <- game_data()
    qre <- qre_vals()

    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

    plot(sim$t, sim$phat, type = "l", ylim = c(0, 1),
         col = "darkorange", lwd = 1.5,
         xlab = "Round", ylab = expression(hat(p)[t]),
         main = "Column's belief about Row")
    abline(h = qre["p"], lty = 2, col = "red", lwd = 1.5)

    plot(sim$t, sim$qhat, type = "l", ylim = c(0, 1),
         col = "darkorange", lwd = 1.5,
         xlab = "Round", ylab = expression(hat(q)[t]),
         main = "Row's belief about Column")
    abline(h = qre["q"], lty = 2, col = "red", lwd = 1.5)
  })
}

# ── Launch ───────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
