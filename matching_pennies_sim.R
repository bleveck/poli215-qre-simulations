logit <- function(x) 1 / (1 + exp(-x))

# ---------- QRE solver for a 2x2 game ----------
qre_2x2 <- function(A, B, lambda, start = c(0.5, 0.5)) {
  obj <- function(z) {
    p <- logit(z[1])   # Prob Row plays action 1
    q <- logit(z[2])   # Prob Column plays action 1

    # Expected payoffs
    ur1 <- A[1,1] * q + A[1,2] * (1 - q)
    ur2 <- A[2,1] * q + A[2,2] * (1 - q)

    uc1 <- B[1,1] * p + B[2,1] * (1 - p)
    uc2 <- B[1,2] * p + B[2,2] * (1 - p)

    br_p <- logit(lambda * (ur1 - ur2))
    br_q <- logit(lambda * (uc1 - uc2))

    (p - br_p)^2 + (q - br_q)^2
  }

  fit <- optim(qlogis(start), obj)
  c(p = logit(fit$par[1]), q = logit(fit$par[2]))
}

# ---------- Smooth fictitious play / noisy best-response learning ----------
simulate_sfp_2x2 <- function(A, B, lambda = 2, alpha = 0.05,
                             T = 3000, N = 400,
                             p0 = 0.9, q0 = 0.1) {

  # phat = Column's belief about Row playing action 1
  # qhat = Row's belief about Column playing action 1
  phat <- p0
  qhat <- q0

  out <- data.frame(
    t = 1:T,
    p_play = NA_real_,
    q_play = NA_real_,
    phat = NA_real_,
    qhat = NA_real_
  )

  for (t in 1:T) {
    # Row's expected payoff difference
    ur1 <- A[1,1] * qhat + A[1,2] * (1 - qhat)
    ur2 <- A[2,1] * qhat + A[2,2] * (1 - qhat)

    # Column's expected payoff difference
    uc1 <- B[1,1] * phat + B[2,1] * (1 - phat)
    uc2 <- B[1,2] * phat + B[2,2] * (1 - phat)

    pr <- logit(lambda * (ur1 - ur2))
    pc <- logit(lambda * (uc1 - uc2))

    # Large population each period, so we get a frequency not just one draw
    p_play <- mean(rbinom(N, 1, pr))
    q_play <- mean(rbinom(N, 1, pc))

    # Belief updating
    phat <- (1 - alpha) * phat + alpha * p_play
    qhat <- (1 - alpha) * qhat + alpha * q_play

    out[t, ] <- c(t, p_play, q_play, phat, qhat)
  }

  out
}

# ---------- Example: asymmetric matching pennies ----------
A <- matrix(c(
   2, -2,
  -1,  1
), nrow = 2, byrow = TRUE)

B <- -A

lambda <- 2
qre_mp <- qre_2x2(A, B, lambda = lambda)
sim_mp <- simulate_sfp_2x2(A, B, lambda = lambda, alpha = 0.05,
                           T = 3000, N = 400, p0 = 0.9, q0 = 0.1)

# Running averages of realized play
sim_mp$run_p <- cumsum(sim_mp$p_play) / sim_mp$t
sim_mp$run_q <- cumsum(sim_mp$q_play) / sim_mp$t

par(mfrow = c(1, 2))
plot(sim_mp$t, sim_mp$run_p, type = "l", ylim = c(0,1),
     xlab = "t", ylab = "Row: Pr(action 1)",
     main = "Asymmetric Matching Pennies")
abline(h = qre_mp["p"], lty = 2)

plot(sim_mp$t, sim_mp$run_q, type = "l", ylim = c(0,1),
     xlab = "t", ylab = "Column: Pr(action 1)",
     main = "Asymmetric Matching Pennies")
abline(h = qre_mp["q"], lty = 2)

qre_mp