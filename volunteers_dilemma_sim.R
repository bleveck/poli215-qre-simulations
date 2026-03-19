# ---------- QRE for volunteer's dilemma ----------
qre_volunteer <- function(n = 4, b = 2, c = 1, lambda = 2) {
  f <- function(p) {
    p - logit(lambda * (b * (1 - p)^(n - 1) - c))
  }
  uniroot(f, interval = c(1e-6, 1 - 1e-6))$root
}

# ---------- Mean-field noisy learning ----------
simulate_volunteer <- function(n = 4, b = 2, c = 1,
                               lambda = 2, alpha = 0.05,
                               T = 3000, N = 400, p0 = 0.8) {
  # common belief about probability another player volunteers
  phat <- p0

  out <- data.frame(
    t = 1:T,
    p_play = NA_real_,
    phat = NA_real_
  )

  for (t in 1:T) {
    delta_u <- b * (1 - phat)^(n - 1) - c
    pr <- logit(lambda * delta_u)

    p_play <- mean(rbinom(N, 1, pr))

    phat <- (1 - alpha) * phat + alpha * p_play
    out[t, ] <- c(t, p_play, phat)
  }

  out
}

# ---------- Example ----------
lambda <- 2
qre_vd <- qre_volunteer(n = 4, b = 2, c = 1, lambda = lambda)
sim_vd <- simulate_volunteer(n = 4, b = 2, c = 1,
                             lambda = lambda, alpha = 0.05,
                             T = 3000, N = 400, p0 = 0.8)

sim_vd$run_p <- cumsum(sim_vd$p_play) / sim_vd$t

plot(sim_vd$t, sim_vd$run_p, type = "l", ylim = c(0,1),
     xlab = "t", ylab = "Pr(volunteer)",
     main = "Volunteer's Dilemma")
abline(h = qre_vd, lty = 2)

qre_vd