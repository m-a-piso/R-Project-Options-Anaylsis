set.seed(6398792)

################################################################
# Global parameters in a single list
################################################################
par <- list(
  N      = 100000,   # Number of simulations
  mu     = 0.1,      # Expected return
  sigma  = 0.2,      # Volatility
  r      = 0.03,     # Risk-free interest rate (cont. comp.)
  Tdays  = 183,      # Time to maturity in calendar days
  S0     = 100,      # Initial stock price
  K      = 110       # Strike price
)
par$T       <- par$Tdays / 365          # Time to maturity (years)
par$td      <- 1 / 365                  # One trading day (years)
par$n_days  <- par$Tdays                # Number of re-hedge steps
################################################################

# ---------------------------------------------------------
# Exercise a
with(par, {
  epsilon_T  <- rnorm(N)                                                        # Simulation of shocks
  final_value <- S0 * exp((mu - 0.5 * sigma^2) * T + sigma * sqrt(T) * epsilon_T)
  call_payoff <- ifelse(final_value > K, final_value - K, 0)
  p_positivepayoff <- mean(call_payoff > 0)                                     # P(call ends ITM)
  cat("(a) P(call ends ITM) =", p_positivepayoff, "\n")
  hist(call_payoff[call_payoff > 0], breaks = 100, freq = FALSE,
       main = "Histogram of positive payoffs", xlab = "Payoff")
})

# ---------------------------------------------------------
# Exercise b
# Simulate whole path matrix once (shared across strategies)
S_paths <- with(par, {
  Z  <- matrix(rnorm(N * n_days), nrow = N, ncol = n_days)
  lr <- (mu - 0.5 * sigma^2) * td + sigma * sqrt(td) * Z
  log_prices <- t(apply(lr, 1, function(x) log(S0) + cumsum(x)))
  exp(log_prices)
})
ST <- S_paths[, par$n_days]

# Case A: Trader Alice without hedging
payoff_A <- 100 * pmax(ST - par$K, 0)
P0_A <- quantile(payoff_A, 0.99) / exp(par$r * par$T)                           # 99-% price
cat("(b1) No-hedge 99%-price P0_A =", P0_A, "\n")

# Case B: Trader Bradley with stop-loss hedging
P_and_L_B <- numeric(par$N)   # pre-allocate storage for speed
for (i in seq_len(par$N)) {
  path <- c(par$S0, S_paths[i, ])
  cash <- stock <- numeric(par$n_days + 1)
  for (j in 1:par$n_days) {
    # If spot > strike we want 100 shares, else 0
    if (path[j] > par$K && stock[j] == 0) {
      cash[j]  <- cash[j] - 100 * path[j]   # Buy 100 shares
      stock[j] <- 100
    } else if (path[j] <= par$K && stock[j] == 100) {
      cash[j]  <- cash[j] + 100 * path[j]   # Sell 100 shares
      stock[j] <- 0
    }
    # We park cash (or debt) at the risk-free rate
    cash[j + 1]  <- cash[j] * exp(par$r * par$td)
    stock[j + 1] <- stock[j]
  }
  call_payoff_i <- 100 * pmax(path[par$n_days + 1] - par$K, 0)
  P_and_L_B[i]  <- cash[par$n_days + 1] + stock[par$n_days + 1] * path[par$n_days + 1] -
    call_payoff_i
}
disc_PnL_B <- exp(-par$r * par$T) * sort(P_and_L_B)
P0_B <- -quantile(disc_PnL_B, 0.01)
cat("(b2) Stop-loss 99%-price P0_B =", P0_B, "\n")

# Case C: Trader Claire with delta hedging
bs_delta <- function(S, t, p) {                 # Black-Scholes delta helper
  tau <- p$T - t
  if (tau <= 0) return(as.numeric(S > p$K))
  d1  <- (log(S / p$K) + (p$r + 0.5 * p$sigma^2) * tau) / (p$sigma * sqrt(tau))
  pnorm(d1)
}
P_and_L_C <- numeric(par$N)
for (i in seq_len(par$N)) {
  path <- c(par$S0, S_paths[i, ])
  cash <- shares <- numeric(par$n_days + 1)
  for (j in 1:par$n_days) {
    t_now      <- (j - 1) * par$td
    delta_now  <- bs_delta(path[j], t_now, par)
    target_sh  <- 100 * delta_now
    trade_sh   <- target_sh - shares[j]
    cash[j]    <- cash[j] - trade_sh * path[j]
    shares[j + 1] <- target_sh
    cash[j + 1]   <- cash[j] * exp(par$r * par$td)
  }
  payout         <- 100 * pmax(path[par$n_days + 1] - par$K, 0)
  P_and_L_C[i]   <- cash[par$n_days + 1] + shares[par$n_days + 1] * path[par$n_days + 1] -
    payout
}
disc_PnL_C <- exp(-par$r * par$T) * sort(P_and_L_C)
P0_C       <- -quantile(disc_PnL_C, 0.01)
cat("(b3) Delta-hedge 99%-price P0_C =", P0_C, "\n")

# Final results summary
cat("Trader A (No Hedge) Price P0:", P0_A, "\n")
cat("Trader B (Stop-Loss) Price P0:", P0_B, "\n")
cat("Trader C (Delta Hedge) Price P0:", P0_C, "\n")

# ---------------------------------------------------------
# Exercise c
hist(payoff_A,      breaks = 50, freq = FALSE,
     main = "No-hedge payoffs (Alice)",   xlab = "EUR")
hist(P_and_L_B,     breaks = 50, freq = FALSE,
     main = "Stop-loss P/L (Bradley)",    xlab = "EUR")
hist(P_and_L_C,     breaks = 50, freq = FALSE,
     main = "Delta-hedge P/L (Claire)",   xlab = "EUR")

# ---------------------------------------------------------
# Exercise d : CARA indifference prices for a grid of 'a'
# The choice of six 'a' values is arbitrary; we wanted to test
# lower levels of risk aversion and compare.
risk_grid <- c(0.001, 0.002, 0.005, 0.01, 0.03, 0.05)
indiff_tbl <- data.frame(a = risk_grid,
                         P0_A = NA_real_,
                         P0_B = NA_real_,
                         P0_C = NA_real_)

for (k in seq_along(risk_grid)) {
  a <- risk_grid[k]
  utility <- function(x) if (a == 0) x else 1/a * (1 - exp(-a * x))
  u0 <- utility(0)
  
  EU_A <- function(P0) mean(utility(exp(par$r * par$T) * P0 - payoff_A))
  EU_B <- function(P0) mean(utility(exp(par$r * par$T) * P0 + P_and_L_B))
  EU_C <- function(P0) mean(utility(exp(par$r * par$T) * P0 + P_and_L_C))
  
  find_P0 <- function(EUfun, lower = 0, upper = 1000, step = 100) {
    while ((EUfun(lower) - u0) * (EUfun(upper) - u0) > 0) {
      upper <- upper + step
      if (upper > 1e6) stop("Could not bracket root")
    }
    uniroot(function(P0) EUfun(P0) - u0,
            lower = lower, upper = upper)$root
  }
  indiff_tbl$P0_A[k] <- find_P0(EU_A)
  indiff_tbl$P0_B[k] <- find_P0(EU_B)
  indiff_tbl$P0_C[k] <- find_P0(EU_C)
}
print(indiff_tbl, row.names = FALSE)
cat("\n")

# --------------------------------------------------------
# Exercise e
B <- 90                                   # Barrier level at €90
breached  <- apply(S_paths, 1, function(x) any(x <= B))
itm       <- ST > par$K                   # Indicator if regular call finishes ITM
cond_prob <- sum(breached & itm) / sum(itm)
cat("(e) P(DO call worthless | call ITM) =", cond_prob, "\n")
# -----------------------------------------------------------
