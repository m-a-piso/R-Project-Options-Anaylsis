############################################
# hedging_plots.R
# ------------------------------------------
# Plotting utilities for “R Project 2025”
# Uses ggplot2 + hrbrthemes::theme_ipsum_rc
#
# Prerequisites in your session:
#   • pnl_A : numeric vector – P&L for Alice (no hedge)
#   • pnl_B : numeric vector – P&L for Bradley (stop-loss)
#   • pnl_C : numeric vector – P&L for Claire (delta-hedge)
# Optional:
#   • path_df : data.frame with columns day, price (if you
#               decide to visualise a sample price path)
############################################

# ----- libraries -----
library(ggplot2)
library(hrbrthemes)   # supplies theme_ipsum_rc()
# If you want the three-in-one layout later, load patchwork:
 library(patchwork)

# ----- helper: histogram of a single strategy -----
plot_pnl_hist <- function(pnl_vec,
                          strategy_name,
                          bins = 60,
                          file_out = NULL,
                          width = 6,
                          height = 4) {
  
  g <- ggplot(data.frame(pnl = pnl_vec),
              aes(x = pnl)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins  = bins,
                   fill  = "#2b70e4",
                   alpha = 0.75) +
    geom_density(size = 1) +
    labs(title = paste0(strategy_name,
                        " – Profit / Loss Distribution"),
         x      = "Profit / Loss (EUR)",
         y      = "Density") +
    theme_ipsum_rc()
  
  if (!is.null(file_out)) {
    ggsave(file_out, g,
           width = width, height = height, dpi = 300)
  }
  
  return(invisible(g))
}

# ----- generate and (optionally) save individual plots -----
p_A <- plot_pnl_hist(P_and_L_A, "No-hedge (Alice)",
                     file_out = "pnl_alice.png")

p_B <- plot_pnl_hist(P_and_L_B, "Stop-loss (Bradley)",
                     file_out = "pnl_bradley.png")

p_C <- plot_pnl_hist(P_and_L_C, "Delta-hedge (Claire)",
                     file_out = "pnl_claire.png")

library(patchwork)
combined <- (p_A / p_B / p_C) + plot_annotation(
                title = "Profit & Loss Distributions – All Strategies",
                theme = theme_ipsum_rc())
ggsave("pnl_all_strategies.png",
       combined, width = 6, height = 12, dpi = 300)

if (exists("path_df")) {
  g_path <- ggplot(path_df, aes(x = day, y = price)) +
     geom_line(size = 0.7) +
     labs(title = "Example Simulated Stock Path",
          x = "Trading Day", y = "Price (EUR)") +
     theme_ipsum_rc()
   ggsave("sample_path.png", g_path, width = 6, height = 4, dpi = 300)
 }