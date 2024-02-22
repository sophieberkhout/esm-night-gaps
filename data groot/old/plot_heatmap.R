load("data groot/estimation/cor_objects.Rdata")

# postModelProb <- function (BFdp, BFds, BFdc) {
#   # pause, stop, continue, different
#   BFsp <- BFdp / BFds
#   BFcp <- BFdp / BFdc
#   pD <- BFdp * 0.25 / (0.25 + BFsp * 0.25 + BFcp * 0.25 + BFdp * 0.25)
#   pS <- (1 / BFds) * 0.25 / ((1 / BFdp) * 0.25 + (1 / BFds) * 0.25 + (1 / BFdc) * 0.25 + 0.25)
#   pP <- (1 / BFdp) * 0.25 / ((1 / BFdp) * 0.25 + (1 / BFds) * 0.25 + (1 / BFdc) * 0.25 + 0.25)
#   pC <- (1 / BFdc) * 0.25 / ((1 / BFdp) * 0.25 + (1 / BFds) * 0.25 + (1 / BFdc) * 0.25 + 0.25)
#   
#   df <- data.frame(pD, pS, pP, pC)
#   return(df)
# }

postModelProb <- function (BFpd, BFsd, BFcd) {
  # pause, stop, continue, different
  BFsp <- BFsd / BFpd
  BFcp <- BFcd / BFpd
  pD <- (1 / BFpd) * 0.25 / (0.25 + BFsp * 0.25 + BFcp * 0.25 + (1 / BFpd) * 0.25)
  pS <- BFsd * 0.25 / (BFpd * 0.25 + BFsd * 0.25 + BFcd * 0.25 + 0.25)
  pP <- BFpd * 0.25 / (BFpd * 0.25 + BFsd * 0.25 + BFcd * 0.25 + 0.25)
  pC <- BFcd * 0.25 / (BFpd * 0.25 + BFsd * 0.25 + BFcd * 0.25 + 0.25)
  
  df <- data.frame(pP, pS, pC, pD)
  return(df)
}

postModelProb(bftest$relaxed$BF_phi, bftest$relaxed$BF_0, bftest$relaxed$BF_phict)

calcPostModProbs <- function (r, bfs) {
  postModelProb(bfs[[r]]$BF_phi, bfs[[r]]$BF_0, bfs[[r]]$BF_phict)
}

dfProbs <- data.table::rbindlist(lapply(1:29, calcPostModProbs, bfs = bftest))
dfProbs$item <- names(bftest)

dfLong <- tidyr::pivot_longer(dfProbs, cols = 1:4)

dfLong$name <- factor(dfLong$name, levels = c("pP", "pS", "pC", "pD"))

bestModel <- apply(dfProbs[, 1:4], 1, which.max)
orderModels <- apply(dfProbs[, 1:4], 2, order)

orderP <- dfProbs$item[bestModel == 1][order(dfProbs$pP[bestModel == 1], decreasing = TRUE)]
orderS <- dfProbs$item[bestModel == 2][order(dfProbs$pS[bestModel == 2], decreasing = TRUE)]
orderC <- dfProbs$item[bestModel == 3][order(dfProbs$pC[bestModel == 3], decreasing = TRUE)]
orderSC <- dfProbs$item[bestModel == 2 | bestModel == 3][order(dfProbs$pC[bestModel == 2 | bestModel == 3], decreasing = TRUE)]
orderD <- dfProbs$item[bestModel == 4][order(dfProbs$pD[bestModel == 4], decreasing = TRUE)]
orderItems <- c(orderP, orderS, orderC, orderD)
orderItems <- c(orderP, orderSC, orderD)

dfLong$item <- factor(dfLong$item, levels = rev(orderItems))
library(ggplot2)

ggplot(dfLong, aes(name, item)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = sprintf("%.3f", round(value, 2)))) +
  # scale_fill_gradient2(low = "green", mid = "white", high = "red", midpoint = 0.5, limits = c(0, 1)) +
  scale_fill_gradient(low = "white", high = "turquoise4", limits = c(0, 1)) +
  theme_void() +
  # geom_hline(yintercept = 5.5) +
  # geom_hline(yintercept = 10.5) +
  # geom_hline(yintercept = 16.5) +
  geom_segment(x = -1, xend = 5.5, y = 5.5, yend = 5.5) +
  # geom_segment(x = -1, xend = 5.5, y = 10.5, yend = 10.5) +
  geom_segment(x = -1, xend = 5.5, y = 16.5, yend = 16.5) +
  geom_segment(x = -1, xend = 5.5, y = 29.5, yend = 29.5) +
  theme(legend.position = "none",
        # legend.title = element_blank(),
        # legend.text = element_text(family = "sans", size = 10),
        axis.title = element_text(family = "sans", size = 12, margin = margin(5, 5, 5, 5)),
        axis.text = element_text(family = "sans", size = 12, margin = margin(5, 5, 5, 5)),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_text(hjust = 0.3)) +
  labs(x = "Posterior Model Probabilities", y = "") +
  scale_x_discrete(labels = c(expression("p(Hp | y)"), "p(Hs | y)", "p(Hc | y)", "p(Hd | y)")) +
  geom_text(data = dfAnn, aes(x = x, y = y, label = label), hjust = 0, size = 0.36 * 12) +
  # annotate(geom = "text", 4.55, 30, label = "Preferred", hjust = 0) +
  # annotate(geom = "text", 4.55, 29, label = "Hp", hjust = 0) +
  # annotate(geom = "text", 4.55, 16, label = "Hs", hjust = 0) +
  # annotate(geom = "text", 4.55, 10, label = "Hc", hjust = 0) +
  # annotate(geom = "text", 4.55, 5, label = "Hd", hjust = 0) +
  coord_cartesian(xlim = c(1, 5), ylim = c(1, 30), clip = "off")
ggsave("posterior_model_probabilities.pdf", width = 6, height = 8)


dfAnn <- data.frame(x = 4.55, y = c(30, 29, 16, 10, 5),
                    label = c("Preferred", "Hp", "Hs", "Hc", "Hd"))

dfAnn <- data.frame(x = 4.55, y = c(30, 29, 16, 5),
                    label = c("Preferred", "Hp", "Hs / Hc", "Hd"))

dfLong$item2 <- factor(dfLong$item, levels = orderItems)
dfLong$category <- rep(itemCategory(), each = 4)
dfLong$category <- factor(dfLong$category, levels = c("NA", "PA", "unrest", "self-esteem", "physical"))
dfLong$item2 <- factor(dfLong$item2, levels = df$item[order(df$category, df$gamma, df$item, decreasing = TRUE)])
dfLong$preferred <- rep(bestModel, each = 4)
dfLong$preferred <- factor(dfLong$preferred, labels = c("Pause", "Stop / Continue", "Stop / Continue", "Different"))
ggplot(dfLong) +
  facet_grid(~ preferred, scales = "free_x", space = "free") +
  geom_bar(aes(x = item2, y = value, fill = name), stat = "identity") +
  viridis::scale_fill_viridis(discrete = TRUE, labels = c(expression("p(Hp | y)"), "p(Hs | y)", "p(Hc | y)", "p(Hd | y)")) +
  theme_void() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1.25, size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "sans", size = 12),
        # axis.title.y = element_text(angle = 90),
        axis.text = element_text(margin = margin(5, 5, 5, 5)),
        axis.text.y = element_text(hjust = 0.95),
        axis.ticks.y = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        strip.text = element_text(margin = margin(5, 5, 5, 5), size = 12),
        panel.spacing = unit(7.5, units = "pt"),
        plot.margin = margin(0, 5, 0, 0))
ggsave("posterior_bar.pdf", height = 4, width = 10)
