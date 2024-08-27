library(ggplot2)
library(gridExtra)

data <- data.frame(
  Metric = factor(rep(c("FA", "FC", "TR", "NC"), 2), levels = c("FA", "FC", "TR", "NC")),
  Estimate = c(7.24e-3, -4.87e-3, 2.56e-3, 9.57e-3, 4.94e-2, -3.70e-2, 2.32e-2, 9.24e-3),
  StdError = c(1.40e-3, 1.29e-3, 1.27e-3, 1.48e-3, 1.17e-2, 1.18e-2, 7.11e-3, 2.17e-2),
  PValue = c(7.222559e-07, 3.082896e-04, 4.375501e-02, 4.468534e-10, 0.0000914357, 0.0035333034, 0.0033426357, 0.6696064187),
  Corpus = rep(c("Dundee (English)", "BCCWJ-EyeTrack (Japanese)"), each = 4)
)

colors <- c("#FF4B00", "#005AFF", "#03AF7A", "grey")

format_p_value <- function(df) {
  df$label_y <- ifelse(df$Metric == "FC", df$Estimate - df$StdError, df$Estimate + df$StdError)
  df$text_label <- ifelse(df$PValue <= 0.001, "p=<0.001", sprintf("p=%.3f", df$PValue))
  df$text_fontface <- ifelse(df$PValue <= 0.05, "bold", "plain")
  return(df)
}

# Dundee
dundee_data <- subset(data, Corpus == "Dundee (English)")
dundee_data <- format_p_value(dundee_data)

dundee_plot <- ggplot(dundee_data, aes(x = Metric, y = Estimate, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Estimate - StdError, ymax = Estimate + StdError),
    width = 0.2, position = position_dodge(0.9)
  ) +
  geom_text(aes(label = text_label, y = label_y, fontface = text_fontface),
    position = position_dodge(0.9), vjust = ifelse(dundee_data$Metric == "FC", 1.5, -0.5),
    size = 6
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") + # add black line at y=0
  scale_y_continuous(limits = c(-0.05, 0.065)) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_blank(), # remove labels on x-axis
    axis.title.y = element_text(size = 14),
    axis.text.x = element_blank(), # remove texts on x-axis
    axis.text.y = element_text(size = 14),
    panel.grid.major.x = element_blank(), # remove grid lines
    panel.grid.minor.x = element_blank(), # remove grid lines
    panel.grid.major.y = element_line(color = "grey", linetype = 1), # write grey lines
    axis.ticks.x = element_blank() # remove the scale on x-axis
  ) +
  labs(title = "Dundee (English)", y = "Coefficient")

# BCCWJ-EyeTrack
bccwj_data <- subset(data, Corpus == "BCCWJ-EyeTrack (Japanese)")
bccwj_data <- format_p_value(bccwj_data)

bccwj_plot <- ggplot(bccwj_data, aes(x = Metric, y = Estimate, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Estimate - StdError, ymax = Estimate + StdError),
    width = 0.2, position = position_dodge(0.9)
  ) +
  geom_text(aes(label = text_label, y = label_y, fontface = text_fontface),
    position = position_dodge(0.9), vjust = ifelse(bccwj_data$Metric == "FC", 1.5, -0.5),
    size = 6
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_y_continuous(limits = c(-0.05, 0.065)) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = 1),
    axis.ticks.x = element_blank()
  ) +
  labs(title = "BCCWJ-EyeTrack (Japanese)", y = "Coefficient")


grid.arrange(dundee_plot, bccwj_plot, ncol = 2)
