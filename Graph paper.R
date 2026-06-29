# --- Chargement des bibliothèques ---
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(stringr)
library(grid)

# --- Lecture des données ---
df <- read_excel("C:\\Users\\evancaenegem\\Documents\\Doctorat\\8.Mémoires master\\Marche\\Data_Analyse_Walk_R\\Data_sorted.xlsx", sheet = "Données Brutes")

# --- Palette personnalisée ---
custom_palette <- c("ME" = "#355070", "MI" = "#EAAC8B")

# --- Figure 1 : Temps réel vs imaginé par participant et distance ---
df_long <- df %>%
  select(Participant, starts_with("ME_"), starts_with("MI_")) %>%
  pivot_longer(cols = -Participant,
               names_to = c("Type", "Distance"),
               names_pattern = "(ME|MI)_(\\d+m)",
               values_to = "Temps") %>%
  mutate(Distance = factor(Distance, levels = c("5m", "10m", "15m")))

ggplot(df_long, aes(x = Distance, y = Temps, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Participant) +
  labs(title = "Real vs Imagined walking timing",
       x = "Distance", y = "Time (s)") +
  scale_fill_manual(name = "Condition", values = custom_palette) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# --- Figure 1A : Moyennes par distance ---
df_summary <- df_long %>%
  group_by(Type, Distance) %>%
  summarise(
    mean_time = mean(Temps, na.rm = TRUE),
    sd_time = sd(Temps, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(df_summary, aes(x = Distance, y = mean_time, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_time - sd_time, ymax = mean_time + sd_time),
                position = position_dodge(0.7), width = 0.2) +
  labs(title = "Average Real vs Imagined Walking Time by Distance",
       x = "Distance", y = "Time (s)") +
  scale_fill_manual(name = "Condition", values = custom_palette) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(5, "pt")
  )

# --- Figure 2 : Variabilité intra-individuelle ---
df_diff_variab <- df %>%
  mutate(
    Err_5m = MI_5m - ME_5m,
    Err_10m = MI_10m - ME_10m,
    Err_15m = MI_15m - ME_15m
  )

df_variabilite <- df_diff_variab %>%
  rowwise() %>%
  mutate(Variabilite = sd(c(Err_5m, Err_10m, Err_15m), na.rm = TRUE)) %>%
  ungroup() %>%
  select(Participant, MIQ_3, Variabilite)

ggplot(df_variabilite, aes(x = MIQ_3, y = Variabilite)) +
  geom_point(color = "#66C2A5", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#FC8D62", fill = "#FC8D62", alpha = 0.4) +
  labs(
    title = "Timing difference and MIQ-3",
    subtitle = "Across Distances (5m, 10m, 15m)",
    x = "MIQ-3 Score",
    y = "Time difference (s)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# --- Figure 3A/B : Corrélations par distance ---
df_corr <- df %>%
  select(Participant, starts_with("ME_"), starts_with("MI_")) %>%
  pivot_longer(cols = -Participant,
               names_to = c("Type", "Distance"),
               names_pattern = "(ME|MI)_(\\d+m)",
               values_to = "Temps") %>%
  pivot_wider(names_from = Type, values_from = Temps)

plot_corr_by_distance <- function(distance_label, x_limits, y_limits) {
  data_sub <- df_corr %>% filter(Distance == distance_label)
  corr_stats <- cor.test(data_sub$ME, data_sub$MI)
  label_text <- paste0("r = ", round(corr_stats$estimate, 2), 
                       "\n", ifelse(corr_stats$p.value < 0.001, "p < 0.001", paste0("p = ", signif(corr_stats$p.value, 3))))
  
  ggplot(data_sub, aes(x = ME, y = MI)) +
    geom_point(color = "#E69F00", size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", color = "#56B4E9", se = FALSE, size = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = distance_label, x = "Real Time (s)", y = "Imagined Time (s)") +
    annotate("text", x = x_limits[2] * 0.6, y = y_limits[2] * 0.95, label = label_text, hjust = 0, size = 5) +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 12),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", size = 0.8),
      axis.ticks = element_line(color = "black")
    )
}

p1 <- plot_corr_by_distance("5m", c(0, 20), c(0, 20))
p2 <- plot_corr_by_distance("10m", c(0, 20), c(0, 20))
p3 <- plot_corr_by_distance("15m", c(0, 20), c(0, 20))
ggarrange(p1, p2, p3, ncol = 3)

# --- Figure 3C : Corrélations croisées ME/MI ---
plot_corr_by_combination <- function(me_var, mi_var) {
  data_sub <- df %>% select(all_of(me_var), all_of(mi_var))
  corr_stats <- cor.test(data_sub[[me_var]], data_sub[[mi_var]])
  label_text <- paste0("r = ", round(corr_stats$estimate, 2), 
                       "\n", ifelse(corr_stats$p.value < 0.001, "p < 0.001", paste0("p = ", signif(corr_stats$p.value, 3))))
  
  ggplot(data_sub, aes(x = .data[[me_var]], y = .data[[mi_var]])) +
    geom_point(color = "#E69F00", size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", color = "#56B4E9", se = FALSE, size = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = paste0(me_var, " vs ", mi_var), x = "Real Time (s)", y = "Imagined Time (s)") +
    annotate("text", x = 1.2, y = 10, label = label_text, hjust = 0, size = 5) +
    scale_x_continuous(limits = c(0, 20), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", size = 0.8),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(5, "pt")
    )
}