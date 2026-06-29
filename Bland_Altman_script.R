# Chargement des librairies nécessaires
library(ggplot2)
library(dplyr)


# Définir le répertoire de travail
setwd("C:\\Users\\evancaenegem\\Documents\\Doctorat\\8.Mémoires master\\Marche\\Data_Analyse_Walk_R\\")

# Chargement des données\ avec adaptation au format francophone
data <- read.csv2('Data_sorted.csv', sep=";", dec=",", header=TRUE)

# Fonction pour Bland-Altman plot adaptée aux colonnes spécifiques
bland_altman_plot <- function(df, measure_ext, measure_int, title, color, filename){
  df <- df %>% mutate(mean = ({{measure_ext}} + {{measure_int}})/2,
                      diff = {{measure_ext}} - {{measure_int}})
  
  mean_diff <- mean(df$diff)
  sd_diff <- sd(df$diff)
  lower <- mean_diff - 1.96 * sd_diff
  upper <- mean_diff + 1.96 * sd_diff
  
  plot <- ggplot(df, aes(x=mean, y=diff)) +
    geom_point(color=color) +
    geom_hline(yintercept = mean_diff, color=color, linetype="solid") +
    geom_hline(yintercept = c(lower, upper), color=color, linetype="dashed") +
    scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +    # Ticks et limites sur X
    scale_y_continuous(limits = c(-7, 5), breaks = seq(-7, 5, 2), expand = c(0, 0)) +    # Inclure 0 dans Y
    labs(title = title, x="Mean of ME and Mi", y="Difference between ME and MI") +
    theme_minimal(base_size = 14) +  # Utilisation de theme minimal
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.line = element_line(color = "black", size = 0.6),        # Bordure des axes
      axis.ticks = element_line(color = "black"),                   # Ticks visibles
      axis.ticks.length = unit(0.25, "cm"),                         # Longueur des ticks
      axis.text = element_text(color = "black"),                    # Couleur du texte des ticks
      axis.title = element_text(color = "black", face = "bold")     # Titres des axes en gras
    )
  
  ggsave(filename, plot, width=8, height=6, bg = "white")
  return(list(plot = plot, mean_diff = mean_diff, lower = lower, upper = upper))
}

# Bland-Altman plots individuels avec sauvegarde
ba_5m <- bland_altman_plot(data, ME_5m, MI_5m, "Bland-Altman Plot à 5m", "#1B9E77", "Bland_Altman_5m.png")
ba_10m <- bland_altman_plot(data, ME_10m, MI_10m, "Bland-Altman Plot à 10m", "#D95F02", "Bland_Altman_10m.png")
ba_15m <- bland_altman_plot(data, ME_15m, MI_15m, "Bland-Altman Plot à 15m", "#7570B3", "Bland_Altman_15m.png")

# Préparation des données globales avec les 30 points
combined_data <- data.frame(
  mean = c((data$ME_5m + data$MI_5m)/2, (data$ME_10m + data$MI_10m)/2, (data$ME_15m + data$MI_15m)/2),
  diff = c(data$ME_5m - data$MI_5m, data$ME_10m - data$MI_10m, data$ME_15m - data$MI_15m),
  Distance = rep(c("5m", "10m", "15m"), each = nrow(data))
)

# Forcer l'ordre pour que les couleurs correspondent
combined_data$Distance <- factor(combined_data$Distance, levels = c("5m", "10m", "15m"))

# Bland-Altman global avec couleurs distinctes et bornes par distance
global_plot <- ggplot(combined_data, aes(x=mean, y=diff, color=Distance)) +
  geom_point() +
  geom_hline(yintercept = c(ba_5m$mean_diff, ba_10m$mean_diff, ba_15m$mean_diff), 
             color=c("#1B9E77", "#D95F02", "#7570B3"), linetype="solid") +
  geom_hline(yintercept = c(ba_5m$lower, ba_10m$lower, ba_15m$lower), 
             color=c("#1B9E77", "#D95F02", "#7570B3"), linetype="dashed") +
  geom_hline(yintercept = c(ba_5m$upper, ba_10m$upper, ba_15m$upper), 
             color=c("#1B9E77", "#D95F02", "#7570B3"), linetype="dashed") +
  
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +    # Ticks et limites sur X
  scale_y_continuous(limits = c(-7, 5), breaks = seq(-7, 5, 2), expand = c(0, 0)) +    # Inclure 0 dans Y
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3")) +  # Définir les couleurs pour chaque Distance
  
  labs(title = "Bland-Altman Plot Global avec Bornes par Distance",
       x="Mean of ME and MI",
       y="Differences between ME and MI",
       color="Distance") +
  
  theme_minimal(base_size = 14) +  # Utilisation de theme minimal
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", size = 0.6),        # Bordure des axes
    axis.ticks = element_line(color = "black"),                   # Ticks visibles
    axis.ticks.length = unit(0.25, "cm"),                         # Longueur des ticks
    axis.text = element_text(color = "black"),                    # Couleur du texte des ticks
    axis.title = element_text(color = "black", face = "bold")     # Titres des axes en gras
  )


ggsave("Bland_Altman_Global.png", global_plot, width=8, height=6, bg = "white")
