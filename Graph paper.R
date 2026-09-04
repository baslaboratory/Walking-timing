# --- Chargement des bibliothèques ---
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(stringr)
library(grid)

# --- Lecture des données ---
df <- read_excel("C:\\Users\\evancaenegem\\Documents\\Doctorat\\8.Mémoires master\\2024-2026\\Marche\\Data_Analyse_Walk_R\\Data_sorted.xlsx", sheet = "Données Brutes")

# --- Palette personnalisée ---
custom_palette <- c("ME" = "#355070", "MI" = "#C9D4E1")

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
# --- Figure 1A bis : Graph combiné ---

# --- Chargement des bibliothèques ---
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(stringr)
library(grid)

# --- Lecture des données ---
df <- read_excel(
  "C:\\Users\\evancaenegem\\Documents\\Doctorat\\8.Mémoires master\\2024-2026\\Marche\\Data_Analyse_Walk_R\\Data_sorted.xlsx",
  sheet = "Données Brutes"
)

# --- Palette personnalisée ---
custom_palette <- c(
  "ME" = "#355070",
  "MI" = "#C9D4E1"
)

# --- Mise en format long ---
df_long <- df %>%
  select(Participant, starts_with("ME_"), starts_with("MI_")) %>%
  pivot_longer(
    cols = -Participant,
    names_to = c("Type", "Distance"),
    names_pattern = "(ME|MI)_(\\d+m)",
    values_to = "Temps"
  ) %>%
  mutate(
    Distance = factor(
      Distance,
      levels = c("5m", "10m", "15m")
    ),
    Type = factor(
      Type,
      levels = c("ME", "MI")
    )
  )

# ============================================================
# POSITIONS HORIZONTALES
# ============================================================

distance_x <- c(
  "5m" = 1,
  "10m" = 2,
  "15m" = 3
)

# Position des barres
condition_offset <- c(
  "ME" = -0.18,
  "MI" =  0.18
)

# Position des points :
# légèrement vers l'extérieur par rapport aux barres
point_offset <- c(
  "ME" = -0.32,
  "MI" =  0.32
)

df_long <- df_long %>%
  mutate(
    x_base = distance_x[as.character(Distance)],
    
    # Position de la barre
    x_bar = x_base + condition_offset[as.character(Type)],
    
    # Position des points et des lignes
    x_point = x_base + point_offset[as.character(Type)]
  )


# ============================================================
# MOYENNES + SEM
# ============================================================

df_summary <- df_long %>%
  group_by(Type, Distance) %>%
  summarise(
    mean_time = mean(Temps, na.rm = TRUE),
    sd_time = sd(Temps, na.rm = TRUE),
    n = sum(!is.na(Temps)),
    sem_time = sd_time / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    x_base = distance_x[as.character(Distance)],
    x_bar = x_base + condition_offset[as.character(Type)]
  )


# ============================================================
# GRAPH
# ============================================================

ggplot() +
  
  # ----------------------------------------------------------
# BARRES = MOYENNES
# ----------------------------------------------------------

geom_col(
  data = df_summary,
  aes(
    x = x_bar,
    y = mean_time,
    fill = Type
  ),
  width = 0.34,
  alpha = 0.75
) +
  
  # ----------------------------------------------------------
# SEM
# ----------------------------------------------------------

geom_errorbar(
  data = df_summary,
  aes(
    x = x_bar,
    y = mean_time,
    ymin = mean_time - sem_time,
    ymax = mean_time + sem_time
  ),
  width = 0.10,
  linewidth = 0.7,
  color = "black"
) +
  
  # ----------------------------------------------------------
# LIGNES INDIVIDUELLES
# ----------------------------------------------------------

geom_line(
  data = df_long,
  aes(
    x = x_point,
    y = Temps,
    group = interaction(Participant, Distance)
  ),
  color = "grey65",
  linewidth = 0.25,
  alpha = 0.30,
  na.rm = TRUE
) +
  
  # ----------------------------------------------------------
# POINTS INDIVIDUELS
# ----------------------------------------------------------

geom_point(
  data = df_long,
  aes(
    x = x_point,
    y = Temps,
    fill = Type
  ),
  shape = 21,
  color = "black",
  size = 2.2,
  stroke = 0.35,
  alpha = 0.85,
  na.rm = TRUE
) +
  
  # ----------------------------------------------------------
# AXE X
# ----------------------------------------------------------

scale_x_continuous(
  breaks = c(1, 2, 3),
  labels = c("5m", "10m", "15m"),
  limits = c(0.5, 3.5)
) +
  
  # ----------------------------------------------------------
# COULEURS
# ----------------------------------------------------------

scale_fill_manual(
  name = "Modality",
  values = custom_palette,
  labels = c(
    "ME" = "Execution",
    "MI" = "Imagery"
  )
) +
  
  # ----------------------------------------------------------
# LABELS
# ----------------------------------------------------------

labs(
  title = "Average Real vs Imagined Walking Time by Distance",
  x = "Distance",
  y = "Time (s)"
) +
  
  # ----------------------------------------------------------
# THEME
# ----------------------------------------------------------

theme_classic() +
  
  theme(
    plot.title = element_text(
      size = 16,
      hjust = 0.5,
      face = "bold"
    ),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    axis.line = element_line(
      color = "black",
      linewidth = 0.8
    ),
    axis.ticks = element_line(
      color = "black"
    ),
    axis.ticks.length = unit(5, "pt")
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

# --- Figure 2 : MIQ-3 correlation ---
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

# --- Calcul de la corrélation ---
cor_test <- cor.test(
  df_variabilite$MIQ_3,
  df_variabilite$Variabilite,
  method = "pearson"
)

r_value <- cor_test$estimate
p_value <- cor_test$p.value

# Texte à afficher sur le graphique
cor_label <- paste0(
  "r = ", round(r_value, 2),
  "\np = ", format.pval(p_value, digits = 2, eps = 0.001)
)

# --- Graphique ---
ggplot(df_variabilite, aes(x = MIQ_3, y = Variabilite)) +
  
 # Shade de l'intervalle de confiance
geom_smooth(
  method = "lm",
  se = TRUE,
  color = NA,
  fill = "#C9D4E1",
  alpha = 0.4
) +

# Droite de régression parfaitement droite
geom_abline(
  intercept = coef(lm(Variabilite ~ MIQ_3, data = df_variabilite))[1],
  slope = coef(lm(Variabilite ~ MIQ_3, data = df_variabilite))[2],
  color = "#7A8796",
  linetype = "dotted",
  linewidth = 0.7
) +

# Points au premier plan
geom_point(
  color = "#1B263B",
  size = 3,
  alpha = 0.8
)+
  
  # r et p-value
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = cor_label,
    hjust = 1.1,
    vjust = 1.5,
    size = 5
  ) +
  
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
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


# --- Figure 3A/B : Corrélations par distance ---

df_corr <- df %>%
  select(
    Participant,
    starts_with("ME_"),
    starts_with("MI_")
  ) %>%
  pivot_longer(
    cols = -Participant,
    names_to = c("Type", "Distance"),
    names_pattern = "(ME|MI)_(\\d+m)",
    values_to = "Temps"
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Temps
  )


# Couleurs des droites de régression
distance_line_colors <- c(
  "5m"  = "#287FA8",  # bleu moyen
  "10m" = "#00717F",  # turquoise foncé
  "15m" = "#003B75"   # bleu foncé
)

# Couleurs des points et intervalles de confiance
distance_light_colors <- c(
  "5m"  = "#9DD5F0",  # bleu clair
  "10m" = "#65C3C8",  # turquoise clair
  "15m" = "#7CA6D8"   # bleu clair associé au bleu foncé
)


# Fonction pour créer un graphique par distance
plot_corr_by_distance <- function(distance_label, x_limits, y_limits) {
  
  data_sub <- df_corr %>%
    filter(Distance == distance_label)
  
  # Test de corrélation
  corr_stats <- cor.test(
    data_sub$ME,
    data_sub$MI,
    use = "complete.obs"
  )
  
  # Texte des résultats
  label_text <- paste0(
    "r = ", round(unname(corr_stats$estimate), 2),
    "\n",
    ifelse(
      corr_stats$p.value < 0.001,
      "p < 0.001",
      paste0(
        "p = ",
        format.pval(
          corr_stats$p.value,
          digits = 3,
          eps = 0.001
        )
      )
    )
  )
  
  # Couleurs correspondant à la distance
  line_color <- distance_line_colors[[distance_label]]
  light_color <- distance_light_colors[[distance_label]]
  
  ggplot(data_sub, aes(x = ME, y = MI)) +
    
    # Intervalle de confiance et droite de régression
    # Placés avant les points pour que ceux-ci restent visibles
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = line_color,
      fill = light_color,
      linewidth = 1.4,
      alpha = 0.30
    ) +
    
    # Ligne d'identité : temps réel = temps imaginé
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "grey45",
      linewidth = 0.6
    ) +
    
    # Points individuels
    geom_point(
      color = line_color,
      fill = light_color,
      shape = 21,
      stroke = 0.6,
      size = 1.5,
      alpha = 0.90
    ) +
    
    labs(
      title = distance_label,
      x = "Executed Time (s)",
      y = "Imagined Time (s)"
    ) +
    
    # Résultats de corrélation en bas à droite
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      label = label_text,
      hjust = 1.15,
      vjust = -0.6,
      size = 4.5,
      color = "black"
    ) +
    
    scale_x_continuous(
      limits = x_limits,
      breaks = seq(
        from = x_limits[1],
        to = x_limits[2],
        by = 5
      ),
      expand = c(0, 0)
    ) +
    
    scale_y_continuous(
      limits = y_limits,
      breaks = seq(
        from = y_limits[1],
        to = y_limits[2],
        by = 5
      ),
      expand = c(0, 0)
    ) +
    
    # Même échelle et même longueur sur les axes X et Y
    coord_fixed(ratio = 1) +
    
    theme_minimal(base_size = 16) +
    
    theme(
      plot.title = element_text(
        size = 22,
        face = "bold",
        hjust = 0.5,
        color = line_color,
        margin = margin(b = 12)
      ),
      
      axis.title.x = element_text(
        size = 16,
        color = "black",
        margin = margin(t = 12)
      ),
      
      axis.title.y = element_text(
        size = 16,
        color = "black",
        margin = margin(r = 12)
      ),
      
      axis.text = element_text(
        size = 16,
        color = "black"
      ),
      
      panel.grid = element_blank(),
      
      axis.line = element_line(
        color = "black",
        linewidth = 1
      ),
      
      axis.ticks = element_line(
        color = "black",
        linewidth = 1
      ),
      
      axis.ticks.length = unit(0.2, "cm"),
      
      aspect.ratio = 1,
      
      plot.margin = margin(
        t = 10,
        r = 15,
        b = 10,
        l = 10
      )
    )
}


# Création des trois graphiques

p1 <- plot_corr_by_distance(
  distance_label = "5m",
  x_limits = c(0, 20),
  y_limits = c(0, 20)
)

p2 <- plot_corr_by_distance(
  distance_label = "10m",
  x_limits = c(0, 20),
  y_limits = c(0, 20)
)

p3 <- plot_corr_by_distance(
  distance_label = "15m",
  x_limits = c(0, 20),
  y_limits = c(0, 20)
)


# Assemblage des graphiques

figure_corr <- ggarrange(
  p1,
  p2,
  p3,
  ncol = 3,
  nrow = 1,
  align = "hv"
)

figure_corr
# --- Figure : Corrélations entre résolutions d'imagerie ---

df_imaging_corr <- df %>%
  select(
    Participant,
    starts_with("ME_"),
    starts_with("MI_")
  )


# Couleurs des droites de régression
comparison_line_colors <- c(
  "5m vs 10m"  = "#007F86",  # turquoise foncé
  "10m vs 15m" = "#005A70",  # bleu pétrole
  "5m vs 15m"  = "#003B5C"   # bleu très foncé
)

# Couleurs des points et intervalles de confiance
comparison_light_colors <- c(
  "5m vs 10m"  = "#9BD3D5",  # turquoise clair
  "10m vs 15m" = "#8BBCC7",  # bleu-gris clair
  "5m vs 15m"  = "#A7B9C5"   # gris bleuté
)


# Fonction pour créer un graphique par comparaison
plot_corr_by_comparison <- function(
    comparison_label,
    x_distance,
    y_distance,
    x_limits,
    y_limits
) {
  
  # Extraction des deux résolutions
  data_sub <- df_imaging_corr %>%
    select(
      Participant,
      x = all_of(paste0("ME_", x_distance)),
      y = all_of(paste0("ME_", y_distance))
    ) %>%
    filter(
      complete.cases(x, y)
    )
  
  
  # Test de corrélation
  corr_stats <- cor.test(
    data_sub$x,
    data_sub$y,
    use = "complete.obs"
  )
  
  
  # Texte des résultats
  label_text <- paste0(
    "r = ", round(unname(corr_stats$estimate), 2),
    "\n",
    ifelse(
      corr_stats$p.value < 0.001,
      "p < 0.001",
      paste0(
        "p = ",
        format.pval(
          corr_stats$p.value,
          digits = 3,
          eps = 0.001
        )
      )
    )
  )
  
  
  # Couleurs correspondant à la comparaison
  line_color <- comparison_line_colors[[comparison_label]]
  light_color <- comparison_light_colors[[comparison_label]]
  
  
  ggplot(data_sub, aes(x = x, y = y)) +
    
    # Intervalle de confiance et droite de régression
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = line_color,
      fill = light_color,
      linewidth = 1.4,
      alpha = 0.30
    ) +
    
    # Ligne d'identité : résolution X = résolution Y
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "grey45",
      linewidth = 0.6
    ) +
    
    # Points individuels
    geom_point(
      color = line_color,
      fill = light_color,
      shape = 21,
      stroke = 0.6,
      size = 1.5,
      alpha = 0.90
    ) +
    
    labs(
      title = comparison_label,
      x = paste0(x_distance, " Imaging Time (s)"),
      y = paste0(y_distance, " Imaging Time (s)")
    ) +
    
    # Résultats de corrélation en bas à droite
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      label = label_text,
      hjust = 1.15,
      vjust = -0.6,
      size = 4.5,
      color = "black"
    ) +
    
    scale_x_continuous(
      limits = x_limits,
      breaks = seq(
        from = x_limits[1],
        to = x_limits[2],
        by = 5
      ),
      expand = c(0, 0)
    ) +
    
    scale_y_continuous(
      limits = y_limits,
      breaks = seq(
        from = y_limits[1],
        to = y_limits[2],
        by = 5
      ),
      expand = c(0, 0)
    ) +
    
    # Même échelle et même longueur sur les axes X et Y
    coord_fixed(ratio = 1) +
    
    theme_minimal(base_size = 16) +
    
    theme(
      plot.title = element_text(
        size = 22,
        face = "bold",
        hjust = 0.5,
        color = line_color,
        margin = margin(b = 12)
      ),
      
      axis.title.x = element_text(
        size = 16,
        color = "black",
        margin = margin(t = 12)
      ),
      
      axis.title.y = element_text(
        size = 16,
        color = "black",
        margin = margin(r = 12)
      ),
      
      axis.text = element_text(
        size = 16,
        color = "black"
      ),
      
      panel.grid = element_blank(),
      
      axis.line = element_line(
        color = "black",
        linewidth = 1
      ),
      
      axis.ticks = element_line(
        color = "black",
        linewidth = 1
      ),
      
      axis.ticks.length = unit(0.2, "cm"),
      
      aspect.ratio = 1,
      
      plot.margin = margin(
        t = 10,
        r = 15,
        b = 10,
        l = 10
      )
    )
}


# --- Création des trois graphiques ---

p1 <- plot_corr_by_comparison(
  comparison_label = "5m vs 10m",
  x_distance = "5m",
  y_distance = "10m",
  x_limits = c(0, 20),
  y_limits = c(0, 20)
)

p2 <- plot_corr_by_comparison(
  comparison_label = "10m vs 15m",
  x_distance = "10m",
  y_distance = "15m",
  x_limits = c(0, 20),
  y_limits = c(0, 20)
)

p3 <- plot_corr_by_comparison(
  comparison_label = "5m vs 15m",
  x_distance = "5m",
  y_distance = "15m",
  x_limits = c(0, 20),
  y_limits = c(0, 20)
)


# --- Assemblage des graphiques ---

figure_imaging_corr <- ggarrange(
  p1,
  p2,
  p3,
  ncol = 3,
  nrow = 1,
  align = "hv"
)


figure_imaging_corr

# --- Figure 3C : Corrélations croisées ME/MI ---
library(patchwork)

plot_corr_by_combination <- function(me_var, mi_var) {
  data_sub <- df %>% select(all_of(c(me_var, mi_var)))
  corr_stats <- cor.test(data_sub[[me_var]], data_sub[[mi_var]])
  
  label_text <- paste0(
    "r = ", round(corr_stats$estimate, 2), "\n",
    ifelse(corr_stats$p.value < 0.001, "p < 0.001",
           paste0("p = ", signif(corr_stats$p.value, 3)))
  )
  
  ggplot(data_sub, aes(x = .data[[me_var]], y = .data[[mi_var]])) +
    geom_point(color = "#E69F00", size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", color = "#56B4E9", se = FALSE, linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey50") +
    annotate("text", x = 19, y = 1, label = label_text,
             hjust = 1, vjust = 0, size = 5) +
    labs(title = paste0(me_var, " vs ", mi_var),
         x = "Executed Time (s)", y = "Imagined Time (s)") +
    scale_x_continuous(limits = c(0, 20), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(5, "pt")
    )
}

ME <- c("ME_5m", "ME_10m", "ME_15m")
MI <- c("MI_5m", "MI_10m", "MI_15m")

# Créer les 9 combinaisons
combinations <- expand.grid(
  ME = ME,
  MI = MI,
  stringsAsFactors = FALSE
)

figure_3C <- wrap_plots(
  lapply(seq_len(nrow(combinations)), function(i) {
    plot_corr_by_combination(
      combinations$ME[i],
      combinations$MI[i]
    )
  }),
  ncol = 3
)

# Afficher
figure_3C

# Enregistrer
ggsave(
  "Figure_3C_correlations.png",
  plot = figure_3C,
  width = 24, height = 22,
  units = "cm", dpi = 300
)