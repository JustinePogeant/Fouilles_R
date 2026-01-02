# Visualisation:
# Visualiser les résultats de l'AFD pour déterminer comment les tweets sont regroupés par 
# sentiment dans l'espace réduit.

library(ggplot2)
library(dplyr)

if(!require(gridExtra)) {
  install.packages("gridExtra")
  library(gridExtra)
}

# Palette de couleurs
couleurs <- c("Positive" = "#2ECC71",
              "Negative" = "#E74C3C",
              "Neutral" = "#95A5A6",
              "Irrelevant" = "#F39C12")

# on crée un premier graphique 2D de base

graph1 <- ggplot(lda_all_proj, aes(x = LD1, y = LD2, color = Sentiment_Real)) +
  geom_point(alpha = 0.3, size = 1) +
  scale_color_manual(values = couleurs) +
  theme_minimal() +
  labs(
    title = "Projection AFD des tweets sur les 2 premiers axes discriminants",
    subtitle = paste0("LD1 (", round(proportions[1]*100, 1), "%) vs LD2 (", 
                      round(proportions[2]*100, 1), "%)"),
    x = paste0("LD1 (", round(proportions[1]*100, 1), "% de variance)"),
    y = paste0("LD2 (", round(proportions[2]*100, 1), "% de variance)"),
    color = "Sentiment"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right"
  )

print(graph1)

# on crée un deuxième graphique avec ellipses de confiance

ellipses <- ggplot(lda_all_proj, aes(x = LD1, y = LD2, color = Sentiment_Real)) +
  geom_point(alpha = 0.2, size = 0.8) +
  stat_ellipse(level = 0.95, linewidth = 1.2) +
  scale_color_manual(values = couleurs) +
  theme_minimal() +
  labs(
    title = "Projection AFD avec ellipses de confiance (95%)",
    subtitle = "Les ellipses délimitent les zones de concentration des sentiments",
    x = paste0("LD1 (", round(proportions[1]*100, 1), "%)"),
    y = paste0("LD2 (", round(proportions[2]*100, 1), "%)"),
    color = "Sentiment"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "right"
  )

print(ellipses)

# on crée un troisième graphique pour la variance expliquée

var_df <- data.frame(
  Axe = factor(paste0("LD", 1:length(proportions)), 
               levels = paste0("LD", 1:length(proportions))),
  Variance = proportions * 100
)

graph_variance <- ggplot(var_df, aes(x = Axe, y = Variance)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_text(aes(label = paste0(round(Variance, 1), "%")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Variance expliquée par chaque axe discriminant",
    subtitle = paste0("LD1 + LD2 = ", round(sum(proportions[1:2])*100, 1), "% de la variance"),
    x = "Axe discriminant",
    y = "Variance expliquée (%)"
  ) +
  ylim(0, max(proportions * 100) * 1.15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

print(graph_variance)


# on crée un quatrième graphique pour les centroïdes
# on calcule les centroïdes de chaque sentiment
centroides <- lda_all_proj %>%
  group_by(Sentiment_Real) %>%
  summarise(
    LD1_mean = mean(LD1),
    LD2_mean = mean(LD2),
    LD3_mean = mean(LD3, na.rm = TRUE),
    .groups = "drop"
  )

print(centroides)

# le graphique des centroïdes
graph_centroides <- ggplot(lda_all_proj, aes(x = LD1, y = LD2)) +
  geom_point(aes(color = Sentiment_Real), alpha = 0.2, size = 0.8) +
  stat_ellipse(aes(color = Sentiment_Real), level = 0.95, linewidth = 1.2) +
  geom_point(data = centroides, 
             aes(x = LD1_mean, y = LD2_mean, color = Sentiment_Real),
             size = 10, shape = 18) +
  geom_text(data = centroides,
            aes(x = LD1_mean, y = LD2_mean, label = Sentiment_Real),
            vjust = -2, fontface = "bold", size = 5, color = "black") +
  scale_color_manual(values = couleurs) +
  theme_minimal() +
  labs(
    title = "Projection de l'AFD avec centroïdes pour les sentiments",
    subtitle = "Les losanges indiquent le centre de chaque groupe",
    x = "LD1",
    y = "LD2",
    color = "Sentiment"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "right"
  )

print(graph_centroides)

# on sauvegarde les graphiques

ggsave("project_2/figure1_projection_ellipses.png", plot = ellipses, 
       width = 12, height = 8, dpi = 300)

ggsave("project_2/figure2_variance_expliquee.png", plot = graph_variance, 
       width = 10, height = 7, dpi = 300)

ggsave("project_2/figure3_centroides.png", plot = graph_centroides, 
       width = 12, height = 8, dpi = 300)

