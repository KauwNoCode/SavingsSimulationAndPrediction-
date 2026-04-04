DTFsnpV2 <- DTFsnp500
DTFsnpV2$Actif <- NULL
DTFsnpV2$Date <- as.Date(DTFsnpV2$Date)

install.packages("ROSE")
install.packages("randomForest")

write.csv2(DTFsnpV2, "snp500df.csv", row.names = FALSE)

library(ggplot2)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(caret)
library(dplyr)
library(ROSE)
library(randomForest)
library(pROC)
library(zoo)

DTFtotalSNP<-read.csv("snp500_enrichi.csv",sep=';',dec=',',header=TRUE)

dtfssdt<-DTFtotalSNP
dtfssdt$Date=NULL
dim(dtfssdt)

dtfssdt<-na.omit(dtfssdt)
sum(is.na(dtfssdt))

Snprix<-dtfssdt$SNP500_Prix
dtfssdt$SNP500_Prix=NULL


cor(Snprix,dtfssdt)

#suppression des variables non stationnaires

dtf_model <- data.frame(
  Date = DTFtotalSNP$Date,
  # ── VARIABLE CIBLE ──────────────────────────────────────
  # Variation → déjà stationnaire ✓
  Rendement_Mensuel    = DTFtotalSNP$Rendement_Mensuel_Pct,
  # ── MOMENTUM (déjà des variations) ──────────────────────
  Momentum_12_1        = DTFtotalSNP$Momentum_12_1_Mois,
  Momentum_6           = DTFtotalSNP$Momentum_6_Mois,
  Momentum_3           = DTFtotalSNP$Momentum_3_Mois,
  Momentum_1           = DTFtotalSNP$Momentum_1_Mois,
  # ── MACRO : on garde les variations, pas les niveaux ────
  Fed_Variation        = DTFtotalSNP$Fed_Taux_Variation,      # diff ✓
  CPI_Variation        = DTFtotalSNP$CPI_Variation_Pct,       # pct ✓
  Chomage_Variation    = DTFtotalSNP$Chomage_Variation,       # diff ✓
  Production_Pct       = DTFtotalSNP$Production_Indus_Pct,    # pct ✓
  Ventes_Detail_Pct    = DTFtotalSNP$Ventes_Detail_Pct,       # pct ✓
  M2_Variation         = DTFtotalSNP$M2_Variation_Pct,        # pct ✓
  # ── TAUX : on garde les niveaux car déjà stationnaires ──
  # (les taux oscillent autour d'une moyenne, pas de tendance infinie)
  Taux_10ans           = DTFtotalSNP$Taux_10ans,
  Spread_10_2          = DTFtotalSNP$Spread_10ans_2ans,
  Spread_10_3m         = DTFtotalSNP$Spread_10ans_3mois,
  Credit_Spread_IG     = DTFtotalSNP$Credit_Spread_IG,
  Credit_Spread_HY     = DTFtotalSNP$Credit_Spread_HY,
  TED_Spread           = DTFtotalSNP$TED_Spread,
  # ── RISQUE : niveaux stationnaires ──────────────────────
  VIX_Niveau           = DTFtotalSNP$VIX_Niveau,
  VIX_Variation        = DTFtotalSNP$VIX_Variation,
  Volatilite_Realisee  = DTFtotalSNP$Volatilite_Realisee_Ann,
  Variance_Risk_Prem   = DTFtotalSNP$Variance_Risk_Premium,
  # ── SENTIMENT : niveaux stationnaires ───────────────────
  Sentiment_Michigan   = DTFtotalSNP$Sentiment_Michigan,
  Sentiment_Mich_Var   = DTFtotalSNP$Sentiment_Michigan_Var,
  # ── EXTERNE : variations uniquement ─────────────────────
  Petrole_Pct          = DTFtotalSNP$Petrole_WTI_Pct,         # pct ✓
  EURUSD_Pct           = DTFtotalSNP$EURUSD_Pct,              # pct ✓
  Taux_Hypothecaire    = DTFtotalSNP$Taux_Hypothecaire_30ans  # niveau ok
)

dtf_model2<-dtf_model
dtf_model2$Date=NULL


dtf_model_clean <-dtf_model2
# Corrélations avec le rendement mensuel uniquement
cor_rendement <- cor(dtf_model_clean, method = "pearson")["Rendement_Mensuel", ]
cor_rendement <- sort(cor_rendement[names(cor_rendement) != "Rendement_Mensuel"],
                      decreasing = TRUE)
print(round(cor_rendement, 4))


##            ============MULTIPLE REG============
  ```{r}
dtf_lag <- dtf_model_clean %>%
  mutate(across(
    -Rendement_Mensuel,   # on ne décale pas la variable cible
    ~ lag(., 1)           # toutes les autres sont décalées d'1 mois
  ))

dtf_lag <- na.omit(dtf_lag)

# Recalcul des corrélations avec le lag
cor_lag <- cor(dtf_lag, method = "pearson")["Rendement_Mensuel", ]
cor_lag <- sort(cor_lag[names(cor_lag) != "Rendement_Mensuel"], decreasing = TRUE)
print(round(cor_lag, 4))
```

# Vérification de la structure du dataset

# ── Division Train / Test ──
# 80% entraînement / 20% test — standard en finance quantitative
n      <- nrow(dtf_lag)
cut    <- floor(0.8 * n)
train  <- dtf_lag[1:cut, ]
test   <- dtf_lag[(cut + 1):n, ]
cat("Total observations  :", n,    "\n")
cat("Entraînement (80%)  :", nrow(train), "\n")
cat("Test (20%)          :", nrow(test),  "\n")

model <- lm(Rendement_Mensuel ~ ., data = train)
summary(model)


# ── Prédictions sur le test ──
predictions <- predict(model, newdata = test)
# ── Métriques de performance ──
residus <- test$Rendement_Mensuel - predictions
RMSE    <- sqrt(mean(residus^2))
MAE     <- mean(abs(residus))
R2_test <- 1 - sum(residus^2) / sum((test$Rendement_Mensuel - mean(test$Rendement_Mensuel))^2)
cat("── Performance sur données test ──\n")
cat("RMSE :", round(RMSE,    4), "\n")
cat("MAE  :", round(MAE,     4), "\n")
cat("R²   :", round(R2_test, 4), "\n")
# ── Visualisation prédictions vs réel ──
plot(test$Rendement_Mensuel, type = "l", col = "steelblue",
     main = "Réel vs Prédit — Rendement mensuel S&P 500",
     ylab = "Rendement (%)", xlab = "Observations test")
lines(predictions, col = "firebrick", lty = 2)
legend("topright", legend = c("Réel", "Prédit"),
       col = c("steelblue", "firebrick"), lty = c(1, 2)) 
#model abominable


##          ============LOGISTIC REG============
  
# ── 1. Création de la variable cible binaire ──────────────────
# 1 = le S&P monte ce mois-ci, 0 = il baisse
dtf_lag$Hausse <- ifelse(dtf_lag$Rendement_Mensuel > 0, 1, 0)
dtf_lag$Rendement_Mensuel <- NULL   # on retire la cible continue

cat("Distribution de la variable cible :\n")
print(table(dtf_lag$Hausse))
cat("Proportion hausse :", round(mean(dtf_lag$Hausse), 3), "\n\n")

# ── 2. Split Train / Test (80/20 chronologique) ──────────────
n     <- nrow(dtf_lag)
cut   <- floor(0.8 * n)
train <- dtf_lag[1:cut, ]
test  <- dtf_lag[(cut + 1):n, ]

cat("Observations totales  :", n, "\n")
cat("Entraînement (80%)    :", nrow(train), "\n")
cat("Test (20%)            :", nrow(test), "\n\n")

# ── 3. Modèle logistique ──────────────────────────────────────
model_log <- glm(Hausse ~ ., data = train, family = binomial(link = "logit"))
summary(model_log)

log_step <- step(model_log, direction = "both", trace = FALSE)
print(names(coef(log_step)))

# ── 4. Prédictions sur le test ────────────────────────────────
prob_pred  <- predict(log_step, newdata = test, type = "response")
class_pred <- ifelse(prob_pred >= 0.5, 1, 0)

# ── 5. Métriques de performance ──────────────────────────────
conf_mat <- confusionMatrix(
  factor(class_pred, levels = c(0, 1)),
  factor(test$Hausse,  levels = c(0, 1)),
  positive = "1"
)
print(conf_mat)

# Accuracy manuelle pour vérif
accuracy <- mean(class_pred == test$Hausse)
cat("\nAccuracy :", round(accuracy, 4), "\n")

# ── 6. Courbe ROC + AUC ──────────────────────────────────────
roc_obj <- roc(test$Hausse, prob_pred)
auc_val <- auc(roc_obj)
cat("AUC :", round(auc_val, 4), "\n")

plot(roc_obj,
     col  = "steelblue", lwd = 2,
     main = paste("Courbe ROC — AUC =", round(auc_val, 3)),
     xlab = "Taux faux positifs (1 - Spécificité)",
     ylab = "Taux vrais positifs (Sensibilité)")
abline(a = 0, b = 1, lty = 2, col = "gray60")   # ligne aléatoire

# ── 7. Probabilités prédites vs réalité ──────────────────────
plot(prob_pred, col = ifelse(test$Hausse == 1, "steelblue", "firebrick"),
     pch  = 19, cex = 0.8,
     ylim = c(0, 1),
     main = "Probabilités prédites — Bleu = hausse réelle, Rouge = baisse réelle",
     xlab = "Observations test", ylab = "P(Hausse)")
abline(h = 0.5, lty = 2, col = "gray40")

# ── 8. Importance des variables (coefficients) ───────────────
coef_df <- data.frame(
  Variable = names(coef(log_step))[-1],
  Coef     = coef(log_step)[-1]
) %>%
  arrange(desc(abs(Coef)))

barplot(coef_df$Coef,
        names.arg = coef_df$Variable,
        las = 2, cex.names = 0.7,
        col = ifelse(coef_df$Coef > 0, "steelblue", "firebrick"),
        main = "Coefficients logistiques (impact sur P(Hausse))",
        ylab = "Coefficient")
abline(h = 0, col = "black")

# ── 9. Matrice de confusion visuelle ─────────────────────────
conf_table <- table(Prédit = class_pred, Réel = test$Hausse)
print(conf_table) #model abominable

##          ============LOG MODEL OPTIMISATION============

# ── 3. Modèle logistique optimisé ─────────────────────────────

# Pondération pour corriger le déséquilibre
weights_vec <- ifelse(train$Hausse == 1, 1, 1.4)

# Modèle complet
model_full <- glm(Hausse ~ ., data = train,
                  family = binomial(link = "logit"),
                  weights = weights_vec)

# Sélection automatique des variables (AIC)
model_step <- step(model_full, direction = "both", trace = FALSE)

cat("Variables sélectionnées :\n")
print(names(coef(model_step)))

summary(model_step)

# ── 4. Prédictions ────────────────────────────────────────────
prob_pred <- predict(model_step, newdata = test, type = "response")

# Trouver le meilleur seuil avec ROC
roc_obj <- roc(test$Hausse, prob_pred)

best_thresh <- coords(roc_obj, "best", ret = "threshold")[[1]]
cat("Seuil optimal :", round(best_thresh, 3), "\n")

# Classification avec seuil optimisé
class_pred <- ifelse(prob_pred >= best_thresh, 1, 0)

# ── 5. Métriques ─────────────────────────────────────────────
conf_mat <- confusionMatrix(
  factor(class_pred, levels = c(0, 1)),
  factor(test$Hausse, levels = c(0, 1)),
  positive = "1"
)

print(conf_mat)

accuracy <- mean(class_pred == test$Hausse)
cat("\nAccuracy :", round(accuracy, 4), "\n")

# ── 6. ROC ───────────────────────────────────────────────────
auc_val <- auc(roc_obj)
cat("AUC :", round(auc_val, 4), "\n")

plot(roc_obj,
     col = "steelblue", lwd = 2,
     main = paste("ROC optimisée — AUC =", round(auc_val, 3)))
abline(a = 0, b = 1, lty = 2, col = "gray60")

dtf_lag_lm <- dtf_model[, names(dtf_model) != "Date"] %>%
  mutate(across(-Rendement_Mensuel, ~ lag(., 1))) %>%
  na.omit()

test_lm       <- dtf_lag_lm[221:nrow(dtf_lag_lm), ]
pred_lm_prob  <- predict(model, newdata = test_lm)
pred_lm_class <- ifelse(pred_lm_prob > 0, 1, 0)
test_hausse   <- ifelse(test_lm$Rendement_Mensuel > 0, 1, 0)

acc_lm  <- mean(pred_lm_class == test_hausse)
roc_lm  <- roc(test_hausse, pred_lm_prob)
auc_lm  <- auc(roc_lm)
cm_lm   <- table(pred_lm_class, test_hausse)
sens_lm <- cm_lm[2,2] / sum(cm_lm[,2])
spec_lm <- cm_lm[1,1] / sum(cm_lm[,1])
f1_lm   <- 2 * sens_lm * (cm_lm[2,2]/sum(cm_lm[2,])) / (sens_lm + cm_lm[2,2]/sum(cm_lm[2,]))

# ── Logistique simple ─────────────────────────────────────────────────────────
prob_log  <- predict(log_step, newdata = test, type = "response")
class_log <- ifelse(prob_log >= 0.5, 1, 0)
acc_log   <- mean(class_log == test$Hausse)
roc_log   <- roc(test$Hausse, prob_log)
auc_log   <- auc(roc_log)
cm_log    <- table(class_log, test$Hausse)
sens_log  <- cm_log[2,2] / sum(cm_log[,2])
spec_log  <- cm_log[1,1] / sum(cm_log[,1])
f1_log    <- 2 * sens_log * (cm_log[2,2]/sum(cm_log[2,])) / (sens_log + cm_log[2,2]/sum(cm_log[2,]))

# ── Logistique optimisée ──────────────────────────────────────────────────────
prob_opt  <- predict(model_step, newdata = test, type = "response")
roc_opt   <- roc(test$Hausse, prob_opt)
auc_opt   <- auc(roc_opt)
best_t    <- coords(roc_opt, "best", ret = "threshold")[[1]]
class_opt <- ifelse(prob_opt >= best_t, 1, 0)
acc_opt   <- mean(class_opt == test$Hausse)
cm_opt    <- table(class_opt, test$Hausse)
sens_opt  <- cm_opt[2,2] / sum(cm_opt[,2])
spec_opt  <- cm_opt[1,1] / sum(cm_opt[,1])
f1_opt    <- 2 * sens_opt * (cm_opt[2,2]/sum(cm_opt[2,])) / (sens_opt + cm_opt[2,2]/sum(cm_opt[2,]))

# ── Figure ────────────────────────────────────────────────────────────────────
couleurs <- c("#4E79A7", "#F28E2B", "#E15759")
modeles  <- c("Rég. Linéaire", "Log. Simple", "Log. Optimisée")

par(mfrow = c(1, 3), mar = c(5, 4, 4, 2))

# 1. Barplot métriques
png("barplot_modeles.png", width = 800, height = 600, res = 150)

metriques <- rbind(
  AUC         = c(auc_lm,  auc_log,  auc_opt),
  Accuracy    = c(acc_lm,  acc_log,  acc_opt),
  Sensibilité = c(sens_lm, sens_log, sens_opt),
  Spécificité = c(spec_lm, spec_log, spec_opt),
  F1          = c(f1_lm,   f1_log,   f1_opt)
)
colnames(metriques) <- modeles

bp <- barplot(metriques, beside = TRUE, col = couleurs,
              ylim = c(0, 1.2), main = "Métriques de performance",
              ylab = "Score", cex.names = 0.85, las = 1, border = NA)
for (i in 1:nrow(metriques))
  text(bp[i,], metriques[i,] + 0.04, labels = round(metriques[i,], 2),
       cex = 0.65, col = "gray30")
legend("topright", legend = rownames(metriques),
       fill = gray.colors(5, 0.3, 0.9), border = NA, cex = 0.72, bty = "n")
abline(h = 0.5, lty = 2, col = "gray60")
dev.off()

# 2. Courbes ROC
png("roc_modeles.png", width = 800, height = 600, res = 150)
plot(roc_lm, col = couleurs[1], lwd = 2, main = "Courbes ROC comparées",
     xlab = "1 - Spécificité", ylab = "Sensibilité", legacy.axes = TRUE)
lines(roc_log, col = couleurs[2], lwd = 2)
lines(roc_opt, col = couleurs[3], lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray60")
legend("bottomright",
       legend = c(paste0("Linéaire (AUC=",    round(auc_lm,  3), ")"),
                  paste0("Log. Simple (AUC=", round(auc_log, 3), ")"),
                  paste0("Log. Opt. (AUC=",   round(auc_opt, 3), ")")),
       col = couleurs, lwd = 2, bty = "n", cex = 0.78)
dev.off()

# 3. Radar
png("radar_modeles.png", width = 800, height = 600, res = 150)

axes_labels <- c("AUC", "Accuracy", "Sensibilité", "Spécificité", "F1")
angles <- seq(0, 2*pi, length.out = length(axes_labels) + 1)[1:length(axes_labels)]

polar_coords <- function(scores) {
  list(x = c(scores * cos(angles), scores[1] * cos(angles[1])),
       y = c(scores * sin(angles), scores[1] * sin(angles[1])))
}

vals <- list(
  c(auc_lm,  acc_lm,  sens_lm, spec_lm, f1_lm),
  c(auc_log, acc_log, sens_log, spec_log, f1_log),
  c(auc_opt, acc_opt, sens_opt, spec_opt, f1_opt)
)

plot(0, 0, type = "n", xlim = c(-1.4,1.4), ylim = c(-1.4,1.4),
     asp = 1, axes = FALSE, main = "Profil des modèles (radar)")
for (r in c(0.25, 0.5, 0.75, 1.0)) {
  theta <- seq(0, 2*pi, length.out = 200)
  lines(r*cos(theta), r*sin(theta), col = "gray85", lwd = 0.7)
}
for (i in seq_along(angles)) {
  lines(c(0, cos(angles[i])), c(0, sin(angles[i])), col = "gray70", lwd = 0.7)
  text(1.25*cos(angles[i]), 1.25*sin(angles[i]),
       labels = axes_labels[i], cex = 0.78, col = "gray30")
}
for (m in seq_along(vals)) {
  coords <- polar_coords(vals[[m]])
  polygon(coords$x, coords$y,
          col = adjustcolor(couleurs[m], alpha.f = 0.18),
          border = couleurs[m], lwd = 2)
}
legend("bottomleft", legend = modeles, col = couleurs, lwd = 2, bty = "n", cex = 0.78)

par(mfrow = c(1,1))
dev.off()



