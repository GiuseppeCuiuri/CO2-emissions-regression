# Emissioni di CO2 dei veicoli: modelli lineari e regolarizzazione -------------
#
# Preprocessing, analisi esplorativa, multicollinearità, selezione dei
# regressori, eteroschedasticità (WLS), Ridge/Lasso/Elastic Net e confronto
# finale sul test set. Le funzioni di supporto sono in R/co2_functions.R.
#
# Eseguire dalla cartella del progetto: `Rscript car_co2.R`
# (in RStudio: Session > Set Working Directory > To Source File Location).

required_packages <- c("car", "corrplot", "dplyr", "glmnet", "leaps", "MASS")
is_installed <- vapply(
  required_packages, requireNamespace, logical(1),
  quietly = TRUE
)
if (!all(is_installed)) {
  stop(
    "Pacchetti mancanti: ", toString(required_packages[!is_installed]),
    call. = FALSE
  )
}

source(file.path("R", "co2_functions.R"))

set.seed(123)


# Preprocessing ----------------------------------------------------------------

co2 <- load_co2_data("co2.csv") |>
  encode_fuel_type(n_top = 2)
str(co2)
print(summary(co2))

target <- "co2_g_km"
predictors <- setdiff(names(co2), target)

splits <- split_train_test(co2, train_frac = 0.7)
train <- splits$train
test <- splits$test


# Analisi esplorativa ----------------------------------------------------------

plot_target_vs_predictors(train, target)
corrplot::corrplot(cor(train), method = "circle", tl.cex = 0.6)


# Modello lineare completo -----------------------------------------------------

model_full <- lm(co2_g_km ~ ., data = train)
print(summary(model_full))

# Relazioni non lineari tra cilindrata/cilindri e risposta
model_nonlinear <- lm(
  co2_g_km ~ . + I(engine_size_l^2) + I(cylinders^2) +
    I(log(engine_size_l)) + I(log(cylinders)),
  data = train
)
print(summary(model_nonlinear))


# Multicollinearità ------------------------------------------------------------

print(collinearity_diagnostics(model_full))

# I tre consumi (urbano, extraurbano, combinato) sono quasi collineari: si
# rimuovono in sequenza quello urbano e quello extraurbano.
model_no_city <- lm(co2_g_km ~ . - fuel_city_l100km, data = train)
print(summary(model_no_city))
print(collinearity_diagnostics(model_no_city)$vif)

formula_reduced <- co2_g_km ~ . - fuel_city_l100km - fuel_hwy_l100km
formula_log <- log(co2_g_km) ~ . - fuel_city_l100km - fuel_hwy_l100km

model_reduced <- lm(formula_reduced, data = train)
print(summary(model_reduced))
print(collinearity_diagnostics(model_reduced)$vif)


# Selezione automatica dei regressori ------------------------------------------

print(best_subset_selection(co2_g_km ~ ., data = train))

model_backward <- step(model_full, direction = "backward", trace = 0)
print(summary(model_backward))


# Eteroschedasticità -----------------------------------------------------------

plot_residuals(
  resid(model_full), fitted(model_full),
  main = "Residui vs fitted (modello lineare)", xlab = "Fitted"
)

# Trasformazione log-lin e sua versione FGLS (funzione di varianza stimata)
model_log <- lm(formula_log, data = train)
weights_log <- fgls_weights(model_log)
model_log_fgls <- lm(formula_log, data = train, weights = weights_log)

# WLS con deviazione standard proporzionale al livello stimato di CO2, dal
# modello lineare o dal log-lin (riportato in scala originale con exp)
model_wls_fit_lin <- lm(
  formula_reduced,
  data = train, weights = 1 / fitted(model_reduced)^2
)
model_wls_fit_log <- lm(
  formula_reduced,
  data = train, weights = 1 / exp(fitted(model_log))^2
)

# ...oppure proporzionale a un regressore legato alla varianza dei residui
plot_residuals_vs_predictors(train, resid(model_log), predictors)
print(bp_by_regressor(model_log, train, predictors))

# Si sceglie il consumo combinato, unico consumo rimasto nel modello
model_wls_comb_log <- lm(
  formula_log,
  data = train, weights = 1 / fuel_comb_l100km^2
)

# Modello con interazioni: selezione stepwise (AIC) fino alle interazioni
# doppie, poi versione FGLS
model_interaction <- MASS::stepAIC(
  model_full,
  scope = list(upper = ~ .^2), direction = "both", trace = FALSE
)
weights_interaction <- fgls_weights(model_interaction)
model_interaction_fgls <- lm(
  formula(model_interaction),
  data = train, weights = weights_interaction
)

hetero_models <- list(
  "lineare senza multicollinearità" = model_reduced,
  "Log-Lin" = model_log,
  "Log-Lin FGLS" = model_log_fgls,
  "lineare WLS 1/fit^2" = model_wls_fit_lin,
  "lineare WLS 1/exp(fit log)^2" = model_wls_fit_log,
  "Log-Lin WLS 1/consumo combinato^2" = model_wls_comb_log,
  "con interazioni" = model_interaction,
  "con interazioni FGLS" = model_interaction_fgls
)
for (name in names(hetero_models)) {
  cat("\n===== Modello", name, "=====\n")
  print(summary(hetero_models[[name]]))
}

hetero_tests <- do.call(
  rbind,
  Map(diagnose_heteroskedasticity, hetero_models, names(hetero_models))
)
print(hetero_tests, row.names = FALSE)


# Regolarizzazione -------------------------------------------------------------

x_train <- as.matrix(train[predictors])
x_test <- as.matrix(test[predictors])
y_train <- train[[target]]
y_test <- test[[target]]

# Stessi fold per ogni alpha: modelli confrontati sulla stessa partizione
cv_folds <- sample(rep_len(1:10, nrow(x_train)))

alphas <- c(
  "Ridge" = 0,
  "Lasso" = 1,
  "Elastic Net 0.25" = 0.25,
  "Elastic Net 0.5" = 0.5,
  "Elastic Net 0.75" = 0.75
)
penalized <- lapply(
  alphas, fit_penalized,
  x_train = x_train, y_train = y_train,
  x_test = x_test, y_test = y_test, foldid = cv_folds
)
invisible(Map(plot_penalized, penalized, names(penalized)))
print(sapply(penalized, function(fit) {
  c(lambda_min = fit$cv$lambda.min, fit$coef)
}))


# Confronto finale sul test set ------------------------------------------------

test_predictions <- c(
  list(
    "Log-Lin (smearing)" = predict_log_smearing(model_log, test),
    "Interazioni FGLS" = predict(model_interaction_fgls, newdata = test)
  ),
  lapply(penalized, `[[`, "pred")
)
for (name in c("Log-Lin (smearing)", "Interazioni FGLS")) {
  plot_residuals(
    y_test - test_predictions[[name]],
    main = paste("Residui", name, "(test)")
  )
}

model_comparison <- t(vapply(
  test_predictions, regression_metrics, numeric(2),
  actual = y_test
))
print(model_comparison[order(model_comparison[, "mse"]), ])
