# Funzioni di supporto per car_co2.R -------------------------------------------
#
# Le chiamate ai pacchetti sono qualificate (`pkg::fun`): il file non dipende
# dall'ordine dei `library()` (es. `MASS::select` maschera `dplyr::select`).


#' Colonne del CSV usate nell'analisi
#'
#' Vettore nominato: i nomi sono le colonne prodotte da [utils::read.csv()], i
#' valori i corrispondenti nomi `snake_case`. Marca, modello, classe,
#' trasmissione e consumo in mpg sono esclusi (basso contenuto informativo o
#' duplicati di altre colonne).
co2_columns <- c(
  Engine.Size.L. = "engine_size_l",
  Cylinders = "cylinders",
  Fuel.Type = "fuel_type",
  Fuel.Consumption.City..L.100.km. = "fuel_city_l100km",
  Fuel.Consumption.Hwy..L.100.km. = "fuel_hwy_l100km",
  Fuel.Consumption.Comb..L.100.km. = "fuel_comb_l100km",
  CO2.Emissions.g.km. = "co2_g_km"
)


# Dati -------------------------------------------------------------------------

#' Carica e valida il dataset delle emissioni di CO2
#'
#' Legge il CSV, tiene le colonne di [co2_columns] rinominandole in
#' `snake_case` ed elimina, con un avviso, le righe con valori mancanti.
#'
#' @param path Percorso del file CSV.
#' @return Data frame con le colonne `co2_columns`.
#' @examples
#' \dontrun{
#' co2 <- load_co2_data("co2.csv")
#' }
load_co2_data <- function(path) {
  stopifnot(is.character(path), length(path) == 1L)
  if (!file.exists(path)) {
    stop("File non trovato: ", path, call. = FALSE)
  }

  raw <- utils::read.csv(path, na.strings = c("NA", ""))
  missing_cols <- setdiff(names(co2_columns), names(raw))
  if (length(missing_cols) > 0) {
    stop("Colonne mancanti nel CSV: ", toString(missing_cols), call. = FALSE)
  }
  data <- stats::setNames(raw[names(co2_columns)], co2_columns)

  numeric_cols <- setdiff(co2_columns, "fuel_type")
  is_num <- vapply(data[numeric_cols], is.numeric, logical(1))
  if (!all(is_num)) {
    stop(
      "Colonne non numeriche: ", toString(numeric_cols[!is_num]),
      call. = FALSE
    )
  }

  complete <- stats::complete.cases(data)
  if (!all(complete)) {
    warning(
      sum(!complete), " righe con valori mancanti eliminate.",
      call. = FALSE
    )
    data <- data[complete, , drop = FALSE]
  }

  # log() e i rapporti usati nei modelli richiedono valori strettamente positivi
  if (any(data[numeric_cols] <= 0)) {
    stop("Valori non positivi nelle colonne numeriche.", call. = FALSE)
  }
  data
}


#' Codifica il tipo di carburante con dummy per le categorie più frequenti
#'
#' Le `n_top` categorie più frequenti diventano dummy 0/1
#' (`fuel_type_<categoria>`); le altre formano la baseline ("Other") e non
#' ricevono una colonna, evitando la collinearità con l'intercetta.
#'
#' @param data Data frame con la colonna `fuel_type`.
#' @param n_top Numero di categorie da codificare.
#' @param target Variabile risposta, spostata in ultima posizione.
#' @return `data` senza `fuel_type`, con le dummy e `target` in fondo.
#' @examples
#' toy <- data.frame(fuel_type = c("X", "X", "Z", "D"), co2_g_km = 1:4)
#' encode_fuel_type(toy)
encode_fuel_type <- function(data, n_top = 2L, target = "co2_g_km") {
  stopifnot(
    is.data.frame(data),
    all(c("fuel_type", target) %in% names(data)),
    !anyNA(data$fuel_type),
    n_top >= 1,
    n_top < dplyr::n_distinct(data$fuel_type)
  )
  fuel_freq <- sort(table(data$fuel_type), decreasing = TRUE)
  top_fuels <- names(fuel_freq)[seq_len(n_top)]
  dummies <- outer(data$fuel_type, top_fuels, `==`) * 1
  colnames(dummies) <- paste0("fuel_type_", tolower(top_fuels))

  data |>
    dplyr::select(-"fuel_type") |>
    dplyr::bind_cols(as.data.frame(dummies)) |>
    dplyr::relocate(dplyr::all_of(target), .after = dplyr::last_col())
}


#' Suddivide casualmente un data frame in train e test
#'
#' Usa lo stato corrente del generatore casuale: chiamare [set.seed()] prima
#' per la riproducibilità.
#'
#' @param data Data frame da suddividere.
#' @param train_frac Quota di righe assegnata al train, in (0, 1).
#' @return Lista con `train` e `test`.
split_train_test <- function(data, train_frac = 0.7) {
  stopifnot(
    is.data.frame(data),
    is.numeric(train_frac), train_frac > 0, train_frac < 1
  )
  train_index <- sample(seq_len(nrow(data)), size = train_frac * nrow(data))
  list(train = data[train_index, ], test = data[-train_index, ])
}


# Grafici ----------------------------------------------------------------------

#' Diagrammi di dispersione della risposta contro ogni regressore
#'
#' Ogni pannello riporta la correlazione nel titolo e una curva `lowess`.
#'
#' @param data Data frame numerico.
#' @param target Nome della variabile risposta.
#' @param layout Griglia `c(righe, colonne)` per `par(mfrow)`.
#' @return `NULL`, invisibilmente.
plot_target_vs_predictors <- function(data, target = "co2_g_km",
                                      layout = c(2, 4)) {
  stopifnot(is.data.frame(data), target %in% names(data))
  old_par <- graphics::par(mfrow = layout)
  on.exit(graphics::par(old_par))

  y <- data[[target]]
  for (var in setdiff(names(data), target)) {
    x <- data[[var]]
    r <- stats::cor(x, y, use = "complete.obs")
    graphics::plot(
      x, y,
      main = sprintf("CO2 vs %s\nCorrelazione: %.2f", var, r),
      xlab = var, ylab = target, pch = 16, col = "blue"
    )
    graphics::lines(stats::lowess(x, y), col = "red", lwd = 2)
  }
  invisible(NULL)
}


#' Diagrammi dei residui contro ciascun regressore
#'
#' @param data Data frame con i regressori.
#' @param residuals Residui del modello, uno per riga di `data`.
#' @param vars Nomi dei regressori da rappresentare.
#' @param layout Griglia `c(righe, colonne)` per `par(mfrow)`.
#' @return `NULL`, invisibilmente.
plot_residuals_vs_predictors <- function(data, residuals, vars,
                                         layout = c(2, 4)) {
  stopifnot(
    is.data.frame(data),
    length(residuals) == nrow(data),
    all(vars %in% names(data))
  )
  old_par <- graphics::par(mfrow = layout)
  on.exit(graphics::par(old_par))

  for (var in vars) {
    graphics::plot(
      data[[var]], residuals,
      main = paste("Plot di", var, "vs residui"),
      xlab = var, ylab = "Residui", col = "blue", pch = 20
    )
    graphics::abline(h = 0, col = "red", lty = 2)
  }
  invisible(NULL)
}


#' Grafico dei residui con linee di riferimento
#'
#' Linea dello zero in rosso, residuo minimo in verde e massimo in blu.
#'
#' @param residuals Vettore numerico dei residui.
#' @param x Ascissa: valori stimati o, per default, indice dell'osservazione.
#' @param main Titolo del grafico.
#' @param xlab Etichetta dell'asse x.
#' @return `NULL`, invisibilmente.
plot_residuals <- function(residuals, x = seq_along(residuals),
                           main = "Residui", xlab = "Index") {
  residuals <- as.vector(residuals)
  stopifnot(
    is.numeric(residuals),
    !anyNA(residuals),
    length(x) == length(residuals)
  )
  res_range <- range(residuals)
  graphics::plot(
    x, residuals,
    main = main, xlab = xlab, ylab = "Residui",
    ylim = 1.2 * range(0, res_range)
  )
  graphics::abline(h = 0, col = "red", lwd = 2)
  graphics::abline(h = res_range, col = c("green", "blue"), lwd = 2)
  invisible(NULL)
}


# Diagnostica ------------------------------------------------------------------

#' Diagnostica di multicollinearità di un modello lineare
#'
#' @param model Oggetto `lm` con almeno due regressori.
#' @return Lista con `det_xtx` (determinante di X'X), `condition_number`
#'   (radice del rapporto tra autovalore massimo e minimo di X'X) e `vif`
#'   (data frame con VIF, tolleranza = 1/VIF e R^2 = 1 - 1/VIF del
#'   regressore sugli altri).
collinearity_diagnostics <- function(model) {
  stopifnot(inherits(model, "lm"))
  xtx <- crossprod(stats::model.matrix(model))
  eigenvalues <- abs(eigen(xtx, symmetric = TRUE, only.values = TRUE)$values)
  vif <- car::vif(model)
  list(
    det_xtx = det(xtx),
    condition_number = sqrt(max(eigenvalues) / min(eigenvalues)),
    vif = data.frame(vif = vif, tolerance = 1 / vif, r_squared = 1 - 1 / vif)
  )
}


#' Best subset selection con R^2, R^2 corretto, AIC e BIC
#'
#' AIC e BIC sono calcolati dalla RSS come `n log(RSS / n) + c k`, con `k` =
#' numero di predittori + intercetta.
#'
#' @param formula Formula del modello completo.
#' @param data Data frame di stima.
#' @return Lista con `scores` (una riga per dimensione del modello) e
#'   `selected` (predittori del modello con BIC minimo).
best_subset_selection <- function(formula, data) {
  stopifnot(inherits(formula, "formula"), is.data.frame(data))
  fit_summary <- summary(
    leaps::regsubsets(formula, data = data, nvmax = ncol(data) - 1)
  )
  n <- nrow(data)
  n_predictors <- seq_along(fit_summary$rss)
  log_rss_term <- n * log(fit_summary$rss / n)
  scores <- data.frame(
    n_predictors = n_predictors,
    r2 = fit_summary$rsq,
    adj_r2 = fit_summary$adjr2,
    aic = log_rss_term + 2 * (n_predictors + 1),
    bic = log_rss_term + log(n) * (n_predictors + 1)
  )
  best <- which.min(scores$bic)
  list(scores = scores, selected = names(which(fit_summary$which[best, -1])))
}


#' Test LM di eteroschedasticità con regressione ausiliaria
#'
#' Regredisce i residui pesati al quadrato ([stats::weighted.residuals()],
#' uguali ai residui grezzi per un modello non pesato) sulle colonne di `z`.
#' Statistica LM = n R^2, chi-quadrato con `rank(z) - 1` gradi di libertà
#' sotto l'ipotesi nulla di omoschedasticità. Su un modello WLS verifica il
#' modello trasformato, cioè se i pesi hanno rimosso l'eteroschedasticità
#' (`lmtest::bptest()` con pesi non lo fa: la sua statistica non è n R^2).
#'
#' @param model Oggetto `lm`, eventualmente pesato.
#' @param z Matrice delle variabili ausiliarie, con colonna dell'intercetta.
#' @param method Nome del test, per la stampa.
#' @return Oggetto `htest`.
aux_regression_test <- function(model, z, method) {
  sq_resid <- stats::weighted.residuals(model)^2
  stopifnot(is.matrix(z), nrow(z) == length(sq_resid))
  aux <- stats::lm.fit(z, sq_resid)
  total_ss <- sum((sq_resid - mean(sq_resid))^2)
  r_squared <- if (total_ss > 0) 1 - sum(aux$residuals^2) / total_ss else 0
  statistic <- c(LM = length(sq_resid) * r_squared)
  df <- aux$rank - 1
  structure(
    list(
      statistic = statistic,
      parameter = c(df = df),
      p.value = unname(stats::pchisq(statistic, df = df, lower.tail = FALSE)),
      method = method,
      data.name = deparse1(stats::formula(model))
    ),
    class = "htest"
  )
}


#' Test di Breusch-Pagan studentizzato (Koenker) sui regressori del modello
#'
#' Per un modello non pesato coincide con `lmtest::bptest(model)`.
#'
#' @param model Oggetto `lm`, eventualmente pesato.
#' @return Oggetto `htest`.
breusch_pagan_test <- function(model) {
  stopifnot(inherits(model, "lm"))
  aux_regression_test(
    model, stats::model.matrix(model), "Breusch-Pagan studentizzato"
  )
}


#' Test di White (forma speciale con i valori stimati)
#'
#' Variabili ausiliarie: `y_hat` e `y_hat^2`, quindi 2 gradi di libertà.
#'
#' @param model Oggetto `lm`, eventualmente pesato.
#' @return Oggetto `htest`.
white_test <- function(model) {
  stopifnot(inherits(model, "lm"))
  y_hat <- stats::fitted(model)
  aux_regression_test(
    model, cbind(1, y_hat, y_hat^2), "White (fitted, fitted^2)"
  )
}


#' Grafico dei residui e test di eteroschedasticità di un modello
#'
#' Per un modello pesato grafico e test usano i residui pesati.
#'
#' @param model Oggetto `lm`, eventualmente pesato.
#' @param title Nome del modello, usato nel titolo e nella tabella.
#' @return Data frame di una riga con statistiche e p-value dei test di
#'   Breusch-Pagan e White.
diagnose_heteroskedasticity <- function(model, title) {
  stopifnot(inherits(model, "lm"))
  plot_residuals(
    stats::weighted.residuals(model), stats::fitted(model),
    main = paste0("Residui vs fitted (", title, ")"), xlab = "Fitted"
  )
  bp <- breusch_pagan_test(model)
  white <- white_test(model)
  data.frame(
    model = title,
    bp_stat = unname(bp$statistic),
    bp_p_value = bp$p.value,
    white_stat = unname(white$statistic),
    white_p_value = white$p.value
  )
}


#' Test di Breusch-Pagan rispetto a un regressore alla volta
#'
#' @param model Oggetto `lm`.
#' @param data Data frame con le variabili in `vars`, una riga per osservazione
#'   del modello.
#' @param vars Nomi delle variabili da testare.
#' @return Data frame con statistica e p-value per variabile.
bp_by_regressor <- function(model, data, vars) {
  stopifnot(
    inherits(model, "lm"),
    nrow(data) == stats::nobs(model),
    all(vars %in% names(data))
  )
  tests <- lapply(vars, function(var) {
    aux_regression_test(
      model, cbind(1, data[[var]]), paste("Breusch-Pagan su", var)
    )
  })
  data.frame(
    variable = vars,
    bp_stat = vapply(tests, function(t) unname(t$statistic), numeric(1)),
    p_value = vapply(tests, `[[`, numeric(1), "p.value")
  )
}


#' Pesi FGLS da una funzione di varianza stimata
#'
#' Stima la varianza degli errori come `h = exp(g(x))`, con `g` ottenuta
#' regredendo `log(residui^2)` sui regressori del modello OLS (Wooldridge,
#' cap. 8). I pesi sono `1 / h`: a differenza di `1 / residui^2` non
#' riproducono il rumore del singolo residuo, quindi il modello pesato non
#' risulta omoschedastico per costruzione.
#'
#' @param model Oggetto `lm` stimato con OLS (senza pesi).
#' @return Vettore di pesi positivi, uno per osservazione.
fgls_weights <- function(model) {
  stopifnot(inherits(model, "lm"), is.null(stats::weights(model)))
  log_sq_resid <- log(stats::resid(model)^2)
  if (!all(is.finite(log_sq_resid))) {
    stop("Residui nulli: log(residui^2) non finito.", call. = FALSE)
  }
  variance_fit <- stats::lm.fit(stats::model.matrix(model), log_sq_resid)
  1 / exp(variance_fit$fitted.values)
}


# Regolarizzazione e valutazione -----------------------------------------------

#' Regressione penalizzata (glmnet) con lambda scelto in cross-validation
#'
#' @param alpha Mix elastic net: 0 = Ridge, 1 = Lasso.
#' @param x_train,x_test Matrici dei regressori.
#' @param y_train,y_test Vettori della risposta.
#' @param foldid Fold di ogni osservazione di train; condividerlo tra modelli
#'   li confronta sulla stessa partizione.
#' @return Lista con `cv` (oggetto `cv.glmnet`; il percorso completo è in
#'   `cv$glmnet.fit`), `coef` a `lambda.min`, `pred` e `residuals` sul test.
fit_penalized <- function(alpha, x_train, y_train, x_test, y_test, foldid) {
  stopifnot(
    is.numeric(alpha), length(alpha) == 1, alpha >= 0, alpha <= 1,
    is.matrix(x_train), is.matrix(x_test),
    identical(colnames(x_train), colnames(x_test)),
    nrow(x_train) == length(y_train),
    nrow(x_test) == length(y_test),
    length(foldid) == nrow(x_train)
  )
  cv <- glmnet::cv.glmnet(x_train, y_train, alpha = alpha, foldid = foldid)
  pred <- as.vector(stats::predict(cv, newx = x_test, s = "lambda.min"))
  list(
    cv = cv,
    coef = stats::coef(cv, s = "lambda.min")[, 1],
    pred = pred,
    residuals = y_test - pred
  )
}


#' Grafici di un modello penalizzato: coefficienti, CV e residui sul test
#'
#' @param fit Risultato di [fit_penalized()].
#' @param label Nome del modello, usato nei titoli.
#' @return `NULL`, invisibilmente.
plot_penalized <- function(fit, label) {
  graphics::plot(
    fit$cv$glmnet.fit,
    xvar = "lambda", label = TRUE, main = paste(label, "- regressori")
  )
  graphics::abline(h = 0, col = "red", lwd = 2)
  graphics::plot(fit$cv, main = paste("Cross validation", label))
  plot_residuals(fit$residuals, main = paste("Residui", label, "(test)"))
}


#' Previsioni in scala originale di un modello log-lineare (smearing di Duan)
#'
#' `exp()` della previsione in scala log stima la mediana condizionata, non la
#' media: il fattore `mean(exp(residui))` corregge la distorsione senza
#' assumere errori normali.
#'
#' @param model Oggetto `lm` non pesato con risposta `log(y)`.
#' @param newdata Data frame su cui prevedere.
#' @return Vettore delle previsioni in scala originale.
predict_log_smearing <- function(model, newdata) {
  stopifnot(
    inherits(model, "lm"),
    is.null(stats::weights(model)),
    is.data.frame(newdata)
  )
  smearing <- mean(exp(stats::resid(model)))
  exp(stats::predict(model, newdata = newdata)) * smearing
}


#' Errore quadratico medio e assoluto medio delle previsioni
#'
#' @param predicted Vettore delle previsioni.
#' @param actual Vettore dei valori osservati.
#' @return Vettore nominato `c(mse, mae)`.
regression_metrics <- function(predicted, actual) {
  predicted <- as.vector(predicted)
  stopifnot(
    is.numeric(predicted), is.numeric(actual),
    length(predicted) == length(actual),
    !anyNA(predicted), !anyNA(actual)
  )
  errors <- predicted - actual
  c(mse = mean(errors^2), mae = mean(abs(errors)))
}
