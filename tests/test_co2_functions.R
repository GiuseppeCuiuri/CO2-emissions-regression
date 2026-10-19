# Controlli minimi delle funzioni di supporto.
# Eseguire dalla cartella del progetto: `Rscript tests/test_co2_functions.R`

source(file.path("R", "co2_functions.R"))

fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

# encode_fuel_type: dummy per le 2 categorie più frequenti, risposta in fondo
toy <- data.frame(
  fuel_type = c("X", "X", "X", "Z", "Z", "D"),
  co2_g_km = 6:1,
  engine_size_l = 1:6
)
encoded <- encode_fuel_type(toy)
stopifnot(
  identical(
    names(encoded),
    c("engine_size_l", "fuel_type_x", "fuel_type_z", "co2_g_km")
  ),
  encoded$fuel_type_x == c(1, 1, 1, 0, 0, 0),
  encoded$fuel_type_z == c(0, 0, 0, 1, 1, 0),
  fails(encode_fuel_type(toy, n_top = 3)) # nessuna baseline
)

# load_co2_data: errore su colonne mancanti
csv <- tempfile(fileext = ".csv")
write.csv(data.frame(Cylinders = 4), csv, row.names = FALSE)
stopifnot(fails(load_co2_data(csv)), fails(load_co2_data("non_esiste.csv")))

# regression_metrics
stopifnot(all.equal(
  regression_metrics(c(1, 2, 5), actual = c(1, 2, 3)),
  c(mse = 4 / 3, mae = 2 / 3)
))

# Test di eteroschedasticità: non rifiutano con errori omoschedastici,
# rifiutano con log-varianza lineare in x; FGLS rimuove l'eteroschedasticità
set.seed(1)
toy_het <- data.frame(x = runif(500, 1, 10))
toy_het$y_homo <- 2 * toy_het$x + rnorm(500)
toy_het$y_hetero <- 2 * toy_het$x + rnorm(500, sd = exp(0.3 * toy_het$x))
homo <- lm(y_homo ~ x, toy_het)
hetero <- lm(y_hetero ~ x, toy_het)
toy_weights <- fgls_weights(hetero)
hetero_fgls <- lm(y_hetero ~ x, toy_het, weights = toy_weights)
stopifnot(
  white_test(homo)$p.value > 0.05,
  breusch_pagan_test(homo)$p.value > 0.05,
  white_test(hetero)$p.value < 0.01,
  breusch_pagan_test(hetero)$p.value < 0.01,
  breusch_pagan_test(hetero_fgls)$p.value > 0.05,
  fails(fgls_weights(hetero_fgls)) # FGLS richiede un modello OLS
)
if (requireNamespace("lmtest", quietly = TRUE)) {
  stopifnot(all.equal(
    breusch_pagan_test(hetero)$statistic,
    lmtest::bptest(hetero)$statistic,
    check.attributes = FALSE
  ))
}

# predict_log_smearing: fattore mean(exp(residui)) >= 1 (Jensen)
toy_het$y_pos <- exp(0.2 * toy_het$x + rnorm(500, sd = 0.3))
log_model <- lm(log(y_pos) ~ x, toy_het)
stopifnot(all(
  predict_log_smearing(log_model, toy_het) >=
    exp(predict(log_model, toy_het))
))

cat("Tutti i controlli superati.\n")
