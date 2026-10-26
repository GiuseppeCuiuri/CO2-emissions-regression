# CO2 Emissions Regression

Statistical modelling of vehicle CO2 emissions (g/km) in R. Starting from a plain linear model, the
analysis diagnoses and fixes multicollinearity and heteroskedasticity, then compares the resulting
models with Ridge, Lasso and Elastic Net regularization on a held-out test set.

## Authors

- [Giuseppe Cuiuri](https://github.com/GiuseppeCuiuri)
- [Davide Cinelli](https://github.com/davidecinelli26)
- [Agazio Geracitano](https://github.com/agaziogeracitano)
- [Vittorio Iusi](https://github.com/VittorioIusi)
- [Giuseppe Scaramuzzo](https://github.com/OnlyyGius)
- [Rocco Vardaro](https://github.com/roccovardaro)
- [Antonio Pio Francica](https://github.com/Anto4K)

Developed for the *Modelli Statistici e Statistical Learning* course, MSc in Computer Engineering,
University of Calabria, A.Y. 2024/2025.
The full written report (in Italian) is in [`Relazione Statisticool.pdf`](Relazione%20Statisticool.pdf).

## What the project does

| Step | Content |
|------|---------|
| **Data** | `co2.csv`, about 7,400 vehicles. Response: `co2_g_km`. Regressors: engine size, cylinders, city / highway / combined fuel consumption, fuel type (dummies for the 2 most frequent categories, the rest is the baseline). |
| **Exploration** | Scatter plots of the response against each regressor (with `lowess` curve), correlation matrix. |
| **Linear model** | Full OLS model, plus a version with quadratic and log terms for engine size and cylinders. |
| **Multicollinearity** | VIF, tolerance, determinant of X'X and condition number. City and highway consumption are removed in sequence, since the three consumption variables are nearly collinear. |
| **Variable selection** | Best subset selection (`leaps`) and backward stepwise selection. |
| **Heteroskedasticity** | Breusch-Pagan and White tests, residual plots, log-linear model, WLS with different weights and FGLS (estimated variance function). A stepwise (AIC) model with pairwise interactions is also fitted, with its FGLS version. |
| **Regularization** | Ridge, Lasso and Elastic Net (α = 0.25, 0.5, 0.75) with `glmnet`, 10-fold CV on shared folds so that every α is compared on the same partition. |
| **Final comparison** | Log-linear model (with Duan smearing to go back to the original scale), interaction model with FGLS, and the penalized models, ranked by MSE and MAE on the test set (70/30 split, seed 123). |

## Main results

Figures below are taken from the written report (`Relazione Statisticool.pdf`).

- **Baseline.** The full linear model reaches R² = 0.919 on the training set, but the three fuel
  consumption variables are almost collinear (VIF in the hundreds to thousands, condition number
  of about 7,400). City and highway consumption are dropped, keeping combined consumption.
- **Variable selection.** Best subset selection (BIC) and backward selection agree on five
  predictors: engine size, cylinders, combined fuel consumption and the two fuel type dummies.
- **Heteroskedasticity.** The Breusch-Pagan and White tests reject homoskedasticity both for the
  linear model and for the log-linear one.
- **Penalized models.** Ridge, Lasso and Elastic Net end up close to each other on the test set
  (test MSE between 266 and 296); Elastic Net with α = 0.5 is the best of the group.

Final comparison on the test set:

| Model                                | MSE    | MAE   |
|--------------------------------------|--------|-------|
| Log-linear                           | 368.72 | 11.90 |
| Elastic Net (α = 0.5)                | 266.03 | 9.97  |
| Interaction model, weighted (WLS)    | 38.95  | 3.40  |

The weighted model with pairwise interactions is the most accurate by a wide margin.

## Quickstart

Requires R ≥ 4.1 (the code uses the native pipe `|>`) and the packages `car`, `corrplot`, `dplyr`,
`glmnet`, `leaps` and `MASS`:

```r
install.packages(c("car", "corrplot", "dplyr", "glmnet", "leaps", "MASS"))
```

From the project folder:

```bash
Rscript car_co2.R                      # full analysis: summaries, tests, plots, final table
Rscript tests/test_co2_functions.R     # checks on the helper functions
```

In RStudio, set the working directory to the source file location and run `car_co2.R`.

## Project layout

```
car_co2.R                    main script, runs the whole analysis from top to bottom
R/co2_functions.R            documented helper functions (data loading, diagnostics, tests, penalized fits)
tests/test_co2_functions.R   checks on the helpers (also on simulated data for the heteroskedasticity tests)
co2.csv                      dataset
Relazione Statisticool.pdf   written report (Italian)
```

## Implementation notes

- **Tests implemented by hand.** Breusch-Pagan and White tests are written in `R/co2_functions.R`
  and, when `lmtest` is installed, checked against `lmtest::bptest`.
- **Reproducibility.** A fixed seed is used for the train/test split and for the CV folds.
- **Back-transformation.** Predictions from the log-linear model are brought back to g/km with
  Duan's smearing estimator, not with a plain `exp()`.

## Limitations

- **Repeated rows.** About 40% of the rows (2,958 of 7,385) have exactly the same values on the
  columns used by the models, because make and model are not used. With a random split, many test
  rows therefore have an identical twin in the training set, which makes the test error somewhat
  optimistic.
- **Single split.** The final comparison relies on one 70/30 split (seed 123); repeating the split
  would give an idea of how stable the ranking of the models is.

## License

[MIT](LICENSE)
