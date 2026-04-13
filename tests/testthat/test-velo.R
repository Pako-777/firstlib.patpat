test_that("filtre boucle renvoit un tibble", {
  res <- filtrer_trajet(df_velo, boucle = "880")
  expect_s3_class(res, "tbl")
})



test_that("filtrer_trajet renvoie tout si boucle est NULL", {
  test_data <- data.frame(
    `Numéro de boucle` = c(880, 881, 787),
    Total = c(100, 200, 300),
    check.names = FALSE
  )

  res <- filtrer_trajet(test_data, boucle = NULL)
  expect_equal(nrow(res), 3)
})




# Tests pour filtre_anomalie
test_that("filtre_anomalie supprime les lignes avec anomalies", {
  test_data <- data.frame(
    `Probabilité de présence d'anomalies` = c(NA, "Oui", NA, "Non"),
    Total = c(10, 20, 30, 40),
    check.names = FALSE
  )
  res <- filtre_anomalie(test_data)
  expect_equal(nrow(res), 2)  # Seulement les NA
})

# Tests pour compter_nombre_trajets
test_that("compter_nombre_trajets calcule la somme correctement", {
  test_data <- data.frame(Total = c(10, 20, 30))
  res <- compter_nombre_trajets(test_data)
  expect_equal(res, 60)
})

# Tests pour compter_nombre_boucle
test_that("compter_nombre_boucle compte les boucles distinctes", {
  test_data <- data.frame(
    `Numéro de boucle` = c(880, 880, 881, 787),
    check.names = FALSE
  )
  res <- compter_nombre_boucle(test_data)
  expect_equal(res, 3)
})

# Tests pour trouver_trajet_max
test_that("trouver_trajet_max trouve le maximum", {
  test_data <- data.frame(
    `Boucle de comptage` = c("A", "B", "C"),
    Jour = as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")),
    Total = c(100, 300, 200),
    check.names = FALSE
  )
  res <- trouver_trajet_max(test_data)
  expect_equal(nrow(res), 1)
  expect_equal(res$Total, 300)
})

# Tests pour calcul_distribution_semaine
test_that("calcul_distribution_semaine compte par jour", {
  test_data <- data.frame(
    `Jour de la semaine` = c(1, 1, 2, 3),
    Total = c(10, 20, 30, 40),
    check.names = FALSE
  )
  res <- calcul_distribution_semaine(test_data)
  expect_equal(nrow(res), 3)  # 3 jours distincts
  expect_true("trajets" %in% names(res))
})


# Tests pour plot_distribution_semaine
test_that("plot_distribution_semaine renvoie un ggplot", {
  test_data <- data.frame(
    `Jour de la semaine` = c(1, 2, 3),
    Total = c(100, 200, 150),
    `Probabilité de présence d'anomalies` = c(NA, NA, NA),
    check.names = FALSE
  )
  res <- plot_distribution_semaine(test_data)
  expect_s3_class(res, "ggplot")
})

