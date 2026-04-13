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
