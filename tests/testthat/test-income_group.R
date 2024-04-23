context("income_group gives a correct quantile")

fake_df <- data.table::data.table(
  "Id" = c(rep(1, 100), rep(2,100)),
  "annee"  = c(rep(3, 100), rep(4,100)),
  "income" = c(seq_len(100), 1 + seq_len(100))
)

output <- income_group(fake_df, quantile = 0.8, incomevar = "income")


test_that("Column name follows expected pattern", {
  expect_equal(colnames(output)[5],
               paste0("top","_",100*(1-0.8)))
})


test_that("Quantiles are correct", {
  expect_equal(output[['top_20']],
               c(rep(0,100), rep(1,100))
               )
})
