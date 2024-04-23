context("We are able to create a consistent fake population")



# -----------------------------------
# generate_pre_indiv
# -----------------------------------

# WE SHOULD RETURN AN ERROR WHEN NUMBERS DONT MATCH
test_that("We return an error when expected", {
  expect_error(
    generate_pre_indiv(Nindiv = 3, anaiss = c(1980,1990),
                       lifetime = c(50,60),
                       findet = c(25,22))
  )
})

test_that("We return as many trajectories as wanted", {
  expect_equal(
    unique(
      generate_pre_indiv(Nindiv = 3, anaiss = NULL,
                       lifetime = NULL,
                       findet = NULL)[['Id']]
      ),
    seq_len(3)
  )
})



# NUMBER OF ROWS CONSISTENT
test_that("Number of rows consistent", {
  expect_equal(
    nrow(
      generate_pre_indiv(Nindiv = 3, anaiss = c(1980,1990,2000),
                         lifetime = c(50,60,70),
                         findet = c(25,22,25))
    ),
    sum(c(50,60,70))
  )
})




dt <- generate_pre_indiv(
  Nindiv = 3,
  anaiss = c(1980,1990,2000),
  lifetime = c(50,60,70),
  findet = c(25,22,25))

dt <- dt[(get('age') < 25 & get('Id')!=2) |
                          (get('Id')==2 & (get('age') < 22))][['salaire']]


test_that("NO INCOME BEFORE END OF STUDYING YEAR", {
  expect_equal(dt,rep(0, length(dt)))
})



# ---------------------------
# generate_fake_data
# --------------------------

fake_data <- generate_fake_data()

test_that("We should have same information in both tables", {
  
  expect_equal(
    unique(
      fake_data$pre_indiv[,.SD,
                          .SDcols = intersect(
                            names(fake_data$pre_indiv),
                            names(fake_data$descript))]
    ),
    fake_data$descript[,.SD,
                       .SDcols = intersect(
                         names(fake_data$pre_indiv),
                         names(fake_data$descript))]
  )
})
