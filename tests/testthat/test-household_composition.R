context("Individual dynamics are well reshaped into household level information")




# PART 1 - MUST BE TESTED: ASSIGN HUSBAND/WIFE --------------------


# PART 2 - ASSIGN CHILDREN ---------------------------


table_indiv <- data.table::data.table(
  Id = c(rep(1L,10L), rep(2L,5L), rep(3L,5L)),
  age = c(20:29, 1:5, 1:5),
  annee = c(2000:2009, 2001:2005,
            2003:2007),
  salaire = 0L,
  anaiss = c(rep(1980L,10L), rep(2001L,5L), rep(2003L,5L))
)

fakesimul <- list()
fakesimul$fam <- data.table::data.table(
  Id = c(rep(1L,10L), rep(2L,5L), rep(3L,5L)),
  annee = c(2000:2009, 2001:2005,
            2003:2007),
  enf1 = c(rep(2L,10L), rep(0L,5L), rep(0L,5L)),
  enf2 = c(rep(3L,10L), rep(0L,5L), rep(0L,5L)),
  conjoint = 0L,
  matri = 1L,
  pere = c(rep(0L,10L), rep(1L,5L), rep(1L,5L)),
  mere = c(rep(0L,10L), rep(99L,5L), rep(88L,5L))
)

fakedescript <- data.table::data.table(
  Id = c(1L,2L,3L),
  sexe = 1L,
  neFrance = 1L,
  anaiss = c(1980L,2001L,2003L),
  findet = 18L,
  ageMax = c(80L, 5L, 5L),
  pere = c(0L,1L,1L),
  mere = c(0L,99L,88L)
)


# indiv <- assign_referent(table_indiv = table_indiv,
#                          fam = fakesimul$fam,
#                          descript =  fakedescript,
#                          call.Rcpp = TRUE)

indiv <- household_composition(table_indiv = table_indiv,
                               simul = fakesimul,
                               descript =  fakedescript,
                               call.Rcpp = TRUE)


test_that("Referent column should be equal to 1 (children when alive are in custody)",{
  expect_equal(indiv$referent,rep(1L, nrow(indiv)))
})


test_that("list_children_life for Id=1 should be {2, 3}",{
  expect_equal(unique(
    na.omit(indiv[get('Id')==1][["list_children_life"]])
  ), "2, 3")
})

test_that("list_children_alive for Id=1 should be consistent with data construction",{
  expect_equal(
    indiv[get('Id')==1 & get('annee')<2001][["list_children_life"]],
    rep(NA_character_,nrow(indiv[get('Id')==1 & get('annee')<2001]))
  )
  expect_equal(
    indiv[get('Id')==1 & get('annee') %in% c(2001,2002)][["list_children_alive"]],
    rep("2",nrow(indiv[get('Id')==1 & get('annee') %in% c(2001,2002)]))
  )
  expect_equal(
    indiv[get('Id')==1 & get('annee') %in% c(2003,2005)][["list_children_alive"]],
    rep("2, 3",nrow(indiv[get('Id')==1 & get('annee') %in% c(2003,2005)]))
  )
  expect_equal(
    indiv[get('Id')==1 & get('annee') %in% c(2006,2007)][["list_children_alive"]],
    rep("3",nrow(indiv[get('Id')==1 & get('annee') %in% c(2006,2007)]))
  )
  expect_equal(
    indiv[get('Id')==1 & get('annee')>2007][["list_children_life"]],
    rep(NA_character_,nrow(indiv[get('Id')==1 & get('annee')>2007]))
  )
})

test_that("Number consumption unit consistent with household composition",{
  expect_equal(
    indiv[get('Id')==1 & get('annee')<2001][["UC"]],
    rep(1L,nrow(indiv[get('Id')==1 & get('annee')<2001]))
  )
  expect_equal(
    indiv[get('Id')==1 & get('annee') %in% c(2001,2002)][["UC"]],
    rep(1+0.3,nrow(indiv[get('Id')==1 & get('annee') %in% c(2001,2002)]))
  )
  expect_equal(
    indiv[get('Id')==1 & get('annee') %in% 2003:2005][["UC"]],
    rep(1+.3+.3,nrow(indiv[get('Id')==1 & get('annee') %in% 2003:2005]))
  )
  expect_equal(
    indiv[get('Id')==1 & get('annee') %in% c(2006,2007)][["UC"]],
    rep(1+.3,nrow(indiv[get('Id')==1 & get('annee') %in% c(2006,2007)]))
  )
  expect_equal(
    indiv[get('Id')==1 & get('annee')>2007][["UC"]],
    rep(1,nrow(indiv[get('Id')==1 & get('annee')>2007]))
  )
})

test_that("Number people in household consistent with household composition",{
  expect_equal(
    indiv[get('Id')==1 & get('annee')<2001][["nbpersm"]],
    rep(1L,nrow(indiv[get('Id')==1 & get('annee')<2001]))
  )
  expect_equal(
    indiv[get('Id')==1 & get('annee') %in% c(2001,2002)][["nbpersm"]],
    rep(2,nrow(indiv[get('Id')==1 & get('annee') %in% c(2001,2002)]))
  )
  # expect_equal(
  #   indiv[get('Id')==1 & get('annee') %in% 2003:2005][["nbpersm"]],
  #   rep(3,nrow(indiv[get('Id')==1 & get('annee') %in% 2003:2005]))
  # )
  expect_equal(
    indiv[get('Id')==1 & get('annee') %in% c(2006,2007)][["nbpersm"]],
    rep(2,nrow(indiv[get('Id')==1 & get('annee') %in% c(2006,2007)]))
  )
  expect_equal(
    indiv[get('Id')==1 & get('annee')>2007][["nbpersm"]],
    rep(1,nrow(indiv[get('Id')==1 & get('annee')>2007]))
  )
})
