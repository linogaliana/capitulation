context("Check transform_household has consistent behavior")



# -----------------------------------
# TWO INDIVIDUALS IN ONE HOUSEHOLD
# -----------------------------------

z <- data.frame(
  Id = rep(1:2,2),
  annee = c(2008,2008,2009,2009),
  sexe = rep(1:2,2),
  # Married together
  matri = rep(2,4),
  conjoint = rep(2:1,2),
  # Male is household head
  referent = rep(1,4),
  ageliq = rep(c(60,65),2),
  salaire = c(10000,2,0,8),
  age = c(25,23,26,24),
  findet = rep(20,4),
  UC = 1.5,
  nbpersm = 2L, 
  list_children_charge = NA_character_,
  list_children_life = NA_character_
)

z_dplyr <- z
z_dt <- data.table::as.data.table(z)


# Test 1: identical between data.table and dplyr
# ------------------------------------------------------


# # ANNOUNCE TIDYVERSE APPROACH IS DEPRECATED
# test_that("Tidyverse approach is announced deprecated", {
#   expect_message(capitulation::transform_household(data.frame(z_dplyr)),
#                  "tidyverse approach is deprected. Output might be incorrect")
# })
# 
# 
# # IDENTICAL RESULTS BETWEEN DATA.TABLE AND TIDYVERSE (DEPRECATED)
# test_that("Identical output between data.table and dplyr", {
#   
#   expect_equal(
#     capitulation::transform_household(data.frame(z_dplyr)),
#     capitulation::transform_household(data.frame(z_dt))
#   ) # NB: we Convert into dataframe to avoid
#   # having false because of class type differences
#   
# 
# })

# Test 2: change column names
# ------------------------------------------------------


# Simple function to rename data column
rename_col <- function(z,var = "age"){
  if (inherits(z, "data.table")){
    z2 <- data.table::copy(z)
    data.table::setnames(z2,old = var, new = paste0(var,"2"))
  } else{
    z2 <- z %>% dplyr::rename(!!rlang::sym(paste0(var,"2")) := !!rlang::sym(var))
  }
  return(z2)
}


test_that("Different column names can be handled", {

  # 1/ id_var  
  # ++++++++++++
  
  expect_equal(
    capitulation::transform_household(z_dt),
    data.table::setnames(
      capitulation::transform_household(rename_col(z_dt, var = 'Id'), id_var = "Id2"), 
      old = "Id2", new = 'Id'
    )
  ) 
  
  # expect_equal(
  #   capitulation::transform_household(z_dplyr),
  #   capitulation::transform_household(rename_col(z_dplyr, var = 'Id'), id_var = "Id2") %>%
  #   dplyr::rename(
  #     Id = .data$Id2
  #   )
  # )
  
  
  # 2/ income_var
  # +++++++++++++++++
  # No need to rename columns afterwards

  expect_equal(
    capitulation::transform_household(z_dt),
      capitulation::transform_household(rename_col(z_dt, var = 'salaire'), income_var = "salaire2")
  ) 
  
  # expect_equal(
  #   capitulation::transform_household(z_dplyr),
  #   capitulation::transform_household(rename_col(z_dplyr, var = 'salaire'), income_var = "salaire2")
  # )
  
  # 3/ age_var
  # +++++++++++++++++
  
  expect_equal(
    capitulation::transform_household(z_dt),
    data.table::setnames(
      capitulation::transform_household(rename_col(z_dt, var = 'age'), age_var = "age2"), 
      old = c("age2","age2_conjoint"), new = c('age','age_conjoint')
    )
  )
  
  # expect_equal(
  #   capitulation::transform_household(z_dplyr),
  #   capitulation::transform_household(rename_col(z_dplyr, var = 'age'), age_var = "age2") %>%
  #     dplyr::rename(
  #       age = .data$age2, age_conjoint = .data$age2_conjoint
  #     )
  # )
  
  
  # 4/ ageliq_var
  # +++++++++++++++++
  
  expect_equal(
    capitulation::transform_household(z_dt),
    data.table::setnames(
      capitulation::transform_household(rename_col(z_dt, var = 'ageliq'), ageliq_var = "ageliq2"), 
      old = c("ageliq2"), new = c('ageliq')
    )
  ) 
  
  # expect_equal(
  #   capitulation::transform_household(z_dplyr),
  #   capitulation::transform_household(rename_col(z_dplyr, var = 'ageliq'), ageliq_var = "ageliq2") %>%
  #     dplyr::rename(
  #       ageliq = .data$ageliq2
  #     )
  # )
  
  
  # 4/ matrimonial_var
  # +++++++++++++++++++++
  
  expect_equal(
    capitulation::transform_household(z_dt),
    data.table::setnames(
      capitulation::transform_household(rename_col(z_dt, var = 'matri'), matrimonial_var = "matri2"), 
      old = c("matri2"), new = c('matri')
    )
  ) 
  
  # expect_equal(
  #   capitulation::transform_household(z_dplyr),
  #   capitulation::transform_household(rename_col(z_dplyr, var = 'matri'), matrimonial_var = "matri2") %>%
  #     dplyr::rename(
  #       matri = .data$matri2
  #     )
  # )
    

})


# Test 3: sum income is consistent
# ------------------------------------------------------


test_that("Household income as sum of individual income at household level", {
  
  expect_equal(
    capitulation::transform_household(z_dt)$salaire_tot,
    c(sum(z_dt$salaire[1:2]),sum(z_dt$salaire[3:4]))
  )
  
})


# Test 4: we keep only row value by household (referent)
#           and spouse information is correct
# ------------------------------------------------------

test_that("Household level data is correct", {
  
  # Individual 1 is household head
  expect_equal(
    capitulation::transform_household(z_dt)$Id,
    rep(unique(z_dt$referent),2)
      )
  
  # Spouse id is correct
  expect_equal(
    capitulation::transform_household(z_dt)$conjoint,
    rep(2,2)
  )
  
  # Spouse information is correct
  expect_equal(
    capitulation::transform_household(z_dt)$age_conjoint,
    z_dt[get('Id')==2][['age']]
  )
  
  # Information on household head is correct
  expect_equal(
    capitulation::transform_household(z_dt)[,.SD,.SDcols = c('age','annee','sexe','findet','ageliq')],
    z_dt[get('Id')==get('referent')][,.SD,.SDcols = c('age','annee','sexe','findet','ageliq')]
  )
  
  
})

  

# Test 5: no duplication for single people
# ------------------------------------------------------

z <- data.frame(
  Id = rep(1:2,2),
  annee = c(2008,2008,2009,2009),
  sexe = rep(1:2,2),
  # Married together
  matri = rep(1,4),
  conjoint = rep(2:1,2),
  # Both are their own household head
  referent = rep(1:2,2),
  ageliq = rep(c(60,65),2),
  salaire = c(10000,2,0,8),
  age = c(25,23,26,24),
  findet = rep(20,4),
  UC = 1.5,
  nbpersm = 2L, 
  list_children_charge = NA_character_,
  list_children_life = NA_character_
)

z_dplyr <- z
z_dt <- data.table::as.data.table(z)
z_output <- capitulation::transform_household(z_dt)


test_that("Single people treatment is consistent", {
  
  expect_equal(
    z_dt[,.SD,.SDcols = intersect(names(z_dt),names(z_output))]
    [order(get('Id'),get('annee'))],
    z_output[,.SD,
             .SDcols = intersect(names(z_dt),names(z_output))]
  )
  
})



