context('simulate_wealth_structural capitalizes wealth as desired')


# -----------------------------------
# SCENARIO
# -----------------------------------

# 1 individual in our database:
#   [0-19]: does not work
#   [20-59]: work
#   [60-79]: does not work
#   79: dies


# ---------------------------------
# TRAJECTOIRE INDIVIDUELLE
# ---------------------------------

z <- data.table::data.table(
  Id = rep(1,60),
  annee = 2000:2059,
  age = 20:79,
  findet = 20
)

z[,'salaire_tot' := lest::case_when(
  get('age')<60 ~ get('age')-20 + exp(rnorm(length(get('age')<60))),
  TRUE ~ 0
)]


z[,'K2009' := NA_real_]
z[get('annee') == 2009, 'K2009' := 100]

z[,'sexe' := 1]

z[,'UC' := 1]
z[,'ageliq' := 60]

household_table <- data.table::copy(z)

8
# ---------------------------------
# NECESSARY STEP: ESTIMATE K0 
# ---------------------------------

r <- .02
gamma <- .5
beta <- .98


# CALIBRATE K0 FOR PEOPLE OBSERVED IN 2009
tempdf_RA_beta <- estimate_K0(data = household_table,
                              r = r,
                              gamma = gamma,
                              beta = beta,
                              wealthvar_survey = "K2009")

# tempdf_RA_beta <- tempdf_RA_beta[get('annee')>=2009]

tempdf_RA_beta[,'K2009' := mean(get('K2009'), na.rm= TRUE)]


# ---------------------------------------
# PART 1
# simulate_wealth_structural_* functions have consistent behavior
# ---------------------------------------

test_functions <- function(household_table, r = .02, gamma = NULL,
         beta = NULL){
  
  if (is.null(beta)){
    message("Ignoring discount factor: modelized as beta=1")
    beta2 <- 1
  }else{
    beta2 <- beta
  }
  
  tempdf_RA_beta <- estimate_K0(data = household_table,
                                r = r,
                                gamma = gamma,
                                beta = beta2,
                                wealthvar_survey = "K2009")
  
  tempdf_RA_beta[,'K2009' := mean(get('K2009'), na.rm= TRUE)]
  
  if (!is.null(beta)){
    tempdf_RA_beta[,'wealth' := simulate_wealth_structural_beta(
      K0 = get('K0'),
      consumption0 = get('C0'),
      income = get('salaire_tot'),
      UC = get('UC'),
      r = r,
      gamma = gamma,
      beta = beta,
      returnLast = TRUE)[-1]
      ]
    return(tempdf_RA_beta)
  }
  
  
  if (!is.null(gamma)){
    tempdf_RA_beta[,'wealth' := simulate_wealth_structural_ra(
      K0 = get('K0'),
      consumption0 = get('C0'),
      income = get('salaire_tot'),
      UC = get('UC'),
      r = r,
      gamma = gamma,
      returnLast = TRUE)[-1]
      ]
    return(tempdf_RA_beta)
  }
  
  tempdf_RA_beta[,'wealth' := simulate_wealth_structural(
    K0 = get('K0'),
    consumption0 = get('C0'),
    income = get('salaire_tot'),
    UC = get('UC'),
    r = r,
    returnLast = TRUE)[-1]
    ]
  
  return(tempdf_RA_beta)
}


model_RA_beta <- test_functions(household_table, r = .02, gamma = .5,
                                beta = .98)
model_RA <- test_functions(household_table, r = .02, gamma = .5,
                                beta = NULL)
model_noRA <- test_functions(household_table, r = .02, gamma = NULL,
                           beta = NULL)


test_that("[beta + gamma] approach should produce 0 wealth on a complete dynamics", {
  expect_equal(
      round(
        model_RA_beta[get('age') ==  max(get('age'))][['wealth']]
        ),
      0
  )
})

test_that("[no beta + gamma] approach should produce 0 wealth on a complete dynamics", {
  expect_equal(
    round(
      model_RA[get('age') ==  max(get('age'))][['wealth']]
    ),
    0
  )
})

test_that("[no beta + no gamma] should produce 0 wealth on a complete dynamics", {
  expect_equal(
    round(
      model_noRA[get('age') ==  max(get('age'))][['wealth']]
    ),
    0
  )
})

# -------------------------
# PART 2
# -------------------------
# simulate_wealth_structural_beta2 equivalent to simulate_wealth_structural_beta
# on the same set of observations

# CALIBRATE K0 FOR PEOPLE OBSERVED IN 2009
r <- 0.03
beta <- 0.98
gamma <- 0.5

tempdf_RA_beta <- estimate_K0(data = household_table,
                              r = r,
                              gamma = gamma,
                              beta = beta,
                              wealthvar_survey = "K2009")


tempdf_RA_beta[,'K2009' := mean(get('K2009'), na.rm= TRUE)]



output1 <- data.table::copy(tempdf_RA_beta)
output2 <- data.table::copy(tempdf_RA_beta)
output2[,'tau' := get('age') - get('findet')]

output1[,'wealth' := simulate_wealth_structural_beta(
  K0 = get('K0'),
  consumption0 = get('C0'),
  income = get('salaire_tot'),
  UC = get('UC'),
  r = r,
  gamma = gamma,
  beta = beta,
  returnLast = FALSE)
  ]


output2[,'wealth' := simulate_wealth_structural_beta2(
  K0 = get('K0'),
  consumption0 = get('C0'),
  income = get('salaire_tot'),
  UC = get('UC'),
  tau = get('tau'),
  r = r,
  gamma = gamma,
  beta = beta,
  returnLast = FALSE)
  ]

test_that("simulate_wealth_structural_beta and simulate_wealth_structural_beta2 produce same result on dynamics 0:(T-1)", {
  expect_equal(
      output1[['wealth']],
      output2[['wealth']]
      )
})




output3 <- data.table::copy(tempdf_RA_beta)
output3[,'tau' := get('age') - get('findet')]

# KEEP ONLY VALUES FROM 2009 TO DEATH
output3 <- output3[get('annee')>=2009]
  
output3[,'wealth' := simulate_wealth_structural_beta2(
  K0 = get('K2009'),
  consumption0 = get('C0'),
  income = get('salaire_tot'),
  UC = get('UC'),
  tau = get('tau'),
  r = r,
  gamma = gamma,
  beta = beta,
  returnLast = FALSE)
  ]


test_that("simulate_wealth_structural_beta and simulate_wealth_structural_beta2 produce same result on dynamics 0:(T-1)", {
  expect_equal(
    output1[get('annee')>=2009][['wealth']],
    output2[get('annee')>=2009][['wealth']],
    output3[get('annee')>=2009][['wealth']]
  )
})
