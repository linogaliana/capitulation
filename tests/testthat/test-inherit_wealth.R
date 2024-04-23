context("inherit_wealth gives consistent transition rules between years")



# library(capitulation)


# =====================================
#           CASE 1: DEATH
# =====================================
# DEATH BETWEEN N AND N-1

# IN THAT CASE, DATAFRAME HAS ONLY ONE ROW
df <- data.frame(
  id = 1,
  patrimoine = 10000,
  annee = 2009,
  matri = NA,
  sexe = 1,
  age = 65,
  ageliq = 60,
  id_conjoint = NA,
  patrimoine_conjoint = NA,
  matri_conjoint = NA
)
referent_list <- 1:10



tab1 <- inherit_wealth(id = df$id,
               patrimoine = df$patrimoine,
               year = df$annee,
               matri_status = df$matri,
               sexe = df$sexe,
               spouse_id = df$id_conjoint,
               patrimoine_conjoint = df$patrimoine_conjoint,
               matri_conjoint = df$matri_conjoint, 
               year_loop = 2010,
               referent_list = referent_list
)

# Do we have a list ?
test_that("inherit_wealth returns a list", {
  expect_true(class(tab1) == "list")
})

tab1 <- data.frame(tab1)

# Output for death case
test_that("Initial values returned when individual dies", {
  expect_equal(tab1$Kprev,df$patrimoine)
  expect_equal(tab1$patrimoine,df$patrimoine)
})


# =====================================
#         CASE 2: LEAVE PARENTS
# =====================================
# matri_status[start]==1

# One individual who leaves parents between N-1 and N
df <- data.frame(
  id = rep(1,2),
  patrimoine = c(10000,0),
  annee = 2009:2010,
  matri = c(NA,1),
  sexe = rep(1,2),
  age = 19:20,
  ageliq = rep(60,2),
  id_conjoint = rep(NA,2),
  patrimoine_conjoint = rep(NA,2),
  matri_conjoint = rep(NA,2)
)
referent_list <- 1:10


tab2 <- inherit_wealth(id = df$id,
                       patrimoine = df$patrimoine,
                       year = df$annee,
                       matri_status = df$matri,
                       sexe = df$sexe,
                       spouse_id = df$id_conjoint,
                       patrimoine_conjoint = df$patrimoine_conjoint,
                       matri_conjoint = df$matri_conjoint, 
                       year_loop = 2010,
                       referent_list = referent_list
)
tab2 <- data.frame(tab2)


test_that("Young people are given 0 wealth when starting life alone", {
  expect_equal(tab2$Kprev,df$patrimoine[1])
  expect_equal(tab2$patrimoine,0)
})



# =====================================
#         CASE 3: MARRIAGE
# =====================================
# matri_status[start]==2


# ------------------------------
# 3.1 : DIVORCE AND MARRIAGE
# ------------------------------
# Two possibilities:
#    * new partner
#    * same partner (assuming we had a bug in previous routine that considered
#         a wrong situation change)


referent_list <- 1:2

df <- data.frame(
  id = c(rep(1,2),
         rep(2,2)
         ),
  patrimoine = rep(c(10000,0),2),
  annee = rep(2009:2010,2),
  matri = rep(2,4), # Both individuals are married directly after divorcing
  sexe = c(rep(1,2), # id1: male
           rep(1,2) # id2: female
           ),
  age = rep(19:20,2),
  ageliq = rep(60,4),
  id_conjoint = c(311,312, #They both change husband/wife between two years
                  32,32),
  patrimoine_conjoint = c(NA,2, # Wife wealth (accumulated with previous husband) was 2 euros last year
                          NA,NA # Husband wealth (accumulated with previous wife) was 4 euros last year
                          ),
  matri_conjoint = rep(2,4) # Husband/wife were married before
)

df$Id <- df$id
df2 <- split(df,df$Id)

tab4 <- lapply(df2, function(.)({
  return(
    data.frame(
      inherit_wealth(id = .$id,
                       patrimoine = .$patrimoine,
                       year = .$annee,
                       matri_status = .$matri,
                       sexe = .$sexe,
                       spouse_id = .$id_conjoint,
                       patrimoine_conjoint = .$patrimoine_conjoint,
                       matri_conjoint = .$matri_conjoint, 
                       year_loop = 2010,
                       referent_list = referent_list
    ))
    )})
)

tab4 <- do.call(rbind,tab4)


test_that("Couples: situation 1 (male, married with another wife last year)", {
  expect_equal(tab4$Kprev[1],df$patrimoine[1])
  expect_equal(tab4$patrimoine[1],df$patrimoine[1]/2 + df$patrimoine_conjoint[2]/2) # patrimoine_conjoint is a lead variable
})


# This case should not happen but is presented to ensure we are able to handle errors
test_that("Couples: situation 2 (male, married with same wife last year)", {
  expect_equal(tab4$Kprev[2],df$patrimoine[3])
  expect_equal(tab4$patrimoine[2],df$patrimoine[3]) # patrimoine_conjoint is a lead variable
})



# --------------------------------------------------
# 3.2 : MARRIAGE AFTER ANOTHER SITUATION BEFORE
# --------------------------------------------------

# In that case, individual is marrying while we/she was on another
#  situation before (e.g. he/she was on parents household)


referent_list <- 1:3  #1 was referent last year, others were not 


df <- data.frame(
  id = c(rep(1,2),
         rep(2,2),
         rep(3,2)
  ),
  patrimoine = c(10000,0,
                 20000,0,
                 30000,0
  ),
  annee = rep(2009:2010,3),
  matri = c(1,2,
            3,2,
            4,2), # All possible situations
  sexe = rep(1,6),
  age = rep(19:20,3),
  ageliq = rep(60,6),
  id_conjoint = c(NA,11, #They all change husband/wife between two years
                  NA,12,
                  NA,13),
  patrimoine_conjoint = c(NA,2, 
                          NA,4,
                          NA,6
  ),
  matri_conjoint = c(NA,1,
                     NA,3,
                     NA,4),
  stand_niveau = 111:116,#  I put different values in those lines to identify inheritance rules
  stand_patri = 211:216,
  stand_niveau_conjoint = 311:316,
  stand_patri_conjoint = 411:416
)


df$Id <- df$id
df2 <- split(df,df$Id)

tab4_2 <- lapply(df2, function(.)({
  return(
    data.frame(
      inherit_wealth(id = .$id,
                     patrimoine = .$patrimoine,
                     year = .$annee,
                     matri_status = .$matri,
                     sexe = .$sexe,
                     spouse_id = .$id_conjoint,
                     patrimoine_conjoint = .$patrimoine_conjoint,
                     matri_conjoint = .$matri_conjoint, 
                     year_loop = 2010,
                     referent_list = referent_list
      ))
  )})
)

tab4_2 <- do.call(rbind,tab4_2)



test_that("Couples: situation 3 (male, new wife had just left family)", {
  expect_equal(tab4_2$Kprev[1],df$patrimoine[1])
  expect_equal(tab4_2$patrimoine[1],df$patrimoine[1]) # wife had no time to accumulate wealth
  # NB: Female only inherits previous capital since no own capital
  # No standard as long as husband lives (in that case she is not head)
})


test_that("Couples: situation 4 (male, new wife had an husband that just died)", {
  expect_equal(tab4_2$Kprev[2],df$patrimoine[3])
  expect_equal(tab4_2$patrimoine[2],df$patrimoine[3] + df$patrimoine_conjoint[4])
      # Wife wealth is individual since husband died previously,
      # No need to split again
  # No standard as long as husband lives (in that case she is not head)
})


test_that("Couples: situation 5 (male, new wife/husband was divorced)", {
  expect_equal(tab4_2$Kprev[3],df$patrimoine[5])
  expect_equal(tab4_2$patrimoine[3],df$patrimoine[5] + df$patrimoine_conjoint[6])
  # wife wealth has already been shared,
  # No need to split again
})



# --------------------------------------------------
# 3.3 : WOMEN THAT WERE NOT REFERENT LAST YEAR
# --------------------------------------------------

# Two situations:
#     * we know previous household wealth she must inherit half
#     * we don't know previous household wealth she must inherit half

referent_list <- 999  #No referent there 


df <- data.frame(
  id = c(rep(1,2),
         rep(2,2)
  ),
  patrimoine = c(10000,0,
                 20000,0
  ),
  annee = rep(2009:2010,2),
  matri = rep(2,4),
  sexe = rep(2,4),
  age = rep(19:20,2),
  ageliq = rep(60,4),
  id_conjoint = rep(NA,4),
  patrimoine_conjoint = c(1,2, # Wife wealth (accumulated with previous husband) was 2 euros last year
                          NA,NA # Husband wealth (accumulated with previous wife) was 4 euros last year
  ),
  matri_conjoint = rep(NA,4) # Husband/wife were married before
)

df$Id <- df$id
df2 <- split(df,df$Id)

tab4_3 <- lapply(df2, function(.)({
  return(
    data.frame(
      inherit_wealth(id = .$id,
                     patrimoine = .$patrimoine,
                     year = .$annee,
                     matri_status = .$matri,
                     sexe = .$sexe,
                     spouse_id = .$id_conjoint,
                     patrimoine_conjoint = .$patrimoine_conjoint,
                     matri_conjoint = .$matri_conjoint, 
                     year_loop = 2010,
                     referent_list = referent_list
      ))
  )})
)

tab4_3 <- do.call(rbind,tab4_3)


test_that("Couples: situation 6 (not referent last year, last year wealth should be splitted when known", {
  expect_equal(tab4_3$Kprev[1],df$patrimoine[1])
  # Values created are inconsistent (individual cannot have personal wealth and not be referent)
  # But Kprev always return value given
  expect_equal(tab4_3$patrimoine[1],df$patrimoine_conjoint[2]/2)
  # No personal wealth, only brings half of previous referent wealth
  # Only referent inherit standard
})

test_that("Couples: situation 7 (not referent last year, last year wealth cannot be splitted when not known", {
  expect_equal(tab4_3$Kprev[2],df$patrimoine[3])
  # Values created are inconsistent (individual cannot have personal wealth and not be referent)
  # But Kprev always return value given
  expect_equal(tab4_3$patrimoine[2],0)
  # Don't know last referent wealth: 0 imputed
})



# =====================================
#         CASE 4: PARTNER DEATH
# =====================================
# matri_status[start]==3

# Two situations:
#    * male: inherit all previous household wealth and his own standard
#    * female: inherit all of previous household wealth and her husband standard

referent_list <- c(1,3) # Only male was referent

df <- data.frame(
  id = c(rep(1,2),
         rep(2,2)
  ),
  patrimoine = c(10000,0,
                 20000,0
  ),
  annee = rep(2009:2010,2),
  matri = rep(c(2,3),2),
  sexe = c(rep(1,2), rep(2,2)),
  age = rep(19:20,2),
  ageliq = rep(60,4),
  id_conjoint = c(NA,NA,3,NA),
  patrimoine_conjoint = c(NA,NA, # Wife wealth (accumulated with previous husband) was 2 euros last year
                          NA,4 # Husband wealth (accumulated with previous wife) was 4 euros last year
  ),
  matri_conjoint = rep(NA,4)
)


df$Id <- df$id
df2 <- split(df,df$Id)

tab5 <- lapply(df2, function(.)({
  return(
    data.frame(
      inherit_wealth(id = .$id,
                     patrimoine = .$patrimoine,
                     year = .$annee,
                     matri_status = .$matri,
                     sexe = .$sexe,
                     spouse_id = .$id_conjoint,
                     patrimoine_conjoint = .$patrimoine_conjoint,
                     matri_conjoint = .$matri_conjoint, 
                     year_loop = 2010,
                     referent_list = referent_list
      ))
  )})
)

tab5 <- do.call(rbind,tab5)


# Tests for id==1
test_that("Partner death, situation 1: male (everything that was household level becomes own possession)", {
  expect_equal(tab5$Kprev[1],df$patrimoine[1])
  # Values created are inconsistent (individual cannot have personal wealth and not be referent)
  # But Kprev always return value given
  expect_equal(tab5$patrimoine[1],df$patrimoine[1])
  # No personal wealth, only brings half of previous referent wealth
})

# Tests for id==2
test_that("Partner death, situation 2:
          female (everything that was attributed to husband (referent) becomes own possession)", {
  expect_equal(tab5$Kprev[2],df$patrimoine[3])
  # Values created are inconsistent (individual cannot have personal wealth and not be referent)
  # But Kprev always return value given
  expect_equal(tab5$patrimoine[2],df$patrimoine_conjoint[4] + df$patrimoine[3])
  # Get husband wealth completely (taking into account lead variable)
})



# =====================================
#         CASE 5: DIVORCE
# =====================================
# matri_status[start]==4

df <- data.frame(
  id = c(rep(1,2),
         rep(2,2)
  ),
  patrimoine = c(10000,0,
                 20000,0
  ),
  annee = rep(2009:2010,2),
  matri = rep(c(2,4),2),
  sexe = c(rep(1,2), rep(2,2)),
  age = rep(19:20,2),
  ageliq = rep(60,4),
  id_conjoint = c(NA,NA,3,NA),
  patrimoine_conjoint = c(2,NA, # Wife wealth (accumulated with previous husband) was 2 euros last year
                          2,NA # Husband wealth (accumulated with previous wife) was 4 euros last year
  ),
  matri_conjoint = rep(NA,4)
)


df$Id <- df$id
df2 <- split(df,df$Id)

tab6 <- lapply(df2, function(.)({
  return(
    data.frame(
      inherit_wealth(id = .$id,
                     patrimoine = .$patrimoine,
                     year = .$annee,
                     matri_status = .$matri,
                     sexe = .$sexe,
                     spouse_id = .$id_conjoint,
                     patrimoine_conjoint = .$patrimoine_conjoint,
                     matri_conjoint = .$matri_conjoint, 
                     year_loop = 2010,
                     referent_list = referent_list
      ))
  )})
)

tab6 <- do.call(rbind,tab6)


test_that("Divorce, situation 1: male (split household level wealth)", {
  expect_equal(tab6$Kprev[1],df$patrimoine[1])
  expect_equal(tab6$patrimoine[1],df$patrimoine[1]/2)
})

test_that("Divorce, situation 2: female (split household level wealth)", {
  expect_equal(tab6$Kprev[2],df$patrimoine[3])
  expect_equal(tab6$patrimoine[2],df$patrimoine_conjoint[3]/2)
})





