context("Check assign_referent gives expected household head")



# -----------------------------------
# CASE 1: IMMIGRANTS
# -----------------------------------

z <- data.frame(
  Id = sort(rep(seq(1:5),3)),
  annee = rep(seq(2008, length.out = 3),5),
  conjoint = rep(c(9,9999,9999),5),
  sexe = sort(rep(sample(0:1, size = 5, replace = T),3)),
  matri = rep(c(NA,99,99),5),
  referent = NA_real_,
  referent2 = NA_real_,
  findet = rep(20,15),
  age = rep(0,15),
  salaire = rep(0,15),
  pere = rep(0,15),
  ageMaxPere = rep(0,15),
  mere = rep(0,15),
  ageMaxMere = rep(0,15),
  neFrance = rep(0,15)
)

df_referent <- capitulation::assign_referent_cpp(id = z$Id,
                                                 annee = z$annee,
                                                 conjoint = z$conjoint,
                                                 sexe = z$sexe,
                                                 matri = z$matri,
                                                 referent = z$referent,
                                                 referent2 = z$referent2,
                                                 findet = z$findet,
                                                 age = z$age,
                                                 salaire = z$salaire,
                                                 pere = z$pere,
                                                 ageMaxPere = z$ageMaxPere,
                                                 mere = z$mere,
                                                 ageMaxMere = z$ageMaxMere,
                                                 neFrance = z$neFrance)

df_referent <- data.frame(
  do.call(cbind,df_referent)
)



test_that("Immigrants: treated as single people when they arrive", {
  
  # matri: 1 when they arrive
  expect_equal(
    df_referent$matri[df_referent$annee==2008],
    rep(1,5)
  )
  # Expect id_household == id
  
  # conjoint: 0 when they arrive  
  expect_equal(
    df_referent$conjoint[df_referent$annee==2008],
    rep(0,5)
  )

  # referent: 0 when they arrive  
  expect_equal(
    df_referent$referent[df_referent$annee==2008],
    rep(0,5)
  )
  
})


test_that('Immigrants: after they arrived, normal treatment',{
  
  # matri: 1 when they arrive
  expect_false(
    isTRUE(all.equal(
    df_referent$matri[df_referent$annee!=2008], rep(1,10)
    ))
  )
  
  # conjoint: 0 when they arrive  
  expect_false(
    isTRUE(all.equal(
      df_referent$conjoint[df_referent$annee==2008], rep(0,10)
    ))
  )
  
  # referent: 0 when they arrive  
  expect_false(
    isTRUE(all.equal(
      df_referent$referent[df_referent$annee==2008], rep(0,10)
  ))
  )
  
  
})



# -----------------------------------
# CASE 2: MARRIED
# -----------------------------------


z <- data.frame(
  Id = c(1,2),
  annee = rep(2008,2),
  conjoint = seq(3,4),
  sexe = c(1,2), #One man, one woman
  matri = rep(2,2), 
  referent = c(3,4),
  referent2 = NA_real_,
  findet = rep(20,2),
  age = rep(0,2),
  salaire = rep(0,2),
  pere = rep(0,2),
  ageMaxPere = rep(0,2),
  mere = rep(0,2),
  ageMaxMere = rep(0,2),
  neFrance = rep(0,2)
)

df_referent <- capitulation::assign_referent_cpp(id = z$Id,
                                                 annee = z$annee,
                                                 conjoint = z$conjoint,
                                                 sexe = z$sexe,
                                                 matri = z$matri,
                                                 referent = z$referent,
                                                 referent2 = z$referent2,
                                                 findet = z$findet,
                                                 age = z$age,
                                                 salaire = z$salaire,
                                                 pere = z$pere,
                                                 ageMaxPere = z$ageMaxPere,
                                                 mere = z$mere,
                                                 ageMaxMere = z$ageMaxMere,
                                                 neFrance = z$neFrance)

df_referent <- data.frame(
  do.call(cbind,df_referent)
)

test_that("Married men are household head while husband is referent for women", {
  
  expect_equal(
    df_referent$referent[df_referent$Id==1],
    df_referent$Id[df_referent$Id==1]
  )
  
  expect_equal(
    df_referent$referent[df_referent$Id==2],
    df_referent$conjoint[df_referent$Id==2]
  )

})


# -----------------------------------
# CASE 3: SPOUSE DEATH OR DIVORCE
# -----------------------------------


z <- data.frame(
  Id = c(1,2),
  annee = rep(2008,2),
  conjoint = c(3,4),
  sexe = c(1,2), 
  matri = c(3,4), #One divorce, one death 
  referent = c(31,41),
  referent2 = NA_real_,
  findet = rep(20,2),
  age = rep(0,2),
  salaire = rep(0,2),
  pere = rep(0,2),
  ageMaxPere = rep(0,2),
  mere = rep(0,2),
  ageMaxMere = rep(0,2),
  neFrance = rep(0,2)
)


df_referent <- capitulation::assign_referent_cpp(id = z$Id,
                                                 annee = z$annee,
                                                 conjoint = z$conjoint,
                                                 sexe = z$sexe,
                                                 matri = z$matri,
                                                 referent = z$referent,
                                                 referent2 = z$referent2,
                                                 findet = z$findet,
                                                 age = z$age,
                                                 salaire = z$salaire,
                                                 pere = z$pere,
                                                 ageMaxPere = z$ageMaxPere,
                                                 mere = z$mere,
                                                 ageMaxMere = z$ageMaxMere,
                                                 neFrance = z$neFrance)

df_referent <- data.frame(
  do.call(cbind,df_referent)
)


test_that("If spouse dies or divorce, individual becomes his/her own head", {

  expect_equal(
    df_referent$referent,
    df_referent$Id
  )
  
})


# =======================
#  MATRI==1
# =======================

# WE GO FROM THE MORE GENERAL TO THE LEAST GENERAL CONDITION:
#  1: Left parents house (has ended school): his own head
#  2: In parents house but earns income: his own head
#  3: Father still living: father is head
#  4: Father is dead but mother lives: mother is head
#  5: Father is dead and mother also: own head
#  6: Father is unknow but mother is known (living): mother is head
#  7: Father is unknow but mother is known (dead): own head
#  8: Father and mother unknown: own head


z <- data.frame(
  Id = 1:8,
  annee = rep(2008,8),
  conjoint = rep(NA,8),
  sexe = rep(NA,8), 
  matri = rep(1,8), 
  referent = rep(NA,8),
  referent2 = NA_real_,
  findet = rep(20,8), #Always ends school at 20
  age = c(21,rep(19,7)),
  salaire = c(0,2,rep(0,6)),
  pere = c(seq(31,length.out = 5),0,0,0),
  ageMaxPere = c(80,80,80,10,10, 10, 10, 10),
  mere = c(seq(51,length.out = 7),0),
  ageMaxMere = c(80,80,80,80,10,80,10,80),
  neFrance = rep(1,8)
)



df_referent <- capitulation::assign_referent_cpp(id = z$Id,
                                                 annee = z$annee,
                                                 conjoint = z$conjoint,
                                                 sexe = z$sexe,
                                                 matri = z$matri,
                                                 referent = z$referent,
                                                 referent2 = z$referent2,
                                                 findet = z$findet,
                                                 age = z$age,
                                                 salaire = z$salaire,
                                                 pere = z$pere,
                                                 ageMaxPere = z$ageMaxPere,
                                                 mere = z$mere,
                                                 ageMaxMere = z$ageMaxMere,
                                                 neFrance = z$neFrance)

df_referent <- data.frame(
  do.call(cbind,df_referent)
)

test_that("Young people assigned to parents when they still live", {
  
  expect_equal(
    df_referent$referent[df_referent$Id %in% c(3,4,6)],
    c(33,54,56) #(Father,Mother,Mother)
  )
  
})

test_that("Own head when parents no longer live, are unkown or when individual started to work", {
  
  expect_equal(
    df_referent$referent[!(df_referent$Id %in% c(3,4,6))],
    df_referent$Id[!(df_referent$Id %in% c(3,4,6))]
    )
  
})

