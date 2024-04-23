library(capitulation)


df <- REtage::data_inheritance()

df <- REtage::prepare_estimation(df)


df[,tr_agfinetu := as.numeric(as.character(tr_agfinetu))]
df[get('tr_agfinetu')<14, tr_agfinetu := 14]
df[get('tr_agfinetu')>30, tr_agfinetu := 30]
df[, tr_agfinetu := as.factor(tr_agfinetu)]
thresholds <- c(3,8,15,30,60,100,150,200,250)*1000

data_estimation <- df[revenu > 0]

inheritance_model <- REtage::ordered_model_threshold(data_estimation, formula = "MTHER ~ I(log(revenu)) + SEXE + tr_age + tr_agfinetu",
                                                     link = c("probit"), thresholds = log(thresholds),
                                                     delta = 0,
                                                     constantMEAN = TRUE, constantSD = TRUE)



household_table <- capitulation::prepare_data_inheritance(path_data = "./inst/dataINSEE",
                                            format = c("data.table","list"),
                                            inheritance_model = inheritance_model,
                                            call.Rcpp = TRUE,
                                            start_year = 2009,
                                            estimation_year = 2009,
                                            final_year = 2070,
                                            id_var = "Id",
                                            income_var = "salaire",
                                            age_var = "age",
                                            ageliq_var = "ageliq",
                                            matrimonial_var = "matri",
                                            wealthvar_survey = "PATFISOM",
                                            findet_var = "findet",
                                            weight_var = NULL,
                                            deflate = TRUE,
                                            quantile = 0.9,
                                            groupvar = "top")



household_table2 <- merge(household_table, tables$description[,.SD,.SDcols = c('Id','findet','sexe')],
                          by = 'Id')
household_table2[,'AGFINETU' := get('findet')]
household_table2[get('AGFINETU')>35, c('AGFINETU') := 35]
household_table2[get('AGFINETU')<10, c('AGFINETU') := 9]

household_table2[, `:=` ('tr_age' = factor(floor(get('age')/5)),
                         'tr_agfinetu' = get('AGFINETU'))]
household_table2[get('tr_agfinetu')<14, tr_agfinetu := 14]
household_table2[get('tr_agfinetu')>30, tr_agfinetu := 30]
household_table2[, 'tr_agfinetu' := as.factor(get('tr_agfinetu'))]

household_table2[,'SEXE' := as.factor(get('sexe'))]

household_table2[,'revenu' := get('y_indiv')]
household_table2 <- household_table2[get("revenu")>0]


household_table2[,'Id' := as.character(get('Id'))]
data.table::setnames(data_event, old = "Id",
                     new = "id_death")
data_prediction <- merge(household_table2, data_event,
                         by.y = c("heir","annee"),
                         by.x = c("Id","annee")
)

data_prediction2 <- data_prediction[,.SD,.SDcols = c("Id","annee","id_death",
                                                     "share",
                                                     "revenu","SEXE","tr_age","tr_agfinetu")]

essai <- predict(inheritance_model, newdata = data_prediction2,
                 type = "latent")

data_prediction2[, 'H' := exp(essai$y_latent_pred)*exp(-log(essai$sigma)^2/2)]
data_prediction2[,c('H') := get('H')*get('share')]
data_prediction2 <- data_prediction2[,.SD,.SDcols = c("Id","annee",
                                                      "id_death",
                                                      "H")]

household_table[,'Id' := as.character(get('Id'))]
data_prediction2[,'Id' := as.character(get('Id'))]

# FIRST INHERITANCE RECEIVED
household_table <- merge(
  household_table,
  data_prediction2[,.SD,.SDcols = c("Id","annee","H")],
  by = c("Id","annee"),
  all.x = TRUE
)


# SECOND INHERITANCE GIVEN
data_prediction2[,'id_death' := as.character(get('id_death'))]
household_table <- merge(
  household_table,
  data_prediction2[,.('H' = sum(get('H'), na.rm = TRUE)),
                       by = c("id_death","annee")],
  by.x = c("Id","annee"),
  by.y = c("id_death","annee"),
  all.x = TRUE,
  suffixes = c("_received","_given")
)


household_table[is.na(get('H_received')), c('H_received') := 0]
household_table[is.na(get('H_given')), c('H_given') := 0]
