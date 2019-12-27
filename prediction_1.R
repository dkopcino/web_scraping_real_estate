setwd("C:\\OnlineSync\\Mega\\R\\work\\web_scraping_real_estate")

if(!exists("hellofunction", mode="function")) source("../functions.R", encoding = "UTF-8")
if(!exists("parse_kat", mode="function")) source("parsing.R", encoding = "UTF-8")

#################################################################################
### START
#################################################################################

# using ggplot2
library('ggplot2')

### --> data preparation
library(dplyr)
library(data.table)

### Analysis

all_ads = data.frame()
ads_files = list.files(path = "data1", pattern = "all_ads_([[:digit:]]|[[:punct:]])+.RDS", full.names = T)
ads_read = lapply(ads_files, FUN = function(ad_file) {
  ads_000 = readRDS(ad_file)
  all_ads <<- rbind(all_ads, ads_000, stringsAsFactors = F)
})

## which algorithm/model shall we use
ALGS = c("lm", "gb", "nn")
dmodels = list()

total_model = readRDS("model_total.RDS")
if ("lm" %in% ALGS) {
  dmodels[["lm"]] = readRDS("model_lm_rsq_XYZ.RDS")
}
if ("gb" %in% ALGS) {
  dmodels[["gb"]] = readRDS("model_gb_rsq_XYZ.RDS")
} 
if ("nn" %in% ALGS) {
  dmodels[["nn"]] = readRDS("model_nn_rsq_XYZ.RDS")
}

# ostavljamo samo one koji imaju lokacija, stambena_povrsina
all_ads = all_ads[!is.na(all_ads$lokacija) & !is.na(all_ads$stambena_povrsina), ]


# parsiranje zupanija iz lokacije
all_ads$zupanija = sapply(strsplit(all_ads$lokacija, ", "), function(l3) l3[1])
mlevels = total_model[["factor_levels"]][["zupanija"]] # model levels
# ako level nije u modelu, moramo ga maknuti - nepoznat podatak
toremove = sum(!(all_ads$zupanija %in% mlevels))
if (toremove > 0) print(paste("Removing", toremove, "entries (unrecognized zupanija levels)."))
all_ads = all_ads[all_ads$zupanija %in% mlevels, ]
all_ads$zupanija = factor(all_ads$zupanija, levels = mlevels)
all_ads$zupanija = NULL


# parsiranje grad_opcina iz lokacije
all_ads$grad_opcina = sapply(strsplit(all_ads$lokacija, ", "), function(l3) l3[2])
mlevels = total_model[["factor_levels"]][["grad_opcina"]] # model levels
# ako level nije u modelu, moramo ga maknuti - nepoznat podatak
toremove = sum(!(all_ads$grad_opcina %in% mlevels))
if (toremove > 0) print(paste("Removing", toremove, "entries (unrecognized grad_opcina levels)."))
all_ads = all_ads[all_ads$grad_opcina %in% mlevels, ]
all_ads$grad_opcina = factor(all_ads$grad_opcina, levels = mlevels)
all_ads$grad_opcina = NULL


# parsiranje naselje iz lokacije
all_ads$naselje = sapply(strsplit(all_ads$lokacija, ", "), function(l3) l3[3])
mlevels = total_model[["factor_levels"]][["naselje"]] # model levels
# ako level nije u modelu, moramo ga maknuti - nepoznat podatak
toremove = sum(!(all_ads$naselje %in% mlevels))
if (toremove > 0) print(paste("Removing", toremove, "entries (unrecognized naselje levels)."))
all_ads = all_ads[all_ads$naselje %in% mlevels, ]
all_ads$naselje = factor(all_ads$naselje, levels = mlevels)
all_ads$naselje = NULL


# faktoriziranje lokacije
mlevels = total_model[["factor_levels"]][["lokacija"]] # model levels
# ako level nije u modelu, moramo ga maknuti - nepoznat podatak
toremove = sum(!(all_ads$lokacija %in% mlevels))
if (toremove > 0) print(paste("Removing", toremove, "entries (unrecognized lokacija levels)."))
all_ads = all_ads[all_ads$lokacija %in% mlevels, ]
all_ads$lokacija = factor(all_ads$lokacija, levels = mlevels)
all_ads = zero_one_encode_factor(all_ads, "lokacija")
all_ads$lokacija = NULL


if (any(is.na(all_ads$tip_stana))) all_ads[is.na(all_ads$tip_stana), ]$tip_stana = "nepoznato"
mlevels = total_model[["factor_levels"]][["tip_stana"]] # model levels
# ako level nije u modelu, moramo ga maknuti - nepoznat podatak
toremove = sum(!(all_ads$tip_stana %in% mlevels))
if (toremove > 0) print(paste("Removing", toremove, "entries (unrecognized tip_stana levels)."))
all_ads = all_ads[all_ads$tip_stana %in% mlevels, ]
all_ads$tip_stana = factor(all_ads$tip_stana, levels = mlevels)
all_ads = zero_one_encode_factor(all_ads, "tip_stana")
all_ads$tip_stana = NULL


if (any(is.na(all_ads$broj_etaza))) all_ads[is.na(all_ads$broj_etaza), ]$broj_etaza = "nepoznato"
mlevels = total_model[["factor_levels"]][["broj_etaza"]] # model levels
# ako level nije u modelu, moramo ga maknuti - nepoznat podatak
toremove = sum(!(all_ads$broj_etaza %in% mlevels))
if (toremove > 0) print(paste("Removing", toremove, "entries (unrecognized broj_etaza levels)."))
all_ads = all_ads[all_ads$broj_etaza %in% mlevels, ]
all_ads$broj_etaza = as.numeric(factor(all_ads$broj_etaza, levels = mlevels)) - 1


if (any(is.na(all_ads$broj_soba))) all_ads[is.na(all_ads$broj_soba), ]$broj_soba = "nepoznato"
mlevels = total_model[["factor_levels"]][["broj_soba"]] # model levels
# ako level nije u modelu, moramo ga maknuti - nepoznat podatak
toremove = sum(!(all_ads$broj_soba %in% mlevels))
if (toremove > 0) print(paste("Removing", toremove, "entries (unrecognized broj_soba levels)."))
all_ads = all_ads[all_ads$broj_soba %in% mlevels, ]
all_ads$broj_soba = as.numeric(factor(all_ads$broj_soba, levels = mlevels)) - 1


# ako kat nije definiran, probati ga parsirati iz teksta
all_ads[is.na(all_ads$kat), "kat"] = unlist(apply(all_ads[is.na(all_ads$kat), ], 1, parse_kat))
if (any(is.na(all_ads$kat))) all_ads[is.na(all_ads$kat), ]$kat = "nepoznato"
mlevels = total_model[["factor_levels"]][["kat"]] # model levels
# ako level nije u modelu, moramo ga maknuti - nepoznat podatak
toremove = sum(!(all_ads$kat %in% mlevels))
if (toremove > 0) print(paste("Removing", toremove, "entries (unrecognized kat levels)."))
all_ads = all_ads[all_ads$kat %in% mlevels, ]
all_ads$kat = as.numeric(factor(all_ads$kat, levels = mlevels)) - 1


# ako lift nije definiran, probati ga parsirati iz teksta
all_ads[is.na(all_ads$lift), "lift"] = parse_lift(all_ads[is.na(all_ads$lift), ])
if (any(is.na(all_ads$lift))) all_ads[is.na(all_ads$lift), ]$lift = "nepoznato"
all_ads$lift = as.numeric(factor(all_ads$lift, levels = total_model[["factor_levels"]][["lift"]])) - 1


# teretni lift nije definiran na velikoj većini podataka, a i ne vjerujem da je previše bitan pa ga jednostavno brišemo
#all_ads$teretni_lift = as.numeric(factor(all_ads$teretni_lift, levels = c("Ne", "Da"))) - 1
all_ads$teretni_lift = NULL


# novogradnja se jako čudno koristi i izbacit ćemo je iz podataka
#all_ads$novogradnja = as.numeric(factor(all_ads$novogradnja, levels = c("Ne", "Da"))) - 1
all_ads$novogradnja = NULL


# ako nije definirana godina izgradnje, probati je parsirati iz opisa
all_ads[is.na(all_ads$godina_izgradnje), ]$godina_izgradnje =
  parse_godina_izgradnje(all_ads[is.na(all_ads$godina_izgradnje), ])
if (any(is.na(all_ads$godina_izgradnje))) all_ads[is.na(all_ads$godina_izgradnje), ]$godina_izgradnje = -1


# ako nije definirana godina zadnje adaptacije, postavimo je na godinu izgradnje (ako je i ona definirana)
all_ads[is.na(all_ads$godina_zadnje_adaptacije), ]$godina_zadnje_adaptacije =
  all_ads[is.na(all_ads$godina_zadnje_adaptacije), ]$godina_izgradnje


all_ads$broj_parkirnih_mjesta = as.numeric(factor(all_ads$broj_parkirnih_mjesta,
                                                  levels = total_model[["factor_levels"]][["broj_parkirnih_mjesta"]])) - 1
# umjesto broja parkirnih mjesta koristit ćemo flag ima_parking
all_ads$ima_parking = NA
if (any(!is.na(all_ads$broj_parkirnih_mjesta))) all_ads[!is.na(all_ads$broj_parkirnih_mjesta), ]$ima_parking = "Da"
all_ads[is.na(all_ads$broj_parkirnih_mjesta), ]$ima_parking =
  parse_ima_parking(all_ads[is.na(all_ads$broj_parkirnih_mjesta), ])
# više ne trebamo broj parkirnih mjesta
all_ads$broj_parkirnih_mjesta = NULL
if (any(is.na(all_ads$ima_parking))) all_ads[is.na(all_ads$ima_parking), ]$ima_parking = "nepoznato"
all_ads$ima_parking = as.numeric(factor(all_ads$ima_parking,
                                        levels = total_model[["factor_levels"]][["ima_parking"]])) - 1


# ako nije definiran energetski razred, pokušat ćemo ga parsirati iz opisa
all_ads[is.na(all_ads$energetski_razred), ]$energetski_razred =
  parse_energetski_razred(all_ads[is.na(all_ads$energetski_razred), ])
if (any(is.na(all_ads$energetski_razred))) all_ads[is.na(all_ads$energetski_razred), ]$energetski_razred = "nepoznato"
mlevels = total_model[["factor_levels"]][["energetski_razred"]] # model levels
# ako level nije u modelu, moramo ga maknuti - nepoznat podatak
toremove = sum(!(all_ads[!is.na(all_ads$energetski_razred), ]$energetski_razred %in% mlevels))
if (toremove > 0) print(paste("Removing", toremove, "entries (unrecognized energetski_razred levels)."))
all_ads = all_ads[all_ads$energetski_razred %in% mlevels, ]
all_ads$energetski_razred = as.numeric(factor(all_ads$energetski_razred, levels = mlevels)) - 1


# umjesto površine vrta, stavimo flag da li ima vrt ili ne
all_ads$ima_vrt = NA
# ako je definirana površina vrta, onda ima vrt
all_ads[!is.na(all_ads$povrsina_vrta), ]$ima_vrt = "Da"
# ako nije definirana površina vrta, onda pokušamo parsirati da li ima vrt iz opisa
all_ads[is.na(all_ads$povrsina_vrta), ]$ima_vrt = parse_vrt(all_ads[is.na(all_ads$povrsina_vrta), ])
# više ne trebamo površinu vrta
all_ads$povrsina_vrta = NULL
if (any(is.na(all_ads$ima_vrt))) all_ads[is.na(all_ads$ima_vrt), ]$ima_vrt = "nepoznato"
#all_ads$ima_vrt = as.numeric(factor(all_ads$ima_vrt, levels = factor_levels[["ima_vrt"]])) - 1
all_ads$ima_vrt = NULL


# umjesto površine balkona i terase, stavimo flag da li ima balkon ili ne
all_ads$ima_balkon = NA
# ako je definirana površina balkona ili terase, onda ima balkon
all_ads[!is.na(all_ads$povrsina_balkona) | !is.na(all_ads$povrsina_terase), ]$ima_balkon = "Da"
# ako nije definirana površina balkona niti terase, onda pokušamo parsirati da li ima balkon iz opisa
all_ads[is.na(all_ads$povrsina_balkona) & is.na(all_ads$povrsina_terase), ]$ima_balkon =
  parse_balkon_terasa_loggia(all_ads[is.na(all_ads$povrsina_balkona) & is.na(all_ads$povrsina_terase), ])
# više ne trebamo površinu balkona niti terase
all_ads$povrsina_balkona = NULL
all_ads$povrsina_terase = NULL
if (any(is.na(all_ads$ima_balkon))) all_ads[is.na(all_ads$ima_balkon), ]$ima_balkon = "nepoznato"
all_ads$ima_balkon = as.numeric(factor(all_ads$ima_balkon, levels = total_model[["factor_levels"]][["ima_balkon"]])) - 1


# srediti ostale podatke
all_ads$objavljen = as.POSIXct(all_ads$objavljen, format = "%Y-%m-%dT%H:%M:%S")
#all_ads$prikazan = as.numeric(all_ads$prikazan)
#all_ads$prikazan_po_danu = all_ads$prikazan/as.numeric((max(all_ads$objavljen) + 60*60*24) - all_ads$objavljen)
#all_ads$cijena_po_kvadratu = all_ads$cijena/all_ads$stambena_povrsina
all_ads$objavljen = NULL
all_ads$prikazan = NULL

all_ads$link = NULL
all_ads$opis = NULL


# dodajemo kvadrat stambene površine
# The non-random pattern in the residuals (correlated residuals) indicates that the deterministic portion
# (predictor variables) of the model is not capturing some explanatory information that is ?leaking? into
# the residuals. Possibilities include:
# A missing variable
# A missing higher-order term of a variable in the model to explain the curvature
# A missing interaction between terms already in the model
#all_ads$stambena_povrsina_sq = all_ads$stambena_povrsina^2


rvar = total_model[["rvar"]]
predictors = total_model[["predictors"]]

d = all_ads

num_mins = total_model[["num_mins"]]
num_maxs = total_model[["num_maxs"]]

l1 = lapply(colnames(d), function(cn) {
#  if (is.numeric(d[[cn]])) d[[cn]] <<- scale(d[[cn]])
  if (is.numeric(d[[cn]])) {
    mn = num_mins[[cn]]
    mx = num_maxs[[cn]]
    if (mn != mx) {
      d[[cn]] <<- (d[[cn]]-mn)/(mx-mn)
    } else {
      d[[cn]] <<- 1
    }
  }
})


# add the new chosen predictors (interactions) to d
for (newpredictor in predictors) {
  if (grepl(interact_sep, newpredictor)) {
    print(paste("Adding", newpredictor))
    # ok, this is an interaction factor, check if it is n-n or f-f
    preds = unlist(strsplit(newpredictor, interact_sep))
    if (is.factor(d[[preds[1]]]) && is.factor(d[[preds[2]]])) {
      itr = interaction(d[[preds[1]]], d[[preds[2]]], sep = interact_sep)
      d[[newpredictor]] = itr
      levels(d[[newpredictor]]) = levels(itr)
    } else if (is.numeric(d[[preds[1]]]) && is.numeric(d[[preds[2]]])) {
      d[[newpredictor]] = d[[preds[1]]] * d[[preds[2]]]
    } else {
      print(paste("Unrecognized predictor", newpredictor))
    }
  }
}

d = d[, colnames(d) %in% c(rvar, predictors)]

library(caret)
library(xgboost)

# ako d nema rvar, dodati dummy zbog model.matrix
if (is.null(d[[rvar]])) {
  d[[rvar]] = rep(0, nrow(d))
}

dpredicteds = dpredicteds_lwr = dpredicteds_upr = list()

if ("lm" %in% ALGS) {

  lm_dpredicted_ci = predict(dmodels[["lm"]][["model"]], newdata = d, type = 'response', interval = "confidence")
  lm_dpredicted = lm_dpredicted_ci[, "fit"]
  lm_dpredicted_lwr = lm_dpredicted_ci[, "lwr"]
  lm_dpredicted_upr = lm_dpredicted_ci[, "upr"]

  dpredicteds[["lm"]] = lm_dpredicted
  dpredicteds_lwr[["lm"]] = lm_dpredicted_lwr
  dpredicteds_upr[["lm"]] = lm_dpredicted_upr
  
}

if ("gb" %in% ALGS) {
  
  gb_dpredicted = predict(dmodels[["gb"]][["model"]], newdata = as.matrix(d[, !names(d) %in% c(rvar)]))
  last_error = dmodels[["gb"]][["last_error"]]
  last_error_upr = last_error# + 1.96 * sqrt(last_error * (1 - last_error) / nrow(d))
  gb_dpredicted_lwr = gb_dpredicted - last_error_upr
  gb_dpredicted_upr = gb_dpredicted + last_error_upr
  
  dpredicteds[["gb"]] = gb_dpredicted
  dpredicteds_lwr[["gb"]] = gb_dpredicted_lwr
  dpredicteds_upr[["gb"]] = gb_dpredicted_upr
  
}

if ("nn" %in% ALGS) {

  library(tensorflow)
  library(keras)

  dmodel = unserialize_model(dmodels[["nn"]][["model"]], compile = T)

  nn_dpredicted = as.numeric(predict_on_batch(dmodel, as.matrix(d[, !names(d) %in% c(rvar)])))

  last_error = dmodels[["nn"]][["last_error"]]
  last_error_upr = last_error #+ 1.96 * sqrt(last_error * (1 - last_error) / nrow(d))
  nn_dpredicted_lwr = nn_dpredicted - last_error_upr
  nn_dpredicted_upr = nn_dpredicted + last_error_upr

  dpredicteds[["nn"]] = nn_dpredicted
  dpredicteds_lwr[["nn"]] = nn_dpredicted_lwr
  dpredicteds_upr[["nn"]] = nn_dpredicted_upr

}

dpredicted = apply(matrix(unlist(dpredicteds), byrow = T, nrow = length(dpredicteds)), 2, mean)
dpredicted_lwr = apply(matrix(unlist(dpredicteds_lwr), byrow = T, nrow = length(dpredicteds_lwr)), 2, mean)
dpredicted_upr = apply(matrix(unlist(dpredicteds_upr), byrow = T, nrow = length(dpredicteds_upr)), 2, mean)

# "odskaliramo" predikcije i eksponenciramo (jer smo prilikom modeliranja radili log od rvar)
dpredicted = exp(dpredicted * (num_maxs[[rvar]]-num_mins[[rvar]]) + num_mins[[rvar]])
dpredicted_lwr = exp(dpredicted_lwr * (num_maxs[[rvar]]-num_mins[[rvar]]) + num_mins[[rvar]])
dpredicted_upr = exp(dpredicted_upr * (num_maxs[[rvar]]-num_mins[[rvar]]) + num_mins[[rvar]])


# usporedba, ako je rvar definiran u novim podacima
if (!is.null(all_ads[[rvar]])) {
  library(ggplot2)
  # regression plot
  print(ggplot() +
          geom_point(aes(x = dpredicted, y = all_ads[, rvar]), color = "blue") +
          geom_abline(intercept = 0, slope = 1, color = "red") +
          geom_line(aes(x = dpredicted, y = dpredicted_lwr), color = "red", linetype = "dotted") +
          geom_line(aes(x = dpredicted, y = dpredicted_upr), color = "red", linetype = "dotted") +
          ggtitle(paste(rvar, "regression")) +
          ylab('True values') +
          xlab('Predicted values'))
  # residuals plot
  print(ggplot() +
          geom_point(aes(x = dpredicted, y = dpredicted - all_ads[, rvar]), color = "blue") +
          geom_abline(intercept = 0, slope = 0, color = "red") +
          ggtitle(paste(rvar, "residuals")) +
          ylab('Residuals') +
          xlab('Predicted values'))

  resids = dpredicted - all_ads[, rvar]
  qs = quantile(resids, c(0.01, 0.99))
  resids_q = resids[which((resids > qs[1]) & (resids < qs[2]))]
  print(paste("RMSE: ", sqrt((sum(resids_q^2))/length(resids_q))))
}


# spremimo predikcije
predictions = list()
predictions[["dpredicted"]] = dpredicted
predictions[["dpredicted_lwr"]] = dpredicted_lwr
predictions[["dpredicted_upr"]] = dpredicted_upr

saveRDS(predictions, "predictions.RDS")

