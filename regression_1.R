setwd("C:\\OnlineSync\\Mega\\R\\work\\web_scraping_real_estate")

if(!exists("hellofunction", mode="function")) source("../functions.R", encoding = "UTF-8")
if(!exists("parse_kat", mode="function")) source("parsing.R", encoding = "UTF-8")


#################################################################################
### START
#################################################################################

## which algorithm/model shall we use
ALGS = c("lm", "gb", "nn")


# using ggplot2
library('ggplot2')

### --> data preparation
library(dplyr)
library(data.table)

# podaci koje bih ja tražio:
# da li ima odvojeni wc
# da li je zadnji kat
# da li ima balkon/terasu
# kakvo je grijanje: toplana, plin, struja, drvo
# da li ima dodatno (podrumsko ili sl.) spremište
# na koje sve strane stan gleda (sjever, istok, jug, zapad, sjever-istok, sjever-zapad, sjever-jug, jug-istok, jug-zapad, istok-zapad, sjever-istok-jug, sjever-istok-zapad, sjever-zapad-jug, istok-jug-zapad, sjever-istok-jug-zapad)
# da li je zona tramvaja
# da li je zona busa
# da li ima osiguran parking
# da li ima garažu
# koliko je blizu osnovna škola
# koliko je blizu trgovina
# koliko je blizu dom zdravlja
# da li je u dobrom stanju ili za adaptaciju
# koliko je puta oglas pogledan u odnosu na vrijeme od kad je objavljen


### Analysis

all_ads = data.frame()
#ads_files = list.files(path = ".", pattern = "all_ads_2018-([[:digit:]]|[[:punct:]])+.RDS", full.names = T)
ads_files = list.files(path = "data0", pattern = "all_ads_([[:digit:]]|[[:punct:]])+.RDS", full.names = T)
ads_read = lapply(ads_files, FUN = function(ad_file) {
  ads_000 = readRDS(ad_file)
  all_ads <<- rbind(all_ads, ads_000, stringsAsFactors = F)
})


# sortiranje po broju elemenata iz kategorije, ovdje za "kat"
#summary(factor(all_ads$kat))[order(summary(factor(all_ads$kat)))]


# moramo spremiti levele za buduće predikcije
factor_levels = list()

#sum(rowSums(is.na(all_ads)) > 0)

# ostavljamo samo one koji imaju lokacija, cijena, stambena_povrsina
all_ads = all_ads[!is.na(all_ads$lokacija) & !is.na(all_ads$cijena) & !is.na(all_ads$stambena_povrsina), ]


# parsiranje zupanija iz lokacije
all_ads$zupanija = sapply(strsplit(all_ads$lokacija, ", "), function(l3) l3[1])
# ostavljamo samo Grad Zagreb
all_ads = all_ads[all_ads$zupanija == "Grad Zagreb", ]
all_ads$zupanija = factor(all_ads$zupanija)
factor_levels[["zupanija"]] = levels(all_ads$zupanija)
all_ads$zupanija = NULL


# parsiranje grad_opcina iz lokacije
all_ads$grad_opcina = sapply(strsplit(all_ads$lokacija, ", "), function(l3) l3[2])
all_ads$grad_opcina = factor(all_ads$grad_opcina)
factor_levels[["grad_opcina"]] = levels(all_ads$grad_opcina)
all_ads$grad_opcina = NULL


# parsiranje naselje iz lokacije
all_ads$naselje = sapply(strsplit(all_ads$lokacija, ", "), function(l3) l3[3])
all_ads$naselje = factor(all_ads$naselje)
factor_levels[["naselje"]] = levels(all_ads$naselje)
all_ads$naselje = NULL


# faktoriziranje lokacije
all_ads$lokacija = factor(all_ads$lokacija)
factor_levels[["lokacija"]] = levels(all_ads$lokacija)
all_ads = zero_one_encode_factor(all_ads, "lokacija")
all_ads$lokacija = NULL


factor_levels[["tip_stana"]] = enc2utf8(c("nepoznato", "u stambenoj zgradi", "u kući"))
if (any(is.na(all_ads$tip_stana))) all_ads[is.na(all_ads$tip_stana), ]$tip_stana = "nepoznato"
all_ads$tip_stana = factor(all_ads$tip_stana, levels = factor_levels[["tip_stana"]])
all_ads = zero_one_encode_factor(all_ads, "tip_stana")
all_ads$tip_stana = NULL


factor_levels[["broj_etaza"]] = enc2utf8(c("nepoznato", "jednoetažni", "dvoetažni", "višeetažni"))
if (any(is.na(all_ads$broj_etaza))) all_ads[is.na(all_ads$broj_etaza), ]$broj_etaza = "nepoznato"
all_ads$broj_etaza = as.numeric(factor(all_ads$broj_etaza, levels = factor_levels[["broj_etaza"]])) - 1


factor_levels[["broj_soba"]] = c("nepoznato", "Garsonijera", "1-1.5 sobni", "2-2.5 sobni", "3-3.5 sobni", "4+")
if (any(is.na(all_ads$broj_soba))) all_ads[is.na(all_ads$broj_soba), ]$broj_soba = "nepoznato"
all_ads$broj_soba = as.numeric(factor(all_ads$broj_soba, levels = factor_levels[["broj_soba"]])) - 1


# ako kat nije definiran, probati ga parsirati iz teksta
all_ads[is.na(all_ads$kat), "kat"] = unlist(apply(all_ads[is.na(all_ads$kat), ], 1, parse_kat))
factor_levels[["kat"]] = c("nepoznato", "Suteren", "Prizemlje", "Visoko prizemlje", "1", "2",
                           "3", "4", "5", "6", "7",
                           "8", "9", "10", "11", "12",
                           "13", "14", "15", "16", "17",
                           "18", "19", "20", "21", "22",
                           "23", "24", "25", "Potkrovlje", "Visoko potkrovlje",
                           "Penthouse")
if (any(is.na(all_ads$kat))) all_ads[is.na(all_ads$kat), ]$kat = "nepoznato"
all_ads$kat = as.numeric(factor(all_ads$kat, levels = factor_levels[["kat"]])) - 1


# ako lift nije definiran, probati ga parsirati iz teksta
all_ads[is.na(all_ads$lift), "lift"] = parse_lift(all_ads[is.na(all_ads$lift), ])
#print(paste("NA lift:", sum(is.na(all_ads$lift))))
factor_levels[["lift"]] = c("nepoznato", "Ne", "Da")
if (any(is.na(all_ads$lift))) all_ads[is.na(all_ads$lift), ]$lift = "nepoznato"
all_ads$lift = as.numeric(factor(all_ads$lift, levels = factor_levels[["lift"]])) - 1


# teretni lift nije definiran na velikoj većini podataka, a i ne vjerujem da je previše bitan pa ga jednostavno brišemo
#all_ads$teretni_lift = as.numeric(factor(all_ads$teretni_lift, levels = c("Ne", "Da"))) - 1
all_ads$teretni_lift = NULL


# novogradnja se jako čudno koristi i izbacit ćemo je iz podataka
#all_ads$novogradnja = as.numeric(factor(all_ads$novogradnja, levels = c("Ne", "Da"))) - 1
all_ads$novogradnja = NULL


# ako nije definirana godina izgradnje, probati je parsirati iz opisa
all_ads[is.na(all_ads$godina_izgradnje), ]$godina_izgradnje =
  parse_godina_izgradnje(all_ads[is.na(all_ads$godina_izgradnje), ])
print(paste("NA godina_izgradnje:", sum(is.na(all_ads$godina_izgradnje))))
if (any(is.na(all_ads$godina_izgradnje))) all_ads[is.na(all_ads$godina_izgradnje), ]$godina_izgradnje = -1


# ako nije definirana godina zadnje adaptacije, postavimo je na godinu izgradnje (ako je i ona definirana)
if (any(is.na(all_ads$godina_zadnje_adaptacije))) 
  all_ads[is.na(all_ads$godina_zadnje_adaptacije), ]$godina_zadnje_adaptacije = all_ads[is.na(all_ads$godina_zadnje_adaptacije), ]$godina_izgradnje


factor_levels[["broj_parkirnih_mjesta"]] = c("1", "2", "3", "4", "5", "6", "7")
all_ads$broj_parkirnih_mjesta = as.numeric(factor(all_ads$broj_parkirnih_mjesta,
                                                  levels = factor_levels[["broj_parkirnih_mjesta"]])) - 1
# umjesto broja parkirnih mjesta koristit ćemo flag ima_parking
all_ads$ima_parking = NA
if (any(!is.na(all_ads$broj_parkirnih_mjesta))) all_ads[!is.na(all_ads$broj_parkirnih_mjesta), ]$ima_parking = "Da"
all_ads[is.na(all_ads$broj_parkirnih_mjesta), ]$ima_parking =
  parse_ima_parking(all_ads[is.na(all_ads$broj_parkirnih_mjesta), ])
print(paste("NA ima_parking:", sum(is.na(all_ads$ima_parking))))
# više ne trebamo broj parkirnih mjesta
all_ads$broj_parkirnih_mjesta = NULL
factor_levels[["ima_parking"]] = c("nepoznato", "Ne", "Da")
if (any(is.na(all_ads$ima_parking))) all_ads[is.na(all_ads$ima_parking), ]$ima_parking = "nepoznato"
all_ads$ima_parking = as.numeric(factor(all_ads$ima_parking, levels = factor_levels[["ima_parking"]])) - 1


# ako nije definiran energetski razred, pokušat ćemo ga parsirati iz opisa
all_ads[is.na(all_ads$energetski_razred), ]$energetski_razred =
  parse_energetski_razred(all_ads[is.na(all_ads$energetski_razred), ])
print(paste("NA energetski_razred:", sum(is.na(all_ads$energetski_razred))))
factor_levels[["energetski_razred"]] = c("nepoznato", "G", "F", "E", "D", "C", "B" , "A+", "A")
if (any(is.na(all_ads$energetski_razred))) all_ads[is.na(all_ads$energetski_razred), ]$energetski_razred = "nepoznato"
all_ads$energetski_razred = as.numeric(factor(all_ads$energetski_razred, levels = factor_levels[["energetski_razred"]])) - 1


# umjesto površine vrta, stavimo flag da li ima vrt ili ne
all_ads$ima_vrt = NA
# ako je definirana površina vrta, onda ima vrt
all_ads[!is.na(all_ads$povrsina_vrta), ]$ima_vrt = "Da"
# ako nije definirana površina vrta, onda pokušamo parsirati da li ima vrt iz opisa
all_ads[is.na(all_ads$povrsina_vrta), ]$ima_vrt = parse_vrt(all_ads[is.na(all_ads$povrsina_vrta), ])
# više ne trebamo površinu vrta
all_ads$povrsina_vrta = NULL
print(paste("NA ima_vrt:", sum(is.na(all_ads$ima_vrt))))
factor_levels[["ima_vrt"]] = c("nepoznato", "Ne", "Da")
if (any(is.na(all_ads$ima_vrt))) all_ads[is.na(all_ads$ima_vrt), ]$ima_vrt = "nepoznato"
all_ads$ima_vrt = as.numeric(factor(all_ads$ima_vrt, levels = factor_levels[["ima_vrt"]])) - 1
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
print(paste("NA ima_balkon:", sum(is.na(all_ads$ima_balkon))))
factor_levels[["ima_balkon"]] = c("nepoznato", "Ne", "Da")
if (any(is.na(all_ads$ima_balkon))) all_ads[is.na(all_ads$ima_balkon), ]$ima_balkon = "nepoznato"
all_ads$ima_balkon = as.numeric(factor(all_ads$ima_balkon, levels = factor_levels[["ima_balkon"]])) - 1


# srediti ostale podatke
all_ads$objavljen = as.POSIXct(all_ads$objavljen, format = "%Y-%m-%dT%H:%M:%S")
#all_ads$prikazan = as.numeric(all_ads$prikazan)
#all_ads$prikazan_po_danu = all_ads$prikazan/as.numeric((max(all_ads$objavljen) + 60*60*24) - all_ads$objavljen)
all_ads$cijena_po_kvadratu = all_ads$cijena/all_ads$stambena_povrsina
all_ads$objavljen = NULL
all_ads$prikazan = NULL
all_ads$link = NULL
all_ads$opis = NULL


# dodajemo kvadrat stambene površine
# The non-random pattern in the residuals (correlated residuals) indicates that the deterministic portion
# (predictor variables) of the model is not capturing some explanatory information that is leaking into
# the residuals. Possibilities include:
# A missing variable
# A missing higher-order term of a variable in the model to explain the curvature
# A missing interaction between terms already in the model
#all_ads$stambena_povrsina_sq = all_ads$stambena_povrsina^2


# brišemo sve oglase s cijena_po_kvadratu < 1000kn (to su oglasi u kojima je oglašena cijena po kvadratu umjesto ukupne cijene)
all_ads = all_ads[all_ads$cijena_po_kvadratu >= 1000, ]
all_ads$cijena_po_kvadratu = NULL


# brišemo sve oglase s godina_izgradnje < 1700
#all_ads = all_ads[all_ads$godina_izgradnje >= 1700, ]

# imamo li kakvih outliera vezano za cijene??
# odbacit ćemo 5% ekstrema (2.5% najmanjih i 2.5% najvećih)
qls = quantile(all_ads$cijena, probs = c(0.025, 0.975))
all_ads = all_ads[(qls[1] <= all_ads$cijena) & (all_ads$cijena <= qls[2]), ]

# zato što su brojevi za cijene veliki i da smanjimo utjecaj outliera, radimo log(cijena) (iako ćemo kasnije to još skalirati)
all_ads$cijena = log(all_ads$cijena)

# # vizualno pregledavanje podataka
# 
# # prosječna cijena ovisno o grad_opcina
# all_ads %>% group_by(grad_opcina) %>% summarise(prosjecna_cijena = mean(cijena)) %>%
#   ggplot(aes(grad_opcina, prosjecna_cijena)) + geom_col()
# 
# # prosječna cijena_po_kvadratu ovisno o zupanija
# all_ads %>% group_by(zupanija) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(zupanija, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # prosječna cijena_po_kvadratu ovisno o grad_opcina
# all_ads %>% group_by(grad_opcina) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(grad_opcina, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # prosječna cijena_po_kvadratu ovisno o naselje
# all_ads %>% group_by(naselje) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(naselje, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # cijena_po_kvadratu ovisno o stambena_povrsina
# ggplot(data = all_ads, aes(stambena_povrsina, cijena_po_kvadratu)) + geom_line()
# 
# # cijena ovisno o stambena_povrsina
# ggplot(data = all_ads, aes(stambena_povrsina, cijena)) + geom_point()
# 
# # distribucija cijene
# ggplot(data = all_ads, aes(cijena)) + geom_histogram()
# 
# # prosječna cijena_po_kvadratu ovisno o tip_stana ("u stambenoj zgradi", "u kući")
# all_ads %>% group_by(tip_stana) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(tip_stana, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # prosječna cijena_po_kvadratu ovisno o broj_etaza ("jednoetažni", "dvoetažni", "višeetažni")
# all_ads %>% group_by(broj_etaza) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(broj_etaza, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # prosječna cijena_po_kvadratu ovisno o broj_soba ("Garsonijera", "1-1.5 sobni", "2-2.5 sobni", "3-3.5 sobni", "4+")
# all_ads %>% group_by(broj_soba) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(broj_soba, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # prosječna cijena_po_kvadratu ovisno o kat ("Suteren", "Prizemlje", "Visoko prizemlje", "1", "2"..."Penthouse")
# all_ads %>% group_by(kat) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(kat, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # prosječna cijena_po_kvadratu ovisno o lift ("Ne", "Da")
# all_ads %>% group_by(lift) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(lift, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # cijena_po_kvadratu ovisno o godina_izgradnje
# ggplot(data = all_ads, aes(godina_izgradnje, cijena_po_kvadratu)) + geom_point()
# 
# # prosječna cijena_po_kvadratu ovisno o energetski_razred ("G", "F"..."A")
# all_ads %>% group_by(energetski_razred) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(energetski_razred, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # prosječna cijena_po_kvadratu ovisno o potrebna_adaptacija ("Ne", "Da")
# all_ads %>% group_by(potrebna_adaptacija) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(potrebna_adaptacija, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # prosječna cijena_po_kvadratu ovisno o ima_parking ("Ne", "Da")
# all_ads %>% group_by(ima_parking) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(ima_parking, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # prosječna cijena_po_kvadratu ovisno o ima_vrt ("Ne", "Da")
# all_ads %>% group_by(ima_vrt) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(ima_vrt, prosjecna_cijena_po_kvadratu)) + geom_col()
# 
# # prosječna cijena_po_kvadratu ovisno o ima_balkon ("Ne", "Da")
# all_ads %>% group_by(ima_balkon) %>% summarise(prosjecna_cijena_po_kvadratu = mean(cijena_po_kvadratu)) %>%
#   ggplot(aes(ima_balkon, prosjecna_cijena_po_kvadratu)) + geom_col()



# Kako prepoznati duplikate i maknuti ih?


# d = all_ads[, colnames(all_ads) %in% c("cijena",
#                                        "lokacija",
# #                                       "grad_opcina",
# #                                       "naselje",
#                                        "stambena_povrsina",
#                                        "stambena_povrsina_sq",
#                                        "tip_stana",
# #                                       "broj_etaza",
#                                        "broj_soba",
#                                        "kat",
# #                                       "lift",
#                                        "godina_izgradnje",
#                                        "energetski_razred",
# #                                       "potrebna_adaptacija",
#                                        "ima_parking",
# #                                       "ima_vrt",
#                                        "ima_balkon"
#                                        )]

#d = all_ads[, !(colnames(all_ads) %in% c("cijena_po_kvadratu"))]
d = all_ads



# trebamo ovo za kasnije predviđanje na novim podacima
num_mins = list()
num_maxs = list()

l1 = lapply(colnames(d), function(cn) {
#  if (is.numeric(d[[cn]])) d[[cn]] <<- scale(d[[cn]])
#  print(cn)
  if (is.numeric(d[[cn]])) {
    mn = min(d[[cn]])
    num_mins[[cn]] <<- mn
    mx = max(d[[cn]])
    num_maxs[[cn]] <<- mx
    if (mn != mx) {
      d[[cn]] <<- (d[[cn]]-mn)/(mx-mn)
    } else {
      d[[cn]] <<- 1
    }
  }
})


# response variable (TRUE, FALSE) name
rvar = "cijena"

# predictor variable names, assumed all by default
old_predictors = dplyr::setdiff(colnames(d), rvar)

# do we want to keep only the most correlated predictors?
only_most_correlated = TRUE
predictors_with_strength = list()
if (only_most_correlated) {
  outcor = correlate(d, rvar)
  # keep only the significant predictors
  outcor = outcor %>% dplyr::filter(significance == "S") %>% dplyr::select(-significance)
  print(outcor)
  # update predictors
  predictors_cleaned = c()
  for (i in 1:nrow(outcor)) {
    the_threshold = 0.5
    if (outcor[i, "corr_indicator"] == "mean.diff") {
      # numeric-factor, anova significant
      the_threshold = 0
    } else if (outcor[i, "corr_indicator"] == "spearman") {
      # numeric-numeric
      the_threshold = 0
    } else if (outcor[i, "corr_indicator"] == "cramersv") {
      # factor-factor
      the_threshold = 0
    } else if (outcor[i, "corr_indicator"] == "lm.estimate") {
      # factor-numeric
      the_threshold = 0
    } else {
      print(paste("Unknown indicator: ", outcor[i, "corr_indicator"]))
      next
    }
    if ((!is.na(outcor[i, "indicator_value"])) & (abs(as.numeric(outcor[i, "indicator_value"])) > the_threshold)) {
      vname = as.character(outcor[i, "V2"])
      predictors_cleaned = append(predictors_cleaned, vname)
      predictors_with_strength[[length(predictors_with_strength)+1]] =
        list(ifelse(is.factor(d[[vname]]), "F", "N"), vname, abs(as.numeric(outcor[i, "indicator_value"])))
    }
  }
  old_predictors = predictors_cleaned
}

# clean d from all but rvar and predictors
d = d %>% dplyr::select(one_of(dplyr::union(old_predictors, rvar)))

factor_predictors = c()
for (pred in old_predictors) {
  if (is.factor(d[[pred]])) {
    factor_predictors = c(factor_predictors, pred)
  }
}
if (length(factor_predictors) > 1) {
  factor_predictors_pairs = combn(factor_predictors, 2)
  for (j in 1:ncol(factor_predictors_pairs)) {
    factor1 = factor_predictors_pairs[1, j]
    factor2 = factor_predictors_pairs[2, j]
    itr = interaction(d[[factor1]], d[[factor2]], sep = interact_sep)
    itr_name = paste(factor1, interact_sep, factor2, sep = "")
#    correlation = correlate_f_f_vars(d[[rvar]], rvar, itr, itr_name)
    correlation = correlate_n_f_vars(d[[rvar]], rvar, itr, itr_name)
    if ((!is.na(correlation["indicator_value"])) & (correlation["significance"] == "S")) {
      indicator = correlation["corr_indicator"]
      indicator_value = as.numeric(correlation["indicator_value"])
      levs = length(levels(itr))
      strength = indicator_value/levs
      print(paste(factor1, ":", factor2, ":", indicator, ":", indicator_value, "levels:", levs, "ratio:", strength))
      # Ovdje treba odlučiti koju vrijednost uzeti u obzir za zadržavanje/odbacivanje interakcije
      #predictors_with_strength[[length(predictors_with_strength)+1]] = list("F", itr_name, correlation["indicator_value"])
      predictors_with_strength[[length(predictors_with_strength)+1]] = list("F", itr_name, strength)
    }
  }
}

numerical_predictors = c()
for (pred in old_predictors) {
  if (is.numeric(d[[pred]])) {
    numerical_predictors = c(numerical_predictors, pred)
  }
}
if (length(numerical_predictors) > 1) {
  numerical_predictors_pairs = combn(numerical_predictors, 2)
  for (j in 1:ncol(numerical_predictors_pairs)) {
    numerical1 = numerical_predictors_pairs[1, j]
    numerical2 = numerical_predictors_pairs[2, j]
    itr = d[[numerical1]] * d[[numerical2]]
    itr_name = paste(numerical1, numerical2, sep = interact_sep)
#    correlation = correlate_f_n_vars(d[[rvar]], rvar, itr, itr_name)
    correlation = correlate_n_n_vars(d[[rvar]], rvar, itr, itr_name)
    if ((!is.na(correlation["indicator_value"])) & (correlation["significance"] == "S")) {
      print(paste(numerical1, ":", numerical2, ":", correlation["corr_indicator"], ":", correlation["indicator_value"]))
      predictors_with_strength[[length(predictors_with_strength)+1]] = list("N", itr_name, correlation["indicator_value"])
    }
  }
}

# now let's choose predictors
pwsmatrix = data.frame(matrix(unlist(predictors_with_strength), ncol = 3, byrow = TRUE), stringsAsFactors = FALSE)
pwsmatrix$X3 = as.numeric(pwsmatrix$X3)
pwsmatrix = pwsmatrix[order(pwsmatrix$X3, decreasing = TRUE),]
pwsmatrix_n = pwsmatrix[pwsmatrix$X1 == "N", c(2, 3)]
pwsmatrix_f = pwsmatrix[pwsmatrix$X1 == "F", c(2, 3)]

keep_n = as.character(pwsmatrix_n[abs(pwsmatrix_n$X3) > 0.03, "X2"])
#keep_n = as.character(pwsmatrix_n$X2) # keep all
keep_f = as.character(pwsmatrix_f[abs(pwsmatrix_f$X3) > 0.03, "X2"])
#keep = dplyr::union(old_predictors, dplyr::union(keep_n, keep_f))
keep = dplyr::union(keep_n, keep_f)

#keep = old_predictors

keep_and_rvar = dplyr::union(keep, rvar)

# add the new chosen predictors (interactions) to d, keeping old predictors for later test data preparation
for (newpredictor in setdiff(keep, old_predictors)) {
  if (grepl(interact_sep, newpredictor)) {
    print(paste("Adding", newpredictor))
    # ok, this is an interaction factor, check if it is n-n or f-f
    preds = unlist(strsplit(newpredictor, interact_sep))
    if (is.factor(d[[preds[1]]]) && is.factor(d[[preds[2]]])) {
      itr = interaction(d[[preds[1]]], d[[preds[2]]], sep = interact_sep)
      d[[newpredictor]] = itr
      levels(d[[newpredictor]]) = levels(itr)
    } else if (is.numeric(d[[preds[1]]]) && is.numeric(d[[preds[2]]])) {
      itr = d[[preds[1]]] * d[[preds[2]]]
#      d[[newpredictor]] = scale(itr)
      d[[newpredictor]] = itr # the numbers are already scaled 0-1 and here we're just multiplying these numbers so they'll stay in this range
    } else {
      print(paste("Unrecognized predictor", newpredictor))
    }
  }
}


predictors = keep

library(caret)
library(xgboost)

# set.seed(11333)

istest = (runif(nrow(d)) < 0.2)

dM = d[, colnames(d) %in% c(rvar, predictors)]

training_fold = dM[istest == FALSE, ]
test_fold = dM[istest == TRUE, ]

dmodels = list()
dpredicteds = dpredicteds_lwr = dpredicteds_upr = list()

if ("lm" %in% ALGS) {

  dmodel = lm(formula = as.formula(paste(rvar, ".", sep = "~")), data = training_fold, model = FALSE)
  dpredicted_ci = predict(dmodel,
                          newdata = test_fold[, !names(test_fold) %in% c(rvar)],
                          type='response',
                          interval = "confidence")
  dpredicted = dpredicted_ci[, "fit"]
  mod_residuals_v = test_fold[, rvar] - dpredicted
  aic = calculate_aic(mod_residuals_v, length(dmodel$coefficients)) # for lm
  dpredicted_lwr = dpredicted_ci[, "lwr"]
  dpredicted_upr = dpredicted_ci[, "upr"]

  dpredicteds[["lm"]] = dpredicted
  dpredicteds_lwr[["lm"]] = dpredicted_lwr
  dpredicteds_upr[["lm"]] = dpredicted_upr
  
  
  ## MODEL INDEPENDENT ->
  
  bt = Box.test(mod_residuals_v, 10, "Ljung-Box")
  bt = ifelse(is.null(bt$p.value) | is.na(bt$p.value), 0, bt$p.value)
  library("nortest")
  st = ad.test(mod_residuals_v)
  #st = shapiro.test(mod_residuals_v)
  st = ifelse(is.null(st$p.value) | is.na(st$p.value), 0, st$p.value)
  acf_critical = 2/sqrt(length(mod_residuals_v))
  acfs = sum(abs((acf(mod_residuals_v, plot = T))$acf) > acf_critical)
  pacfs = sum(abs((pacf(mod_residuals_v, plot = T))$acf) > acf_critical)
  
  print(summary(dmodel))
  
  library(ggplot2)
  # regression plot
  print(ggplot() +
          geom_point(aes(x = dpredicted, y = test_fold[, rvar]), color = "blue") +
          geom_abline(intercept = 0, slope = 1, color = "red") +
          geom_line(aes(x = dpredicted, y = dpredicted_lwr), color = "red", linetype = "dotted") +
          geom_line(aes(x = dpredicted, y = dpredicted_upr), color = "red", linetype = "dotted") +
          ggtitle(paste(rvar, "regression")) +
          ylab('True values') +
          xlab('Predicted values'))
  # residuals plot
  print(ggplot() +
          geom_point(aes(x = dpredicted, y = dpredicted - test_fold[, rvar]), color = "blue") +
          geom_abline(intercept = 0, slope = 0, color = "red") +
          ggtitle(paste(rvar, "residuals")) +
          ylab('Residuals') +
          xlab('Predicted values'))
  
  # r squared
  rsquared = rsq(test_fold[, rvar], dpredicted)
  print(paste("R squared: ", rsquared))
  
  # rounded mean square error
  print(paste("Rounded mean square error: ", rmse(test_fold[, rvar], dpredicted)))
  
  # compute the Hosmer-Lemeshow statistic to see how bad the predictions are
  # we want this statistic to be non-significant! (p-value above e.g. 5%)
  require(ResourceSelection)
  print(hoslem.test(test_fold[, rvar], dpredicted))
  
  dmodels[["lm"]] = list("model" = dmodel)

}

if ("gb" %in% ALGS) {
  
  dmodel = xgboost(data = as.matrix(training_fold[, !names(training_fold) %in% c(rvar)]),
                   label = as.numeric(training_fold[, rvar]),
                   nrounds = 50,
                   params = list(objective = "reg:linear"),
                   verbose = 1#,
  )
  dpredicted = predict(dmodel, newdata = as.matrix(test_fold[, !names(test_fold) %in% c(rvar)]))
  mod_residuals_v = test_fold[, rvar] - dpredicted
  aic = calculate_aic(mod_residuals_v, length(dmodel$coefficients))
  
  last_error = last(dmodel$evaluation_log$train_rmse)
  last_error_upr = last_error# + 1.96 * sqrt(last_error * (1 - last_error) / nrow(training_fold))
  dpredicted_lwr = dpredicted - last_error_upr
  dpredicted_upr = dpredicted + last_error_upr

  dpredicteds[["gb"]] = dpredicted
  dpredicteds_lwr[["gb"]] = dpredicted_lwr
  dpredicteds_upr[["gb"]] = dpredicted_upr
  

  ## MODEL INDEPENDENT ->
  
  bt = Box.test(mod_residuals_v, 10, "Ljung-Box")
  bt = ifelse(is.null(bt$p.value) | is.na(bt$p.value), 0, bt$p.value)
  library("nortest")
  st = ad.test(mod_residuals_v)
  #st = shapiro.test(mod_residuals_v)
  st = ifelse(is.null(st$p.value) | is.na(st$p.value), 0, st$p.value)
  acf_critical = 2/sqrt(length(mod_residuals_v))
  acfs = sum(abs((acf(mod_residuals_v, plot = T))$acf) > acf_critical)
  pacfs = sum(abs((pacf(mod_residuals_v, plot = T))$acf) > acf_critical)
  
  print(summary(dmodel))
  
  library(ggplot2)
  # regression plot
  print(ggplot() +
          geom_point(aes(x = dpredicted, y = test_fold[, rvar]), color = "blue") +
          geom_abline(intercept = 0, slope = 1, color = "red") +
          geom_line(aes(x = dpredicted, y = dpredicted_lwr), color = "red", linetype = "dotted") +
          geom_line(aes(x = dpredicted, y = dpredicted_upr), color = "red", linetype = "dotted") +
          ggtitle(paste(rvar, "regression")) +
          ylab('True values') +
          xlab('Predicted values'))
  # residuals plot
  print(ggplot() +
          geom_point(aes(x = dpredicted, y = dpredicted - test_fold[, rvar]), color = "blue") +
          geom_abline(intercept = 0, slope = 0, color = "red") +
          ggtitle(paste(rvar, "residuals")) +
          ylab('Residuals') +
          xlab('Predicted values'))
  
  # r squared
  rsquared = rsq(test_fold[, rvar], dpredicted)
  print(paste("R squared: ", rsquared))
  
  # rounded mean square error
  print(paste("Rounded mean square error: ", rmse(test_fold[, rvar], dpredicted)))
  
  # compute the Hosmer-Lemeshow statistic to see how bad the predictions are
  # we want this statistic to be non-significant! (p-value above e.g. 5%)
  require(ResourceSelection)
  print(hoslem.test(test_fold[, rvar], dpredicted))
  
  dmodels[["gb"]] = list("model" = dmodel, "last_error" = last_error)
}

if ("nn" %in% ALGS) {

  library(tensorflow)
  library(keras)

  ## build model
  dmodel = keras_model_sequential() %>%
    layer_dense(units = 3, activation = "tanh", input_shape = ncol(training_fold[, !names(training_fold) %in% c(rvar)])) %>%
    layer_dense(units = 3, activation = "tanh") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 1)
  dmodel %>% compile(optimizer = "rmsprop", loss = "mse", metrics = c("mae"))
  history = dmodel %>% fit(as.matrix(training_fold[, !names(training_fold) %in% c(rvar)]),
                           training_fold[, rvar],
                           epochs = 40,
                           batch_size = 300,
                           verbose = 1)
  dpredicted = as.numeric(predict_on_batch(dmodel, as.matrix(test_fold[, !names(test_fold) %in% c(rvar)])))

  mod_residuals_v = test_fold[, rvar] - dpredicted
  aic = calculate_aic(mod_residuals_v, count_params(dmodel)) # for NN

  last_error = tail(history$metrics$mean_absolute_error, n = 1) # for NN
  last_error_upr = last_error #+ 1.96 * sqrt(last_error * (1 - last_error) / nrow(training_fold))
  dpredicted_lwr = dpredicted - last_error_upr
  dpredicted_upr = dpredicted + last_error_upr

  dpredicteds[["nn"]] = dpredicted
  dpredicteds_lwr[["nn"]] = dpredicted_lwr
  dpredicteds_upr[["nn"]] = dpredicted_upr
  
  
  ## MODEL INDEPENDENT ->
  
  bt = Box.test(mod_residuals_v, 10, "Ljung-Box")
  bt = ifelse(is.null(bt$p.value) | is.na(bt$p.value), 0, bt$p.value)
  library("nortest")
  st = ad.test(mod_residuals_v)
  #st = shapiro.test(mod_residuals_v)
  st = ifelse(is.null(st$p.value) | is.na(st$p.value), 0, st$p.value)
  acf_critical = 2/sqrt(length(mod_residuals_v))
  acfs = sum(abs((acf(mod_residuals_v, plot = T))$acf) > acf_critical)
  pacfs = sum(abs((pacf(mod_residuals_v, plot = T))$acf) > acf_critical)
  
  print(summary(dmodel))
  
  library(ggplot2)
  # regression plot
  print(ggplot() +
          geom_point(aes(x = dpredicted, y = test_fold[, rvar]), color = "blue") +
          geom_abline(intercept = 0, slope = 1, color = "red") +
          geom_line(aes(x = dpredicted, y = dpredicted_lwr), color = "red", linetype = "dotted") +
          geom_line(aes(x = dpredicted, y = dpredicted_upr), color = "red", linetype = "dotted") +
          ggtitle(paste(rvar, "regression")) +
          ylab('True values') +
          xlab('Predicted values'))
  # residuals plot
  print(ggplot() +
          geom_point(aes(x = dpredicted, y = dpredicted - test_fold[, rvar]), color = "blue") +
          geom_abline(intercept = 0, slope = 0, color = "red") +
          ggtitle(paste(rvar, "residuals")) +
          ylab('Residuals') +
          xlab('Predicted values'))
  
  # r squared
  rsquared = rsq(test_fold[, rvar], dpredicted)
  print(paste("R squared: ", rsquared))
  
  # rounded mean square error
  print(paste("Rounded mean square error: ", rmse(test_fold[, rvar], dpredicted)))
  
  # compute the Hosmer-Lemeshow statistic to see how bad the predictions are
  # we want this statistic to be non-significant! (p-value above e.g. 5%)
  require(ResourceSelection)
  print(hoslem.test(test_fold[, rvar], dpredicted))
  
  dmodels[["nn"]] = list("model" = dmodel, "last_error" = last_error)
}


## CHECK ENSEMBLE
if (TRUE) {
  
  dpredicted = apply(matrix(unlist(dpredicteds), byrow = T, nrow = length(dpredicteds)), 2, mean)
  dpredicted_lwr = apply(matrix(unlist(dpredicteds_lwr), byrow = T, nrow = length(dpredicteds_lwr)), 2, mean)
  dpredicted_upr = apply(matrix(unlist(dpredicteds_upr), byrow = T, nrow = length(dpredicteds_upr)), 2, mean)

  mod_residuals_v = test_fold[, rvar] - dpredicted

  
  ## MODEL INDEPENDENT ->
  
  bt = Box.test(mod_residuals_v, 10, "Ljung-Box")
  bt = ifelse(is.null(bt$p.value) | is.na(bt$p.value), 0, bt$p.value)
  library("nortest")
  st = ad.test(mod_residuals_v)
  #st = shapiro.test(mod_residuals_v)
  st = ifelse(is.null(st$p.value) | is.na(st$p.value), 0, st$p.value)
  acf_critical = 2/sqrt(length(mod_residuals_v))
  acfs = sum(abs((acf(mod_residuals_v, plot = T))$acf) > acf_critical)
  pacfs = sum(abs((pacf(mod_residuals_v, plot = T))$acf) > acf_critical)
  
  library(ggplot2)
  # regression plot
  print(ggplot() +
          geom_point(aes(x = dpredicted, y = test_fold[, rvar]), color = "blue") +
          geom_abline(intercept = 0, slope = 1, color = "red") +
          geom_line(aes(x = dpredicted, y = dpredicted_lwr), color = "red", linetype = "dotted") +
          geom_line(aes(x = dpredicted, y = dpredicted_upr), color = "red", linetype = "dotted") +
          ggtitle(paste(rvar, "regression")) +
          ylab('True values') +
          xlab('Predicted values'))
  # residuals plot
  print(ggplot() +
          geom_point(aes(x = dpredicted, y = dpredicted - test_fold[, rvar]), color = "blue") +
          geom_abline(intercept = 0, slope = 0, color = "red") +
          ggtitle(paste(rvar, "residuals")) +
          ylab('Residuals') +
          xlab('Predicted values'))
  
  # r squared
  rsquared = rsq(test_fold[, rvar], dpredicted)
  print(paste("R squared: ", rsquared))
  
  # rounded mean square error
  print(paste("Rounded mean square error: ", rmse(test_fold[, rvar], dpredicted)))
  
  # compute the Hosmer-Lemeshow statistic to see how bad the predictions are
  # we want this statistic to be non-significant! (p-value above e.g. 5%)
  require(ResourceSelection)
  print(hoslem.test(test_fold[, rvar], dpredicted))

}


# ako smo zadovoljni, spremimo cijeli kontekst
total_model = list()
total_model[["factor_levels"]] = factor_levels
total_model[["num_mins"]] = num_mins
total_model[["num_maxs"]] = num_maxs
total_model[["rvar"]] = rvar
total_model[["predictors"]] = predictors
saveRDS(total_model, "model_total.RDS")

if ("lm" %in% ALGS) {
  saveRDS(dmodels[["lm"]], "model_lm_rsq_XYZ.RDS")
}

if ("gb" %in% ALGS) {
  saveRDS(dmodels[["gb"]], "model_gb_rsq_XYZ.RDS")
}

if ("nn" %in% ALGS) {
  dmodels[["nn"]][["model"]] = serialize_model(dmodels[["nn"]][["model"]], include_optimizer = T)
  saveRDS(dmodels[["nn"]], "model_nn_rsq_XYZ.RDS")
}

