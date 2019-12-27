# a is a single row and has a column "opis"
parse_kat = function(a) {

  # print(a["opis"])

  suteren = grepl("suteren|razizemlj", a["opis"], ignore.case = T)
  prizemlje = grepl("prizem", a["opis"], ignore.case = T)
  visoko_prizemlje = grepl("visoko[m]?[[:blank:]]*prizemlj", a["opis"], ignore.case = T)
  potkrovlje = grepl("potkrovlj|tavan", a["opis"], ignore.case = T)
  visoko_potkrovlje = grepl("visoko[m]?[[:blank:]]*potkrovlj", a["opis"], ignore.case = T)
  penthouse = grepl("penthouse|mansard", a["opis"], ignore.case = T)

  redni_brojevi = c("prv", "drug", "treć", "četvrt", "pet", "šest", "sedm", "osm", "devet", "deset",
                    "jedanaest", "dvanaest", "trinaest", "četrnaest", "petnaest", "šesnaest", "sedamnaest",
                    "osamnaest", "devetnaest", "dvadeset", "dvadesetprv", "dvadesetdrug", "dvadesettreć",
                    "dvadesetčetvrt", "dvadesetpet")
  rimski_brojevi = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII", "XIII", "XIV",
                     "XV", "XVI", "XVII", "XVIII", "XIX", "XX", "XXI", "XXII", "XXIII", "XXIV", "XXV")
  pretraga_0 = "(((0[\\.]?)|(N[\\.]?)|(nulti)|(nultom)|(nultem)|(nultog))[[:blank:]]+kat)|(((0[\\.]?)|(N[\\.]?)|(nulta)|(nultoj)|(nulte))[[:blank:]]+etaž)|(kat[[:blank:]]+((0\\.)|(0[[:blank:]]*)|(N\\.)|(N[[:blank:]]*)|(nulti)))|(etaža[[:blank:]]+((0\\.)(0[[:blank:]]*)|(N\\.)(N[[:blank:]]*)|(nulta)))"
  grepl_pretraga = unlist(lapply(1:25, function(i) {
    gsub("nult", redni_brojevi[i], gsub("0", i, gsub("N", rimski_brojevi[i], pretraga_0)))
  }))

#  print(grepl_pretraga)

  kat = unlist(lapply(grepl_pretraga, function(pretraga_i) {
    grepl(pretraga_i, a["opis"], ignore.case = T)
  }))

  # print(paste("Suteren:", suteren == T))
  # print(paste("Prizemlje:", prizemlje == T))
  # print(paste("Visoko prizemlje:", visoko_prizemlje == T))
  # print(paste("Potkrovlje:", potkrovlje == T))
  # print(paste("Visoko potkrovlje:", visoko_potkrovlje == T))
  # print(paste("Penthouse:", penthouse == T))

#  print(kat)

#  return(NA)

  if (any(kat)) return(as.character(min(which(kat))))

  if (suteren) return("Suteren")
  if (visoko_prizemlje) return("Visoko prizemlje") # provjeri prije Prizemlje jer ako je Visoko prizemlje, onda su oba T
  if (prizemlje) return("Prizemlje")
  if (visoko_potkrovlje) return("Visoko potkrovlje") # provjeri prije Potkrovlje jer ako je Visoko potkrovlje, onda su oba T
  if (potkrovlje) return("Potkrovlje")
  if (penthouse) return("Penthouse")

  return(NA)

}

# a is a data.frame with a column "opis"
parse_lift = function(a) {

  lift_yes = which(grepl("((ima|s[a]?|[[:digit:]]+)[[:blank:]]+(lift|dizal))|((lift|dizal)[[:space:]]*[[:punct:]]*[[:space:]]*(da))", a$opis, ignore.case = T))
  lift_no = which(grepl("((nema|bez)[[:blank:]]+(lift|dizal))|((lift|dizal)[[:space:]]*[[:punct:]]*[[:space:]]*(ne))", a$opis, ignore.case = T))

  ret = rep(NA, nrow(a))
  ret[lift_yes] = "Da"
  ret[lift_no] = "Ne"

  ret

}

# a is a data.frame with a column "opis"
parse_godina_izgradnje = function(a) {

  library(tidyverse)

  # sagrađena 1927. godine
  # građena 1864.
  # građena 2000. godine
  # izgrađena 1958. godine
  # izgrađena 1930-ih godina
  # je izgrađena 1895
  # izgrađena je 1920-ih godina
  # izgrađene 1983. godine
  # građene 1978. godine
  # izgrađene 1950 -ih godina
  # izgrađene 1960-ih godina
  # izgraženoj 1928. godine
  # izgrađenoj 1975. godine
  # sagrađenoj 2008. godine
  # građenoj 2007. godine
  r1 = "gra(đ|ž)en[[:alpha:]]+[[:space:]]+(je[[:space:]]+)?[[:digit:]]+"

  # u završnoj fazi gradnje
  r2 = "završn[[:alpha:]]+[[:space:]]+faz[[:alpha:]]+[[:space:]]+gradnj"

  # godina izgradnje je 2008.
  r3 = "izgradnj[[:alpha:]]+[[:space:]]+(je[[:space:]]+)[[:digit:]]+"

  # G. izgradnje zgrade: 1953 g.
  r4 = "izgradnj[[:alpha:]]+[[:space:]]+zgrade[[:punct:]][[:space:]]*[[:digit:]]+"

  # zgrada je iz 60-ih godina
  # iz 1958. godine
  r5 = "iz[[:space:]]+[[:digit:]]+"

  # kuća je stara 50-ak godina
  r6 = "stara[[:space:]]+[[:digit:]]+"

  # nova zgrada
  # novija zgrada
  r7 = "nov[[:alpha:]]+[[:space:]]+zgrad"

  # zgrada je nova
  r8 = "zgrada[[:space:]]+[[:alpha:]][[:space:]]+nova"

  gie = unlist(lapply(a$opis, function(o) {

    o = tolower(o)

    gi1 = str_extract(str_extract(o, r1), "[[:digit:]]+")
    gi2 = grepl(r2, o)
    gi3 = str_extract(str_extract(o, r3), "[[:digit:]]+")
    gi4 = str_extract(str_extract(o, r4), "[[:digit:]]+")
    gi5 = str_extract(str_extract(o, r5), "[[:digit:]]+")
    gi6 = str_extract(str_extract(o, r6), "[[:digit:]]+")
    gi7 = grepl(r7, o)
    gi8 = grepl(r8, o)

    gi = NA

    if (!is.na(gi1)) {
#      print(paste(r1, gi1))
      gi = gi1
    }
    if (gi2) {
#      print(paste(r2, gi2))
      gi = year(Sys.Date())
    }
    if (!is.na(gi3)) {
#      print(paste(r3, gi3))
      gi = gi3
    }
    if (!is.na(gi4)) {
#      print(paste(r4, gi4))
      gi = gi4
    }
    if (!is.na(gi5)) {
#      print(paste(r5, gi5))
      if (nchar(gi5) == 2) gi5 = paste("19", gi5, sep = "")
      gi = gi5
    }
    if (!is.na(gi6)) {
#      print(paste(r6, gi6))
      if (nchar(gi6) == 2) gi6 = paste("19", gi6, sep = "")
      gi = gi6
    }
    if (gi7) {
#      print(paste(r7, gi7))
      gi = year(Sys.Date())
    }
    if (gi8) {
#      print(paste(r8, gi8))
      gi = year(Sys.Date())
    }

#    if (is.na(gi)) print(o)

    as.numeric(gi)

  }))

  gie

}

# a is a data.frame with a column "opis"
parse_potrebna_adaptacija = function(a) {

  # za adaptaciju
  # za kompletnu adaptaciju
  # za rekonstrukciju krovišta i adaptaciju
  # za uređenje i adaptaciju
  r1 = "za[[:space:]]+([[:alpha:]]|[[:space:]])*adaptaciju"
  # potrebna je adaptacija
  # potrebna adaptacija
  # potrebna kompletna adaptacija
  # potrebna mu je djelomična adaptacija
  # potrebna i poželjeljna adaptacija
  r2 = "potrebna[[:space:]]+([[:alpha:]]|[[:space:]])*adaptacija"
  # potrebno mu je uređenje
  r3 = "potrebna[[:space:]]+([[:alpha:]]|[[:space:]])*uređenje"
  # potrebno adaptirati
  r4 = "potrebno[[:space:]]+([[:alpha:]]|[[:space:]])*adaptirati"
  # potrebno renovirati
  r5 = "potrebno[[:space:]]+([[:alpha:]]|[[:space:]])*renovirati"
  # potrebno urediti
  r6 = "potrebno[[:space:]]+([[:alpha:]]|[[:space:]])*urediti"
  # neadaptiran
  r7 = "neadaptiran"

  gie = unlist(lapply(a$opis, function(o) {

    o = tolower(o)

    gi = NA

    gi1 = grepl(r1, o)
    gi2 = grepl(r2, o)
    gi3 = grepl(r3, o)
    gi4 = grepl(r4, o)
    gi5 = grepl(r5, o)
    gi6 = grepl(r6, o)
    gi7 = grepl(r7, o)

    if (gi1 | gi2 | gi3 | gi4 | gi5 | gi6 | gi7) gi = "Da"

    gi

  }))

  gie

}

# a is a data.frame with a column "opis"
parse_ima_parking = function(a) {

  # parking mjesto
  # parkiranje
  # parkirati
  # parkirna mjesta
  r1 = "parki(r|n)"

  gie = unlist(lapply(a$opis, function(o) {

    o = tolower(o)

    gi = NA

    gi1 = grepl(r1, o)

    if (gi1) {
      gi = "Da"
#      print(o)
    }

    gi

  }))

  gie

}

# a is a data.frame with a column "opis"
parse_energetski_razred = function(a) {

  library(tidyverse)

  # b energetski certifikat
  # b energetski razred
  r21 = "([[:space:]]|[[:punct:]])+"
  r22 = "((a(\\+)?)|b|c|d|e|f|g)[[:space:]]+energetski[[:space:]]+(razred|certifikat)"

  # energetski certifikat b
  # energetski certifikat: c
  # energetski razred - b
  # energetski razred d.
  # energetski razred je c.
  # energetska klasa: c
  # NE! energetski certifikat je u izradi, energetski certifikat u izradi
  r3 = "energetsk(i|a)[[:space:]]+(razred|certifikat|klasa)([[:space:]]+je)?([[:space:]]|[[:punct:]])+((a(\\+)?)|b|c|d|e|f|g)"

  # energetski certifikat, razred c
  # energetski certifikat razred c
  # energetski certifikat razreda c
  r4 = "energetski[[:space:]]+certifikat([[:space:]]|[[:punct:]])+razred(a)?[[:space:]]+((a(\\+)?)|b|c|d|e|f|g)"

#  r1 = "energ"

  gie = unlist(lapply(a$opis, function(o) {

    o = tolower(o)

    gi = NA

#    gi1 = grepl(r1, o)

    gi2 = word(str_extract(str_extract(o, paste(r21, r22, sep="")), r22))
    gi3 = word(str_extract(o, r3), start = -1)
    gi4 = word(str_extract(o, r4), start = -1)

    if (!is.na(gi2)) {
      gi = gi2
    }

    if (!is.na(gi3)) {
      gi = gi3
    }

    if (!is.na(gi4)) {
      gi = gi4
    }

    if (!is.na(gi)) {
      if (substring(gi, 1, 1) == "\"") gi = substring(gi, 2)

      # sanity check for possible parsing errors
      if (!(gi %in% c("a", "a+", "b", "c", "d", "e", "f", "g"))) {
        # print(paste("ERR:", gi))
        # print(o)
        gi = NA
      }
    }

    toupper(gi)

  }))

  gie

}

# a is a data.frame with a column "opis"
parse_vrt = function(a) {

  r1 = "([[:space:]]|[[:punct:]])+vrt[^i]"

  gie = unlist(lapply(a$opis, function(o) {

    o = tolower(o)

    gi = NA

    gi1 = grepl(r1, o)

    if (gi1) {
      gi = "Da"
      # print(gsub("vrt", "VRT", o))
      # print("-----------------------------------------------------------------------------------------------")
    }

    gi

  }))

  gie

}

# a is a data.frame with a column "opis"
parse_balkon_terasa_loggia = function(a) {

  r1 = "([[:space:]]|[[:punct:]])+balkon"
  r2 = "([[:space:]]|[[:punct:]])+teras"
  r3 = "([[:space:]]|[[:punct:]])+lođ"

  gie = unlist(lapply(a$opis, function(o) {

    o = tolower(o)

    gi = NA

    gi1 = grepl(r1, o)
    gi2 = grepl(r2, o)
    gi3 = grepl(r3, o)

    # if (gi1) {
    #   print(gsub("balkon", "BALKON", o))
    #   print("-----------------------------------------------------------------------------------------------")
    # }
    #
    # if (gi2) {
    #   print(gsub("teras", "TERAS", o))
    #   print("-----------------------------------------------------------------------------------------------")
    # }
    #
    # if (gi3) {
    #   print(gsub("lođ", "LOĐ", o))
    #   print("-----------------------------------------------------------------------------------------------")
    # }
    #
    if (gi1 | gi2 | gi3) gi = "Da"

    gi

  }))

  gie

}

