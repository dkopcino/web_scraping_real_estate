#setwd("C:\\OnlineSync\\Mega\\R\\work\\web_scraping_real_estate\\cijenastanaapp")
# rsconnect::setAccountInfo(name='itmarket', token='C1A599E9B054414D2AF7752A881E154E', secret='aqXbJmCkZsdOiqoWmaF3Gi6UIOUXQdjYIGGcNxu3')
# library(rsconnect)
# rsconnect::deployApp('.')

# -- READ THE MODEL AND SET THE VARIABLES

#ALGS = c("lm", "gb", "nn")
ALGS = c("gb", "nn")
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


# -- NOW THE SHINY PART

library(shiny)

ui <- shinyUI(
  fluidPage(
    fluidRow(
      column(
        h1("Izračun cijene stana"),
        width = 12
        )
      ),
    fluidRow(
      column(
        selectInput("lokacija", "Lokacija", total_model[["factor_levels"]][["lokacija"]],
                    selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        # selectInput("zupanija", "Županija", total_model[["factor_levels"]][["zupanija"]],
        #             selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        # selectInput("grad_opcina", "Grad-općina", total_model[["factor_levels"]][["grad_opcina"]],
        #             selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        # selectInput("naselje", "Naselje", total_model[["factor_levels"]][["naselje"]],
        #             selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        selectInput("tip_stana", "Tip stana", total_model[["factor_levels"]][["tip_stana"]][-1],
                    selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        selectInput("broj_etaza", "Broj etaža", total_model[["factor_levels"]][["broj_etaza"]][-1],
                    selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        selectInput("broj_soba", "Broj soba", total_model[["factor_levels"]][["broj_soba"]][-1],
                    selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        numericInput("stambena_povrsina", "Stambena površina (m2)", value = total_model[["num_mins"]][["stambena_povrsina"]], 
                     min = total_model[["num_mins"]][["stambena_povrsina"]],
                     max = total_model[["num_maxs"]][["stambena_povrsina"]]),
        selectInput("kat", "Kat", total_model[["factor_levels"]][["kat"]][-1],
                    selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        width = 3
      ),
      column(
        selectInput("lift", "Lift", total_model[["factor_levels"]][["lift"]][-1],
                    selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        numericInput("godina_izgradnje", "Godina izgradnje", value = 2018, min = 1600, max = 2018),
        numericInput("godina_zadnje_adaptacije", "Godina zadnje adaptacije", value = 2018, min = 1600, max = 2018),
        selectInput("ima_parking", "Parking", total_model[["factor_levels"]][["ima_parking"]][-1],
                    selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        selectInput("energetski_razred", "Energetski razred", total_model[["factor_levels"]][["energetski_razred"]][-1],
                    selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        # selectInput("ima_vrt", "Vrt", total_model[["factor_levels"]][["ima_vrt"]],
        #             selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        selectInput("ima_balkon", "Balkon", total_model[["factor_levels"]][["ima_balkon"]][-1],
                    selected = NULL, multiple = F, selectize = T, width = NULL, size = NULL),
        width = 3
      )
    ),
    fluidRow(
      column(
        actionButton(inputId = "izracunaj_cijenu", label = "Izračunaj cijenu"),
        tags$h3(textOutput(outputId = "cijena")),
        tags$h4(textOutput(outputId = "debugger")),
        width = 12
      )
    )
  )
)

server <- function(input, output){
  re <- eventReactive(
    input$izracunaj_cijenu, 
    { 
      the_add = data.frame(
        "lokacija" = factor(input$lokacija, levels = total_model[["factor_levels"]][["lokacija"]]),
        "zupanija" = factor((strsplit(input$lokacija, ", "))[1], levels = total_model[["factor_levels"]][["zupanija"]]),
        "grad_opcina" = factor((strsplit(input$lokacija, ", "))[2], levels = total_model[["factor_levels"]][["grad_opcina"]]),
        "naselje" = factor((strsplit(input$lokacija, ", "))[3], levels = total_model[["factor_levels"]][["naselje"]]),
        "tip_stana" = factor(input$tip_stana, levels = total_model[["factor_levels"]][["tip_stana"]]),
        "broj_etaza" = as.numeric(factor(input$broj_etaza, levels = total_model[["factor_levels"]][["broj_etaza"]])) - 1,
        "broj_soba" = as.numeric(factor(input$broj_soba, levels = total_model[["factor_levels"]][["broj_soba"]])) - 1,
        "kat" = as.numeric(factor(input$kat, levels = total_model[["factor_levels"]][["kat"]])) - 1,
        "lift" = as.numeric(factor(input$lift, levels = total_model[["factor_levels"]][["lift"]])) - 1,
        "stambena_povrsina" = input$stambena_povrsina,
        "godina_izgradnje" = input$godina_izgradnje,
        "godina_zadnje_adaptacije" = input$godina_zadnje_adaptacije,
        "ima_parking" = as.numeric(factor(input$ima_parking, levels = total_model[["factor_levels"]][["ima_parking"]])) - 1,
        "energetski_razred" = as.numeric(factor(input$energetski_razred, levels = total_model[["factor_levels"]][["energetski_razred"]])) - 1,
#        "ima_vrt" = as.numeric(factor(input$ima_vrt, levels = total_model[["factor_levels"]][["ima_vrt"]])) - 1,
        "ima_balkon" = as.numeric(factor(input$ima_balkon, levels = total_model[["factor_levels"]][["ima_balkon"]])) - 1
      )
      
      # return(paste(
      #   the_add[1, ]$lokacija,
      #   the_add[1, ]$zupanija,
      #   the_add[1, ]$grad_opcina,
      #   the_add[1, ]$naselje,
      #   the_add[1, ]$tip_stana,
      #   the_add[1, ]$broj_etaza,
      #   the_add[1, ]$broj_soba,
      #   the_add[1, ]$kat,
      #   the_add[1, ]$lift,
      #   the_add[1, ]$godina_izgradnje,
      #   the_add[1, ]$godina_zadnje_adaptacije,
      #   the_add[1, ]$stambena_povrsina,
      #   the_add[1, ]$ima_parking,
      #   the_add[1, ]$energetski_razred,
      #   the_add[1, ]$ima_vrt,
      #   the_add[1, ]$ima_balkon
      # ))

      # post procesiranje
      #the_add$stambena_povrsina_sq = the_add$stambena_povrsina^2

      ### iz functions.R
      
      zero_one_encode_factor = function(df, fv) {
        
        lvs = levs = levels(df[[fv]])
        lvs = gsub("č|ć", "c", lvs)
        lvs = gsub("Č|Ć", "C", lvs)
        lvs = gsub("đ", "dj", lvs)
        lvs = gsub("Đ", "Dj", lvs)
        lvs = gsub("š", "s", lvs)
        lvs = gsub("Š", "S", lvs)
        lvs = gsub("ž", "z", lvs)
        lvs = gsub("Ž", "Z", lvs)
        lvs = gsub("[^[:alnum:]]", "_", lvs)
        
        duplvs = anyDuplicated(lvs)
        if (duplvs != 0) {
          print(paste("zero_one_encode_factor: Duplicated level:", lvs[duplvs]))
          return(df)
        }
        
        lvs = gsub("^", paste(fv, "_", sep = ""), lvs)
        
        if (length(lvs) == 0) {
          print("zero_one_encode_factor: No levels")
          return(df)
        }
        
        newdf = df
        
        ret = lapply(1:length(lvs), function(i) {
          newdf[[lvs[i]]] <<- ifelse(df[[fv]] == levs[i], 1, 0)
        })
        
        newdf
        
      }
      
      interact_sep = "__CO0Gd3KgPU__"

      ###
      
      
      the_add = zero_one_encode_factor(the_add, "lokacija")
      the_add = zero_one_encode_factor(the_add, "tip_stana")

      rvar = total_model[["rvar"]]
      predictors = total_model[["predictors"]]
      
      d = the_add
      
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
      
      paste("Cijena: ", round(dpredicted), " kn (", round(dpredicted_lwr), "kn - ", round(dpredicted_upr), "kn)", sep = "")

    }
  )
  output$cijena <- renderText({
    re()
  })
}

shinyApp(ui = ui, server = server)
