setwd("C:\\OnlineSync\\Mega\\R\\work\\web_scraping_real_estate")

# https://www.njuskalo.hr/prodaja-stanova?page=399
library(rvest)
library(XML)

# RSelenium new install
# library(devtools)
# install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
# install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
# install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")


# CONSOLE: java -jar -Dwebdriver.gecko.driver=C:\OnlineSync\Mega\R\work\web_scraping_real_estate\geckodriver.exe C:\OnlineSync\Mega\R\work\web_scraping_real_estate\selenium-server-standalone-3.8.1.jar
library(RSelenium)
#remDr = remoteDriver(remoteServerAddr = "localhost", port = 4444L, browserName = "firefox")
# TODO: check how to start and use Chrome driver, https://chromedriver.storage.googleapis.com/index.html?path=2.37/
# CONSOLE: java -jar -Dwebdriver.chrome.driver=C:\OnlineSync\Mega\R\work\web_scraping_real_estate\chromedriver.exe C:\OnlineSync\Mega\R\work\web_scraping_real_estate\selenium-server-standalone-3.12.0.jar
remDr = remoteDriver(remoteServerAddr = "localhost", port = 4444L, browserName = "chrome")
remDr$open()

remDr$navigate("https://www.njuskalo.hr")
accept_cookies_button = remDr$findElements(using = 'css selector', '.js-CookiePolicy-agree')
if (length(accept_cookies_button) > 0) {
  accept_cookies_button[[1]]$clickElement()
}

all_ads = data.frame()
page_counter = 0 # za prije 1.1.2018. 287
save_counter = 1
no_more_links = F
from_date = as.Date.character("2018-10-22")
to_date = as.Date.character("2018-10-22")
current_page_date = to_date

#base_url = "https://www.njuskalo.hr/prodaja-stanova/zagreb?adsWithImages=1"
base_url = "https://www.njuskalo.hr/prodaja-stanova?adsWithImages=1"

while ((current_page_date >= from_date) & (no_more_links == F)) {

  page_counter = page_counter + 1

  tryCatch(
    {

      print(paste("PAGE:", page_counter))

      go_url = paste(base_url, "&page=", page_counter, sep = "")
      # webpage = read_html(go_url)
      remDr$navigate(go_url)

      # fetch all the ads links and process each page
      # ads_links_html = html_nodes(webpage, '.EntityList--Regular .EntityList-item .entity-body .entity-title a')
      # ads_links_texts = html_text(ads_links_html)
      # ads_links = html_attr(ads_links_html, "href")

      webElems = remDr$findElements(using = 'css selector', '.EntityList--Regular .EntityList-item .entity-body .entity-title a')
      ads_links = unlist(lapply(webElems, function(x) { x$getElementAttribute("href") }))

      if (length(ads_links) > 0) {

        dateElems = remDr$findElements(using = 'css selector', '.EntityList--Regular .EntityList-item .entity-body .entity-pub-date time')
        dates_published = as.Date.character(unlist(lapply(dateElems, function(x) { x$getElementAttribute("datetime") })))

        current_page_date = min(dates_published)

        to_process = which((dates_published >= from_date) & (dates_published <= to_date))

        ads_links2 = ads_links[to_process]

        if (length(ads_links2) > 0) {

          for (i in 1:length(ads_links2)) {
            #      for (i in 1:1) {

            ad_link = ads_links2[[i]]
            # ad_page_html = read_html(paste(base_url, ad_link, sep = ""))

            remDr$navigate(ad_link)

            # cijena
            cijena = tryCatch(
              {
                # cijena = html_text(html_nodes(ad_page_html, '.base-entity-prices .price-items .price-item:nth-child(1) strong'))

                webElems2 = remDr$findElements(using = 'css selector', '.base-entity-prices .price-items .price-item:nth-child(1) strong')
                cijena = unlist(lapply(webElems2, function(x) { x$getElementText() }))

                if (length(cijena) == 0) {
                  cijena = NA
                } else {
                  cijena = gsub("\n", "", cijena)
                  cijena = gsub(" ", "", cijena)
                  cijena = gsub("kn", "", cijena)
                  cijena = gsub("\\.", "", cijena)
                  cijena = as.numeric(cijena)
                }
              },
              error = function(cond) {
                print(paste("page:", pge, cond))
                cijena = NA
              }
            )

            lokacija = NA

            zupanija = NA
            grad_opcina = NA
            naselje = NA
            stambena_povrsina = NA
            tip_stana = NA
            broj_etaza = NA
            broj_soba = NA
            kat = NA
            lift = NA
            teretni_lift = NA
            povrsina_vrta = NA
            povrsina_balkona = NA
            povrsina_terase = NA
            godina_izgradnje = NA
            godina_zadnje_adaptacije = NA
            novogradnja = NA
            broj_parkirnih_mjesta = NA
            energetski_razred = NA

            gradski_plin = NA
            gradski_vodovod = NA
            gradska_kanalizacija = NA
            telefon = NA
            opis = NA
            dodatno = NA

            # razni podaci
            tryCatch(
              {
                # descriptions1 = html_nodes(ad_page_html, '.base-entity-description .wrap-table-summary table tbody tr th')
                # descriptions2 = html_nodes(ad_page_html, '.base-entity-description .wrap-table-summary table tbody tr td')

                descriptions1 = remDr$findElements(using = 'css selector', '.base-entity-description .wrap-table-summary table tbody tr th')
                descriptions2 = remDr$findElements(using = 'css selector', '.base-entity-description .wrap-table-summary table tbody tr td')

                for (j in 1:length(descriptions1)) {
                  # hdr = html_text(descriptions1[[j]])
                  # val = html_text(descriptions2[[j]])

                  hdr = unlist(descriptions1[[j]]$getElementText())
                  val = unlist(descriptions2[[j]]$getElementText())

                  if (hdr == "Lokacija:") {
                    lokacija = as.character(val)
                  } else if (hdr == "Županija:") {
                    zupanija = as.character(val)
                  } else if (hdr == "Grad/Općina:") {
                    grad_opcina = as.character(val)
                  } else if (hdr == "Naselje:") {
                    naselje = val
                  } else if (hdr == "Stambena površina:") {
                    stambena_povrsina = as.numeric(gsub(",", ".", val))
                  } else if (hdr == "Tip stana:") {
                    tip_stana = val
                  } else if (hdr == "Broj etaža:") {
                    broj_etaza = val
                  } else if (hdr == "Broj soba:") {
                    broj_soba = val
                  } else if (hdr == "Kat:") {
                    kat = val
                  } else if (hdr == "Lift:") {
                    lift = val
                  } else if (hdr == "Teretni lift:") {
                    teretni_lift = val
                  } else if (hdr == "Površina vrta:") {
                    povrsina_vrta = as.numeric(gsub(",", ".", val))
                  } else if (hdr == "Površina balkona:") {
                    povrsina_balkona = as.numeric(gsub(",", ".", val))
                  } else if (hdr == "Površina terase:") {
                    povrsina_terase = as.numeric(gsub(",", ".", val))
                  } else if (hdr == "Godina izgradnje:") {
                    godina_izgradnje = as.numeric(gsub("\\.", "", val))
                  } else if (hdr == "Godina zadnje adaptacije:") {
                    godina_zadnje_adaptacije = as.numeric(gsub("\\.", "", val))
                  } else if (hdr == "Novogradnja:") {
                    novogradnja = val
                  } else if (hdr == "Broj parkirnih mjesta:") {
                    broj_parkirnih_mjesta = val
                  } else if (hdr == "Energetski razred:") {
                    energetski_razred = val
                    # } else if (hdr == "Gradski plin:") {
                    #   gradski_plin = val
                    # } else if (hdr == "Gradski vodovod:") {
                    #   gradski_vodovod = val
                    # } else if (hdr == "Gradska kanalizacija:") {
                    #   gradska_kanalizacija = val
                    # } else if (hdr == "Telefon:") {
                    #   telefon = val
                    # } else if (hdr == "Opis:") {
                    #   opis = val
                    # } else if (hdr == "Dodatno:") {
                    #   dodatno = val
                  } else {
                    ###wtf = val
                  }
                }
              },
              error = function(cond) {
                print(paste("page:", page_counter, cond))
              }
            )

            # meta info
            objavljen = NA
            prikazan = NA

            tryCatch(
              {
                # cijena = html_text(html_nodes(ad_page_html, '.base-entity-prices .price-items .price-item:nth-child(1) strong'))

                webElems2 = remDr$findElements(using = 'css selector', '.base-entity-meta-02 .meta-items .meta-item time')
                objavljen = unlist(lapply(webElems2, function(x) { x$getElementAttribute('datetime') }))
                if (length(objavljen) > 0) {
                  objavljen = objavljen[1]
                }

                webElems2 = remDr$findElements(using = 'css selector', '.base-entity-meta-02 .meta-items .meta-item .base-entity-display-count')
                prikazan = unlist(lapply(webElems2, function(x) { x$getElementText() }))
                if (length(prikazan) > 0) {
                  prikazan = prikazan[1]
                }
              },
              error = function(cond) {
                print(paste("page:", page_counter, cond))
                objavljen = NA
                prikazan = NA
              }
            )

            tryCatch(
              {
                tekst_oglasa = remDr$findElements(using = 'css selector', '.base-entity-description .passage-standard')
                if (length(tekst_oglasa) > 0) {
                  opis = unlist(tekst_oglasa[[1]]$getElementText())
                }
              },
              error = function(cond) {
                print(paste("page:", page_counter, cond))
                opis = NA
              }
            )

            ad = list("cijena" = cijena,
                      "lokacija" = lokacija,
                      "zupanija" = zupanija,
                      "grad_opcina" = grad_opcina,
                      "naselje" = naselje,
                      "stambena_povrsina" = stambena_povrsina,
                      "tip_stana" = tip_stana,
                      "broj_etaza" = broj_etaza,
                      "broj_soba" = broj_soba,
                      "kat" = kat,
                      "lift" = lift,
                      "teretni_lift" = teretni_lift,
                      "povrsina_vrta" = povrsina_vrta,
                      "povrsina_balkona" = povrsina_balkona,
                      "povrsina_terase" = povrsina_terase,
                      "godina_izgradnje" = godina_izgradnje,
                      "godina_zadnje_adaptacije" = godina_zadnje_adaptacije,
                      "novogradnja" = novogradnja,
                      "broj_parkirnih_mjesta" = broj_parkirnih_mjesta,
                      "energetski_razred" = energetski_razred,
                      "objavljen" = objavljen,
                      "prikazan" = prikazan,
                      "link" = ad_link,
                      "opis" = opis
            )

            all_ads = rbind(all_ads, ad, stringsAsFactors = F)

            if ((nrow(all_ads) > 0) & ((nrow(all_ads) %% 1000) == 0)) {
              # temp saving, in case of something breaking up later
              saveRDS(all_ads, paste("all_ads_", sprintf("%03d", save_counter), ".RDS", sep = ""))
              save_counter = save_counter + 1
            }

            Sys.sleep(1)

          }

        } else {

          Sys.sleep(1)

        }

      } else {

        no_more_links = T
        print("NO MORE LINKS")

      }

      print(paste("ADS:", nrow(all_ads)))

    },
    error = function(cond) {
      print(paste("page:", page_counter, cond))
    }
  )

}

remDr$close()

saveRDS(all_ads, paste("all_ads_", as.character(from_date), "_", as.character(to_date), ".RDS", sep = ""))
all_ads = data.frame()

print("DONE")

