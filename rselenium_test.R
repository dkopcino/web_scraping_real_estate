#install.packages("RSelenium")
# https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html
# CONSOLE: java -jar -Dwebdriver.gecko.driver=C:\OnlineSync\Mega\R\work\web_scraping\geckodriver.exe C:\OnlineSync\Mega\R\work\web_scraping\selenium-server-standalone-3.8.1.jar
library(XML)
library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4444L, 
                      browserName = "firefox"
)
remDr$open()
remDr$getStatus()
remDr$navigate("http://www.google.com/ncr")
remDr$navigate("http://www.bbc.co.uk")
remDr$getCurrentUrl()
remDr$goBack()
remDr$getCurrentUrl()
remDr$goForward()
remDr$getCurrentUrl()

# search by name
remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = 'name', value = "q")
webElem$getElementAttribute("name")
webElem$getElementAttribute("class")
webElem$getElementAttribute("id")

# search by id
remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = 'id', value = "lst-ib")

# highlight an element
webElem$highlightElement()

# search by class
remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = 'class', "gsfi")
webElem$getElementAttribute("class")
webElem$getElementAttribute("type")

# search using css
webElem <- remDr$findElement(using = 'css', "input[name='q']")
webElem2 <- remDr$findElement(using = 'css', "[name='q']")
webElem$compareElements(webElem2)
webElem <- remDr$findElement(using = 'css', "input#lst-ib")
webElem$getElementAttribute("name")
webElem <- remDr$findElement('css', "[class = 'gsfi lst-d-f']")

# search using xpath
webElem <- remDr$findElement('xpath', "//input[@id = 'lst-ib']")
webElem <- remDr$findElement('xpath', "//input[@class = 'gsfi lst-d-f']")

# sending text to elements
remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = "css", "[name = 'q']")
webElem$sendKeysToElement(list("R Cran"))

# sending key press to elements
remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = "css", "[name = 'q']")
webElem$sendKeysToElement(list("R Cran", key = "enter"))

# sending mouse events to elements
remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = "css", "[name = 'q']")
webElem$sendKeysToElement(list("R Cran", key = "enter"))
webElems <- remDr$findElements(using = 'css selector', "h3.r")
resHeaders <- unlist(lapply(webElems, function(x){x$getElementText()}))
resHeaders
webElem <- webElems[[which(resHeaders == "The Comprehensive R Archive Network")]]
webElem$clickElement()
remDr$getCurrentUrl()
remDr$getTitle()

# injecting javascript synchronously
remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement("css", "img#hplogo")
script <- "return document.getElementById('hplogo').hidden;"
remDr$executeScript(script, args = list())
script <- "document.getElementById('hplogo').hidden = true; return document.getElementById('hplogo').hidden;"
remDr$executeScript(script, args = list())
script <-"arguments[0].hidden = false; return arguments[0].hidden;"
remDr$executeScript(script, args = list(webElem))
script <- "return document.getElementsByName('q');"
test <- remDr$executeScript(script, args = list())
test[[1]]
test[[1]]$highlightElement()

# injecting javascript asynchronously
remDr$navigate("http://www.google.com/ncr")
remDr$setAsyncScriptTimeout(10000)
webElem <- remDr$findElement("css", "img#hplogo")
script <- "cb = arguments[arguments.length -1]; webElem = arguments[0]; setTimeout(function(){webElem.hidden = true; cb('DONE');},5000);"
remDr$executeAsyncScript(script, args = list(webElem))

remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement("css", "img#hplogo")
script <- "webElem = arguments[0]; setTimeout(function(){webElem.hidden = true;},5000); return 'DONE';"
remDr$executeScript(script, args = list(webElem))

# frames
remDr$navigate("https://CRAN.r-project.org/")
XML::htmlParse(remDr$getPageSource()[[1]])
remDr$maxWindowSize()
remDr$screenshot(display = TRUE)
webElems <- remDr$findElements(using = "tag name", "frame")
# webElems <- remDr$findElements(value = "//frame") # using xpath
# webElems <- remDr$findElements("css", value = "frame") # using css
sapply(webElems, function(x){x$getElementAttribute("src")})

remDr$switchToFrame(webElems[[2]])
XML::htmlParse(remDr$getPageSource()[[1]])

webElems <- remDr$findElements("css", "[href]")
sapply(webElems, function(x){x$getElementAttribute("href")})
unlist(sapply(webElems, function(x){x$getElementAttribute("href")}))

remDr$switchToFrame(NULL)

remDr$switchToFrame("banner")
XML::htmlParse(remDr$getPageSource()[[1]])



remDr$close()
#rD$server$stop()
