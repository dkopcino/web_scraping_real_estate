#setInternet2(T) # not needed anymore
my_url = "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
web_connection = url(my_url)
web_connection
open(web_connection)
html_code = readLines(web_connection, 300)
close(web_connection)
html_code

my_url = "http://irfanelahi.com/churn_data_set.txt"
web_connection = url(my_url)
open(web_connection)
churn_data = readLines(web_connection)
close(web_connection)
churn_data

#install.packages("XML")
library("XML")
my_url = "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
html_page = htmlTreeParse(my_url, useInternalNodes = T)
html_page = htmlTreeParse(my_url)
#html_page = read_html(my_url)



################### GA #####################
# https://github.com/jdeboer/ganalytics
# https://github.com/MarkEdmondson1234/searchConsoleR
# http://code.markedmondson.me/googleAuthR/
# http://code.markedmondson.me/googleAnalyticsR/


# Client ID	874766780984-9ag835ccu29ko7etbci51b641i507b59.apps.googleusercontent.com
# Client secret	BsFP9E-Kp8jWB9E0kMURiDb-
# Name R client 1


