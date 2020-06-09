
library(plyr)
translationContent <- read.delim("src/dictionary.csv", header = TRUE, sep = ",", as.is = TRUE, quote = '"') 
translation <- dlply(translationContent ,.(key), function(s) key = as.list(s))

save(translation, file = "upd/translation.bin")

translation[['title']][[lg]] 

# translation <- list(
#   "youhaveselected" = list("en" = "You have selected:", "fr"="Vous avez sélectionné:"),
#   "greetings" = list("en" = "Hello", "fr"="Bonjour"),
#   
#   "title" = list("en" = "vizCovid Dashboard:  Confirmed Cases, Deaths, Recovered", 
#                  "pt" = "Painel vizCovid:  Casos Confirmados, Mortes etc"),
#   "about" = list("en" = "about", 
#                  "pt" = "sobre")
#   
# )
#"title" = list("en" = "", 
#               "pt" = ""),
#translation[['']][[lg]]





