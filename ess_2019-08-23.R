### European Social Survey


# loading stuff -----------------------------------------------------------

list.of.packages <- c("tidyverse",
                      "plyr",
                      "qgraph",
                      "dplyr",
                      "knitr",
                      "kableExtra",
                      "psych",
                      "semPlot",
                      "lavaan")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  install.packages(new.packages, repos='http://cran.us.r-project.org')
}
tmp <- lapply(list.of.packages, require, character.only = TRUE)

rm(list.of.packages, new.packages, tmp)
# knitr::opts_chunk$set(echo = FALSE)

# rm(list = ls())


# selecting items and reading data ----------------------------------------------

items <- list(
  discrimination = c("dscrrce",
                     "dscrntn",
                     "dscrrlg",
                     "dscrlng",
                     "dscretn",
                     "dscrage",
                     "dscrgnd",
                     "dscrsex",
                     "dscrdsb",
                     "dscroth"
                     ),
  
  trust_social = c("ppltrst",
                   "pplfair",
                   "pplhlp"
                   ),
  
  trust_political = c("trstprl",
                      "trstlgl",
                      "trstplc",
                      "trstplt",
                      "trstprt"
                      ),
  
  hope_political = c("psppsgva",
                     "actrolga",
                     "psppipla",
                     "cptppola"),
  
  interest_political = "polintr"
  
)

# items_ordered <- items
# items_ordered$trust_social <- NULL
# items_ordered$trust_political <- NULL
items_ordered <- items$hope_political

items_ordered <- items_ordered %>% unlist() %>% as.character()

## reading, selecting, and saving the abridged data to GitHub
# data.orig <- read.csv("https://raw.githubusercontent.com/psyguy/sem-course-project/master/data/ESS8e02.1_F1.csv")
# data.orig <- read.csv("data/ESS8e02.1_F1.csv")
# d <- data.orig %>% select(cntry, as.character(unlist(items)))
# d %>% write.csv("data/ess2016_selected.csv", row.names = FALSE)

data <- read.csv("https://github.com/psyguy/sem-course-project/raw/master/data/ess2016_selected.csv")


# cleaning/recoding the data ----------------------------------------------

d <- data
# removing invalid/missing data
d[d>10] <- NA
d$polintr[d$polintr>4] <- NA
d[,20:23][d[,20:23]>5] <- NA
# reverse-coding polint
d$polintr <- (d$polintr-5) %>% abs()

# d <- d %>% filter(cntry == "BE") %>%  na.omit()

# making model syntaxes ---------------------------------------------------

m.1 <- "

discrimination <~ 1*dscrrce + dscrntn + dscrrlg +
                  dscrlng + dscretn + dscrage +
                  dscrgnd + dscrsex + dscrdsb +
                  dscroth

# trust =~ trust_social + trust_political

trust_social =~ 1*ppltrst + pplfair + pplhlp

trust_political =~ 1*trstprl + trstlgl + trstplc + trstplt + trstprt

hope_political =~ 1*psppsgva + actrolga + psppipla + cptppola

hope_political + trust_political + discrimination ~ polintr

hope_political + trust_social + trust_political ~ discrimination


"

Sys.time()
f <- lavaan(m.1, d, ordered = items_ordered, std.lv = TRUE)
Sys.time()

f %>% summary(standardized=TRUE)
f %>% fitmeasures()
f %>% modificationindices()

