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
  
  political_hope = c("psppsgva",
                     "actrolga",
                     "psppipla",
                     "cptppola"),
  
  political_interest = "polintr"
  
)

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

d <- d %>% na.omit()

# making model syntaxes ---------------------------------------------------


