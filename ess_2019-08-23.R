### European Social Survey


# loading stuff -----------------------------------------------------------

list.of.packages <- c("tidyverse",
                      "plyr",
                      "qgraph",
                      "dplyr",
                      "knitr",
                      "kableExtra",
                      "psych",
                      "kutils",
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

dscrscore <- d %>% select(contains("dscr")) %>% rowSums()

ds <- d %>% select(contains("dscr"))
dspc <- (princomp(ds,1)$scores)[,1]

d <- d %>% cbind(dscrscore, dspc)

d <- d %>% filter(cntry == "BE") %>%  na.omit()

# making model syntaxes ---------------------------------------------------

m.1 <- "

discrimination <~ dscrrce + dscrntn + dscrrlg +
                  dscrlng + dscretn + dscrage +
                  dscrgnd + dscrsex + dscrdsb +
                  dscroth

# trust =~ trust_social + trust_political

trust_social =~ ppltrst + pplfair + pplhlp

trust_political =~ trstprl + trstlgl + trstplc + trstplt + trstprt

hope_political =~ psppsgva + actrolga + psppipla + cptppola

hope_political ~ polintr

hope_political + trust_social + trust_political ~ discrimination

# discrimination ~~ 1*discrimination
# hope_political ~~ 1*hope_political
# trust_social ~~ 1*trust_social
# trust_political ~~ 1*trust_political

"


m.2 <- "

# trust =~ trust_social + trust_political

trust_social =~ NA*ppltrst + pplfair + pplhlp

trust_political =~ NA*trstprl + trstlgl + trstplc + trstplt + trstprt

trust_social ~~ trust_political

hope_political =~ NA*psppsgva + actrolga + psppipla + cptppola

hope_political ~ polintr

hope_political + trust_social + trust_political ~ dscrscore

# discrimination ~~ discrimination
hope_political ~~ 1*hope_political
trust_social ~~ 1*trust_social
trust_political ~~ 1*trust_political

"

m.3 <- "

# 1. latent variable definitions

  hope_political =~ NA*psppsgva + actrolga + psppipla + cptppola
  trust_social =~ NA*ppltrst + pplfair + pplhlp
  trust_political =~ NA*trstprl + trstlgl + trstplc + trstplt + trstprt
  # trust =~ trust_social + trust_political

# 2. regressions

  hope_political + trust_political ~ polintr
  hope_political + trust_social + trust_political ~ dscrscore

# 3. (co)variances

  hope_political ~~ 1*hope_political
  trust_social ~~ 1*trust_social
  trust_political ~~ 1*trust_political
  
  hope_political ~~ trust_social + trust_political
  trust_social ~~ trust_political

# 4. intercepts
  
  psppsgva + actrolga + psppipla + cptppola ~ 1
  ppltrst + pplfair + pplhlp ~ 1
  trstprl + trstlgl + trstplc + trstplt + trstprt ~ 1

# 5. thresholds
  
 # psppsgva + actrolga + psppipla + cptppola | t1 + t2 + t3 + t4

"

m.4 <- "

# 1. latent variable definitions

  hope_political =~ NA*psppsgva + actrolga + psppipla + cptppola
  trust_social =~ NA*ppltrst + pplfair + pplhlp
  trust_political =~ NA*trstprl + trstlgl + trstplc + trstplt + trstprt
  optimism_general =~ NA*trust_social + trust_political + hope_political
  optimism_political =~ NA*psppsgva + actrolga + psppipla + cptppola +
                        trstprl + trstlgl + trstplc + trstplt + trstprt

# 2. regressions

  hope_political + trust_political ~ polintr
  hope_political + trust_social + trust_political ~ dscrscore

# 3. (co)variances

  hope_political ~~ 1*hope_political
  trust_social ~~ 1*trust_social
  trust_political ~~ 1*trust_political
  optimism_general ~~ 1*optimism_general
  optimism_political ~~ 1*optimism_political
  
  # hope_political ~~ trust_social + trust_political
  # trust_social ~~ trust_political

# 4. intercepts
  
  psppsgva + actrolga + psppipla + cptppola ~ 1
  ppltrst + pplfair + pplhlp ~ 1
  trstprl + trstlgl + trstplc + trstplt + trstprt ~ 1

# 5. thresholds
  
 # psppsgva + actrolga + psppipla + cptppola | t1 + t2 + t3 + t4

"

m.5 <- "

# 1. latent variable definitions

  hope_political =~ NA*psppsgva + actrolga + psppipla + cptppola
  trust_social =~ NA*ppltrst + pplfair + pplhlp
  trust_political =~ NA*trstprl + trstlgl + trstplc + trstplt + trstprt
  # optimism_general =~ NA*trust_social + trust_political + hope_political
  optimism_political =~ NA*psppsgva + actrolga + psppipla + cptppola +
                        trstprl + trstlgl + trstplc + trstplt + trstprt

# 2. regressions

  hope_political + trust_political ~ polintr
  hope_political + trust_social + trust_political ~ dscrscore

# 3. (co)variances

  hope_political ~~ 1*hope_political
  trust_social ~~ 1*trust_social
  trust_political ~~ 1*trust_political
  # optimism_general ~~ 1*optimism_general
  optimism_political ~~ 1*optimism_political
  
  hope_political ~~ trust_social + trust_political
  trust_social ~~ trust_political

# 4. intercepts
  
  psppsgva + actrolga + psppipla + cptppola ~ 1
  ppltrst + pplfair + pplhlp ~ 1
  trstprl + trstlgl + trstplc + trstplt + trstprt ~ 1

# 5. thresholds
  
 # psppsgva + actrolga + psppipla + cptppola | t1 + t2 + t3 + t4

"

m.55 <- "

# 1. latent variable definitions

  hope_political =~ NA*psppsgva + actrolga + psppipla + cptppola
  trust_social =~ NA*ppltrst + pplfair + pplhlp
  trust_political =~ NA*trstprl + trstlgl + trstplc + trstplt + trstprt
  optimism_general =~ NA*trust_social + trust_political + hope_political
  optimism_political =~ NA*psppsgva + actrolga + psppipla + cptppola +
                        trstprl + trstlgl + trstplc + trstplt + trstprt

# 2. regressions

  hope_political + trust_political ~ polintr
  hope_political + trust_social + trust_political ~ dscrscore

# 3. (co)variances

  hope_political ~~ 1*hope_political
  trust_social ~~ 1*trust_social
  trust_political ~~ 1*trust_political
  optimism_general ~~ 1*optimism_general
  optimism_political ~~ 1*optimism_political
  
  hope_political ~~ trust_social + trust_political
  trust_social ~~ trust_political

# 4. intercepts
  
  psppsgva + actrolga + psppipla + cptppola ~ 1
  ppltrst + pplfair + pplhlp ~ 1
  trstprl + trstlgl + trstplc + trstplt + trstprt ~ 1

# 5. thresholds
  
 # psppsgva + actrolga + psppipla + cptppola | t1 + t2 + t3 + t4

"

m.6 <- "

# 1. latent variable definitions

  hope_political =~ NA*psppsgva + actrolga + psppipla + cptppola
  trust_social =~ NA*ppltrst + pplfair + pplhlp
  trust_political =~ NA*trstprl + trstlgl + trstplc + trstplt + trstprt
  # optimism_general =~ NA*trust_social + trust_political + hope_political
  optimism_political =~ NA*psppsgva + actrolga + psppipla + cptppola +
                        trstprl + trstlgl + trstplc + trstplt + trstprt
                        
  
  discrimination <~ NA*dscrrce + dscrntn + dscrrlg +
                    dscrlng + dscretn + dscrage +
                    dscrgnd + dscrsex + dscrdsb +
                    dscroth

# 2. regressions

  hope_political + trust_political ~ polintr
  hope_political + trust_social + trust_political ~ discrimination

# 3. (co)variances

  hope_political ~~ 1*hope_political
  trust_social ~~ 1*trust_social
  trust_political ~~ 1*trust_political
  # optimism_general ~~ 1*optimism_general
  optimism_political ~~ 1*optimism_political
  discrimination ~~ 1*discrimination
  
  hope_political ~~ trust_social + trust_political
  trust_social ~~ trust_political

# 4. intercepts
  
  psppsgva + actrolga + psppipla + cptppola ~ 1
  ppltrst + pplfair + pplhlp ~ 1
  trstprl + trstlgl + trstplc + trstplt + trstprt ~ 1

# 5. thresholds
  
 # psppsgva + actrolga + psppipla + cptppola | t1 + t2 + t3 + t4

"

m.7 <- "

# 1. latent variable definitions

  hope_political =~ NA*psppsgva + actrolga + psppipla + cptppola
  trust_social =~ NA*ppltrst + pplfair + pplhlp
  trust_political =~ NA*trstprl + trstlgl + trstplc + trstplt + trstprt
  # optimism_general =~ NA*trust_social + trust_political + hope_political
  optimism_political =~ NA*psppsgva + actrolga + psppipla + cptppola +
                        trstprl + trstlgl + trstplc + trstplt + trstprt

# 2. regressions

  hope_political + trust_political ~ polintr
  hope_political + trust_social + trust_political + optimism_political ~ dscrrce + dscrntn + dscrrlg + dscrlng + dscretn + dscrage + dscrgnd + dscrsex + dscrdsb + dscroth
  

# 3. (co)variances

  hope_political ~~ 1*hope_political
  trust_social ~~ 1*trust_social
  trust_political ~~ 1*trust_political
  # optimism_general ~~ 1*optimism_general
  optimism_political ~~ 1*optimism_political
  
  hope_political ~~ trust_social + trust_political
  trust_social ~~ trust_political

# 4. intercepts
  
  psppsgva + actrolga + psppipla + cptppola ~ 1
  ppltrst + pplfair + pplhlp ~ 1
  trstprl + trstlgl + trstplc + trstplt + trstprt ~ 1

# 5. thresholds
  
 # psppsgva + actrolga + psppipla + cptppola | t1 + t2 + t3 + t4

"


m.8 <- "

# 1. latent variable definitions

  hope_political =~ NA*psppsgva + actrolga + psppipla + cptppola
  trust_social =~ NA*ppltrst + pplfair + pplhlp
  trust_political =~ NA*trstprl + trstlgl + trstplc + trstplt + trstprt
  # optimism_general =~ NA*trust_social + trust_political + hope_political
  optimism_political =~ NA*psppsgva + actrolga + psppipla + cptppola +
                        trstprl + trstlgl + trstplc + trstplt + trstprt

# 2. regressions

  hope_political + trust_political ~ polintr
  hope_political + trust_social + trust_political ~ dspc

# 3. (co)variances

  hope_political ~~ 1*hope_political
  trust_social ~~ 1*trust_social
  trust_political ~~ 1*trust_political
  # optimism_general ~~ 1*optimism_general
  optimism_political ~~ 1*optimism_political
  
  hope_political ~~ trust_social + trust_political
  trust_social ~~ trust_political

# 4. intercepts
  
  psppsgva + actrolga + psppipla + cptppola ~ 1
  ppltrst + pplfair + pplhlp ~ 1
  trstprl + trstlgl + trstplc + trstplt + trstprt ~ 1

# 5. thresholds
  
 # psppsgva + actrolga + psppipla + cptppola | t1 + t2 + t3 + t4

"

m.10 <- "

# 1. latent variable definitions

  hope_political =~ NA*psppsgva + actrolga + psppipla + cptppola
  trust_social =~ NA*ppltrst + pplfair + pplhlp
  trust_political =~ NA*trstprl + trstlgl + trstplc + trstplt + trstprt
  # optimism_general =~ NA*trust_social + trust_political + hope_political
  optimism_political =~ NA*psppsgva + actrolga + psppipla + cptppola +
                        trstprl + trstlgl + trstplc + trstplt + trstprt

# 2. regressions

  # hope_political + trust_political ~ polintr
  hope_political + trust_social + trust_political + optimism_political ~ dscrscore + polintr

# 3. (co)variances

  hope_political ~~ 1*hope_political
  trust_social ~~ 1*trust_social
  trust_political ~~ 1*trust_political
  # optimism_general ~~ 1*optimism_general
  optimism_political ~~ 1*optimism_political
  
  hope_political ~~ trust_social + trust_political
  trust_social ~~ trust_political

# 4. intercepts
  
  psppsgva + actrolga + psppipla + cptppola ~ 1
  ppltrst + pplfair + pplhlp ~ 1
  trstprl + trstlgl + trstplc + trstplt + trstprt ~ 1

# 5. thresholds
  
 # psppsgva + actrolga + psppipla + cptppola | t1 + t2 + t3 + t4

"



Sys.time()
f <- lavaan(m.10, d,
            ordered = c(items_ordered, as.character(unlist(items$discrimination))),
            std.lv = TRUE,
            # likelihood = "wishart",
            # estimator = "MLR",
            auto.var=TRUE)
Sys.time()

f %>% summary(standardized=TRUE)
(fm <- f %>% fitmeasures())
f %>% modificationindices()


# plot and table ----------------------------------------------------------


semPaths(f,
         title = FALSE,
         curvePivot = TRUE,
         what = "std",
         rotation = 2,
         # style = "mx",
         layout = "tree2",
         optimizeLatRes = TRUE,
         intercepts = FALSE,
         edge.label.cex = 0.01,
         exoVar=FALSE,
         sizeMan=5,
         sizeLat=7,
         nCharNodes=5,
         residuals=FALSE,
         fixedStyle=1,
         freeStyle=1,
         # filetype = "pdf", # Store to PDF
         # filename = "x", # Set the name of the file
         # mar = c(1, 1, 1, 1),
         curvePivot = FALSE)



 semTable(f)


semTable(f, file = NULL, paramSets = "all", paramSetLabels,
         columns = c(est = "Estimate", se = "SE", z = "z", p = "p"),
         columnLabels, fits = c("chisq", "cfi", "tli", "rmsea"),
         fitLabels = toupper(fits), varLabels = NULL, groups = NULL,
         type = "latex", table.float = FALSE, caption = NULL,
         label = NULL, longtable = FALSE, print.results = TRUE,
         centering = "siunitx", alpha = c(0.05, 0.01, 0.001))



# testtable("fit1.t1.tex", "C:/Users/r0607671/OneDrive - student.kuleuven.be/Master of Psychology/2. Present/Structural Equation Modeling/sem-course-project")


fit1.t1 <- semTable(f,
                    # file = "semtable.csv",
                    columns = c("estse"),
                    fits = c("chisq", "rmsea", "cfi", "srmr"),
                    #file = file.path("C:/Users/r0607671/OneDrive - student.kuleuven.be/Master of Psychology/2. Present/Structural Equation Modeling/sem-course-project", "fit1.t1.html"),
                    # varLabels = c("x1" = "hello"),
                    # type = "html",
                    print.results = TRUE)

