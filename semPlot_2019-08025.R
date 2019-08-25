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

L <- c("dscrrce", "trust_social",       "pplhlp",
       "dscrntn", NA,                   "pplfair",
       "dscrrlg", NA,                   "ppltrst",
       "dscrlng", NA,                   "trstprl",
       "dscretn", "trust_political",    "trstlgl",
       "dscrage", NA,                   "trstplc",
       "dscrgnd", NA,                   "trstplt",
       "dscrsex", NA,                   "trstprt",
       "dscrdsb", "hope_political",     "psppsgva",
       "dscroth", NA,                   "actrolga",
       NA,        "optimism_political", "psppipla",
       "polintr", NA,                   "cptppola"                    
       ) %>% matrix(12, byrow = TRUE)



semPaths(f,
         title = FALSE,
         curvePivot = TRUE,
         what = "std",
         rotation = 2,
         # style = "mx",
         # layout = L,
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
