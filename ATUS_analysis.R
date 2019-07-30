### This code generates time transfer matrices by age, sex, and race/ethnicity in the United States,
### using the American Time Use Survey data, available for download at https://www.bls.gov/tus/ 

# By Denys Dukhovnov (contact denys_dukhovnov@berkeley.edu)
# Last revised on July 29, 2019

# NOTE: Given the compexity of the following code that has been written over the years, the users 
# are encouraged to read comments and use discretion when working with the output.

# NOTE: Running the entire code at once is not recommended, as minor differences in directory structure,
# versions of installed packages, or other minor errors will likely cascade down, producing nonsensical
# results or errors. Some users may find portions of the code redundant for their purposes (such blocks are often commented out).

# NOTE: This code can be used as the basis for generating informal care time transfer matrices by age/sex and categories, 
# other than RACE/ETHNICITY (e.g. educational attainment). If desired, this code can serve as template for such modifications.


###################################

#rm(list=ls())

# Set the appropriate parent directory where the data are placed in the /data/ subdirectory
setwd("")

datasets <- c("atusrost_0315.dat", "atuswho_0315.dat", "atusresp_0315.dat",
              "atusact_0315.dat", "atussum_0315.dat", "atusrostec_1115.dat",
              "atuscps_0315.dat", "atuswgts_0315.dat")

## Reading in ATUS main questionnaire components (Multi-year 2003-2015, eldercare 2011-2015)

RosterFile <- as.data.frame(read.table(file=paste0(getwd(), "/data/", datasets[1]), header = T, stringsAsFactors = F, sep = ","))
WhoFile <- as.data.frame(read.table(file=paste0(getwd(), "/data/", datasets[2]), header = T, stringsAsFactors = F, sep = ","))
#RespondentFile <- as.data.frame(read.table(file=paste0(getwd(), "/data/", datasets[3]), header = T, stringsAsFactors = F, sep = ","))
ActFile <- as.data.frame(read.table(file=paste0(getwd(), "/data/", datasets[4]), header = T, stringsAsFactors = F, sep = ","))
ActSumFile <- as.data.frame(read.table(file=paste0(getwd(), "/data/", datasets[5]), header = T, stringsAsFactors = F, sep = ","))
#ECroster <- as.data.frame(read.table(file=paste0(getwd(), "/data/", datasets[6]), header = T, sep = ",", stringsAsFactors = F))
#CPS <- as.data.frame(read.table(file=paste0(getwd(), "/data/", datasets[7]), header = T, sep = ",", stringsAsFactors = F))

## Reformatting Respondent IDs and activity codes to match ATUS dictionary notation
RosterFile$TUCASEID <- as.character(RosterFile$TUCASEID, length = 14)  
WhoFile$TUCASEID <- as.character(WhoFile$TUCASEID, length = 14)  
#RespondentFile$TUCASEID <- as.character(RespondentFile$TUCASEID, length = 14)  
ActFile$TUCASEID <- as.character(ActFile$TUCASEID, length = 14)  
ActFile$TRCODEP <- as.character(sprintf("%06d", ActFile$TRCODEP), length = 6)
ActSumFile$TUCASEID <- as.character(ActSumFile$TUCASEID, length = 14)  
#ECroster$TUCASEID <- as.character(ECroster$TUCASEID, length = 14)
#CPS$TUCASEID <- as.character(CPS$TUCASEID, length = 14)

## Creating a subset of data for years of interest based on Respondent ID (2011-2015)
subsetFUN <- function(df) {
  df$sel <- (as.numeric(substr(df$TUCASEID, 1, 4)) >= 2011) & 
            (as.numeric(substr(df$TUCASEID, 1, 4)) <= 2015)
  df <- df[df$sel==TRUE, ]
  df <- within(df, rm("sel"))
  return(df)
}

RosterFile <- subsetFUN(RosterFile)
WhoFile <- subsetFUN(WhoFile)
#RespondentFile <- subsetFUN(RespondentFile)
ActFile <- subsetFUN(ActFile)
ActSumFile <- subsetFUN(ActSumFile)
#ECroster <- subsetFUN(ECroster)
#CPS <- subsetFUN(CPS) 

#### Subset for analysis, conditional on presence of spouse or children 

RosterFile$spouse <- RosterFile$TERRP == 20 | RosterFile$TERRP == 21  # spouse OR partner
RosterFile$children <- RosterFile$TEAGE < 18 & RosterFile$TULINENO != 1   # any children in HH
temp <- split(RosterFile, RosterFile$TUCASEID)
RosterFile <- RosterFile[,1:5]
spouse.children <- data.frame(ActSumFile$TUCASEID, rep(rep(NA, length(ActSumFile$TUCASEID))),
                              rep(NA, length(ActSumFile$TUCASEID)), stringsAsFactors = FALSE)
colnames(spouse.children) <- c("TUCASEID", "SPOUSE", "CHILDREN")
for (i in 1:length(spouse.children$TUCASEID)) {
    spouse.children[i,2] <- ifelse(any(temp[[i]]$spouse) == TRUE, 1, 0)
    spouse.children[i,3] <- ifelse(any(temp[[i]]$children) == TRUE, 1, 0)
}

# RosterFile <- RosterFile[RosterFile$TUCASEID %in% spouse.children[spouse.children$SPOUSE == 1 ,"TUCASEID"], ]
# ActFile <- ActFile[ActFile$TUCASEID %in% spouse.children[spouse.children$SPOUSE == 1 ,"TUCASEID"], ]
# ActSumFile <- ActSumFile[ActSumFile$TUCASEID %in% spouse.children[spouse.children$SPOUSE == 1 ,"TUCASEID"], ]
# WhoFile <- WhoFile[WhoFile$TUCASEID %in% spouse.children[spouse.children$SPOUSE == 1 ,"TUCASEID"], ]

# RosterFile <- RosterFile[RosterFile$TUCASEID %in% spouse.children[spouse.children$CHILDREN == 1 ,"TUCASEID"], ]
# ActFile <- ActFile[ActFile$TUCASEID %in% spouse.children[spouse.children$CHILDREN == 1 ,"TUCASEID"], ]
# ActSumFile <- ActSumFile[ActSumFile$TUCASEID %in% spouse.children[spouse.children$CHILDREN == 1 ,"TUCASEID"], ]
# WhoFile <- WhoFile[WhoFile$TUCASEID %in% spouse.children[spouse.children$CHILDREN == 1 ,"TUCASEID"], ]




## Defining Household (HH) and Non-Household (NonHH) CHILDCARE (CC) and ADULT CARE (AC)
## activity classes in the "Activity File"

ActFile$HH_CC <- (substr(ActFile$TRCODEP, 1, 4) == "0301") |
                 (substr(ActFile$TRCODEP, 1, 4) == "0302") |
                 (substr(ActFile$TRCODEP, 1, 4) == "0303") |
                 (substr(ActFile$TRCODEP, 1, 6) == "180381")
    
ActFile$HH_AC <- (substr(ActFile$TRCODEP, 1, 4) == "0304") | 
                 (substr(ActFile$TRCODEP, 1, 4) == "0305") |
                 (substr(ActFile$TRCODEP, 1, 6) == "180382")
    
ActFile$NonHH_CC <- (substr(ActFile$TRCODEP, 1, 4) == "0401") |
                    (substr(ActFile$TRCODEP, 1, 4) == "0402") |
                    (substr(ActFile$TRCODEP, 1, 4) == "0403") |
                    (substr(ActFile$TRCODEP, 1, 6) == "180481")

ActFile$NonHH_AC <- (substr(ActFile$TRCODEP, 1, 4) == "0404") | 
                    (substr(ActFile$TRCODEP, 1, 4) == "0405") |
                    (substr(ActFile$TRCODEP, 1, 6) == "180482")    
    

## Defining Racial/Ethnic categories from Activity Summary File

ActSumFile$raceShort <- rep(0, length(ActSumFile$TUCASEID))
for (i in 1:length(ActSumFile$TUCASEID)) {
      
  # 1 = Non-Hispanic White only
  ifelse(ActSumFile$PTDTRACE[i] == 1 & ActSumFile$PEHSPNON[i] == 2, ActSumFile$raceShort[i] <- 1, 0)
  
  # 2 = Non-Hispanic Black or African-American only
  ifelse(ActSumFile$PTDTRACE[i] == 2 & ActSumFile$PEHSPNON[i] == 2, ActSumFile$raceShort[i] <- 2, 0)
  
  # 3 = Non-Hispanic Asian only
  ifelse(ActSumFile$PTDTRACE[i] == 4 & ActSumFile$PEHSPNON[i] == 2, ActSumFile$raceShort[i] <- 3, 0)
  
  # 4 = Hispanic, any race
  ifelse(ActSumFile$PEHSPNON[i] == 1, ActSumFile$raceShort[i] <- 4, 0)
}

# ## Breakdown by educational attainment 
# #0 = Less than HS, 
# #1 = HS or GED, 
# #2 = Some College/2-year/4-year degree
# #3 = Professional or graduate degree
# CPS$EDUCATION <- rep(0, length(CPS$TUCASEID))
# CPS$EDUCATION <- ifelse(CPS$PEEDUCA >= 31 & CPS$PEEDUCA <= 38, CPS$EDUCATION <- 0,
#                       ifelse(CPS$PEEDUCA == 39, CPS$EDUCATION <- 1,
#                             ifelse(CPS$PEEDUCA >= 40 & CPS$PEEDUCA <= 43, CPS$EDUCATION <- 2,
#                                   ifelse(CPS$PEEDUCA > 43, CPS$EDUCATION <- 4, NA))))
# 
# ## Breakdown by nativity (country of birth)
# #0 = U.S. and Canada
# #1 = Europe
# #2 = Asia
# #3 = Central and South America (incl. Mexico)
# #4 = Africa
# #5 = Australia and New Zealand + other
# CPS$NATIVITY <- rep(0, length(CPS$TUCASEID))
# CPS$NATIVITY <- ifelse((CPS$PENATVTY < 100 & CPS$PENATVTY >= 57) | CPS$PENATVTY == 301, CPS$NATIVITY <- 0,
#                        ifelse(CPS$PENATVTY > 99 & CPS$PENATVTY <= 199, CPS$NATIVITY <- 1,
#                               ifelse(CPS$PENATVTY > 199 & CPS$PENATVTY <= 299, CPS$NATIVITY <- 2,
#                                      ifelse(CPS$PENATVTY > 301 & CPS$PENATVTY <= 399, CPS$NATIVITY <- 3,
#                                             ifelse(CPS$PENATVTY > 399 & CPS$PENATVTY <= 499, CPS$NATIVITY <- 4, 5)))))


RowAgeLab <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
               "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

ColAgeLab <- c("0-4", "5-9", "10-14", RowAgeLab)

## Total time spent on childcare or adult care
ActSumFile$HHCC_time <- apply(ActSumFile[ , c(69:89, 399)], 1, sum) 
ActSumFile$HHAC_time <- apply(ActSumFile[ , c(90:100, 400)], 1, sum)
ActSumFile$HHANYCARE_time <- apply(ActSumFile[ , c(69:100, 399:401)], 1, sum) 
ActSumFile$nonHHANYCARE_time <- apply(ActSumFile[ , c(102:138, 402:404)], 1, sum)

# ### Average daily participation rates by race
# ActSumFile$HHCC.participation <- ifelse(apply(ActSumFile[ , c(69:89, 399)], 1, sum) > 0 & apply(ActSumFile[ , c(90:100, 400)], 1, sum) == 0, 1, 0)
# ActSumFile$HHAC.participation <- ifelse(apply(ActSumFile[ , c(90:100, 400)], 1, sum) > 0 & apply(ActSumFile[ , c(69:89, 399)], 1, sum) == 0, 1, 0)
# ActSumFile$HHANYCARE.participation <- ifelse(apply(ActSumFile[ , c(69:100, 399, 400)], 1, sum) > 0, 1, 0)
# ActSumFile$HHBOTH.participation <- with(ActSumFile, HHANYCARE.participation - HHCC.participation - HHAC.participation)
# 
# white.HHCC.participation <- sum(ActSumFile[ActSumFile$raceShort == 1 & ActSumFile$HHCC.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 1,"TUFNWGTP"])
# white.HHAC.participation <- sum(ActSumFile[ActSumFile$raceShort == 1 & ActSumFile$HHAC.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 1,"TUFNWGTP"])
# white.HHBOTH.participation <- sum(ActSumFile[ActSumFile$raceShort == 1 & ActSumFile$HHBOTH.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 1,"TUFNWGTP"])
# white.HHANYCARE.participation <- sum(ActSumFile[ActSumFile$raceShort == 1 & ActSumFile$HHANYCARE.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 1,"TUFNWGTP"])
# white.HHNOCARE.participation <- sum(ActSumFile[ActSumFile$raceShort == 1 & ActSumFile$HHANYCARE.participation == 0,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 1,"TUFNWGTP"])
# 
# black.HHCC.participation <- sum(ActSumFile[ActSumFile$raceShort == 2 & ActSumFile$HHCC.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 2,"TUFNWGTP"])
# black.HHAC.participation <- sum(ActSumFile[ActSumFile$raceShort == 2 & ActSumFile$HHAC.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 2,"TUFNWGTP"])
# black.HHBOTH.participation <- sum(ActSumFile[ActSumFile$raceShort == 2 & ActSumFile$HHBOTH.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 2,"TUFNWGTP"])
# black.HHANYCARE.participation <- sum(ActSumFile[ActSumFile$raceShort == 2 & ActSumFile$HHANYCARE.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 2,"TUFNWGTP"])
# black.HHNOCARE.participation <- sum(ActSumFile[ActSumFile$raceShort == 2 & ActSumFile$HHANYCARE.participation == 0,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 2,"TUFNWGTP"])
# 
# asian.HHCC.participation <- sum(ActSumFile[ActSumFile$raceShort == 3 & ActSumFile$HHCC.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 3,"TUFNWGTP"])
# asian.HHAC.participation <- sum(ActSumFile[ActSumFile$raceShort == 3 & ActSumFile$HHAC.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 3,"TUFNWGTP"])
# asian.HHBOTH.participation <- sum(ActSumFile[ActSumFile$raceShort == 3 & ActSumFile$HHBOTH.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 3,"TUFNWGTP"])
# asian.HHANYCARE.participation <- sum(ActSumFile[ActSumFile$raceShort == 3 & ActSumFile$HHANYCARE.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 3,"TUFNWGTP"])
# asian.HHNOCARE.participation <- sum(ActSumFile[ActSumFile$raceShort == 3 & ActSumFile$HHANYCARE.participation == 0,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 3,"TUFNWGTP"])
# 
# hispanic.HHCC.participation <- sum(ActSumFile[ActSumFile$raceShort == 4 & ActSumFile$HHCC.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 4,"TUFNWGTP"])
# hispanic.HHAC.participation <- sum(ActSumFile[ActSumFile$raceShort == 4 & ActSumFile$HHAC.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 4,"TUFNWGTP"])
# hispanic.HHBOTH.participation <- sum(ActSumFile[ActSumFile$raceShort == 4 & ActSumFile$HHBOTH.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 4,"TUFNWGTP"])
# hispanic.HHANYCARE.participation <- sum(ActSumFile[ActSumFile$raceShort == 4 & ActSumFile$HHANYCARE.participation == 1,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 4,"TUFNWGTP"])
# hispanic.HHNOCARE.participation <- sum(ActSumFile[ActSumFile$raceShort == 4 & ActSumFile$HHANYCARE.participation == 0,"TUFNWGTP"]) /
#   sum(ActSumFile[ActSumFile$raceShort == 4,"TUFNWGTP"])
# 
# overall.HHCC.participation <- sum(ActSumFile[ActSumFile$HHCC.participation == 1, "TUFNWGTP"]) / sum(ActSumFile[,"TUFNWGTP"])
# overall.HHAC.participation <- sum(ActSumFile[ActSumFile$HHAC.participation == 1,"TUFNWGTP"]) / sum(ActSumFile[,"TUFNWGTP"])
# overall.HHBOTH.participation <- sum(ActSumFile[ActSumFile$HHBOTH.participation == 1,"TUFNWGTP"]) / sum(ActSumFile[,"TUFNWGTP"])
# overall.HHANYCARE.participation <- sum(ActSumFile[ActSumFile$HHANYCARE.participation == 1,"TUFNWGTP"]) / sum(ActSumFile[,"TUFNWGTP"])
# overall.HHNOCARE.participation <- sum(ActSumFile[ActSumFile$HHANYCARE.participation == 0,"TUFNWGTP"]) / sum(ActSumFile[,"TUFNWGTP"])
# 
# fractions.care.by.race <- matrix(
#   c(white.HHCC.participation, white.HHAC.participation, white.HHBOTH.participation, white.HHANYCARE.participation, white.HHNOCARE.participation,
#     black.HHCC.participation, black.HHAC.participation, black.HHBOTH.participation, black.HHANYCARE.participation, black.HHNOCARE.participation,
#     asian.HHCC.participation, asian.HHAC.participation, asian.HHBOTH.participation, asian.HHANYCARE.participation, asian.HHNOCARE.participation,
#     hispanic.HHCC.participation, hispanic.HHAC.participation, hispanic.HHBOTH.participation, hispanic.HHANYCARE.participation, hispanic.HHNOCARE.participation,
#     overall.HHCC.participation, overall.HHAC.participation, overall.HHBOTH.participation, overall.HHANYCARE.participation, overall.HHNOCARE.participation),
#   5, 5, byrow = FALSE)
# 
# colnames(fractions.care.by.race) <- c("White", "Black", "Asian", "Hispanic", "Overall")
# rownames(fractions.care.by.race) <- c("Childcare", "Adult Care", "Both", "Any Care", "No Care")
# 
# rm(white.HHCC.participation, white.HHAC.participation, white.HHBOTH.participation, white.HHANYCARE.participation, white.HHNOCARE.participation,
#    black.HHCC.participation, black.HHAC.participation, black.HHBOTH.participation, black.HHANYCARE.participation, black.HHNOCARE.participation,
#    asian.HHCC.participation, asian.HHAC.participation, asian.HHBOTH.participation, asian.HHANYCARE.participation, asian.HHNOCARE.participation,
#    hispanic.HHCC.participation, hispanic.HHAC.participation, hispanic.HHBOTH.participation, hispanic.HHANYCARE.participation, hispanic.HHNOCARE.participation,
#    overall.HHCC.participation, overall.HHAC.participation, overall.HHBOTH.participation, overall.HHANYCARE.participation, overall.HHNOCARE.participation)
# 
# print("Fractions participating in informal caregiving by race (daily)"); fractions.care.by.race
# 
# library(RColorBrewer)
# pie.colors <- brewer.pal(4, "Accent")
# 
# pdf(paste0(getwd(), "/results/graphs and plots/Daily caregiving prevalence by race (ATUS 2011-2015).pdf"))
# par(mfrow = c(2, 2), "mar" = c(2, 2, 3, 2), oma = c(0, 0, 1, 0), cex.main = 1.35)
# pie(fractions.care.by.race[c(1,2,3,5), 1], col = pie.colors, radius = 1, main = "White (Non-Hispanic)            ", labels = paste0(round(fractions.care.by.race[c(1, 2, 3, 5), 1]*100), "%"), clockwise = TRUE, cex = 1.3)
# mtext(text = "N = 38,402", side = 1, line = -3, cex = 1)
# legend("topleft", bg = "white", legend = c("Childcare", "Adult Care", "Both", "No caregiving"), fill = pie.colors, cex = 1.1)
# pie(fractions.care.by.race[c(1,2,3,5), 2], col = pie.colors, radius = 1, main = "            Black (Non-Hispanic)", labels = paste0(round(fractions.care.by.race[c(1, 2, 3, 5), 2]*100), "%"), clockwise = TRUE, cex = 1.3)
# mtext(text = "N = 8,551", side = 1, line = -3, cex = 1)
# legend("topleft", bg = "white", legend = c("Childcare", "Adult Care", "Both", "No caregiving"), fill = pie.colors, cex = 1.1)
# pie(fractions.care.by.race[c(1,2,3,5), 3], col = pie.colors, radius = 1, main = "Asian (Non-Hispanic)            ", labels = paste0(round(fractions.care.by.race[c(1, 2, 3, 5), 3]*100), "%"), clockwise = TRUE, cex = 1.3)
# mtext(text = "N = 2,246", side = 1, line = -3, cex = 1)
# legend("topleft", bg = "white", legend = c("Childcare", "Adult Care", "Both", "No caregiving"), fill = pie.colors, cex = 1.1)
# pie(fractions.care.by.race[c(1,2,3,5), 4], col = pie.colors, radius = 1, main = "            Hispanic, any race", labels = paste0(round(fractions.care.by.race[c(1, 2, 3, 5), 4]*100), "%"), clockwise = TRUE, cex = 1.3)
# mtext(text = "N = 8,549", side = 1, line = -3, cex = 1)
# legend("topleft", bg = "white", legend = c("Childcare", "Adult Care", "Both", "No caregiving"), fill = pie.colors, cex = 1.1)
# title(main = "Daily caregiving prevalence \n by race & ethnicity", outer = TRUE, line = -1.5)
# mtext(text = "Total: 57,748", outer = TRUE, side = 1, line = -21, cex = 1.5)
# dev.off()



                  ##################################################
                  #### SURVEY WEIGHT DENOMINATOR COLUMN VECTORS ####
                  ##################################################

require(data.table)
HH.WGT.DF <- merge(ActSumFile[, c(1,7,12,22,456)], 
                   ActFile[(ActFile[30] & ActFile[31]) | ActFile[30] | ActFile[31],
                           c(1,30,31)],
                   by = c("TUCASEID"), sort = TRUE, all = FALSE)
HH.WGT.DF <- data.table(HH.WGT.DF, key = "TUCASEID")
HH.WGT.DF <- HH.WGT.DF[HH.WGT.DF$raceShort > 0, list(TEAGE = max(TEAGE), TESEX = max(TESEX), raceShort = max(raceShort, na.rm = TRUE),
                              TUFNWGTP = max(TUFNWGTP),
                              HH_CC = max(HH_CC), HH_AC = max(HH_AC)), by = TUCASEID]

HH.WGT.DF$AgeGrpResp <- as.integer(cut(HH.WGT.DF$TEAGE, breaks = c(seq(0, 85, 5), 120),
                                       right = FALSE, include.lowest = TRUE))


### Creating subsets of weight data to contain caregivers that satisfy care type conditions
CC.HH.WGT.DF <- subset.data.frame(HH.WGT.DF, subset = HH.WGT.DF$HH_CC==1)
AC.HH.WGT.DF <- subset.data.frame(HH.WGT.DF, subset = HH.WGT.DF$HH_AC==1)
SANDWICH.HH.WGT.DF <- subset.data.frame(HH.WGT.DF, subset = HH.WGT.DF$HH_CC==1 & HH.WGT.DF$HH_AC==1)

WGT.DF.LST <- list(CC.HH.WGT.DF, AC.HH.WGT.DF, SANDWICH.HH.WGT.DF, HH.WGT.DF)


WGT.MAT.LST <- lapply(WGT.DF.LST, function(x) {
  
  ### Setting up weight matrices for appropriate care types:
  # 1) Childcare weights (same respondents could have also provided adult care on separate occasions)
  # 2) Adult care weights (same respondents could have also provided childcare on separate occasions)
  # 3) Sandwich generation weights (respondents MUST have provided both childcare and adult care)
  # 4) Overall weights (total weights for all caregivers in the U.S. in the 4 racial/ethnic categories)
  
  # Making sure all groups are represented, even if no weight is generated (age/sex/race ID vector)
  agg_wgt_ID <- as.data.frame(matrix(c(rep(4:18, 8), rep(1, 60), rep(2, 60),
                                       rep(c(rep(1, 15), rep(2, 15), rep(3, 15), rep(4, 15)), 2)), 120, 3))
  AGGR.VEC <- paste(agg_wgt_ID$V1, agg_wgt_ID$V2, agg_wgt_ID$V3, sep = "") # [AgeGrpResp, TESEX, raceShort] combined ID
  agg_wgt_ID <- cbind(agg_wgt_ID, AGGR.VEC)
  
  # Generating a vector of sum weights by age/sex/race group
  agg_wgt_temp <- aggregate(x$TUFNWGTP, by=list(x$AgeGrpResp, x$TESEX, x$raceShort), 
                            sum, na.rm=FALSE)
  AGGR.VEC.TEMP <- paste(agg_wgt_temp$Group.1, agg_wgt_temp$Group.2, agg_wgt_temp$Group.3, sep = "")
  agg_wgt_temp <- as.data.frame(matrix(c(AGGR.VEC.TEMP, as.numeric(agg_wgt_temp$x)), nrow = NROW(agg_wgt_temp), ncol = 2),
                                stringsAsFactors = FALSE)
  
  # Matching aggregated weights with the corresponding age/sex/race from the full list
  agg_wgt_ID <- merge(agg_wgt_ID, agg_wgt_temp, by.x = "AGGR.VEC", by.y = "V1", all.x = TRUE)
  agg_wgt_ID[is.na(agg_wgt_ID)] <- 1
  for (i in 1:length(agg_wgt_ID)) agg_wgt_ID[,i] <- as.numeric(as.character(agg_wgt_ID[,i]))
  agg_wgt_ID <- agg_wgt_ID[order(agg_wgt_ID[3], agg_wgt_ID[4], agg_wgt_ID[2], decreasing = FALSE), ]
  
  # Converting the vectors into matrices of weights 
  # (columns represent sum of weights for each sex/race group of caregivers by age group)
  wgt_final <- matrix(0, 15, 8)
  for (i in 1:8) {
    wgt_final[,i] <- agg_wgt_ID[(i*15-14):(i*15), 5]
    
  }
  
  # Assigning labels to the weight matrices' rows and columns
  rownames(wgt_final) <- RowAgeLab
  
  RACE.NAMES <- list("WHITE", "BLACK", "ASIAN", "HISPANIC")
  SEX.NAMES <- list("MALE", "FEMALE")
  
  wgt.col.names <- unlist(lapply(SEX.NAMES, function(Z) {
    lapply(RACE.NAMES, function(Y) {
      paste(Z, Y, sep = ".")
    })
  }), recursive = TRUE)
  colnames(wgt_final) <- wgt.col.names
  
  return(wgt_final)
  
  
})

# Reformatting the list of weight sums by age/sex/race/care type into an equivalent array
WGT.MAT.ARRAY <- array(unlist(WGT.MAT.LST), dim = c(nrow(WGT.MAT.LST[[1]]), ncol(WGT.MAT.LST[[1]]), length(WGT.MAT.LST)))
dimnames(WGT.MAT.ARRAY) <- list(RowAgeLab, colnames(WGT.MAT.LST[[1]]),
                                c("CHILDCARE WEIGHTS", "ADULT CARE WEIGHTS", "SANDWICH WEIGHTS", "OVERALL WEIGHTS"))

ActSumFile$AgeGrpResp <- as.integer(cut(ActSumFile$TEAGE, breaks = c(seq(0, 85, 5), 120),
                                       right = FALSE, include.lowest = TRUE))

## US.wgt.den.mat will store weights for estimating US time averages, regardless of caregiving status
US.wgt <- aggregate(ActSumFile$TUFNWGTP, by=list(ActSumFile$AgeGrpResp, ActSumFile$TESEX, ActSumFile$raceShort), sum, na.rm = FALSE)
US.wgt.den.mat <- matrix(US.wgt[31:150, 4], 15, 8)
rownames(US.wgt.den.mat) <- RowAgeLab
colnames(US.wgt.den.mat) <- c("US.whitemale.wgt", "US.whitefemale.wgt", "US.blackmale.wgt", "US.blackfemale.wgt",
                              "US.asianmale.wgt", "US.asianfemale.wgt", "US.hispmale.wgt", "US.hispfemale.wgt")


                    ##############################################
                    ############### Sample sizes #################
                    ##############################################

# sample.colnames <- c("AgeGroup", "Race", "Sex", "Count")
# sample.array <- array(0, dim = c(15, 8, 5))
# 
# ## Check sample size by age, sex, and race for the ENTIRE U.S. SAMPLE (only for the 4 race/ethnicity categories)
# sample.size.all <- data.frame(table(list(ActSumFile$AgeGrpResp[ActSumFile$raceShort > 0],
#                                          ActSumFile$raceShort[ActSumFile$raceShort > 0],
#                                          ActSumFile$TESEX[ActSumFile$raceShort > 0])))
# colnames(sample.size.all) <-  sample.colnames
# sample.size.all
# 
# ## Check sample size by age, sex, and race for ALL CAREGIVERS subset (only for the 4 race/ethnicity categories)
# sample.size.caregivers <- data.frame(table(list(HH.WGT.DF$AgeGrpResp[HH.WGT.DF$raceShort > 0],
#                                                 HH.WGT.DF$raceShort[HH.WGT.DF$raceShort > 0],
#                                                 HH.WGT.DF$TESEX[HH.WGT.DF$raceShort > 0])))
# colnames(sample.size.caregivers) <- sample.colnames
# 
# 
# # Sample for CHILDCARE CAREGIVERS (only for the 4 race/ethnicity categories)
# sample.size.caregivers.CC <- with(subset(HH.WGT.DF[HH.WGT.DF$HH_CC == 1 & HH.WGT.DF$raceShort > 0, ]),
#                                   data.frame(table(list(AgeGrpResp, raceShort, TESEX))))
# colnames(sample.size.caregivers.CC) <- sample.colnames
# 
# 
# # Sample for ADULT CARE CAREGIVERS (only for the 4 race/ethnicity categories)
# sample.size.caregivers.AC <- with(subset(HH.WGT.DF[HH.WGT.DF$HH_AC == 1 & HH.WGT.DF$raceShort > 0, ]),
#                                   data.frame(table(list(AgeGrpResp, raceShort, TESEX))))
# colnames(sample.size.caregivers.AC) <- sample.colnames
# 
# 
# # Sample for SANDWICH CAREGIVERS (only for the 4 race/ethnicity categories)
# sample.size.caregivers.sandwich <- with(subset(HH.WGT.DF[HH.WGT.DF$HH_CC == 1 & HH.WGT.DF$HH_AC == 1 & HH.WGT.DF$raceShort>0, ]),
#                                         data.frame(table(list(AgeGrpResp, raceShort, TESEX))))
# colnames(sample.size.caregivers.sandwich) <- sample.colnames
# 
# 
# # Combining the samples size tables into an array
# sample.array <- array(c(sample.size.caregivers.CC[, 4], sample.size.caregivers.AC[, 4], sample.size.caregivers[,4], sample.size.all[,4]),
#                       dim = c(15, 8, 4), dimnames = list(RowAgeLab, names(WGT.MAT.ARRAY[1,,1]), 
#                       c("CHILDCARE CAREGIVERS", "ADULT CARE CAREGIVERS", "ALL CAREGIVERS", "ENTIRE SAMPLE")))
# 
# print("Array containing survey sample distribution by age, sex, and race"); sample.array
# 
# 
# # Export sample size array to CSV
# # write.csv(sample.array, file=paste0(getwd(), "/results/", "HH_sample_size_array.csv"), sep = ",", row.names = TRUE, col.names = TRUE)
# 
# # Sample size array (all 0 replaced with 1) that can be used as denominator (if ATUS weights are not used)
# sample.array.wgt <- ifelse(sample.array == 0, 1, sample.array)





                    ###############################################
                    #### General setup for numerator matrices #####
                    ###############################################


## Creating a new dataframe containg variables necessary to construct final matrices

# Checking/Pre-sorting on key variables
ActFile <- ActFile[order(ActFile$TUCASEID, ActFile$TUACTIVITY_N, decreasing = FALSE), ]
ActSumFile <- ActSumFile[order(ActSumFile$TUCASEID, decreasing = FALSE), ]
WhoFile <- WhoFile[order(WhoFile$TUCASEID, WhoFile$TUACTIVITY_N, WhoFile$TULINENO, decreasing = FALSE), ]
RosterFile <- RosterFile[order(RosterFile$TUCASEID, RosterFile$TULINENO, decreasing = FALSE), ]                          

### Setting up a new data frame (HH-focused only for the moment)
transfers <- data.frame(WhoFile)
transfers <- within(transfers, rm(TRWHONA)) 

## Merging WhoFile and RosterFile for each activity: HH participants (and own NonHH children) 
## whose age and sex are known
transfers <- merge(transfers, RosterFile, by = c("TUCASEID", "TULINENO"), sort = FALSE, all = FALSE)

## Merging transfers and Activity Summary File (importing Age/Sex, Race, and survey weight
## of each respondent)
transfers <- merge(transfers, ActSumFile, by = "TUCASEID", sort = FALSE, all = FALSE)
transfers <- transfers[, c(1:4, 6, 7, 13, 18, 28, 462)]

## Merging transfers and Acivity File (importing activity duration and classification)
transfers <- merge(transfers, ActFile, by = c("TUCASEID", "TUACTIVITY_N"), sort = TRUE, all = FALSE)
transfers <- transfers[, c(1:10, 25, 38:41)]

### DO NOT ENABLE WHEN RUNNING MATRICES OR PROFILES - NEEDS FIXING - ADJUST VARABLES TO BE RETAINED
# ## Merging transfers and CPS file (importing respodent's and HH members' education and immigration status/nativity)
# transfers <- merge(transfers, subset(CPS, subset = CPS$TULINENO == 1)[ , c("TUCASEID", "NATIVITY", "EDUCATION")],
#                    by = "TUCASEID", sort = TRUE, all.x = TRUE, all.y = FALSE)


# Assigning respondents to 5-year age group categories
transfers$AgeGrpResp <- as.integer(cut(transfers$TEAGE.y, breaks = c(seq(0, 85, 5), 120),
                            right = FALSE, include.lowest = TRUE))

# Assigning all present during activities to 5-year age group categories 
transfers$AgeGrp <- as.integer(cut(transfers$TEAGE.x, breaks = c(seq(0, 85, 5), 120),
                            right = FALSE, include.lowest = TRUE))

transfers <- transfers[order(transfers$TUCASEID, transfers$TUACTIVITY_N, transfers$TULINENO, decreasing = FALSE), ]                          

## Creating a unique ID combination for each activity in the data set
transfers$ID2 <- paste(transfers$TUCASEID, transfers$TUACTIVITY_N, sep = ":")





                               #######################
                               ###### IMPORTANT ######
                               #######################

#      Select race for the analysis of childcare and adult care:
#      Change race "number" to:
#        1 (White, Non-Hispanic),
#        2 (Black/African American, Non-Hispanic),
#        3 (Asian, Non-Hispanic),
#        4 (Hispanic, any race)
#
#   OR comment out by inserting "#" in front of the entire line of code to run 
#                       the analysis for the overall population of caregivers

#   race <- 1; transfers <- subset(transfers[transfers$raceShort == race, ])

#      NOW RERUN THE CODE STARTING FROM "General setup for numerator matrices" SECTION


                            #############################
                            #### HOUSEHOLD CHILDCARE ####
                            #############################


transfers.HH_CC <- subset(transfers[transfers$HH_CC == TRUE, ])
transfers.HH_CC <- transfers.HH_CC[order(transfers.HH_CC$ID2, decreasing = FALSE), ]

transList <- split(transfers.HH_CC, transfers.HH_CC$ID2)

childWHO_CODE <- c(22, 23, 25, 26, 27, 30)          # "Who" relation codes from ATUS lexicon, directly or potentially
                                                    # indicating children

HH.CC.lst <- lapply(transList, function(tx) {

  personNUM <- tx$TEAGE.x < 18 & tx$TUWHO_CODE %in% childWHO_CODE  # Selecting children who received care
  den <- sum(personNUM)                                            # Number of children who potentially received care in an activity
  rec_min <- tx$TUACTDUR[1]/den                                    # Number of minutes potentially received by each child under 18 present
  tx$minPer <- 0*personNUM                                         # Making sure the non-qualifying time entries per respondent ID are excluded from the data
  tx$minPer[personNUM] <- rec_min * tx$TUFNWGTP[1]                 # Multiplying the time entries by the caregiver(respondent) survey weight 
  
  return(tx)
  
})

HH.CC.data <- unsplit(HH.CC.lst, transfers.HH_CC$ID2)
HH.CC.data$tx.count <- ifelse(HH.CC.data$minPer > 0, 1, 0)    # flagging transfer instances


## HH Childcare Male to Male
HH.CC.TIME.MM <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
                                HH.CC.data$TEAGE.x < 18 &
                                HH.CC.data$TESEX.x == 1 & HH.CC.data$TESEX.y == 1,],
                          tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
HH.CC.TIME.MM[is.na(HH.CC.TIME.MM)] <- 0

# Number of transfers
HH.CC.NUMTRANSFERS.MM <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
                                           HH.CC.data$TEAGE.x < 18 &
                                           HH.CC.data$TESEX.x == 1 & HH.CC.data$TESEX.y == 1,],
                              tapply(tx.count, list(AgeGrpResp, AgeGrp), sum))
HH.CC.NUMTRANSFERS.MM[is.na(HH.CC.NUMTRANSFERS.MM)] <- 0


## HH Childcare Male to Female
HH.CC.TIME.MF <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
                                   HH.CC.data$TEAGE.x < 18 &
                                   HH.CC.data$TESEX.x == 2 & HH.CC.data$TESEX.y == 1,],
                      tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
HH.CC.TIME.MF[is.na(HH.CC.TIME.MF)] <- 0

# Number of transfers
HH.CC.NUMTRANSFERS.MF <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
                                           HH.CC.data$TEAGE.x < 18 &
                                           HH.CC.data$TESEX.x == 2 & HH.CC.data$TESEX.y == 1,],
                              tapply(tx.count, list(AgeGrpResp, AgeGrp), sum))
HH.CC.NUMTRANSFERS.MF[is.na(HH.CC.NUMTRANSFERS.MF)] <- 0


## HH Childcare Female to Male
HH.CC.TIME.FM <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
                                   HH.CC.data$TEAGE.x < 18 &
                                   HH.CC.data$TESEX.x == 1 & HH.CC.data$TESEX.y == 2,],
                      tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
HH.CC.TIME.FM[is.na(HH.CC.TIME.FM)] <- 0

# Number of transfers
HH.CC.NUMTRANSFERS.FM <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
                                           HH.CC.data$TEAGE.x < 18 &
                                           HH.CC.data$TESEX.x == 1 & HH.CC.data$TESEX.y == 2,],
                              tapply(tx.count, list(AgeGrpResp, AgeGrp), sum))
HH.CC.NUMTRANSFERS.FM[is.na(HH.CC.NUMTRANSFERS.FM)] <- 0


## HH Childcare Female to Female
HH.CC.TIME.FF <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
                                   HH.CC.data$TEAGE.x < 18 &
                                   HH.CC.data$TESEX.x == 2 & HH.CC.data$TESEX.y == 2,],
                      tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
HH.CC.TIME.FF[is.na(HH.CC.TIME.FF)] <- 0

# Number of transfers
HH.CC.NUMTRANSFERS.FF <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
                                           HH.CC.data$TEAGE.x < 18 &
                                           HH.CC.data$TESEX.x == 2 & HH.CC.data$TESEX.y == 2,],
                              tapply(tx.count, list(AgeGrpResp, AgeGrp), sum))
HH.CC.NUMTRANSFERS.FF[is.na(HH.CC.NUMTRANSFERS.FF)] <- 0



# Function to check for any absent columns and inserting empty ones to retain matrix structure
absent.cols.fun.CC <- function(m) {
  if (length(m) > 0) {
    
    cols.names <- c(1, 2, 3, 4)
    m <- as.data.frame(m)
    absent <- setdiff(cols.names, names(m)) 
  
    if (length(absent) > 0) {
      m <- cbind(m, as.data.frame(matrix(0, NROW(m), length(absent))))
      colnames(m) <- c(colnames(m[1:(length(m)-length(absent))]), absent)
    } else {
      m <- m
    }
  
    m <- m[, as.character(cols.names)]
    m <- as.matrix(m)
  
    } else if (length(m) == 0) {    # Preventing 0x0 matrices, if no tranfers are registered
      m <- matrix(0, 15, 4)
      colnames(m) <- seq(1, 4, 1)
      rownames(m) <- seq(4, 18, 1)
    }
  
  return(m)
}


## Constructing numerator matrices by age/sex and race for childcare

## List containing matrices of sums of weight-multiplied childcare time by sex of caregiver and care recipient
HH.CC.MAT.LST <- lapply(list(HH.CC.TIME.MM, HH.CC.TIME.MF, HH.CC.TIME.FM, HH.CC.TIME.FF), absent.cols.fun.CC)
HH.CC.MAT.LST <- lapply(HH.CC.MAT.LST, function(x) {
  CCmat <- matrix(0, 15, 4)
  rownames(CCmat) <- seq(4, 18, 1)
  colnames(CCmat) <- seq(1, 4, 1)
  mcol <- match(colnames(x), colnames(CCmat))
  mrow <- match(rownames(x), rownames(CCmat))
  CCmat[mrow, ] <- x + CCmat[mrow, ]
  x <- CCmat
  
  rownames(x) <- RowAgeLab[1:nrow(x)]
  colnames(x) <- ColAgeLab[1:ncol(x)]
  
  return(x)
})

HH.CC.MAT.NUMTRANSF.LST <- lapply(list(HH.CC.NUMTRANSFERS.MM, HH.CC.NUMTRANSFERS.MF, HH.CC.NUMTRANSFERS.FM, HH.CC.NUMTRANSFERS.FF), absent.cols.fun.CC)
HH.CC.MAT.NUMTRANSF.LST <- lapply(HH.CC.MAT.NUMTRANSF.LST, function(x) {
  CCmat <- matrix(0, 15, 4)
  rownames(CCmat) <- seq(4, 18, 1)
  colnames(CCmat) <- seq(1, 4, 1)
  mcol <- match(colnames(x), colnames(CCmat))
  mrow <- match(rownames(x), rownames(CCmat))
  CCmat[mrow, ] <- x + CCmat[mrow, ]
  x <- CCmat
  
  rownames(x) <- RowAgeLab[1:nrow(x)]
  colnames(x) <- ColAgeLab[1:ncol(x)]
  
  return(x)
})

names(HH.CC.MAT.LST) <- c("Male.to.Male", "Male.to.Female", "Female.to.Male", "Female.to.Female")
names(HH.CC.MAT.NUMTRANSF.LST) <- c("Male.to.Male", "Male.to.Female", "Female.to.Male", "Female.to.Female")



                            ##############################
                            #### HOUSEHOLD ADULT CARE ####
                            ##############################


transfers.HH_AC <- subset(transfers[transfers$HH_AC == TRUE, ])
transfers.HH_AC <- transfers.HH_AC[order(transfers.HH_AC$ID2, decreasing = FALSE), ]

transList <- split(transfers.HH_AC, transfers.HH_AC$ID2)

adultWHO_CODE <- c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)       # "Who" relation codes from ATUS lexicon, directly or potentially
                                                                     # indicating adults who received care

HH.AC.lst <- lapply(transList, function(tx) {
  
  personNUM <- tx$TEAGE.x >= 18 & tx$TUWHO_CODE %in% adultWHO_CODE   # Selecting adults who received care
  den <- sum(personNUM)                                              # Number of adults who potentially received care in an activity
  rec_min <- tx$TUACTDUR[1]/den                                      # Number of minutes potentially received by each adult 18+ present
  tx$minPer <- 0*personNUM                                           # Making sure the non-qualifying time entries per respondent ID are excluded from the data
  tx$minPer[personNUM] <- rec_min * tx$TUFNWGTP[1]                   # Multiplying the time entries by the caregiver(respondent) survey weight 
  
  return(tx)

})

HH.AC.data <- unsplit(HH.AC.lst, transfers.HH_AC$ID2)
HH.AC.data$tx.count <- ifelse(HH.AC.data$minPer > 0, 1, 0)    # flagging transfer instances

## HH Adult Care Male to Male
HH.AC.TIME.MM <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
                                   HH.AC.data$TEAGE.x >= 18 &
                                   HH.AC.data$TESEX.x == 1 & HH.AC.data$TESEX.y == 1,],
                      tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
HH.AC.TIME.MM[is.na(HH.AC.TIME.MM)] <- 0

# Number of transfers
HH.AC.NUMTRANSFERS.MM <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
                                        HH.AC.data$TEAGE.x >= 18 &
                                        HH.AC.data$TESEX.x == 1 & HH.AC.data$TESEX.y == 1,],
                           tapply(tx.count, list(AgeGrpResp, AgeGrp), sum))
HH.AC.NUMTRANSFERS.MM[is.na(HH.AC.NUMTRANSFERS.MM)] <- 0


## HH Adult Care Male to Female
HH.AC.TIME.MF <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
                                   HH.AC.data$TEAGE.x >= 18 &
                                   HH.AC.data$TESEX.x == 2 & HH.AC.data$TESEX.y == 1,],
                      tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
HH.AC.TIME.MF[is.na(HH.AC.TIME.MF)] <- 0

# Number of transfers
HH.AC.NUMTRANSFERS.MF <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
                                           HH.AC.data$TEAGE.x >= 18 &
                                           HH.AC.data$TESEX.x == 2 & HH.AC.data$TESEX.y == 1,],
                              tapply(tx.count, list(AgeGrpResp, AgeGrp), sum))
HH.AC.NUMTRANSFERS.MF[is.na(HH.AC.NUMTRANSFERS.MF)] <- 0


## HH Adult Care Female to Male
HH.AC.TIME.FM <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
                                   HH.AC.data$TEAGE.x >= 18 &
                                   HH.AC.data$TESEX.x == 1 & HH.AC.data$TESEX.y == 2,],
                      tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
HH.AC.TIME.FM[is.na(HH.AC.TIME.FM)] <- 0

# Number of transfers
HH.AC.NUMTRANSFERS.FM <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
                                           HH.AC.data$TEAGE.x >= 18 &
                                           HH.AC.data$TESEX.x == 1 & HH.AC.data$TESEX.y == 2,],
                              tapply(tx.count, list(AgeGrpResp, AgeGrp), sum))
HH.AC.NUMTRANSFERS.FM[is.na(HH.AC.NUMTRANSFERS.FM)] <- 0



## HH Adult Care Female to Female
HH.AC.TIME.FF <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
                                   HH.AC.data$TEAGE.x >= 18 &
                                   HH.AC.data$TESEX.x == 2 & HH.AC.data$TESEX.y == 2,],
                      tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
HH.AC.TIME.FF[is.na(HH.AC.TIME.FF)] <- 0

# Number of transfers
HH.AC.NUMTRANSFERS.FF <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
                                           HH.AC.data$TEAGE.x >= 18 &
                                           HH.AC.data$TESEX.x == 2 & HH.AC.data$TESEX.y == 2,],
                              tapply(tx.count, list(AgeGrpResp, AgeGrp), sum))
HH.AC.NUMTRANSFERS.FF[is.na(HH.AC.NUMTRANSFERS.FF)] <- 0



# Function to check for any absent columns and inserting empty ones to retain matrix structure
absent.cols.fun.AC <- function(m) {
  if (length(m) > 0) {
    
    cols.names <- seq(4, 18, 1)
    m <- as.data.frame(m)
    absent <- setdiff(cols.names, names(m)) 
  
    if (length(absent) > 0) {
      m <- cbind(m, as.data.frame(matrix(0, NROW(m), length(absent))))
      colnames(m) <- c(colnames(m[1:(length(m)-length(absent))]), absent)
    } else {
      m <- m
    }
  
    m <- m[, as.character(cols.names)]
    m <- as.matrix(m)
  
  } else if (length(m) == 0) {    # Preventing 0x0 matrices, if no tranfers are registered
    m <- matrix(0, 15, 15)
    colnames(m) <- seq(4, 18, 1)
    rownames(m) <- seq(4, 18, 1)
  }
  
  return(m)
}


## Constructing numerator matrices by age/sex and race for adult care

## List containing matrices of sums of weight-multiplied adult care time by sex of caregiver and care recipient
HH.AC.MAT.LST <- lapply(list(HH.AC.TIME.MM, HH.AC.TIME.MF, HH.AC.TIME.FM, HH.AC.TIME.FF), absent.cols.fun.AC)
HH.AC.MAT.LST <- lapply(HH.AC.MAT.LST, function(x) {
  ACmat <- matrix(0, 15, 15)
  rownames(ACmat) <- seq(4, 18, 1)
  colnames(ACmat) <- seq(4, 18, 1)
  mcol <- match(colnames(x), colnames(ACmat))
  mrow <- match(rownames(x), rownames(ACmat))
  ACmat[mrow, mcol] <- x + ACmat[mrow, mcol]
  x <- ACmat
  
  rownames(x) <- RowAgeLab[1:nrow(x)]
  colnames(x) <- ColAgeLab[4:18]
  
  return(x)
})

HH.AC.MAT.NUMTRANSF.LST <- lapply(list(HH.AC.NUMTRANSFERS.MM, HH.AC.NUMTRANSFERS.MF, HH.AC.NUMTRANSFERS.FM, HH.AC.NUMTRANSFERS.FF), absent.cols.fun.AC)
HH.AC.MAT.NUMTRANSF.LST <- lapply(HH.AC.MAT.NUMTRANSF.LST, function(x) {
  ACmat <- matrix(0, 15, 15)
  rownames(ACmat) <- seq(4, 18, 1)
  colnames(ACmat) <- seq(4, 18, 1)
  mcol <- match(colnames(x), colnames(ACmat))
  mrow <- match(rownames(x), rownames(ACmat))
  ACmat[mrow, mcol] <- x + ACmat[mrow, mcol]
  x <- ACmat
  
  rownames(x) <- RowAgeLab[1:nrow(x)]
  colnames(x) <- ColAgeLab[4:18]
  
  return(x)
})

names(HH.AC.MAT.LST) <- c("Male.to.Male", "Male.to.Female", "Female.to.Male", "Female.to.Female")
names(HH.AC.MAT.NUMTRANSF.LST) <- c("Male.to.Male", "Male.to.Female", "Female.to.Male", "Female.to.Female")



                      ######################################################
                      #### OVERALL WEIGHT-MULTIPLIED NUMERATOR MATRICES ####
                      ######################################################

# Setting up empty shell for the overall numerator matrix array, containing sums of weight-multiplied time
HH.NUM.ARRAY <- array(0, dim = c(15, 18, 4))
HH.NUMTRANSF.ARRAY <- array(0, dim = c(15, 18, 4))  # Empty shell to fill in the number of transfer instances

# Populating the overall numerator matrix array with appropriate values for each age/sex group by transfer type
for (i in 1:4) {
  HH.NUM.ARRAY[, 1:3, i] <- HH.CC.MAT.LST[[i]][,1:3]
  HH.NUM.ARRAY[, 4, i] <- HH.CC.MAT.LST[[i]][,4] + HH.AC.MAT.LST[[i]][,1]     # Weight-multiplied sums for childcare and adult care portion of 15-19 y.o.
  HH.NUM.ARRAY[, 5:18, i] <- HH.AC.MAT.LST[[i]][, 2:15]
  
  # Combining lists of matrix transfer instances into an overall array by age and sex
  HH.NUMTRANSF.ARRAY[, 1:3, i] <- HH.CC.MAT.NUMTRANSF.LST[[i]][,1:3]
  HH.NUMTRANSF.ARRAY[, 4, i] <- HH.CC.MAT.NUMTRANSF.LST[[i]][,4] + HH.AC.MAT.NUMTRANSF.LST[[i]][,1]
  HH.NUMTRANSF.ARRAY[, 5:18, i] <- HH.AC.MAT.NUMTRANSF.LST[[i]][, 2:15]
}

# Labeling the numerator matrix and transfer instance arrays
rownames(HH.NUM.ARRAY) <- rownames(HH.NUMTRANSF.ARRAY) <- RowAgeLab
colnames(HH.NUM.ARRAY) <- colnames(HH.NUMTRANSF.ARRAY) <- ColAgeLab
dimnames(HH.NUM.ARRAY)[[3]] <- dimnames(HH.NUMTRANSF.ARRAY)[[3]] <- c("Male.to.Male", "Male.to.Female", "Female.to.Male", "Female.to.Female")



          ###############################################################################
          #### MATRICES OF AVERAGE TIME PRODUCTION/CONSUMPTION BY AGE, SEX, and RACE ####
          ###############################################################################




### Weights for US-level averages by race
US.wgt.cc <- aggregate(ActSumFile$TUFNWGTP[ActSumFile$raceShort==race], 
                       by=list(ActSumFile$AgeGrpResp[ActSumFile$raceShort==race],
                               ActSumFile$TESEX[ActSumFile$raceShort==race],
                               ActSumFile$raceShort[ActSumFile$raceShort==race]), sum, na.rm = FALSE)

US.wgt.ac <- aggregate(ActSumFile$TUFNWGTP[ActSumFile$raceShort==race], 
                       by=list(ActSumFile$AgeGrpResp[ActSumFile$raceShort==race],
                               ActSumFile$TESEX[ActSumFile$raceShort==race],
                               ActSumFile$raceShort[ActSumFile$raceShort==race]), sum, na.rm = FALSE)


missing.row.fun <- function(x, race = race) {
  y <- data.frame(matrix(0, 30, 4))
  y[1:NROW(x),] <- x
  w <- data.frame(rep(seq(4, 18, 1), 2), c(rep(1,15), rep(2, 15)), rep(race, 30))
  colnames(w) <- colnames(y)[1:3]
  z <- merge(w, y, by = c("X1","X2","X3"), all.x = TRUE)
  z[is.na(z$X4), 4] <- 1
  z <- z[order(z[,3], z[,2], z[,1], decreasing = FALSE), ]
  return(z)
}

### Composite weight for the US for particular race
US.wgt.cc <- missing.row.fun(US.wgt.cc, race)
US.wgt.ac <- missing.row.fun(US.wgt.ac, race)
US.wgt.mat.race <- cbind(matrix(US.wgt.cc[,4], 15, 2), matrix(US.wgt.ac[,4], 15, 2))
rownames(US.wgt.mat.race) <- RowAgeLab
colnames(US.wgt.mat.race) <- c(paste0("US.male.cc.race", race, ".wgt"), paste0("US.female.cc.race", race, ".wgt"),
                               paste0("US.male.ac.race", race, ".wgt"), paste0("US.female.ac.race", race, ".wgt"))


####### FINAL.OUTPUT represents a set of transfer matrices with national averages for specific race/sex group. 
####### If no RACE is specified (i.e. commented out ~line 467), the US.OVERALL output will be
####### generated with averages representing national totals.


FINAL.OUTPUT <- array(0, dim = c(15, 18, 4))
FINAL.OUTPUT.US <- FINAL.OUTPUT                   # Empty array for US-level averages by race, regardless of caregiving status
dimnames(FINAL.OUTPUT) <- dimnames(FINAL.OUTPUT.US) <- dimnames(HH.NUM.ARRAY)
US.OVERALL <- FINAL.OUTPUT

### Overall INTRA-HOUSEHOLD informal care time (average among caregivers)

if ((max(range(transfers$raceShort, na.rm = TRUE)) - min(range(transfers$raceShort, na.rm = TRUE))) > 1) {   # Weight vectors for overall
    ActSumFile$CHILDCARE <- ActSumFile$HHCC_time > 0
    ActSumFile$ADULTCARE <- ActSumFile$HHAC_time > 0
    
    cc.wgt <- merge(data.frame(Group.1 = rep(levels(as.factor(ActSumFile$AgeGrpResp)), 2),
                             Group.2 = c(rep(levels(as.factor(ActSumFile$TESEX))[1], 15), rep(levels(as.factor(ActSumFile$TESEX))[2], 15)),
                             Group.3 = c(rep(levels(as.factor(ActSumFile$CHILDCARE))[1], 30), rep(levels(as.factor(ActSumFile$CHILDCARE))[2], 30))),
                             aggregate(ActSumFile$TUFNWGTP,
                                       by=list(ActSumFile$AgeGrpResp, ActSumFile$TESEX, ActSumFile$CHILDCARE),
                                       sum, na.rm=FALSE), all.x = TRUE)
    cc.wgt <- cc.wgt[cc.wgt$Group.3 == TRUE, c(1, 2, 4)]
    cc.wgt[is.na(cc.wgt)] <- 1
    cc.wgt <- cc.wgt[order(as.integer(cc.wgt$Group.2), as.numeric(as.character(cc.wgt$Group.1)), decreasing = FALSE), ]
    
    ac.wgt <- merge(data.frame(Group.1 = rep(levels(as.factor(ActSumFile$AgeGrpResp)), 2),
                               Group.2 = c(rep(levels(as.factor(ActSumFile$TESEX))[1], 15), rep(levels(as.factor(ActSumFile$TESEX))[2], 15)),
                               Group.3 = c(rep(levels(as.factor(ActSumFile$ADULTCARE))[1], 30), rep(levels(as.factor(ActSumFile$ADULTCARE))[2], 30))),
                    aggregate(ActSumFile$TUFNWGTP,
                              by=list(ActSumFile$AgeGrpResp, ActSumFile$TESEX, ActSumFile$ADULTCARE),
                              sum, na.rm=FALSE), all.x = TRUE)
    ac.wgt <- ac.wgt[ac.wgt$Group.3 == TRUE, c(1, 2, 4)]
    ac.wgt[is.na(ac.wgt)] <- 1
    ac.wgt <- ac.wgt[order(as.integer(ac.wgt$Group.2), as.numeric(as.character(ac.wgt$Group.1)), decreasing = FALSE), ]
    
    CC.male.wgt <- cc.wgt[1:15, 3]
    CC.female.wgt <- cc.wgt[16:30, 3]
    AC.male.wgt <- ac.wgt[1:15, 3]
    AC.female.wgt <- ac.wgt[16:30, 3]
    
    rm(cc.wgt, ac.wgt)
    
    RACE <- "All Caregivers"
    RACE.FILENAME <- "All_caregivers"
    
    ## DO NOT USE - NEEDS FIXING Activate only when no ATUS weights are used for the numerator
#    CC.male.wgt <- rowSums(sample.array.wgt[, 1:4, 1])
#    CC.female.wgt <- rowSums(sample.array.wgt[, 5:8, 1])
#    AC.male.wgt <- rowSums(sample.array.wgt[, 1:4, 2])
#    AC.female.wgt <- rowSums((sample.array.wgt[, 5:8, 2]))
    
    
  } else if (median(transfers$raceShort) == 1) {        # Weight vectors for Non-Hispanic whites
    CC.male.wgt <- WGT.MAT.ARRAY[, 1, 1]
    CC.female.wgt <- WGT.MAT.ARRAY[, 5, 1]
    AC.male.wgt <- WGT.MAT.ARRAY[, 1, 2]
    AC.female.wgt <- WGT.MAT.ARRAY[, 5, 2]
    
    RACE <- "White caregivers (Non-Hispanic)"
    RACE.FILENAME <- "White_caregivers"
    
    
    ## Activate only when no ATUS weights are used for the numerator
#    CC.male.wgt <- sample.array.wgt[, 1, 1]
#    CC.female.wgt <- sample.array.wgt[, 5, 1]
#    AC.male.wgt <- sample.array.wgt[, 1, 2]
#    AC.female.wgt <- sample.array.wgt[, 5, 2]   
    
    
  } else if (median(transfers$raceShort) == 2) {        # Weight vectors for Non-Hispanic blacks/African-Americans
    CC.male.wgt <- WGT.MAT.ARRAY[, 2, 1]
    CC.female.wgt <- WGT.MAT.ARRAY[, 6, 1]
    AC.male.wgt <- WGT.MAT.ARRAY[, 2, 2]
    AC.female.wgt <- WGT.MAT.ARRAY[, 6, 2]
    
    RACE = "Black caregivers (Non-Hispanic)"
    RACE.FILENAME <- "Black_or_African_American_caregivers"
    
    ## Activate only when no ATUS weights are used for the numerator
#    CC.male.wgt <- sample.array.wgt[, 2, 1]
#    CC.female.wgt <- sample.array.wgt[, 6, 1]
#    AC.male.wgt <- sample.array.wgt[, 2, 2]
#    AC.female.wgt <- sample.array.wgt[, 6, 2] 
    

  } else if (median(transfers$raceShort) == 3) {        # Weight vectors for Non-Hispanic Asians
    CC.male.wgt <- WGT.MAT.ARRAY[, 3, 1]
    CC.female.wgt <- WGT.MAT.ARRAY[, 7, 1] 
    AC.male.wgt <- WGT.MAT.ARRAY[, 3, 2]
    AC.female.wgt <- WGT.MAT.ARRAY[, 7, 2]

    RACE <- "Asian caregivers (Non-Hispanic)"
    RACE.FILENAME <- "Asian_caregivers"
    
    ## Activate only when no ATUS weights are used for the numerator
#    CC.male.wgt <- sample.array.wgt[, 3, 1]
#    CC.female.wgt <- sample.array.wgt[, 7, 1]
#    AC.male.wgt <- sample.array.wgt[, 3, 2]
#    AC.female.wgt <- sample.array.wgt[, 7, 2] 
    
  } else if (median(transfers$raceShort) == 4) {        # Weight vectors for Hispanic, any race
    CC.male.wgt <- WGT.MAT.ARRAY[, 4, 1]
    CC.female.wgt <- WGT.MAT.ARRAY[, 8, 1] 
    AC.male.wgt <- WGT.MAT.ARRAY[, 4, 2]
    AC.female.wgt <- WGT.MAT.ARRAY[, 8, 2]
    
    RACE <- "Hispanic caregivers (any race)"
    RACE.FILENAME <- "Hispanic_caregivers"
    
    ## Activate only when no ATUS weights are used for the numerator
#    CC.male.wgt <- sample.array.wgt[, 4, 1]
#    CC.female.wgt <- sample.array.wgt[, 8, 1]
#    AC.male.wgt <- sample.array.wgt[, 4, 2]
#    AC.female.wgt <- sample.array.wgt[, 8, 2] 
}


## Male caregiver to child groups from 0-4 to 10-14
for (i in 1:3) {
  # To Male recipient
  FINAL.OUTPUT[, i, 1] <- HH.NUM.ARRAY[, i, 1] / CC.male.wgt
  # To Female recipient
  FINAL.OUTPUT[, i, 2] <- HH.NUM.ARRAY[, i, 2] / CC.male.wgt
}

## Male caregiver to group 15-19 (combining childcare and adult care)
# To Male recipient
FINAL.OUTPUT[, 4, 1] <- HH.NUM.ARRAY[, 4, 1] / (CC.male.wgt + AC.male.wgt)
# To Female recipient
FINAL.OUTPUT[, 4, 2] <- HH.NUM.ARRAY[, 4, 2] / (CC.male.wgt + AC.male.wgt)
  

## Male caregiver to adult groups from 20-24 to 85+
for (i in 5:18) {
  # To Male recipient
  FINAL.OUTPUT[, i, 1] <- HH.NUM.ARRAY[, i, 1] / AC.male.wgt
  # To Female recipient
  FINAL.OUTPUT[, i, 2] <- HH.NUM.ARRAY[, i, 2] / AC.male.wgt
}


## Female caregiver to groups from 0-4 to 10-14
for (i in 1:3) {
  # To Male recipient
  FINAL.OUTPUT[, i, 3] <- HH.NUM.ARRAY[, i, 3] / CC.female.wgt
  # To Female recipient
  FINAL.OUTPUT[, i, 4] <- HH.NUM.ARRAY[, i, 4] / CC.female.wgt
}

## Female caregiver to group 15-19 (combining childcare and adult care)
# To Male recipient
FINAL.OUTPUT[, 4, 3] <- HH.NUM.ARRAY[, 4, 3] / (CC.female.wgt + AC.female.wgt)
# To Female recipient
FINAL.OUTPUT[, 4, 4] <- HH.NUM.ARRAY[, 4, 4] / (CC.female.wgt + AC.female.wgt)


## Female caregiver to groups from 20-24 to 85+
for (i in 5:18) {
  # To Male recipient
  FINAL.OUTPUT[, i, 3] <- HH.NUM.ARRAY[, i, 3] / AC.female.wgt
  # To Female recipient
  FINAL.OUTPUT[, i, 4] <- HH.NUM.ARRAY[, i, 4] / AC.female.wgt
}


FINAL.OUTPUT


## US Averages by age, sex, and race/ethnicity
if((max(range(transfers$raceShort, na.rm = TRUE)) - min(range(transfers$raceShort, na.rm = TRUE))) > 1) {
 for (i in 1:18) {
   # Male caregiver to Male recipient
   US.OVERALL[, i, 1] <- HH.NUM.ARRAY[, i, 1] / apply(US.wgt.den.mat[,c(1,3,5,7)], 1, sum)
   # Male Caregiver to Female recipient
   US.OVERALL[, i, 2] <- HH.NUM.ARRAY[, i, 2] / apply(US.wgt.den.mat[,c(1,3,5,7)], 1, sum)
   # Female caregiver to Male recipient
   US.OVERALL[, i, 3] <- HH.NUM.ARRAY[, i, 3] / apply(US.wgt.den.mat[,c(2,4,6,8)], 1, sum)
   # Female caregiver to Female recipient
   US.OVERALL[, i, 4] <- HH.NUM.ARRAY[, i, 4] / apply(US.wgt.den.mat[,c(2,4,6,8)], 1, sum)
 }
 US.OVERALL
 
} else {
  
  ## Male caregiver to child groups from 0-4 to 10-14
  for (i in 1:3) {
    # To Male recipient
    FINAL.OUTPUT.US[, i, 1] <- HH.NUM.ARRAY[, i, 1] / US.wgt.mat.race[,1]
    # To Female recipient
    FINAL.OUTPUT.US[, i, 2] <- HH.NUM.ARRAY[, i, 2] / US.wgt.mat.race[,1]
  }
  
  ## Male caregiver to group 15-19 (combining childcare and adult care)
  # To Male recipient
  FINAL.OUTPUT.US[, 4, 1] <- HH.NUM.ARRAY[, 4, 1] / (US.wgt.mat.race[,1] + US.wgt.mat.race[,3])
  # To Female recipient
  FINAL.OUTPUT.US[, 4, 2] <- HH.NUM.ARRAY[, 4, 2] / (US.wgt.mat.race[,1] + US.wgt.mat.race[,3])
  
  
  ## Male caregiver to adult groups from 20-24 to 85+
  for (i in 5:18) {
    # To Male recipient
    FINAL.OUTPUT.US[, i, 1] <- HH.NUM.ARRAY[, i, 1] / US.wgt.mat.race[,3]
    # To Female recipient
    FINAL.OUTPUT.US[, i, 2] <- HH.NUM.ARRAY[, i, 2] / US.wgt.mat.race[,3]
  }
  
  
  ## Female caregiver to groups from 0-4 to 10-14
  for (i in 1:3) {
    # To Male recipient
    FINAL.OUTPUT.US[, i, 3] <- HH.NUM.ARRAY[, i, 3] / US.wgt.mat.race[,2]
    # To Female recipient
    FINAL.OUTPUT.US[, i, 4] <- HH.NUM.ARRAY[, i, 4] / US.wgt.mat.race[,2]
  }
  
  ## Female caregiver to group 15-19 (combining childcare and adult care)
  # To Male recipient
  FINAL.OUTPUT.US[, 4, 3] <- HH.NUM.ARRAY[, 4, 3] / (US.wgt.mat.race[,2] + US.wgt.mat.race[,4])
  # To Female recipient
  FINAL.OUTPUT.US[, 4, 4] <- HH.NUM.ARRAY[, 4, 4] / (US.wgt.mat.race[,2] + US.wgt.mat.race[,4])
  
  
  ## Female caregiver to groups from 20-24 to 85+
  for (i in 5:18) {
    # To Male recipient
    FINAL.OUTPUT.US[, i, 3] <- HH.NUM.ARRAY[, i, 3] / US.wgt.mat.race[,4]
    # To Female recipient
    FINAL.OUTPUT.US[, i, 4] <- HH.NUM.ARRAY[, i, 4] / US.wgt.mat.race[,4]
  }
  
  FINAL.OUTPUT.US
} 



## Creating directories to store results of the analysis
#dir.create(paste(getwd(), "results", sep = "/"))
#dir.create(paste(getwd(), "results", "graphs and plots", sep = "/"))





## Plot of average time production and consumption per day by age group of care recepient
if ((max(range(transfers$raceShort, na.rm = TRUE)) - min(range(transfers$raceShort, na.rm = TRUE))) > 1) { 

  # cons.HH.total.caregivers <- rowSums(colSums(FINAL.OUTPUT, dims = 1))
  # prod.HH.total.caregivers <- rowSums(rowSums(FINAL.OUTPUT, dims = 2))
  
  # Calculating household and non-household production and consumption profiles by age for overall US
  cons.HH.total.US <- rowSums(colSums(US.OVERALL, dims = 1))
  prod.HH.total.US <- rowSums(rowSums(US.OVERALL, dims = 2))
  prod.nonHH.total.US <- (aggregate(ActSumFile$nonHHANYCARE_time*ActSumFile$TUFNWGTP, by=list(ActSumFile$AgeGrpResp), sum, na.rm=FALSE)[,2] /
    aggregate(ActSumFile$TUFNWGTP, by=list(ActSumFile$AgeGrpResp), sum, na.rm=FALSE)[,2])
  
  
#  tiff(paste0(getwd(), "/results/", "Mean care time production and consumption - US Overall.tif"), res = 300, units = 'in', width = 7, height = 5.5, pointsize = 1/300)
  windowsFonts(A = windowsFont("Arial Bold"))
  
  par(cex.axis = 1.6, cex.lab = 1.4, mar = c(6, 4, 1, 1))
  plot(1:18, cons.HH.total.US/60, lty = 1, type = "l", xaxt = "n", col = "grey50", lwd = 2.5,
     xlab = "", ylab = "", family = "A",
     yaxp = c(0, 6, 6),
     ylim = c(0, 6), las = 2)
#  title(main = paste0("Average daily care time production consumption \n by age group of caregivers and care recipients: \n", "US Overall Mean"),
#        line = 1, cex.main = 1)
  title(xlab = "Age group", line = 4.5)
  title(ylab = "Mean hours of per day", line = 2.7)
  points.default(cons.HH.total.US/60, pch = 0, col = "grey50", type = "p", cex = 1.2)
  axis(1, 1:18, labels = ColAgeLab, las = 2)
  lines(1:18, c(0, 0, 0, prod.HH.total.US/60), col = "grey50", lwd = 2.5, lty = 5)
  points.default(c(0, 0, 0, prod.HH.total.US/60), pch = 17, col = "grey50", type = "p", cex = 1.4)
  lines(1:18, c(0, 0, 0, prod.nonHH.total.US/60), col = "black", lwd = 2.5, lty = 6)
  points.default(c(0, 0, 0, prod.nonHH.total.US/60), pch = 2, col = "black", type = "p", cex = 1.2)
  grid(col = "lightgray")
  legend(x = 10, y = 6.3, legend = c("HH Consumption", "HH Production", "Non-HH Production"),
         lty = c(1, 5, 6), lwd = c(2.5, 2.5, 2.5), cex = 1.4, col = c("grey50", "grey50", "black"), bty = "n", pch = c(0,17,2), pt.cex = c(1.2,1.4,1.2))
  
#dev.off()
    
  } else {
    
    # ## Activate when generating average profiles for caregiver only subset by race/ethnicity
    # cons.HH.total.caregivers <- rowSums(colSums(FINAL.OUTPUT, dims = 1))
    # prod.HH.total.caregivers <- rowSums(rowSums(FINAL.OUTPUT, dims = 2))
    # prod.nonHH.total.caregivers <- (aggregate(ActSumFile$nonHHANYCARE_time[ActSumFile$raceShort==race & ActSumFile$nonHHANYCARE_time>0]*ActSumFile$TUFNWGTP[ActSumFile$raceShort==race & ActSumFile$nonHHANYCARE_time>0],
    #                                           by=list(ActSumFile$AgeGrpResp[ActSumFile$raceShort==race & ActSumFile$nonHHANYCARE_time>0]), sum, na.rm=FALSE)[,2] /
    #                                   aggregate(ActSumFile$TUFNWGTP[ActSumFile$raceShort==race & ActSumFile$nonHHANYCARE_time>0], by=list(ActSumFile$AgeGrpResp[ActSumFile$raceShort==race & ActSumFile$nonHHANYCARE_time>0]), sum, na.rm=FALSE)[,2])
    
    
    ## Activate when generating average profiles for the US population by race/ethnicity
    # Calculating household and non-household production and consumption profiles by age for overall US
    cons.HH.total.US <- rowSums(colSums(FINAL.OUTPUT.US, dims = 1))
    prod.HH.total.US <- rowSums(rowSums(FINAL.OUTPUT.US, dims = 2))
    prod.nonHH.total.US <- (aggregate(ActSumFile$nonHHANYCARE_time[ActSumFile$raceShort==race]*ActSumFile$TUFNWGTP[ActSumFile$raceShort==race], by=list(ActSumFile$AgeGrpResp[ActSumFile$raceShort==race]), sum, na.rm=FALSE)[,2] /
                              aggregate(ActSumFile$TUFNWGTP[ActSumFile$raceShort==race], by=list(ActSumFile$AgeGrpResp[ActSumFile$raceShort==race]), sum, na.rm=FALSE)[,2])
    
    
    ## sum( prod.total.caregiver * age structure of population ) = total number of minutes produced
    ## sum( cons.total.caregiver * age structure of population ) = total number of minutes consumed  
    ## ratio used for rescaling consumption = total number of minutes produced / total number of minutes consumed 

#  tiff(paste0(getwd(), "/results/", "Mean care time production and consumption - ", RACE.FILENAME, ".tif"), res = 300, units = 'in', width = 7, height = 5.5, pointsize = 1/300)
  windowsFonts(A = windowsFont("Arial Bold"))
  
  par(cex.axis = 1.6, cex.lab = 1.6, mar = c(6.2, 4, 2, 1), family = "A", mfrow = c(4,2))
  plot(1:18, cons.HH.total.US/60, lty = 1, type = "l", xaxt = "n", col = "grey50", lwd = 2.5,
     xlab = "", ylab = "", family = "A",
     # yaxp = c(0, 24, 12),
     # ylim = c(0, 24), las = 2)
     yaxp = c(0, 6, 6),
     ylim = c(0, 6), las = 2)
  
  title(main = "Non-Hispanic white", cex.main = 1.7, line = 1)
  title(xlab = "Age group", line = 5)
  title(ylab = "Mean hours per day", line = 2.7)  
  points.default(cons.HH.total.US/60, pch = 0, col = "grey50", type = "p", cex = 1.2)
  axis(1, 1:18, labels = ColAgeLab, las = 2)
  lines(1:18, c(0, 0, 0, prod.HH.total.US/60), col = "grey50", lwd = 2.5, lty = 5)
  points.default(c(0, 0, 0, prod.HH.total.US/60), pch = 17, col = "grey50", type = "p", cex = 1.4)
  lines(1:18, c(0, 0, 0, prod.nonHH.total.US/60), col = "black", lwd = 2.5, lty = 6)
  points.default(c(0, 0, 0, prod.nonHH.total.US/60), pch = 19, col = "black", type = "p", cex = 1.2)
  grid(col = "lightgray")
  legend(x = 9, y = 6.3, legend = c("HH Consumption", "HH Production", "Non-HH Production"),
         lty = c(1, 5, 6), lwd = c(2.5, 2.5, 2.5), cex = 1.4, col = c("grey50", "grey50", "black"), bty = "n", pch = c(0,17,19), pt.cex = c(1.2,1.4,1.2))
  
#  dev.off()   
}



# Exporting matrices for each transfer type by sex
# write.csv(FINAL.OUTPUT[,,1], file=paste0(getwd(), "/results/", "HH_", RACE.FILENAME, "_matrix_Male_Male.csv"), 
#           sep = ",", row.names = TRUE, col.names = TRUE)
# write.csv(FINAL.OUTPUT[,,2], file=paste0(getwd(), "/results/", "HH_", RACE.FILENAME, "_matrix_Male_Female.csv"), 
#           sep = ",", row.names = TRUE, col.names = TRUE)
# write.csv(FINAL.OUTPUT[,,3], file=paste0(getwd(), "/results/", "HH_", RACE.FILENAME, "_matrix_Female_Male.csv"), 
#           sep = ",", row.names = TRUE, col.names = TRUE)
# write.csv(FINAL.OUTPUT[,,4], file=paste0(getwd(), "/results/", "HH_", RACE.FILENAME, "_matrix_Female_Female.csv"), 
#           sep = ",", row.names = TRUE, col.names = TRUE)
# 
# # Exporting matrices for each transfer type by sex: For US overall estimates only (if no single race is specified)
# if ((max(range(transfers$raceShort, na.rm = TRUE)) - min(range(transfers$raceShort, na.rm = TRUE))) > 1) { 
#   write.csv(US.OVERALL[,,1], file=paste0(getwd(), "/results/", "HH_US_OVERALL_matrix_Male_Male.csv"), 
#             sep = ",", row.names = TRUE, col.names = TRUE)
#   write.csv(US.OVERALL[,,2], file=paste0(getwd(), "/results/", "HH_US_OVERALL_matrix_Male_Female.csv"), 
#             sep = ",", row.names = TRUE, col.names = TRUE)
#   write.csv(US.OVERALL[,,3], file=paste0(getwd(), "/results/", "HH_US_OVERALL_matrix_Female_Male.csv"), 
#             sep = ",", row.names = TRUE, col.names = TRUE)
#   write.csv(US.OVERALL[,,4], file=paste0(getwd(), "/results/", "HH_US_OVERALL_matrix_Female_Female.csv"), 
#             sep = ",", row.names = TRUE, col.names = TRUE)
# }


                      #################################
                      ########### Graphics ############
                      #################################


library(fields)

rowlabel <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")

collabel <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")

## Attention: remove/add "_UNWEIGHTED" portion of the file path name below to use weighted/unweighted estimates (if previously generated)
dataMM <- read.csv(paste0(getwd(), "/results/HH_", RACE.FILENAME, "_matrix_Male_Male.csv"), header = FALSE)
dataMF <- read.csv(paste0(getwd(), "/results/HH_", RACE.FILENAME, "_matrix_Male_Female.csv"), header = FALSE)
dataFM <- read.csv(paste0(getwd(), "/results/HH_", RACE.FILENAME, "_matrix_Female_Male.csv"), header = FALSE)
dataFF <- read.csv(paste0(getwd(), "/results/HH_", RACE.FILENAME, "_matrix_Female_Female.csv"), header = FALSE)

# # Activate for the US overall averages
# dataMM <- read.csv(paste0(getwd(), "/results/HH_", "US_OVERALL", "_matrix_Male_Male.csv"), header = FALSE)
# dataMF <- read.csv(paste0(getwd(), "/results/HH_", "US_OVERALL", "_matrix_Male_Female.csv"), header = FALSE)
# dataFM <- read.csv(paste0(getwd(), "/results/HH_", "US_OVERALL", "_matrix_Female_Male.csv"), header = FALSE)
# dataFF <- read.csv(paste0(getwd(), "/results/HH_", "US_OVERALL", "_matrix_Female_Female.csv"), header = FALSE)

dataMM <- dataMM[2:16, 2:19] 
dataMF <- dataMF[2:16, 2:19] 
dataFM <- dataFM[2:16, 2:19] 
dataFF <- dataFF[2:16, 2:19] 

matrixMM <- t(as.matrix(dataMM)); storage.mode(matrixMM) <- "numeric"
matrixMF <- t(as.matrix(dataMF)); storage.mode(matrixMF) <- "numeric"
matrixFM <- t(as.matrix(dataFM)); storage.mode(matrixFM) <- "numeric"
matrixFF <- t(as.matrix(dataFF)); storage.mode(matrixFF) <- "numeric"



# Maximum z-axis value for plotting, absolute or log10
# maxval <- 120 #round(quantile(c(matrixMM, matrixMF, matrixFM, matrixFF), 0.996) + 1)
maxval <- max(log10(c(matrixMM, matrixMF, matrixFM, matrixFF)))

library(RColorBrewer)
## Attention: Remove comma afer RACE.FILENAME and replace "_unweighted.pdf" with ".pdf" to change filename
## when using weighted estimtes in the line of code below this one
#pdf(paste0(getwd(), "/results/graphs and plots/", RACE.FILENAME, ".pdf"))
par(mfrow = c(2, 2), "mar" = c(2, 2.5, 1.5, 1), oma = c(3, 2.5, 1, 1.5), cex.main = 1.3, cex.axis = 1)

#pdf("male_to_male.pdf")
image.plot(log10(matrixMM), axes = FALSE, legend.lab = "Log 10 minutes per day", legend.cex = 1, legend.line = 2.15, zlim = c(0, maxval),
           breaks = c(0, 0.5, 1, 1.5, 2), col = brewer.pal(4, "Greys"))
title(main = "Male to Male", line = 0.5)
#axis(1, at = seq(0, 1, length = 18), labels = collabel, las = 2)
axis(2, at = seq(0, 1, length = 15), labels = rowlabel, las = 1)
#dev.off()

#pdf("male_to_female.pdf")
image.plot(log10(matrixMF), axes = FALSE, legend.lab = "Log 10 minutes per day", legend.cex = 1, legend.line = 2.15, zlim = c(0, maxval),
           breaks = c(0, 0.5, 1, 1.5, 2), col = brewer.pal(4, "Greys"))
title(main = "Male to Female", line = 0.5, cex = 2)
#axis(1, at = seq(0, 1, length = 18), labels = collabel, las = 2)
#axis(2, at = seq(0, 1, length = 15), labels = rowlabel, las = 1)
#dev.off()

#pdf("female_to_male.pdf")
image.plot(log10(matrixFM), axes = FALSE, legend.lab = "Log 10 minutes per day", legend.cex = 1, legend.line = 2.15, zlim = c(0, maxval),
           breaks = c(0, 0.5, 1, 1.5, 2), col = brewer.pal(4, "Greys"))
title(main = "Female to Male", line = 0.5)
axis(1, at = seq(0, 1, length = 18), labels = collabel, las = 2)
axis(2, at = seq(0, 1, length = 15), labels = rowlabel, las = 1)
#dev.off()

#pdf("female_to_female.pdf")
image.plot(log10(matrixFF), axes = FALSE, legend.lab = "Log 10 minutes per day", legend.cex = 1, legend.line = 2.15, zlim = c(0, maxval),
           breaks = c(0, 0.5, 1, 1.5, 2), col = brewer.pal(4, "Greys"))
title(main = "Female to Female", line = 0.5)
axis(1, at = seq(0, 1, length=18), labels = collabel, las = 2)
#axis(2, at=seq(0, 1, length = 15), labels = rowlabel, las = 1)

mtext("Age groups of care recipients", side = 1, outer = TRUE, line = 1.5, cex = 1.2)
mtext("Age groups of caregivers", side = 2, outer = TRUE, line = 1.3, cex = 1.2)
# title(main = paste0("Mean daily time transfer: ", RACE), outer = TRUE, line = 0.5, cex = 1.35)

dev.off()


#                     ################################################
#                     ########### STANDARD ERROR ANALYSIS ############
#                     ################################################
# 
# NOTE: This part largely replicates the code from the above 1100 or so lines of code. Computation of standard errors
# is an iterative process, as per ATUS User Manual methodology, and may take substantial amount of time to run, depending 
# on the size of the data set used.
#
#
# RepWGT <- as.data.frame(read.table(file=paste0(getwd(), "/data/", datasets[8]), header = T, sep = ",", stringsAsFactors = F))
# RepWGT$TUCASEID <- as.character(RepWGT$TUCASEID, length = 14)
# 
# SDERR.FUN <- function(race = 0) {
# 
#     
#   #####################################################
#   #### REPLICATE WEIGHT DENOMINATOR COLUMN VECTORS ####
#   #####################################################
#   
#   
#   ## Setting up a list for replicate-weight estimates to be stored in
#   EST.LST <- list()
#   
#   for(v in 1:160) {
#     
#     
#       
#       wgt_num <- formatC(v, width = 3, format = "d", flag = "0")
#       RepWGT$wgt_var <- RepWGT[, paste("TUFNWGTP", wgt_num, sep = "")]
#       
#       
#       require(data.table)
#       HH.WGT.DF <- merge(ActSumFile[, c(1,7,12,22,456)], 
#                          ActFile[(ActFile[30] & ActFile[31]) | ActFile[30] | ActFile[31],
#                                  c(1,30,31)],
#                          by = c("TUCASEID"), sort = TRUE, all = FALSE)
#       
#       HH.WGT.DF <- merge(HH.WGT.DF, RepWGT[, c("TUCASEID", "wgt_var")], by = "TUCASEID", sort = FALSE, all = FALSE)
#       ActSumFile <- merge(ActSumFile, RepWGT[, c("TUCASEID", "wgt_var")], by = "TUCASEID", sort = FALSE, all = FALSE)
#       
#       
#       HH.WGT.DF <- data.table(HH.WGT.DF, key = "TUCASEID")
#       HH.WGT.DF <- HH.WGT.DF[HH.WGT.DF$raceShort > 0, list(TEAGE = max(TEAGE), TESEX = max(TESEX), raceShort = max(raceShort, na.rm = TRUE),
#                                                            wgt_var = max(wgt_var),
#                                                            HH_CC = max(HH_CC), HH_AC = max(HH_AC)), by = TUCASEID]
#       
#       HH.WGT.DF$AgeGrpResp <- as.integer(cut(HH.WGT.DF$TEAGE, breaks = c(seq(0, 85, 5), 120),
#                                              right = FALSE, include.lowest = TRUE))
#       
#       
#       ### Creating subsets of weight data to contain caregivers that satisfy care type conditions
#       CC.HH.WGT.DF <- subset.data.frame(HH.WGT.DF, subset = HH.WGT.DF$HH_CC==1)
#       AC.HH.WGT.DF <- subset.data.frame(HH.WGT.DF, subset = HH.WGT.DF$HH_AC==1)
#       SANDWICH.HH.WGT.DF <- subset.data.frame(HH.WGT.DF, subset = HH.WGT.DF$HH_CC==1 & HH.WGT.DF$HH_AC==1)
#       
#       WGT.DF.LST <- list(CC.HH.WGT.DF, AC.HH.WGT.DF, SANDWICH.HH.WGT.DF, HH.WGT.DF)
#       
#       
#       WGT.MAT.LST <- lapply(WGT.DF.LST, function(x) {
#         
#         ### Setting up weight matrices for appropriate care types:
#         # 1) Childcare weights (same respondents could have also provided adult care on separate occasions)
#         # 2) Adult care weights (same respondents could have also provided childcare on separate occasions)
#         # 3) Sandwich generation weights (respondents MUST have provided both childcare and adult care)
#         # 4) Overall weights (total weights for all caregivers in the U.S. in the 4 racial/ethnic categories)
#         
#         # Making sure all groups are represented, even if no weight is generated (age/sex/race ID vector)
#         agg_wgt_ID <- as.data.frame(matrix(c(rep(4:18, 8), rep(1, 60), rep(2, 60),
#                                              rep(c(rep(1, 15), rep(2, 15), rep(3, 15), rep(4, 15)), 2)), 120, 3))
#         AGGR.VEC <- paste(agg_wgt_ID$V1, agg_wgt_ID$V2, agg_wgt_ID$V3, sep = "") # [AgeGrpResp, TESEX, raceShort] combined ID
#         agg_wgt_ID <- cbind(agg_wgt_ID, AGGR.VEC)
#         
#         # Generating a vector of sum weights by age/sex/race group
#         agg_wgt_temp <- aggregate(x$wgt_var, by=list(x$AgeGrpResp, x$TESEX, x$raceShort), 
#                                   sum, na.rm=FALSE)
#         AGGR.VEC.TEMP <- paste(agg_wgt_temp$Group.1, agg_wgt_temp$Group.2, agg_wgt_temp$Group.3, sep = "")
#         agg_wgt_temp <- as.data.frame(matrix(c(AGGR.VEC.TEMP, as.numeric(agg_wgt_temp$x)), nrow = NROW(agg_wgt_temp), ncol = 2),
#                                       stringsAsFactors = FALSE)
#         
#         # Matching aggregated weights with the corresponding age/sex/race from the full list
#         agg_wgt_ID <- merge(agg_wgt_ID, agg_wgt_temp, by.x = "AGGR.VEC", by.y = "V1", all.x = TRUE)
#         agg_wgt_ID[is.na(agg_wgt_ID)] <- 1
#         for (i in 1:length(agg_wgt_ID)) agg_wgt_ID[,i] <- as.numeric(as.character(agg_wgt_ID[,i]))
#         agg_wgt_ID <- agg_wgt_ID[order(agg_wgt_ID[3], agg_wgt_ID[4], agg_wgt_ID[2], decreasing = FALSE), ]
#         
#         # Converting the vectors into matrices of weights 
#         # (columns represent sum of weights for each sex/race group of caregivers by age group)
#         wgt_final <- matrix(0, 15, 8)
#         for (i in 1:8) {
#           wgt_final[,i] <- agg_wgt_ID[(i*15-14):(i*15), 5]
#           
#         }
#         
#         # Assigning labels to the weight matrices' rows and columns
#         rownames(wgt_final) <- RowAgeLab
#         
#         RACE.NAMES <- list("WHITE", "BLACK", "ASIAN", "HISPANIC")
#         SEX.NAMES <- list("MALE", "FEMALE")
#         
#         wgt.col.names <- unlist(lapply(SEX.NAMES, function(Z) {
#           lapply(RACE.NAMES, function(Y) {
#             paste(Z, Y, sep = ".")
#           })
#         }), recursive = TRUE)
#         colnames(wgt_final) <- wgt.col.names
#         
#         return(wgt_final)
#         
#         
#       })
#       
#       # Reformatting the list of weight sums by age/sex/race/care type into an equivalent array
#       WGT.MAT.ARRAY <- array(unlist(WGT.MAT.LST), dim = c(nrow(WGT.MAT.LST[[1]]), ncol(WGT.MAT.LST[[1]]), length(WGT.MAT.LST)))
#       dimnames(WGT.MAT.ARRAY) <- list(RowAgeLab, colnames(WGT.MAT.LST[[1]]),
#                                       c("CHILDCARE WEIGHTS", "ADULT CARE WEIGHTS", "SANDWICH WEIGHTS", "OVERALL WEIGHTS"))
#       
#       ActSumFile$AgeGrpResp <- as.integer(cut(ActSumFile$TEAGE, breaks = c(seq(0, 85, 5), 120),
#                                               right = FALSE, include.lowest = TRUE))
#       
#       ## US.wgt.den.mat will store weights for estimating US time averages, regardless of caregiving status
#       US.wgt <- aggregate(ActSumFile$wgt_var, by=list(ActSumFile$AgeGrpResp, ActSumFile$TESEX), sum, na.rm = FALSE)
#       US.wgt.den.mat <- matrix(US.wgt[, 3], 15, 2)
#       rownames(US.wgt.den.mat) <- RowAgeLab
#       colnames(US.wgt.den.mat) <- c("US.male.wgt", "US.female.wgt")
#       
#       
#       
#       ### Setting up a new data frame 
#       transfers <- data.frame(WhoFile)
#       transfers <- within(transfers, rm(TRWHONA)) 
#       
#       ## Merging WhoFile and RosterFile for each activity: HH participants (and own NonHH children) 
#       ## whose age and sex are known
#       transfers <- merge(transfers, RosterFile, by = c("TUCASEID", "TULINENO"), sort = FALSE, all = FALSE)
#       
#       ## Merging transfers and Activity Summary File (importing Age/Sex, Race, and survey weight
#       ## of each respondent)
#       transfers <- merge(transfers, ActSumFile, by = "TUCASEID", sort = FALSE, all = FALSE)
#       transfers <- transfers[, c(1:4, 6, 7, 13, 18, 28, 462)]
#       
#       ## Merging transfers and Acivity File (importing activity duration and classification)
#       transfers <- merge(transfers, ActFile, by = c("TUCASEID", "TUACTIVITY_N"), sort = TRUE, all = FALSE)
#       transfers <- transfers[, c(1:10, 25, 38:41)]
#       
#       # Assigning respondents to 5-year age group categories
#       transfers$AgeGrpResp <- as.integer(cut(transfers$TEAGE.y, breaks = c(seq(0, 85, 5), 120),
#                                              right = FALSE, include.lowest = TRUE))
#       
#       # Assigning all present during activities to 5-year age group categories 
#       transfers$AgeGrp <- as.integer(cut(transfers$TEAGE.x, breaks = c(seq(0, 85, 5), 120),
#                                          right = FALSE, include.lowest = TRUE))
#       
#       ### Merging in replicate weights
#       transfers <- merge(transfers, RepWGT[, c("TUCASEID", "wgt_var")], by = "TUCASEID", sort = FALSE, all = FALSE)
#       
#       transfers <- transfers[order(transfers$TUCASEID, transfers$TUACTIVITY_N, transfers$TULINENO, decreasing = FALSE), ]                          
#       
#       ## Creating a unique ID combination for each activity in the data set
#       transfers$ID2 <- paste(transfers$TUCASEID, transfers$TUACTIVITY_N, sep = ":")
#       
#       ## Ensuring the correct race is used
#       if (race > 0) transfers <- subset(transfers[transfers$raceShort == race, ])
#       
#       
#       ##############################
#       #### HOUSEHOLD CHILDCARE #####
#       ##############################
#       
#       transfers.HH_CC <- subset(transfers[transfers$HH_CC == TRUE, ])
#       transfers.HH_CC <- transfers.HH_CC[order(transfers.HH_CC$ID2, decreasing = FALSE), ]
#       
#       transList <- split(transfers.HH_CC, transfers.HH_CC$ID2)
#       
#       childWHO_CODE <- c(22, 23, 25, 26, 27, 30)      # "Who" relation codes from ATUS lexicon, directly or potentially
#       # indicating children
#       
#       HH.CC.lst <- lapply(transList, function(tx) {
#         
#         personNUM <- tx$TEAGE.x < 18 & tx$TUWHO_CODE %in% childWHO_CODE  # Selecting children who received care
#         den <- sum(personNUM)                                            # Number of children who potentially received care in an activity
#         rec_min <- tx$TUACTDUR[1]/den                                    # Number of minutes potentially received by each child under 18 present
#         tx$minPer <- 0*personNUM                                         # Making sure the non-qualifying time entries per respondent ID are excluded from the data
#         tx$minPer[personNUM] <- rec_min * tx$wgt_var[1]                 # Multiplying the time entries by the caregiver(respondent) survey weight 
#         
#         return(tx)
#         
#       })
#       
#       HH.CC.data <- unsplit(HH.CC.lst, transfers.HH_CC$ID2)
#       
#       ## HH Childcare Male to Male
#       HH.CC.TIME.MM <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
#                                          HH.CC.data$TEAGE.x < 18 &
#                                          HH.CC.data$TESEX.x == 1 & HH.CC.data$TESEX.y == 1,],
#                             tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
#       
#       HH.CC.TIME.MM[is.na(HH.CC.TIME.MM)] <- 0
#       
#       ## HH Childcare Male to Female
#       HH.CC.TIME.MF <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
#                                          HH.CC.data$TEAGE.x < 18 &
#                                          HH.CC.data$TESEX.x == 2 & HH.CC.data$TESEX.y == 1,],
#                             tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
#       
#       HH.CC.TIME.MF[is.na(HH.CC.TIME.MF)] <- 0
#       
#       
#       ## HH Childcare Female to Male
#       HH.CC.TIME.FM <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
#                                          HH.CC.data$TEAGE.x < 18 &
#                                          HH.CC.data$TESEX.x == 1 & HH.CC.data$TESEX.y == 2,],
#                             tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
#       
#       HH.CC.TIME.FM[is.na(HH.CC.TIME.FM)] <- 0
#       
#       ## HH Childcare Female to Female
#       HH.CC.TIME.FF <- with(HH.CC.data[HH.CC.data$TUWHO_CODE %in% childWHO_CODE & 
#                                          HH.CC.data$TEAGE.x < 18 &
#                                          HH.CC.data$TESEX.x == 2 & HH.CC.data$TESEX.y == 2,],
#                             tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
#       
#       HH.CC.TIME.FF[is.na(HH.CC.TIME.FF)] <- 0
#       
#       # Function to check for any absent columns and inserting empty ones to maintain matrix structure
#       absent.cols.fun.CC <- function(m) {
#         if (length(m) > 0) {
#           
#           cols.names <- c(1, 2, 3, 4)
#           m <- as.data.frame(m)
#           absent <- setdiff(cols.names, names(m)) 
#           
#           if (length(absent) > 0) {
#             m <- cbind(m, as.data.frame(matrix(0, NROW(m), length(absent))))
#             colnames(m) <- c(colnames(m[1:(length(m)-length(absent))]), absent)
#           } else {
#             m <- m
#           }
#           
#           m <- m[, as.character(cols.names)]
#           m <- as.matrix(m)
#           
#         } else if (length(m) == 0) {    # Preventing 0x0 matrices, if no tranfers are registered
#           m <- matrix(0, 15, 4)
#           colnames(m) <- seq(1, 4, 1)
#           rownames(m) <- seq(4, 18, 1)
#         }
#         
#         return(m)
#       }
#       
#       
#       ## Constructing numerator matrices by age/sex and race for childcare
#       
#       ## List containing matrices of sums of weight-multiplied childcare time by sex of caregiver and care recipient
#       HH.CC.MAT.LST <- lapply(list(HH.CC.TIME.MM, HH.CC.TIME.MF, HH.CC.TIME.FM, HH.CC.TIME.FF), absent.cols.fun.CC)
#       HH.CC.MAT.LST <- lapply(HH.CC.MAT.LST, function(x) {
#         CCmat <- matrix(0, 15, 4)
#         rownames(CCmat) <- seq(4, 18, 1)
#         colnames(CCmat) <- seq(1, 4, 1)
#         mcol <- match(colnames(x), colnames(CCmat))
#         mrow <- match(rownames(x), rownames(CCmat))
#         CCmat[mrow, ] <- x + CCmat[mrow, ]
#         x <- CCmat
#         
#         rownames(x) <- RowAgeLab[1:nrow(x)]
#         colnames(x) <- ColAgeLab[1:ncol(x)]
#         
#         return(x)
#       })
#       
#       names(HH.CC.MAT.LST) <- c("Male.to.Male", "Male.to.Female", "Female.to.Male", "Female.to.Female")
#       
#       
#       
#       ##############################
#       #### HOUSEHOLD ADULT CARE ####
#       ##############################
#       
#       
#       transfers.HH_AC <- subset(transfers[transfers$HH_AC == TRUE, ])
#       transfers.HH_AC <- transfers.HH_AC[order(transfers.HH_AC$ID2, decreasing = FALSE), ]
#       
#       transList <- split(transfers.HH_AC, transfers.HH_AC$ID2)
#       
#       adultWHO_CODE <- c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)      # "Who" relation codes from ATUS lexicon, directly or potentially
#       # indicating adults who received care
#       
#       HH.AC.lst <- lapply(transList, function(tx) {
#         
#         personNUM <- tx$TEAGE.x >= 18 & tx$TUWHO_CODE %in% adultWHO_CODE   # Selecting adults who received care
#         den <- sum(personNUM)                                              # Number of adults who potentially received care in an activity
#         rec_min <- tx$TUACTDUR[1]/den                                      # Number of minutes potentially received by each adult 18+ present
#         tx$minPer <- 0*personNUM                                           # Making sure the non-qualifying time entries per respondent ID are excluded from the data
#         tx$minPer[personNUM] <- rec_min * tx$wgt_var[1]                   # Multiplying the time entries by the caregiver(respondent) survey weight 
#         
#         return(tx)
#         
#       })
#       
#       HH.AC.data <- unsplit(HH.AC.lst, transfers.HH_AC$ID2)
#       
#       ## HH Adult Care Male to Male
#       HH.AC.TIME.MM <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
#                                          HH.AC.data$TEAGE.x >= 18 &
#                                          HH.AC.data$TESEX.x == 1 & HH.AC.data$TESEX.y == 1,],
#                             tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
#       
#       HH.AC.TIME.MM[is.na(HH.AC.TIME.MM)] <- 0
#       
#       ## HH Adult Care Male to Female
#       HH.AC.TIME.MF <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
#                                          HH.AC.data$TEAGE.x >= 18 &
#                                          HH.AC.data$TESEX.x == 2 & HH.AC.data$TESEX.y == 1,],
#                             tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
#       
#       HH.AC.TIME.MF[is.na(HH.AC.TIME.MF)] <- 0
#       
#       
#       ## HH Adult Care Female to Male
#       HH.AC.TIME.FM <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
#                                          HH.AC.data$TEAGE.x >= 18 &
#                                          HH.AC.data$TESEX.x == 1 & HH.AC.data$TESEX.y == 2,],
#                             tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
#       
#       HH.AC.TIME.FM[is.na(HH.AC.TIME.FM)] <- 0
#       
#       ## HH Adult Care Female to Female
#       HH.AC.TIME.FF <- with(HH.AC.data[HH.AC.data$TUWHO_CODE %in% adultWHO_CODE & 
#                                          HH.AC.data$TEAGE.x >= 18 &
#                                          HH.AC.data$TESEX.x == 2 & HH.AC.data$TESEX.y == 2,],
#                             tapply(minPer, list(AgeGrpResp, AgeGrp), sum))
#       
#       HH.AC.TIME.FF[is.na(HH.AC.TIME.FF)] <- 0
#       
#       
#       # Function to check for any absent columns and inserting empty ones to maintain matrix structure
#       absent.cols.fun.AC <- function(m) {
#         if (length(m) > 0) {
#           
#           cols.names <- seq(4, 18, 1)
#           m <- as.data.frame(m)
#           absent <- setdiff(cols.names, names(m)) 
#           
#           if (length(absent) > 0) {
#             m <- cbind(m, as.data.frame(matrix(0, NROW(m), length(absent))))
#             colnames(m) <- c(colnames(m[1:(length(m)-length(absent))]), absent)
#           } else {
#             m <- m
#           }
#           
#           m <- m[, as.character(cols.names)]
#           m <- as.matrix(m)
#           
#         } else if (length(m) == 0) {    # Preventing 0x0 matrices, if no tranfers are registered
#           m <- matrix(0, 15, 15)
#           colnames(m) <- seq(4, 18, 1)
#           rownames(m) <- seq(4, 18, 1)
#         }
#         
#         return(m)
#       }
#       
#       
#       ## Constructing numerator matrices by age/sex and race for adult care
#       
#       ## List containing matrices of sums of weight-multiplied adult care time by sex of caregiver and care recipient
#       HH.AC.MAT.LST <- lapply(list(HH.AC.TIME.MM, HH.AC.TIME.MF, HH.AC.TIME.FM, HH.AC.TIME.FF), absent.cols.fun.AC)
#       HH.AC.MAT.LST <- lapply(HH.AC.MAT.LST, function(x) {
#         ACmat <- matrix(0, 15, 15)
#         rownames(ACmat) <- seq(4, 18, 1)
#         colnames(ACmat) <- seq(4, 18, 1)
#         mcol <- match(colnames(x), colnames(ACmat))
#         mrow <- match(rownames(x), rownames(ACmat))
#         ACmat[mrow, mcol] <- x + ACmat[mrow, mcol]
#         x <- ACmat
#         
#         rownames(x) <- RowAgeLab[1:nrow(x)]
#         colnames(x) <- ColAgeLab[4:18]
#         
#         return(x)
#       })
#       
#       names(HH.AC.MAT.LST) <- c("Male.to.Male", "Male.to.Female", "Female.to.Male", "Female.to.Female")
#       
#       
#       ######################################################
#       #### OVERALL WEIGHT-MULTIPLIED NUMERATOR MATRICES ####
#       ######################################################
#       
#       # Setting up empty shell for the overall numerator matrix array, containing sums of weight-multiplied time
#       HH.NUM.ARRAY <- array(0, dim = c(15, 18, 4))
#       
#       # Populating the overall numerator matrix array with appropriate values for each age/sex group by transfer type
#       for (i in 1:4) {
#         HH.NUM.ARRAY[, 1:3, i] <- HH.CC.MAT.LST[[i]][,1:3]
#         HH.NUM.ARRAY[, 4, i] <- HH.CC.MAT.LST[[i]][,4] + HH.AC.MAT.LST[[i]][,1]     # Weight-multiplied sums for childcare and adult care portion of 15-19 y.o.
#         HH.NUM.ARRAY[, 5:18, i] <- HH.AC.MAT.LST[[i]][, 2:15]
#       }
#       
#       # Labeling the numerator matrix array
#       rownames(HH.NUM.ARRAY) <- RowAgeLab
#       colnames(HH.NUM.ARRAY) <- ColAgeLab
#       dimnames(HH.NUM.ARRAY)[[3]] <- c("Male.to.Male", "Male.to.Female", "Female.to.Male", "Female.to.Female")
#       
#       
#       
#       ###############################################################################
#       #### MATRICES OF AVERAGE TIME PRODUCTION/CONSUMPTION BY AGE, SEX, and RACE ####
#       ###############################################################################
#       
#       OUTPUT <- array(0, dim = c(15, 18, 4))
#       dimnames(OUTPUT) <- dimnames(HH.NUM.ARRAY)
#       US.OVERALL <- OUTPUT
#       
#       ### Overall INTRA-HOUSEHOLD informal care time (average among caregivers)
#       
#       if ((max(range(transfers$raceShort, na.rm = TRUE)) - min(range(transfers$raceShort, na.rm = TRUE))) > 1) {   # Weight vectors for overall
#         ActSumFile$CHILDCARE <- ActSumFile$HHCC_time > 0
#         ActSumFile$ADULTCARE <- ActSumFile$HHAC_time > 0
#         
#         cc.wgt <- merge(data.frame(Group.1 = rep(levels(as.factor(ActSumFile$AgeGrpResp)), 2),
#                                    Group.2 = c(rep(levels(as.factor(ActSumFile$TESEX))[1], 15), rep(levels(as.factor(ActSumFile$TESEX))[2], 15)),
#                                    Group.3 = c(rep(levels(as.factor(ActSumFile$CHILDCARE))[1], 30), rep(levels(as.factor(ActSumFile$CHILDCARE))[2], 30))),
#                         aggregate(ActSumFile$wgt_var,
#                                   by=list(ActSumFile$AgeGrpResp, ActSumFile$TESEX, ActSumFile$CHILDCARE),
#                                   sum, na.rm=FALSE), all.x = TRUE)
#         cc.wgt <- cc.wgt[cc.wgt$Group.3 == TRUE, c(1, 2, 4)]
#         cc.wgt[is.na(cc.wgt)] <- 1
#         cc.wgt <- cc.wgt[order(as.integer(cc.wgt$Group.2), as.numeric(as.character(cc.wgt$Group.1)), decreasing = FALSE), ]
#         
#         ac.wgt <- merge(data.frame(Group.1 = rep(levels(as.factor(ActSumFile$AgeGrpResp)), 2),
#                                    Group.2 = c(rep(levels(as.factor(ActSumFile$TESEX))[1], 15), rep(levels(as.factor(ActSumFile$TESEX))[2], 15)),
#                                    Group.3 = c(rep(levels(as.factor(ActSumFile$ADULTCARE))[1], 30), rep(levels(as.factor(ActSumFile$ADULTCARE))[2], 30))),
#                         aggregate(ActSumFile$wgt_var,
#                                   by=list(ActSumFile$AgeGrpResp, ActSumFile$TESEX, ActSumFile$ADULTCARE),
#                                   sum, na.rm=FALSE), all.x = TRUE)
#         ac.wgt <- ac.wgt[ac.wgt$Group.3 == TRUE, c(1, 2, 4)]
#         ac.wgt[is.na(ac.wgt)] <- 1
#         ac.wgt <- ac.wgt[order(as.integer(ac.wgt$Group.2), as.numeric(as.character(ac.wgt$Group.1)), decreasing = FALSE), ]
#         
#         CC.male.wgt <- cc.wgt[1:15, 3]
#         CC.female.wgt <- cc.wgt[16:30, 3]
#         AC.male.wgt <- ac.wgt[1:15, 3]
#         AC.female.wgt <- ac.wgt[16:30, 3]
#         
#         rm(cc.wgt, ac.wgt)
#         
#         RACE <- "All Caregivers"
#         RACE.FILENAME <- "All_caregivers"
#         
#         ## DO NOT USE - NEEDS FIXING Activate only when no ATUS weights are used for the numerator
#         #    CC.male.wgt <- rowSums(sample.array.wgt[, 1:4, 1])
#         #    CC.female.wgt <- rowSums(sample.array.wgt[, 5:8, 1])
#         #    AC.male.wgt <- rowSums(sample.array.wgt[, 1:4, 2])
#         #    AC.female.wgt <- rowSums((sample.array.wgt[, 5:8, 2]))
#         
#         
#       } else if (median(transfers$raceShort) == 1) {        # Weight vectors for Non-Hispanic whites
#         CC.male.wgt <- WGT.MAT.ARRAY[, 1, 1]
#         CC.female.wgt <- WGT.MAT.ARRAY[, 5, 1]
#         AC.male.wgt <- WGT.MAT.ARRAY[, 1, 2]
#         AC.female.wgt <- WGT.MAT.ARRAY[, 5, 2]
#         
#         RACE <- "White caregivers (Non-Hispanic)"
#         RACE.FILENAME <- "White_caregivers"
#         
#         ## Activate only when no ATUS weights are used for the numerator
#         #    CC.male.wgt <- sample.array.wgt[, 1, 1]
#         #    CC.female.wgt <- sample.array.wgt[, 5, 1]
#         #    AC.male.wgt <- sample.array.wgt[, 1, 2]
#         #    AC.female.wgt <- sample.array.wgt[, 5, 2]   
#         
#         
#       } else if (median(transfers$raceShort) == 2) {        # Weight vectors for Non-Hispanic blacks/African-Americans
#         CC.male.wgt <- WGT.MAT.ARRAY[, 2, 1]
#         CC.female.wgt <- WGT.MAT.ARRAY[, 6, 1]
#         AC.male.wgt <- WGT.MAT.ARRAY[, 2, 2]
#         AC.female.wgt <- WGT.MAT.ARRAY[, 6, 2]
#         
#         RACE = "Black caregivers (Non-Hispanic)"
#         RACE.FILENAME <- "Black_or_African_American_caregivers"
#         
#         ## Activate only when no ATUS weights are used for the numerator
#         #    CC.male.wgt <- sample.array.wgt[, 2, 1]
#         #    CC.female.wgt <- sample.array.wgt[, 6, 1]
#         #    AC.male.wgt <- sample.array.wgt[, 2, 2]
#         #    AC.female.wgt <- sample.array.wgt[, 6, 2] 
#         
#         
#       } else if (median(transfers$raceShort) == 3) {        # Weight vectors for Non-Hispanic Asians
#         CC.male.wgt <- WGT.MAT.ARRAY[, 3, 1]
#         CC.female.wgt <- WGT.MAT.ARRAY[, 7, 1] 
#         AC.male.wgt <- WGT.MAT.ARRAY[, 3, 2]
#         AC.female.wgt <- WGT.MAT.ARRAY[, 7, 2]
#         
#         RACE <- "Asian caregivers (Non-Hispanic)"
#         RACE.FILENAME <- "Asian_caregivers"
#         
#         ## Activate only when no ATUS weights are used for the numerator
#         #    CC.male.wgt <- sample.array.wgt[, 3, 1]
#         #    CC.female.wgt <- sample.array.wgt[, 7, 1]
#         #    AC.male.wgt <- sample.array.wgt[, 3, 2]
#         #    AC.female.wgt <- sample.array.wgt[, 7, 2] 
#         
#       } else if (median(transfers$raceShort) == 4) {        # Weight vectors for Hispanic, any race
#         CC.male.wgt <- WGT.MAT.ARRAY[, 4, 1]
#         CC.female.wgt <- WGT.MAT.ARRAY[, 8, 1] 
#         AC.male.wgt <- WGT.MAT.ARRAY[, 4, 2]
#         AC.female.wgt <- WGT.MAT.ARRAY[, 8, 2]
#         
#         RACE <- "Hispanic caregivers (any race)"
#         RACE.FILENAME <- "Hispanic_caregivers"
#         
#         ## Activate only when no ATUS weights are used for the numerator
#         #    CC.male.wgt <- sample.array.wgt[, 4, 1]
#         #    CC.female.wgt <- sample.array.wgt[, 8, 1]
#         #    AC.male.wgt <- sample.array.wgt[, 4, 2]
#         #    AC.female.wgt <- sample.array.wgt[, 8, 2] 
#       }
#       
#       
#       ## Male caregiver to child groups from 0-4 to 10-14
#       for (i in 1:3) {
#         # To Male recipient
#         OUTPUT[, i, 1] <- HH.NUM.ARRAY[, i, 1] / CC.male.wgt
#         # To Female recipient
#         OUTPUT[, i, 2] <- HH.NUM.ARRAY[, i, 2] / CC.male.wgt
#       }
#       
#       ## Male caregiver to group 15-19 (combining childcare and adult care)
#       # To Male recipient
#       OUTPUT[, 4, 1] <- HH.NUM.ARRAY[, 4, 1] / (CC.male.wgt + AC.male.wgt)
#       # To Female recipient
#       OUTPUT[, 4, 2] <- HH.NUM.ARRAY[, 4, 2] / (CC.male.wgt + AC.male.wgt)
#       
#       
#       ## Male caregiver to adult groups from 20-24 to 85+
#       for (i in 5:18) {
#         # To Male recipient
#         OUTPUT[, i, 1] <- HH.NUM.ARRAY[, i, 1] / AC.male.wgt
#         # To Female recipient
#         OUTPUT[, i, 2] <- HH.NUM.ARRAY[, i, 2] / AC.male.wgt
#       }
#       
#       
#       
#       ## Female caregiver to groups from 0-4 to 10-14
#       for (i in 1:3) {
#         # To Male recipient
#         OUTPUT[, i, 3] <- HH.NUM.ARRAY[, i, 3] / CC.female.wgt
#         # To Female recipient
#         OUTPUT[, i, 4] <- HH.NUM.ARRAY[, i, 4] / CC.female.wgt
#       }
#       
#       ## Female caregiver to group 15-19 (combining childcare and adult care)
#       # To Male recipient
#       OUTPUT[, 4, 3] <- HH.NUM.ARRAY[, 4, 3] / (CC.female.wgt + AC.female.wgt)
#       # To Female recipient
#       OUTPUT[, 4, 4] <- HH.NUM.ARRAY[, 4, 4] / (CC.female.wgt + AC.female.wgt)
#       
#       
#       ## Female caregiver to groups from 20-24 to 85+
#       for (i in 5:18) {
#         # To Male recipient
#         OUTPUT[, i, 3] <- HH.NUM.ARRAY[, i, 3] / AC.female.wgt
#         # To Female recipient
#         OUTPUT[, i, 4] <- HH.NUM.ARRAY[, i, 4] / AC.female.wgt
#       }
#       
#       ## Drop the specific replicate weight variable to be replaced with the new one in the next iteration
#       ActSumFile <- ActSumFile[, 1:(length(ActSumFile)-1)]
#     
#       EST.LST[[v]] <- assign(paste0('OUTPUT.', v), OUTPUT)
#       
#   }
#   
#   
#   
#   
#   sq.diff.lst <- list()
#   for (i in 1:160) {
# 
#     ## Finding the squared difference between the replicate-weight estimates and the original mean estimate
#     sq.diff.lst[[i]] <- (EST.LST[[i]] - FINAL.OUTPUT)^2
#     
#   }
#   
#   ## Generating variance and computing the standard errors
#   VARIANCE.ARRAY <- (4/160) * Reduce('+', sq.diff.lst)
#   STD.ERR.ARRAY <- sqrt(VARIANCE.ARRAY)
#   
#   return(STD.ERR.ARRAY)
#   
# }
# 
# ## Generating standard errors for every cell corresponding to cells of FINAL.OUTPUT
# SE.ALL.CAREGIVERS <- SDERR.FUN(race = 0)
# SE.WHITE <- SDERR.FUN(race = 1)
# SE.BLACK <- SDERR.FUN(race = 2)
# SE.ASIAN <- SDERR.FUN(race = 3)
# SE.HISP <- SDERR.FUN(race = 4)
# 
# write.csv(SE.ALL.CAREGIVERS,  paste0(getwd(), "/results/ATUS_care_by_race_standard_errors_all_caregivers.csv")
