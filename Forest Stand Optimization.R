# at the beginning of your script with the 'library' statement (not require! see google)
#install.packages("xlsx")
library("xlsx")
#require(dplyr)
library(dplyr)



#################################################################################
# do once

#age of cohorts = numbers for the loop. 1 is equal to 30,2 is equal to 35 etc.
#all combinations of these harvest timings are possible
Combinations <-
  expand.grid(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  )

#ascending sorting

Combinations_sort <- t(apply(Combinations, 1, sort))

#only unique combinations are important for further calculations
Combinations_unique <- unique(Combinations_sort)
Combinations_unique <- as.data.frame(Combinations_unique)


#fixed values in data.frame
#growth_norm is the value growth factor without additional growth for each possible harvest timing
valuegrowth <- data.frame(
  Alter = seq(from = 30, to = 100, by = 5),
  Zuwachs_norm = c(
    1.10013420400602,
    1.07444737682355,
    1.05476666390227,
    1.04554437542938,
    1.03299978888970,
    1.02889727302238,
    1.02500361193436,
    1.02270595969040,
    1.01703142374639,
    1.01581881322597,
    1.01325801690725,
    1.01258726777500,
    1.01045400581946,
    1.01017280958037,
    1.00000000000000
  ),
  Zuwachs_gest = c(   #Knoke and Plusczyk (2001)
    1.107634204006020,
    1.081947376823550,
    1.062266663902270,
    1.053044375429380,
    1.037999788889700,
    1.033897273022380,
    1.030003611934360,
    1.027705959690400,
    1.022031423746390,
    1.020818813225970,
    1.018258016907250,
    1.017587267775000,
    1.015454005819460,
    1.015172809580370,
    1.000000000000000
  ),
  Zahlencode = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
)


#Creating results matrix
results <-
  data.frame(
    Combinations_unique,
    value_C1 = rep(0, 11628),
    value_C2 = rep(0, 11628),
    value_C3 = rep(0, 11628),
    value_C4 = rep(0, 11628),
    value_C5 = rep(0, 11628)
  )

i = 2



harvest <- function(value_C1, b, a) {
  value_C2 <- value_C1
  if (Combinations_unique[j, b] == Combinations_unique[j, a])
    return(value_C2)
  if (Combinations_unique[j, b] <= Combinations_unique[j, a] + 3) {
    for (i in seq(from = Combinations_unique[j, a],
                  to = Combinations_unique[j, b] - 1,
                  by = 1)) {
      value_C2 <- value_C2 * valuegrowth[i, 3] ^ 5
    }
  } else {
    for (i in seq(from = Combinations_unique[j, a],
                  to = Combinations_unique[j, a] + 2,
                  by = 1)) {
      value_C2 <- value_C2 * valuegrowth[i, 3] ^ 5
    }
    for (i in seq(from = Combinations_unique[j, a] + 3,
                  to = Combinations_unique[j, b] - 1,
                  by = 1)) {
      value_C2 <- value_C2 * valuegrowth[i, 2] ^ 5
    }
  }
  return(value_C2)
}



#calculation of all combinations
for (j in 1:nrow(Combinations_unique)) {
  #harvest cohort C1
  if (Combinations_unique[j, 1] == 1) {
    value_C1 <-
      809.3862127        #value of C1 at age 30 when harvested,
    
  } else {
    for (i in 1:Combinations_unique[j, 1] - 1) {
      if (i == 1) {
        value_C1 <- 809.3862127 * valuegrowth[i, 2] ^ 5
      }  else {
        value_C1 <- value_C1 * valuegrowth[i, 2] ^ 5
      }
    }
  }
  
  #harvest Cohort C2
  #same harvest timing as C1
  value_C2 <- harvest(value_C1, 2, 1)
  value_C3 <- harvest(value_C2, 3, 2)
  value_C4 <- harvest(value_C3, 4, 3)
  value_C5 <- harvest(value_C4, 5, 4)
  
  #writing results in dataframe
  results[j, 6:10] <-
    c(value_C1,
      value_C2,
      value_C3,
      value_C4,
      value_C5)
}

#Changing numbers to age
results_age <- results
results_age[, 1:5][results_age[, 1:5] == 1] <- 30
results_age[, 1:5][results_age[, 1:5] == 2] <- 35
results_age[, 1:5][results_age[, 1:5] == 3] <- 40
results_age[, 1:5][results_age[, 1:5] == 4] <- 45
results_age[, 1:5][results_age[, 1:5] == 5] <- 50
results_age[, 1:5][results_age[, 1:5] == 6] <- 55
results_age[, 1:5][results_age[, 1:5] == 7] <- 60
results_age[, 1:5][results_age[, 1:5] == 8] <- 65
results_age[, 1:5][results_age[, 1:5] == 9] <- 70
results_age[, 1:5][results_age[, 1:5] == 10] <- 75
results_age[, 1:5][results_age[, 1:5] == 11] <- 80
results_age[, 1:5][results_age[, 1:5] == 12] <- 85
results_age[, 1:5][results_age[, 1:5] == 13] <- 90
results_age[, 1:5][results_age[, 1:5] == 14] <- 95
results_age[, 1:5][results_age[, 1:5] == 15] <- 100

##################################################################################


# all three must have the same length
r_params <-
  c(1.035, #discount rates for sensetivity analysis
    1.03,
    1.025,
    1.02,  #discount rate for the baseline scenario
    1.015,
    1.01)
b_params <-
  c(
    243.016381377131,
    #depending on the discount rate the bevor calculated LEV (land expectation value)for one cohort
    112.914086480992,
    38.6532093718817,
    541.91460371773,
    1215.93275513975,
    4368.54086230392
  )  # depends on r = LEV



for (i in 1:length(r_params)) {
  r <- r_params[i]
  b <- b_params[i]
  
  
  
  #costs for artificial regeneration
  k <- 2000
  #VaR (Value at risk) can change (eg.: 0.01 oder 0.1)
  l <- 0.05
  #future LEVs could be also reduced
  n <- 1
  #cohortard deviation introduced as a variation coefficient of the discounted net revenue and the discounted LEV can change in the sensetivity analysis to 0.1 and 0.5
  s <- 0.2
  
  
  
  #Calculating NPV and Rowsum
  results_age_discountrate <-
    results_age %>%
    rowwise() %>%
    mutate(
      #Calculating NPV
      NPV_C1 = value_C1 * r ^ -V1,
      #v is equal to the harvest timing of a cohort [1-5]
      NPV_C2 = value_C2 * r ^ -V2,
      NPV_C3 = value_C3 * r ^ -V3,
      NPV_C4 = value_C4 * r ^ -V4,
      NPV_C5 = value_C5 * r ^ -V5,
      all_NPV = NPV_C1 + NPV_C2 + NPV_C3 +
        NPV_C4 + NPV_C5,
      
      
      SD_C5 =                                    #standard deviation = coefficient of variation of 20% of the NPV for each cohort is calculated.
        #if age cohort C5 = age cohort C4
        if (V5 == V4) {
          #and if age cohort C5=age cohort C3
          #and age cohort C5=age cohort C2 and
          #age cohort C5 = age cohort C1
          if (V5 == V3 & V5 == V2 & V5 == V1) {
            # SD_1, SD_2,SD_3,SD_4 =0
            SD_C1 = 0
            SD_C2 = 0
            SD_C3 = 0
            SD_C4 = 0
            #and SD_5 = (SD_coeff+Summe(NPV))^2
            SD_C5 = (s * (NPV_C1 + NPV_C2 + NPV_C3 + NPV_C4 + NPV_C5)) ^ 2
          } else if (#and if  age cohort C5=age cohort C3
            #and age cohort C5=age cohort C2
            V5 == V3 & V5 == V2) {
            # SD_2,SD_3,SD_4 =0
            SD_C2 = 0
            SD_C3 = 0
            SD_C4 = 0
            #and SD_5 = (SD_coeff+Summe(NPV))^2
            SD_C5 = (s * (NPV_C2 + NPV_C3 +
                            NPV_C4 + NPV_C5)) ^ 2
          } else if (#and if age cohort C5=age cohort C3
            V5 == V3) {
            #SD_3,SD_4 =0
            SD_C3 = 0
            SD_C4 = 0
            #and SD_5 = (SD_coeff+Summe(NPV))^2
            SD_C5 = (s * (NPV_C3 + NPV_C4 +
                            NPV_C5)) ^ 2
          } else {
            #if age cohort C5 = age cohort C4
            #SD_4=0
            SD_C4 = 0
            #and SD_5 = (SD_coeff+Summe(NPV))^2
            SD_C5 = (s * (NPV_C4 + NPV_C5)) ^
              2
          }
          #SD_5 normal
        }  else
          SD_C5 = (s * NPV_C5) ^ 2 ,
      
      SD_C4 =  if (V5 == V4) {
        SD_C4 = 0
      } else if (V4 == V3) {
        if (V4 == V2 & V4 == V1) {
          SD_C1 = 0
          SD_C2 = 0
          SD_C3 = 0
          SD_C4 = (s * (NPV_C1 + NPV_C2 +
                          NPV_C3 + NPV_C4)) ^ 2
        } else if (V4 == V2) {
          SD_C2 = 0
          SD_C3 = 0
          SD_C4 = (s * (NPV_C2 + NPV_C3 +
                          NPV_C4)) ^ 2
        } else {
          SD_C3 = 0
          SD_C4 = (s * (NPV_C3 + NPV_C4)) ^
            2
        }
      } else
        SD_C4 = (s * NPV_C4) ^ 2 ,
      
      SD_C3 =  if (V4 == V3) {
        SD_C3 = 0
      } else if (V3 == V2) {
        if (V3 == V1) {
          SD_C1 = 0
          SD_C2 = 0
          SD_C3 = (s * (NPV_C1 + NPV_C2 +
                          NPV_C3)) ^ 2
        } else {
          SD_C2 = 0
          SD_C3 = (s * (NPV_C2 + NPV_C3)) ^
            2
        }
      } else
        SD_C3 = (s * NPV_C3) ^ 2  ,
      #if age 3 = age 2 dann SD_2=0, if not age 2=age1
      SD_C2 =  if (V3 == V2) {
        SD_C2 = 0
      } else if (V2 == V1) {
        #SD_1=0 and SD_2 comined NPV
        SD_C1 = 0
        SD_C2 = (s * (NPV_C1 + NPV_C2)) ^
          2
      } else
        SD_C2 = (s * NPV_C2) ^ 2 ,
      SD_C1 =  if (V2 == V1) {
        SD_C1 = 0
      } else
        SD_C1 = (s * NPV_C1) ^ 2 ,
      
      
      LEV_C1 = b / r ^ V1 * n,
      #LEV discounted depending on harvest timing of C1-C5 in the first rotaion period
      LEV_C2 = b / r ^ V2 * n,
      LEV_C3 = b / r ^ V3 * n,
      LEV_C4 = b / r ^ V4 * n,
      LEV_C5 = b / r ^ V5 * n,
      
      SD_LEV_C5 =
        if (V5 == V4) {
          if (V5 == V3 & V5 == V2 & V5 == V1) {
            #SD_1, SD_2,SD_3,SD_4 =0
            SD_LEV_C1 = 0
            SD_LEV_C2 = 0
            SD_LEV_C3 = 0
            SD_LEV_C4 = 0
            SD_LEV_C5 = (s * (LEV_C1 + LEV_C2 + LEV_C3 + LEV_C4 + LEV_C5) * n) ^ 2
          } else if (#and ifage cohort C5=age cohort C3
            #and age cohort C5=age cohort C2
            V5 == V3 & V5 == V2) {
            SD_LEV_C2 = 0
            SD_LEV_C3 = 0
            SD_LEV_C4 = 0
            SD_LEV_C5 = (s * (LEV_C2 + LEV_C3 + LEV_C4 + LEV_C5) *
                           n) ^ 2
          } else if (#and ifage cohort C5=age cohort C3
            V5 == V3) {
            SD_LEV_C3 = 0
            SD_LEV_C4 = 0
            SD_LEV_C5 = (s * (LEV_C3 + LEV_C4 + LEV_C5) *
                           n) ^ 2
          } else {
            #if age cohort C5 = age cohort C4
            SD_LEV_C4 = 0
            SD_LEV_C5 = (s * (LEV_C4 + LEV_C5) * n) ^ 2
          }
          #Sonst SD_5 normal
        }   else
          SD_LEV_C5 = (s * LEV_C5 * n) ^ 2 ,
      
      SD_LEV_C4 =  if (V5 == V4) {
        SD_LEV_C4 = 0
      } else if (V4 == V3) {
        if (V4 == V2 & V4 == V1) {
          SD_LEV_C1 = 0
          SD_LEV_C2 = 0
          SD_LEV_C3 = 0
          SD_LEV_C4 = (s * (LEV_C1 + LEV_C2 + LEV_C3 + LEV_C4) *
                         n) ^ 2
        } else if (V4 == V2) {
          SD_LEV_C2 = 0
          SD_LEV_C3 = 0
          SD_LEV_C4 = (s * (LEV_C2 + LEV_C3 + LEV_C4) *
                         n) ^ 2
        } else {
          SD_LEV_C3 = 0
          SD_LEV_C4 = (s * (LEV_C3 + LEV_C4) * n) ^ 2
        }
      } else
        SD_BWE_C4 = (s * LEV_C4 * n) ^ 2 ,
      
      SD_LEV_C3 =  if (V4 == V3) {
        SD_LEV_C3 = 0
      } else if (V3 == V2) {
        if (V3 == V1) {
          SD_LEV_C1 = 0
          SD_LEV_C2 = 0
          SD_LEV_C3 = (s * (LEV_C1 + LEV_C2 + LEV_C3) *
                         n) ^ 2
        } else {
          SD_LEV_C2 = 0
          SD_LEV_C3 = (s * (LEV_C2 + LEV_C3) * n) ^ 2
        }
      } else
        SD_LEV_C3 = (s * LEV_C3 * n) ^ 2  ,
      SD_LEV_C2 =  if (V3 == V2) {
        SD_LEV_C2 = 0
      } else if (V2 == V1) {
        SD_LEV_C1 = 0
        SD_LEV_C2 = (s * (LEV_C1 + LEV_C2) * n) ^ 2
      } else
        SD_LEV_C2 = (s * LEV_C2 * n) ^ 2 ,
      SD_LEV_C1 =  if (V2 == V1) {
        SD_LEV_C1 = 0
      } else
        SD_LEV_C1 = (s * LEV_C1 * n) ^ 2 ,
      
      
      all1 = NPV_C1 + LEV_C1,
      all2 = NPV_C2 + LEV_C2,
      all3 = NPV_C3 + LEV_C3,
      all4 = NPV_C4 + LEV_C4,
      all5 = NPV_C5 + LEV_C5,
      all = all1 + all2 + all3 + all4 + all5 -
        k,
      SD_all = SD_C1 + SD_C2 + SD_C3 +
        SD_C4 + SD_C5 + SD_LEV_C1 + SD_LEV_C2 + SD_LEV_C3 +
        SD_LEV_C4 + SD_LEV_C5,
      VaR = qnorm(l, all, sqrt(SD_all))
    )  #NORMINV function and calculating Vakue at Risk (VaR)
  
  #Looking for row with the best result
  print(paste("results_test_growth gain", "R", r, "L", l, "S", s))
  print(results_age_discountrate[which.max(results_age_discountrate$all),])
  
  #Looking for row with the best result including uncertainty
  
  print(results_age_discountrate[which.max(results_age_discountrate$VaR),])
  
  #an excel sheet for each discount rate is built
  write.xlsx2(
    results_age_discountrate,
    paste0("ergebnis_test_ohne", "R", r, "L", l , "S", s, ".xlsx")
  )
}

? qnorm
