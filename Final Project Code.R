############### prework ####################
# read in libraries 
library(tidyverse) # general dataframe manipulation
library(MASS) # statistical package
library(rms) # regression modeling package

# read in data file and column names file 
crime<- read_csv("crime.txt",col_names=F)
colsnames <- read_csv("colnames.csv")
colnames(crime)<-colnames(colsnames)

############### Step 1: Exploratory Data Analysis ####################
# get basic view of raw dataset
glimpse(crime)

# function to clean up columns that had ? in them 
convert_types <- function(x) {
  stopifnot(is.list(x))
  x[] <- rapply(x, utils::type.convert, classes = "character",
                how = "replace", as.is = TRUE)
  return(x)
}

# replace question marks with NAs and convert all columns back to what they should be
crime[crime=="?"] <- NA
crime <- convert_types(crime)

# analyze the removed columns 
crime.removed <- crime %>% dplyr::select(county,community,communityname,c(LemasSwornFT:PolicAveOTWorked),c(PolicCars:LemasGangUnitDeploy),PolicBudgPerPop) 

# make table describing the columns removed from analysis
colSums(is.na(crime.removed))%>% data.frame()%>% rename("Count of NAs" = ".") %>% rownames_to_column("Removed Variables")%>% mutate(`Percent NAs` = `Count of NAs`/dim(crime.removed)[1],`Column Type` = sapply(crime.removed,class))

# remove columns with too many NA values
crime <- crime %>% dplyr::select(-county,-community,-communityname,-c(LemasSwornFT:PolicAveOTWorked),-c(PolicCars:LemasGangUnitDeploy),-PolicBudgPerPop)
# remove rows with NA values
crime <- crime %>% na.omit()

## In Depth Single Variable Exploration

ggplot(
  data = crime,
  aes(y = PctEmploy,x = ViolentCrimesPerPop)
) + geom_point() + ggtitle("The percentage of people employed vs crimes shows lower employment trends with more crimes")

ggplot(
  data = crime,
  aes(y = perCapInc,x = ViolentCrimesPerPop)
) + geom_point() + ggtitle("Per capita income vs crimes shows lower income trends with more crimes")

ggplot(
  data = crime,
  aes(y = RentMedian,x = ViolentCrimesPerPop)
) + geom_point() + ggtitle("Lower Median Rent vs crime shows lower rent trends with more crimes")

ggplot(
  data = crime,
  aes(y = PctHousOwnOcc,x = ViolentCrimesPerPop)
) + geom_point() + ggtitle("Percentage of houses owned by the occupants shows lower homeownership trends with more crime")


############### # Step 2: Fit a linear Model ####################

# make lm with 4 predictors chosen in EDA
simple_4var_lm <- lm(ViolentCrimesPerPop ~ PctEmploy + perCapInc + RentMedian + PctHousOwnOcc,data = crime)
summary(simple_4var_lm)

############### Step 3: Perform model selection ####################

### fastbw 
# fit ols model with every variable
mod.ols <- ols( ViolentCrimesPerPop ~ ., data=crime)
# run fastbw variable selection
fastbw.parameters <- fastbw(mod.ols, rule="p", sls=0.05)
# print out variables saved
fastbw.parameters$coefficients

### stepAIC 
mod <- lm(ViolentCrimesPerPop ~ ., data=crime)
summary(mod)
mod.aic <- stepAIC(mod)

# Final stepAIC model
summary(mod.aic)

# Final fastbw model
mod.fastbw <- lm(ViolentCrimesPerPop ~ state+racepctblack+agePct12t29+pctUrban +            
                   pctWWage+pctWFarmSelf+pctWInvInc+OtherPerCap +          
                   PctEmploy+MalePctDivorce+MalePctNevMarr+TotalPctDiv+
                   PctKids2Par+PctWorkMom+PctIlleg+NumImmig+       
                   PctNotSpeakEnglWell+PersPerOccupHous+PersPerRentOccHous+PctPersOwnOccup+    
                   PctPersDenseHous+HousVacant+PctHousOccup+PctHousOwnOcc+       
                   OwnOccLowQuart+RentLowQ+MedRent+MedOwnCostPctIncNoMtg+
                   NumStreet+PctForeignBorn , data=crime)
summary(mod.fastbw)



############### Step 4: Apply diagnostics to the model ####################

## Fitted values vs residuals plot
plot(mod.aic$fitted.values, mod.aic$residuals)

## Q-Q plot
qqnorm(mod.aic$residuals)

## Lagged residual plot
n <- length(residuals(mod.aic))
plot(tail(residuals(mod.aic),n-1)~head(residuals(mod.aic),n-1),
     xlab = expression(hat(epsilon)), ylab=expression(hat(epsilon)[i+1]))
abline(h=0,v=0,col=grey(.75))

############### Step 5: Investigate fit for individual observations ####################

X <- model.matrix(mod.aic)
n <- dim(X)[1]
p <- dim(X)[2]

hatv <- hatvalues(mod.aic)

# Leverage investigation

high.leverage <- hatv[hatv>2*p/n]
print(paste("Number of high leverage points:",length(high.leverage)))

# Outlier investigation

rstd <- rstandard(mod.aic)
outlier <- rstd[abs(rstd)>3]

print(paste("Number of outlier points:",length(outlier)))


rstd.hand <- mod.aic$residuals/(sqrt(1- hatv)*
                                  sqrt(sum(mod.aic$residuals^2)/mod.aic$df.residual))

# Problematic investigation

problematic <- which(hatv>2*p/n&abs(rstd)>3)
print(paste("Number of problematic points:",length(problematic)))

# Cooks distance influence investigation

cook <- cooks.distance(mod.aic)

print(paste("Largest cook's distance:",max(cook)))


num.df <- p
den.df <- n-p
F.thresh <- qf(0.5,num.df,den.df)

print(paste("F statistic threshold:",F.thresh))

influential <- which(hatv>2*p/n&abs(rstd)>3&cook>F.thresh)
print(paste("Number of influential points:",length(influential)))

############### Step 6: Apply transformations to model as needed ####################

# make Violent Crimes Per Pop always positive for boxcox
crime.pos <- crime
crime.pos$ViolentCrimesPerPop <- crime.pos$ViolentCrimesPerPop + 1

mod.aic.pos <- lm(formula = ViolentCrimesPerPop ~ state + fold + racepctblack + 
                    racePctHisp + agePct12t29 + pctUrban + medIncome + pctWWage + 
                    pctWFarmSelf + pctWInvInc + pctWRetire + medFamInc + whitePerCap + 
                    indianPerCap + OtherPerCap + PctPopUnderPov + PctLess9thGrade + 
                    PctEmploy + PctEmplManu + PctOccupManu + PctOccupMgmtProf + 
                    MalePctDivorce + MalePctNevMarr + TotalPctDiv + PctKids2Par + 
                    PctWorkMom + NumIlleg + PctIlleg + NumImmig + PctNotSpeakEnglWell + 
                    PctLargHouseOccup + PersPerOccupHous + PersPerRentOccHous + 
                    PctPersOwnOccup + PctPersDenseHous + HousVacant + PctHousOccup + 
                    PctHousOwnOcc + PctVacantBoarded + PctVacMore6Mos + OwnOccLowQuart + 
                    OwnOccMedVal + RentLowQ + MedRent + MedOwnCostPctIncNoMtg + 
                    NumInShelters + NumStreet + PctForeignBorn + PctSameCity85 + 
                    PctUsePubTrans + LemasPctOfficDrugUn, data = crime.pos)

print(paste("R-squared value before Box-Cox transformation:",
            round(summary(mod.aic.pos)$r.squared,4)))

bc <- boxcox(mod.aic.pos, plotit=T)
lambda <- bc$x[which.max(bc$y)]
print(paste("Box-Cox transformation Optimal Lambda Value:",lambda))

# fit model with transformation

mod.aic.pos.transform <- lm(formula = ViolentCrimesPerPop^lambda ~ state + fold + 
                              racepctblack + racePctHisp + agePct12t29 + pctUrban + medIncome + pctWWage + 
                              pctWFarmSelf + pctWInvInc + pctWRetire + medFamInc + whitePerCap + 
                              indianPerCap + OtherPerCap + PctPopUnderPov + PctLess9thGrade + 
                              PctEmploy + PctEmplManu + PctOccupManu + PctOccupMgmtProf + 
                              MalePctDivorce + MalePctNevMarr + TotalPctDiv + PctKids2Par + 
                              PctWorkMom + NumIlleg + PctIlleg + NumImmig + PctNotSpeakEnglWell + 
                              PctLargHouseOccup + PersPerOccupHous + PersPerRentOccHous + 
                              PctPersOwnOccup + PctPersDenseHous + HousVacant + PctHousOccup + 
                              PctHousOwnOcc + PctVacantBoarded + PctVacMore6Mos + OwnOccLowQuart + 
                              OwnOccMedVal + RentLowQ + MedRent + MedOwnCostPctIncNoMtg + 
                              NumInShelters + NumStreet + PctForeignBorn + PctSameCity85 + 
                              PctUsePubTrans + LemasPctOfficDrugUn, data = crime.pos)

print(paste("R-squared value after Box-Cox transformation:",
            round(summary(mod.aic.pos.transform)$r.squared,4)))

#### Step 7: Report inferences and make predictions using your final model #############

mod.coef <- data.frame(coef(summary(mod.aic.pos.transform)))

mod.coef$Predictor <- rownames(mod.coef)
rownames(mod.coef)<- NULL
mod.coef <- mod.coef[,c(5,1,4)] 
colnames (mod.coef) <- c("Predictor", "Parameter Estimate", "p-Value")

output <- mod.coef
output$`p-Value` <- format(output$`p-Value`, digits = 3)
output%>% data.frame()

print(paste("The R-squared value for this model is:",
            round(summary(mod.aic.pos.transform)$r.squared,4)))

confint(mod.aic.pos.transform, 'racepctblack', level=0.95)

# create new data out of median of all columns used in model
mod.data <- crime.pos %>% dplyr::select(ViolentCrimesPerPop, state , fold , 
                                        racepctblack , racePctHisp , agePct12t29 , pctUrban , medIncome , pctWWage , 
                                        pctWFarmSelf , pctWInvInc , pctWRetire , medFamInc , whitePerCap , 
                                        indianPerCap , OtherPerCap , PctPopUnderPov , PctLess9thGrade , 
                                        PctEmploy , PctEmplManu , PctOccupManu , PctOccupMgmtProf , 
                                        MalePctDivorce , MalePctNevMarr , TotalPctDiv , PctKids2Par , 
                                        PctWorkMom , NumIlleg , PctIlleg , NumImmig , PctNotSpeakEnglWell , 
                                        PctLargHouseOccup , PersPerOccupHous , PersPerRentOccHous , 
                                        PctPersOwnOccup , PctPersDenseHous , HousVacant , PctHousOccup , 
                                        PctHousOwnOcc , PctVacantBoarded , PctVacMore6Mos , OwnOccLowQuart , 
                                        OwnOccMedVal , RentLowQ , MedRent , MedOwnCostPctIncNoMtg , 
                                        NumInShelters , NumStreet , PctForeignBorn , PctSameCity85 , 
                                        PctUsePubTrans , LemasPctOfficDrugUn)
newdata <-  data.frame(rbind(apply(mod.data, 2, median)))

# confidence interval of ViolentCrimesPerPop for new data
predict(mod.aic.pos.transform, newdata, interval="confidence") 

# prediction interval of ViolentCrimesPerPop for new data
predict(mod.aic.pos.transform, newdata, interval = "predict") 