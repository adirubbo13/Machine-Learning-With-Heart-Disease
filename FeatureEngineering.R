HDdf <- read_csv("heart_disease_uci.csv") #reading in the CSV

#Updates data so that heart disease diagnoses is a two factor variable
HDdf <- within(HDdf,{num<-factor(num, labels = c("No","Yes","Yes","Yes","Yes"))}) 

#renaming variables so that they are easier to understand at a quick glance
names(HDdf)[names(HDdf) == "cp"] <- "CerebralPalsy"
names(HDdf)[names(HDdf) == "chol"] <- "Cholestoral"
names(HDdf)[names(HDdf) == "fbs"] <- "FastingBloodSugar"
names(HDdf)[names(HDdf) == "restecg"] <- "RestingECGReading"
names(HDdf)[names(HDdf) == "exang"] <- "ExercisedAngina"
names(HDdf)[names(HDdf) == "trestbps"] <- "RestingBPS"
names(HDdf)[names(HDdf) == "thalch"] <- "MaxHeartRate"
names(HDdf)[names(HDdf) == "oldpeak"] <- "STDepression"
names(HDdf)[names(HDdf) == "slope"] <- "STSlope"
names(HDdf)[names(HDdf) == "ca"] <- "FluoroscopyColoredVessels"
names(HDdf)[names(HDdf) == "thal"] <- "ThalassemiaDiagnoses"
names(HDdf)[names(HDdf) == "num"] <- "HeartDisease"

#saves data as a data frame
HDdf <- as.data.frame(HDdf)

skim(HDdf)

HDdf <- HDdf %>%
  mutate(age = ifelse(is.na(age), median(age, na.rm=TRUE), age),
         RestingBPS = ifelse(is.na(RestingBPS), median(RestingBPS, na.rm=TRUE), RestingBPS),
         Cholestoral = ifelse(is.na(Cholestoral), median(Cholestoral, na.rm=TRUE), Cholestoral),
         MaxHeartRate = ifelse(is.na(MaxHeartRate), median(MaxHeartRate, na.rm=TRUE), MaxHeartRate),
         STDepression = ifelse(is.na(STDepression), median(STDepression, na.rm=TRUE), STDepression),
         FluoroscopyColoredVessels = ifelse(is.na(FluoroscopyColoredVessels), median(FluoroscopyColoredVessels, na.rm=TRUE), FluoroscopyColoredVessels))

#NumericVariablesOnly
HDn <- select(HDdf, c("age","RestingBPS","Cholestoral","MaxHeartRate","STDepression","FluoroscopyColoredVessels","HeartDisease"))

#RegressionVariables
HDreg <- select(HDdf, c("HeartDisease","age","RestingBPS","Cholestoral","MaxHeartRate","STDepression","FluoroscopyColoredVessels","sex","FastingBloodSugar","RestingECGReading","ExercisedAngina","ThalassemiaDiagnoses"))

#Creating Dummy Variables for Regression (HDreg2)
#ifelse(test_expression, x, y)
HDreg$HeartDisease<-ifelse(HDreg$HeartDisease == "Yes",1.0,0)
names(HDreg)[names(HDreg) == "sex"] <- "Male"
HDreg$Male<-ifelse(HDreg$Male == "Male",1,0)
HDreg$FastingBloodSugar<-ifelse(HDreg$FastingBloodSugar == TRUE,1,0)
names(HDreg)[names(HDreg) == "RestingECGReading"] <- "Hypertrophy"
HDreg$Hypertrophy<-ifelse(HDreg$Hypertrophy == "normal",0,1)
HDreg$ExercisedAngina<-ifelse(HDreg$ExercisedAngina == TRUE,1,0)
HDreg$ThalassemiaDiagnoses<-ifelse(HDreg$ThalassemiaDiagnoses == "normal",0,1)

# Impute missing values with mean column-wise
for (col in names(HDreg)) {
  HDreg[is.na(HDreg[, col]), col] <- mean(HDreg[, col], na.rm = TRUE)
}