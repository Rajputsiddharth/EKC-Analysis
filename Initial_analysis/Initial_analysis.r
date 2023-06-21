# QUESTION-1

df <- read.csv("/Users/dhruvsood/Downloads/NDAP_REPORT_7065.csv")

View(df)


# Fetching data from different columns of df dataset
df$District.LGD.Code
df$YearCode

# Transforming this original dataset into  district-year level data-set that includes district-year ID for each row in the sample.
df$District_Year_ID<-with(df,(df$District.LGD.Code)*10000+df$YearCode)

#Removing all rows with NA values with respect to the variables in the data-set
remove_me_na <- c(11:32)
df <- df[rowSums(is.na(df[, remove_me_na])) != length(remove_me_na), ]

#Serializing for uniqueness again
library(dplyr)
df <- df %>% mutate(KeyValue = row_number())
df <- df %>% select(KeyValue, everything())

# Upper case values to State
df$State <- toupper(df$State)
colnames(df)[34] <- "YEAR"



#Loading the SDP Table in pf and modifying it in accordance with 'df' data-set
pf <- read.csv("/Users/dhruvsood/Downloads/HSB.csv")
#pf <- pf[-c(18,25,31,39),]
names(pf) <- gsub("\\.\\.\\.", " AND ", names(pf)) 
names(pf) <- gsub("\\.", " ", names(pf))

View(pf)
#Cleaning the data-set and modifying it --> YEAR format,NA values inclusion
pf <- pf[-c(1:4),]
pf$YEAR <- substr(pf$YEAR, 1, 4)
pf[pf == ""] <- NA
pf[pf == "-"] <- NA

View(pf)

#Changing the format of 'pf' to Year->State->SDP
library(tidyr)
pf_f <- gather(pf, key = "State", value = "value", -YEAR)
View(pf_f)

#Merging SDP with NDAP report on the basis of year and state.
merged_data <- merge(df,pf_f, by = c("YEAR","State"))
View(merged_data)



# Loading the GINI Index merged csv file from the pdf File with a lot of efforts.
cf <- read.csv("/Users/dhruvsood/Downloads/Gini.csv")
View(cf)

# Renaming the columns 
colnames(cf)[1] <- "District"
colnames(cf)[2] <- "Gini.Index"

# Merging the Gini Index by districts in the already merged data by NDAP & SDP 
merged_data_final <- merge(cf, merged_data, by = c("District"))
View(merged_data_final)
# Renaming the columns
colnames(merged_data_final)[37] <- "SDP"

# Writing the csv final file to a location  
write.csv(merged_data_final, file = "NN.csv", row.names = FALSE)

#QUESTION 6

#Reading the merged CSV file 
nf <- read.csv("/Users/dhruvsood/NN.csv")
View(nf)
# Omitting the NA values for regression analysis
nf <- nf[!is.na(nf$Amount.of.Electrical.Conductivity),]

#numeric check
library(dplyr)

nf <- nf %>%
  mutate_at(vars(14:35, 37,38), as.numeric)

# Taken Amount.of.Electrical.Conductivity as our environmental quality indicator
#Running the RM
rm <- lm(formula = merged_data_final$Amount.of.Electrical.Conductivity ~ merged_data_final$SDP, data = nf)
summary(rm)
stargazer(rm) 
stargazer(rm, type = "text", out = "rm.txt")


#Residuals
residuals <- residuals(rm)

# Calculate model residuals
nf$residuals <- residuals(rm)
nf

# Plot SDP vs. EQI with residuals as the color
plot(nf$SDP, nf$Amount.of.Electrical.Conductivity, col = abs(nf$residuals), 
     xlab = "SDP", ylab = "Environmental Quality Indicator",
     main = "Residuals Colored by SDP vs. EQI")

# Plot residuals vs. SDP
plot(nf$SDP, nf$resid, 
     xlab = "SDP", ylab = "Residuals",
     main = "Residuals vs. SDP")

# Plot predicted vs. true Environmental Quality Indicator
plot(nf$Amount.of.Electrical.Conductivity, predict(rm), col = "brown",
     xlab = "True Environmental Quality Indicator", ylab = "Predicted Environmental Quality Indicator",
     main = "Predicted vs. True Environmental Quality Indicator")


hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")
sum_resid <- sum(residuals)
print(paste0("Sum of residuals: ", sum_resid))
print("Since the sum of the residuals is such a small value that it is tending to zero, we can assume it to zero and hence it is verified that ∑i,t ûi,t = 0")



#Estimating the given regression
nf$SDP_sq <- nf$SDP^2
nf$SDP_cu <- nf$SDP^3
mr <- lm(formula = Amount.of.Electrical.Conductivity ~ SDP + SDP_sq + SDP_cu + Gini.Index, data = nf)
summary(mr)
stargazer(mr) 
stargazer(mr, type = "text", out = "mr.txt")


hist(merged_data_final$SDP, main = "Histogram of SDP",xlab ="SDP")
hist(merged_data_final$Gini.Index, main = "Histogram of Gini",xlab = "Gini")
boxplot(merged_data_final$SDP, main = "Boxplot of SDP",ylab=a,xlab="SDP")
boxplot(merged_data_final$Gini.Index, main = "Boxplot of Gini",ylab=a,xlab="Gini")

library(ggplot2)
#####################################################################################
# Amount.of.Arsenic
summary(merged_data_final$Amount.of.Arsenic)
# Histogram
ggplot(merged_data_final, aes(x = Amount.of.Arsenic)) + 
  geom_histogram(fill = "white", colour = "black")
labs(x = "Amount.of.Arsenic)", y = "Frequency")
# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Arsenic)) +
  geom_boxplot() +
  labs(y = "Amount.of.Arsenic")
#####################################################################################
# Amount.of.carbonate
summary(merged_data_final$Amount.of.carbonate)
# Histogram;
ggplot(merged_data_final, aes(x = Amount.of.carbonate)) + 
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.carbonate", y = "Frequency")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.carbonate)) +
  geom_boxplot() +
  labs(y = "Amount.of.Carbonate")

#####################################################################################
# Amount.of.Calcium
summary(merged_data_final$Amount.of.Calcium)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Calcium)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Calcium", y = "Frequency")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Calcium)) +
  geom_boxplot() +
  labs(y = "Amount.of.Calcium")
#####################################################################################
# Amount.of.Chloride
summary(merged_data_final$Amount.of.Chloride)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Chloride)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Chloride", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Chloride)) +
  geom_boxplot() +
  labs(y = "Amount.of.Chloride")
#####################################################################################
# Amount.of.Electrical.Conductivity
summary(merged_data_final$Amount.of.Electrical.Conductivity)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Electrical.Conductivity)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount of Electrical Conductivity", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Electrical.Conductivity)) +
  geom_boxplot() +
  labs(y = "Amount of Electrical Conductivity")
#####################################################################################
# Amount.of.Fluorine
summary(merged_data_final$Amount.of.Fluorine)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Fluorine)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Fluorine", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Fluorine)) +
  geom_boxplot() +
  labs(y = "Amount.of.Fluorine")
#####################################################################################
# Amount.of.Iron
summary(merged_data_final$Amount.of.Iron)
# Histogram
ggplot(data = merged_data_final, aes(x = Amount.of.Iron)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Iron", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Iron)) +
  geom_boxplot() +
  labs(y = "Amount.of.Iron")
#####################################################################################
# Amount.of.Hydrogencarbonate
summary(merged_data_final$Amount.of.Hydrogencarbonate)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Hydrogencarbonate)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.HydrogenCarbonate", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Hydrogencarbonate)) +
  geom_boxplot() +
  labs(y = "Amount.of.HydrogenCarbonate")

#####################################################################################
# Amount.of.Potassium
summary(merged_data_final$Amount.of.Potassium)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Potassium)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Potassium", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Potassium)) +
  geom_boxplot() +
  labs(y = "Amount.of.Potassium")
#####################################################################################
# Amount.of.Magnesium
summary(merged_data_final$Amount.of.Magnesium)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Magnesium)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Magnesium", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Magnesium)) +
  geom_boxplot() +
  labs(y = "Amount.of.Magnesium")
#####################################################################################
# Amount.of.Nitrate
summary(merged_data_final$Amount.of.Nitrate)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Nitrate)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Nitrate", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Nitrate)) +
  geom_boxplot() +
  labs(y = "Amount.of.Nitrate")
#####################################################################################
# Amount.of.Sodium
summary(merged_data_final$Amount.of.Sodium)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Sodium)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Sodium", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Sodium)) +
  geom_boxplot() +
  labs(y = "Amount.of.Sodium")
#####################################################################################
# Amount.of.Phosphate.Ion
summary(merged_data_final$Amount.of.Phosphate.Ion)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Phosphate.Ion)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Phosphate.Ion", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Phosphate.Ion)) +
  geom_boxplot() +
  labs(y = "Amount.of.Phosphate.Ion")
#####################################################################################
# Amount.of.Residual.Sodium.Carbonate
summary(merged_data_final$Amount.of.Residual.Sodium.Carbonate)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Residual.Sodium.Carbonate)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Residual.Sodium.Carbonate", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Residual.Sodium.Carbonate)) +
  geom_boxplot() +
  labs(y = "Amount.of.Residual.Sodium.Carbonate")
#####################################################################################
# Amount.of.Sodium.absorption.ratio
summary(merged_data_final$Amount.of.Sodium.absorption.ratio)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Sodium.absorption.ratio)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Sodium.absorption.ratio", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Sodium.absorption.ratio)) +
  geom_boxplot() +
  labs(y = "Amount.of.Sodium.absorption.ratio")
#####################################################################################
# Amount.of.Sulfate
summary(merged_data_final$Amount.of.Sulfate)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Sulfate)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Sulfate", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Sulfate)) +
  geom_boxplot() +
  labs(y = "Amount.of.Sulfate")
#####################################################################################
# Amount.of.Silicon.dioxide
summary(merged_data_final$Amount.of.Silicon.dioxide)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Silicon.dioxide)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Silicon.dioxide", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Silicon.dioxide)) +
  geom_boxplot() +
  labs(y = "Amount.of.Silicon.dioxide")
#####################################################################################
# Amount.of.Hardness.Total
summary(merged_data_final$Amount.of.Hardness.Total)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Hardness.Total)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Hardness.Total", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Hardness.Total)) +
  geom_boxplot() +
  labs(y = "Amount.of.Hardness.Total")
#####################################################################################
# Amount.of.Alkalinity.Total
summary(merged_data_final$Amount.of.Alkalinity.Total)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Alkalinity.Total)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Alkalinity.Total", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Alkalinity.Total)) +
  geom_boxplot() +
  labs(y = "Amount.of.Alkalinity.Total")
#####################################################################################
# Amount.of.Total.Dissolved.Solids
summary(merged_data_final$Amount.of.Total.Dissolved.Solids)
# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Total.Dissolved.Solids)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Total.Dissolved.Solids", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Total.Dissolved.Solids)) +
  geom_boxplot() +
  labs(y = "Amount.of.Total.Dissolved.Solids")
#####################################################################################
# Amount.of.Potential.of.Hydrogen                           

# Histogram;
ggplot(data = merged_data_final, aes(x = Amount.of.Potential.of.Hydrogen)) +
  geom_histogram(fill = "white", colour = "black") +
  labs(x = "Amount.of.Potential.of.Hydrogen", y = "Count")

# Box-plot
ggplot(data = merged_data_final, aes(y = Amount.of.Potential.of.Hydrogen)) +
  geom_boxplot() +
  labs(y = "Amount.of.Potential.of.Hydrogen")

summary(merged_data_final$Amount.of.Potential.of.Hydrogen)
#####################################################################################
library(stargazer)
stargazer(merged_data_final)
stargazer(merged_data_final, type = "text", out = "merged_data_final_table.txt")

library(moments)
skewness(merged_data_final$SDP, na.rm = TRUE)
skewness(merged_data_final$Gini.Index ,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Arsenic,na.rm=TRUE)
skewness(merged_data_final$Amount.of.carbonate,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Calcium,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Chloride,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Electrical.Conductivity,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Fluorine,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Iron,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Hydrogencarbonate,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Potassium,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Magnesium,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Nitrate,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Sodium,na.rm=TRUE)
skewness(merged_data_final$Percentage.of.Sodium,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Phosphate.Ion,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Residual.Sodium.Carbonate,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Sodium.absorption.ratio,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Sulfate,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Silicon.dioxide,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Hardness.Total,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Alkalinity.Total,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Total.Dissolved.Solids,na.rm=TRUE)
skewness(merged_data_final$Amount.of.Potential.of.Hydrogen,na.rm=TRUE)