Fertiliser <- read.csv("/Users/dhruvsood/Downloads/Sem 4/Econometrics/GroupAssig2/New data/fertiliser data.csv")
Turnout <- read.csv("/Users/dhruvsood/Downloads/Sem 4/Econometrics/GroupAssig2/Turn_Out.csv")
rainfall <- read.csv("/Users/dhruvsood/Downloads/Sem 4/Econometrics/GroupAssig2/New data/rainfall.csv")
sexy <- read.csv("/Users/dhruvsood/Downloads/Sem 4/Econometrics/GroupAssig2/Mer.csv")
Telecom <- read.csv("/Users/dhruvsood/Downloads/Sem 4/Econometrics/GroupAssig2/New data/Telecom.csv")
gini <- read.csv("/Users/dhruvsood/Downloads/Sem 4/Econometrics/GroupAssig2/Gini_New.csv")
ratioo <- read.csv("/Users/dhruvsood/Downloads/Sem 4/Econometrics/GroupAssig2/New data/ratio4.csv")

Telecom$State <- sub("Uttar Pradesh-(E&W)", "UTTAR PRADESH",Telecom$State)
library(tidyr)
colnames(Telecom)[1:18] <- c("State", "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",'2015',"2016")
Telecom <- gather(Telecom, Year, Value, -State)
Telecom$State <- toupper(Telecom$State)



Fertiliser$State <- toupper(Fertiliser$State)
Fertiliser$District <- toupper(Fertiliser$District)
Fertiliser$State <- toupper(Fertiliser$State)
Fertiliser$District <- toupper(Fertiliser$District)
Fertiliser <- Fertiliser[, -c(6:18)]
Fertiliser <- Fertiliser[, -7]
Fertiliser <- Fertiliser[, -1]
Fertiliser <- Fertiliser[, -2]




Turnout<- Turnout[, -1]
colnames(Turnout)[1:5] <- c("District","State","2019","2014","2009")
Turnout<- Turnout %>%mutate(across(starts_with("20"), ~ gsub("%", "", .)))
Turnout$State <- gsub("Uttar Pradesh \\[2000 Onwards\\]", "UTTAR PRADESH",Turnout$State)
Turnout$State <- gsub("Andhra Pradesh \\[2014 Onwards\\]", "ANDHRA PRADESH",Turnout$State)
Turnout$State <- gsub("Madhya Pradesh \\[2000 Onwards\\]", "MADHYA PRADESH",Turnout$State)
Turnout$State <- gsub("Bihar \\[2000 Onwards\\]", "BIHAR",Turnout$State)
Turnout$State <- gsub("Delhi \\[1977 Onwards\\]", "DELHI",Turnout$State)
Turnout$State <- toupper(Turnout$State)
Turnout$District <- toupper(Turnout$District)
Turnout <- Turnout[, -c(3:5)]
names(Turnout)[3] <- "Turnout_Average"





gini$State <- toupper(gini$State)
gini$State <- sub("\\(INCLUDING TELANGANA\\)", "", gini$State)
colnames(gini)[1:5] <- c("State", "2005","2014","2019","Average")
gini <- gini[, -c(2:4)]
names(gini)[2] <- "Gini.Index_new"

rainfall <- rainfall[, -c(6:17)]
rainfall<- rainfall[, -1]
rainfall<- rainfall[, -2]
colnames(rainfall)[2:3] <- c("State","District")
rainfall$State <- toupper(rainfall$State)
rainfall$District <- toupper(rainfall$District)

sexy$District<- toupper(sexy$District)
names(sexy)[3] <- "Year"
names(sexy)[36] <- "Yearinfo"

ratioo$State<- toupper(ratioo$State)

sexy1 <- merge(sexy, Fertiliser, by = c("District", "State","Year"))
sexy3 <- merge(sexy1, rainfall, by = c("State","Year","District"))
sexy4 <- merge(sexy3,Turnout,by = c("State","District"))
sexy5 <- merge(sexy4,ratioo,by = c("State","Year"))
sexy6 <- merge(sexy5,gini,by = c("State"))
sexy7 <- merge(sexy6,Telecom,by = c("State","Year"))
names(sexy7)[names(sexy7) == "Value"] <- "Telephones per sq Km"
View(sexy7)

sexy7$SDP_sq <- sexy7$SDP^2
sexy7$SDP_cu <- sexy7$SDP^3

model <- lm(formula = sexy7$Amount.of.Electrical.Conductivity ~ sexy7$SDP + sexy7$SDP_sq + sexy7$SDP_cu + sexy7$Gini.Index + sexy7$Turnout_Average + sexy7$ratio+ sexy7$ANNUAL.RAINFALL..Millimeters.+sexy7$TOTAL.PER.HA.OF.NCA..Kg.per.ha.+sexy7$`Telephones per sq Km`,data=sexy7)

summary(model)
stargazer(model)

#Question 3
error <- summary(model)$coefficients[,"Std. Error"]
print(error)

##Question 5
#install.packages("pacman")
library(pacman)
pacman::p_load(data.table, fixest, stargazer, dplyr, magrittr) 
rm <- model <- lm(formula = sexy7$Amount.of.Electrical.Conductivity ~ sexy7$SDP ,data=sexy7)
summary(rm)

# Step 1: Define true population parameters and sample size
beta_0 <- 2399      # true intercept
beta_1 <- -0.0003773 # true slope coefficient
n <- 10000           # sample size
set.seed(1)
m <- 500

slope_DT <- rep(0,m)
intercept_DT <- rep(0,m)

for (i in 1:m){ #  M is the number of iterations
  
  # Generate data
  U_i = rnorm(n, mean = 0, sd = 1524) # Error
  X_i = sample(sexy7$SDP, n) # Independent variable
  Y_i = beta_0 + beta_1*X_i + U_i  # Dependent variable
  
  # Formulate data.table
  data_i = data.table(Y = Y_i, X = X_i)
  
  # Run regressions
  ols_i <- fixest::feols(data = data_i, Y ~ X)
  
  # Extract slope coefficient and save
  slope_DT[i] <- ols_i$coefficients[2]
  intercept_DT[i] <- ols_i$coefficients[1]
  
}

# Summary statistics
estimates_DT <- data.table(beta_1 = slope_DT, beta_0 = intercept_DT)
stargazer(estimates_DT[, c("beta_1", "beta_0")], type = "text")

# Visual inspection
hist(estimates_DT[, beta_1], xlim = c(-0.00050,-0.00025))
#Question 6
library(dplyr)

data <- sexy7 %>% 
  rename(tele_per_sqkm = `Telephones per sq Km`)
View(data)
data <- data[c("State","Year","District","KeyValue","ROWID",	"Country","State.LGD.Code","District.LGD.Code","Ground.Water.Station.Name",	"Ground.Water.Station.Latitude",	"Ground.Water.Station.Longitude",	"SourceYear"
                ,"Amount.of.Electrical.Conductivity", "SDP", "SDP_sq", "SDP_cu", "Gini.Index_new", 
                "Turnout_Average", "ratio", "ANNUAL.RAINFALL..Millimeters.", "TOTAL.PER.HA.OF.NCA..Kg.per.ha.", 
                "tele_per_sqkm")]
cols <- c("State", "Year", "District", "KeyValue", "ROWID", "Country", "State.LGD.Code", 
          "District.LGD.Code", "Ground.Water.Station.Name", "Ground.Water.Station.Latitude", 
          "Ground.Water.Station.Longitude", "SourceYear", "Amount.of.Electrical.Conductivity", 
          "SDP", "SDP_sq", "SDP_cu", "Gini.Index_new", "Turnout_Average", "ratio", 
          "ANNUAL.RAINFALL..Millimeters.", "TOTAL.PER.HA.OF.NCA..Kg.per.ha.", "tele_per_sqkm")

data <- data[complete.cases(data[, cols]), ]
model <- lm(formula = data$Amount.of.Electrical.Conductivity ~ data$SDP + data$SDP_sq + data$SDP_cu + data$Gini.Index_new + data$Turnout_Average + data$ratio+ data$ANNUAL.RAINFALL..Millimeters.+data$TOTAL.PER.HA.OF.NCA..Kg.per.ha.+data$tele_per_sqkm,data=data)


# Define log-likelihood function
logLikelihood_lm <- function(beta, data) {
  # Extract variables from data
  X <- data[, c("SDP", "SDP_sq", "SDP_cu", "Gini.Index_new", "Turnout_Average", "ratio", "ANNUAL.RAINFALL..Millimeters.", "TOTAL.PER.HA.OF.NCA..Kg.per.ha.", "tele_per_sqkm")]
  y <- data$Amount.of.Electrical.Conductivity
  
  # Calculate linear predictor and residuals
  mu <- predict(model, newdata = data)
  epsilon <- y - mu
  
  # Calculate log-likelihood
  n <- length(y)
  sigma <- exp(beta[length(beta)]) # transform last parameter to ensure positivity
  logLik <- -n/2*log(2*pi*sigma^2) - sum(epsilon^2)/(2*sigma^2)
  
  return(-logLik)
}

# Set starting values for coefficients

startValues <- c(coef(model)[-1], log(sd(resid(model))))

# Use maximum likelihood estimation to estimate coefficients assuming a normal distribution
fit_lm <- optim(par = startValues, fn = logLikelihood_lm, data = data, method = "BFGS")

# Print estimated coefficients
coef_lm <- c(fit_lm$par[1:9], exp(fit_lm$par[10])) # transform last parameter back to original scale
names(coef_lm) <- c("SDP", "SDP_sq", "SDP_cu", "Gini.Index_new", "Turnout_Average", "ratio", "ANNUAL.RAINFALL..Millimeters.", "TOTAL.PER.HA.OF.NCA..Kg.per.ha.", "tele_per_sqkm","sigma")
print(coef_lm)



# Question 7
# Create dummy variables for state groups
DSouth <- ifelse(data$State %in% c("ANDHRA PRADESH", "TAMIL NADU", "KERALA", "TELENGANA", "JHARKHAND"), 1, 0)
DEast <- ifelse(data$State %in% c("ASSAM", "MANIPUR", "TRIPURA", "SIKKIM", "WEST BENGALl", "ODISHA","MADHYA PRADESH","UTTAR PRADESH"), 1, 0)
DWest <- ifelse(data$State %in% c("GUJARAT", "MAHARASHTRA", "KARNATAKA"), 1, 0)
DNorth <- ifelse(data$State %in% c("JAMMU AND KASHMIR", "HIMACHAL PRADESH", "DELHI", "PUNJAB", "HARYANA", "RAJASTHAN", "UTTARAKHAND", "CHHATTISGARH"), 1, 0)

# Test for equality of variances across state groups
result <- bartlett.test(Amount.of.Electrical.Conductivity ~ interaction(DSouth + DEast + DWest + DNorth), data = data)
print(result)

# a)
# Bartlett's test
bartlett.test(data$Amount.of.Electrical.Conductivity, data$State)
print("The output of the Bartlett's test you provided shows a test statistic of 46911, degrees of freedom (df) of 14, and a p-value less than 2.2e-16. This indicates strong evidence against the null hypothesis that the variances of the merge$Amount.of.Electrical.Conductivity variable are equal across different state-groups. In other words, the variance of environmental quality is not the same across different state-groups, and there may be significant differences in the levels of environmental quality between the states.")

# Levene's test
library(car)
leveneTest(data$Amount.of.Electrical.Conductivity, data$State)

# b)
print("If the variance of the dependent variable (Amount.of.Electrical.Conductivity) differs significantly across state groups, then the assumption of equal variances or homoscedasticity in OLS is violated.")
print("the estimated standard errors of the regression coefficients may be biased, leading to incorrect inferences and hypothesis testing")


# Q4
# CHOW TEST
DSouth <- ifelse(sexy7$State %in% c("ANDHRA PRADESH", "TAMIL NADU", "KERALA", "TELENGANA", "JHARKHAND"), 1, 0)
DEast <- ifelse(sexy7$State %in% c("ASSAM", "MANIPUR", "TRIPURA", "SIKKIM", "WEST BENGAL", "ODISHA","MADHYA PRADESH","UTTAR PRADESH"), 1, 0)
DWest <- ifelse(sexy7$State %in% c("GUJARAT", "MAHARASHTRA", "KARNATAKA"), 1, 0)
DNorth <- ifelse(sexy7$State %in% c("JAMMU AND KASHMIR", "HIMACHAL PRADESH", "DELHI", "PUNJAB", "HARYANA", "RAJASTHAN", "UTTARAKHAND", "CHHATTISGARH"), 1, 0)

## NORTH SOUTH

# Fit separate models for the north and south groups
model_north <- lm(Amount.of.Electrical.Conductivity ~ SDP + SDP_sq + SDP_cu + Gini.Index_new + Turnout_Average + ratio + ANNUAL.RAINFALL..Millimeters. + TOTAL.PER.HA.OF.NCA..Kg.per.ha. + `Telephones per sq Km`, data = sexy7, subset = DNorth == 1)
model_south <- lm(Amount.of.Electrical.Conductivity ~ SDP + SDP_sq + SDP_cu + Gini.Index_new + Turnout_Average + ratio + ANNUAL.RAINFALL..Millimeters. + TOTAL.PER.HA.OF.NCA..Kg.per.ha. + `Telephones per sq Km`, data = sexy7, subset = DSouth == 1)

# Fit a combined model with an interaction term
model_combined <- lm(Amount.of.Electrical.Conductivity ~ SDP + SDP_sq + SDP_cu + Gini.Index_new + Turnout_Average + ratio + ANNUAL.RAINFALL..Millimeters. + TOTAL.PER.HA.OF.NCA..Kg.per.ha. + `Telephones per sq Km` + DSouth + DNorth + DSouth*ratio + DNorth*ratio, data = sexy7)

# Calculate the sum of squared residuals for each model
SSR_north <- sum(resid(model_north)^2)
SSR_south <- sum(resid(model_south)^2)
SSR_combined <- sum(resid(model_combined)^2)

# Calculate the Chow test statistic
n <- length(sexy7$State)
k <- length(model_combined$coefficients)
p <- 2  # number of group indicators
q <- k - p - 1  # number of other coefficients
Chow_stat <- ((SSR_combined - (SSR_north + SSR_south)) / q) / ((SSR_north + SSR_south) / (n - 2*p - k))

# Calculate the p-value for the Chow test
p_value <- 1 - pf(Chow_stat, q, n - 2*p - k)

# Print the results
cat("Chow test statistic:", round(Chow_stat, 3), "\n")
cat("p-value:", format(p_value, scientific = TRUE), "\n")

if (p_value > Chow_stat) {
  print("There is evidence of structural change.")
} else {
  print("There is no evidence of structural change.")
}

## EAST WEST

# Fit separate models for the East and West groups
model_east <- lm(Amount.of.Electrical.Conductivity ~ SDP + SDP_sq + SDP_cu + Gini.Index_new + Turnout_Average + ratio + ANNUAL.RAINFALL..Millimeters. + TOTAL.PER.HA.OF.NCA..Kg.per.ha. + `Telephones per sq Km`, data = sexy7, subset = DEast == 1)
model_west <- lm(Amount.of.Electrical.Conductivity ~ SDP + SDP_sq + SDP_cu + Gini.Index_new + Turnout_Average + ratio + ANNUAL.RAINFALL..Millimeters. + TOTAL.PER.HA.OF.NCA..Kg.per.ha. + `Telephones per sq Km`, data = sexy7, subset = DWest == 1)

# Fit a combined model with an interaction term
model_combined <- lm(Amount.of.Electrical.Conductivity ~ SDP + SDP_sq + SDP_cu + Gini.Index_new + Turnout_Average + ratio + ANNUAL.RAINFALL..Millimeters. + TOTAL.PER.HA.OF.NCA..Kg.per.ha. + `Telephones per sq Km` + DEast + DWest + DEast*ratio + DWest*ratio, data = sexy7)

# Calculate the sum of squared residuals for each model
SSR_east <- sum(resid(model_east)^2)
SSR_west <- sum(resid(model_west)^2)
SSR_combined <- sum(resid(model_combined)^2)

# Calculate the Chow test statistic
n <- length(sexy7$State)
k <- length(model_combined$coefficients)
p <- 2  # number of group indicators
q <- k - p - 1  # number of other coefficients
Chow_stat <- ((SSR_combined - (SSR_east + SSR_west)) / q) / ((SSR_east + SSR_west) / (n - 2*p - k))

# Calculate the p-value for the Chow test
p_value <- 1 - pf(Chow_stat, q, n - 2*p - k)

# Print the results
cat("Chow test statistic:", round(Chow_stat, 3), "\n")
cat("p-value:", format(p_value, scientific = TRUE), "\n")


if (p_value > Chow_stat) {
  print("There is evidence of structural change.")
} else {
  print("There is no evidence of structural change.")
}


# Perform t-test for North and South groups
ttest_north_south <- t.test(Amount.of.Electrical.Conductivity ~ DNorth, data = sexy7, subset = DSouth == 0)
ttest_north_south
print("alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0. 95 percent confidence interval: -690.9314 -604.0068")

# Perform t-test for East and West groups
ttest_east_west <- t.test(Amount.of.Electrical.Conductivity ~ DEast, data = sexy7, subset = DWest == 0)
ttest_east_west
print("alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0. 95 percent confidence interval: 469.3640 539.6443")

# T Test
model <- lm(formula = sexy7$Amount.of.Electrical.Conductivity ~ sexy7$SDP + sexy7$SDP_sq + sexy7$SDP_cu + sexy7$Gini.Index_new + sexy7$Turnout_Average + sexy7$ratio + sexy7$ANNUAL.RAINFALL..Millimeters. + sexy7$TOTAL.PER.HA.OF.NCA..Kg.per.ha. + sexy7$`Telephones per sq Km`, data = sexy7)
summary(model)$coefficients["sexy7$ratio", c("t value", "Pr(>|t|)")]