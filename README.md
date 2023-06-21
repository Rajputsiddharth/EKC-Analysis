# Environmental Quality, Economic Growth, and Income Inequality Analysis in India

## Introduction

This project examines the relationship between economic growth, income inequality, and environmental quality in India. It aims to explore the Environmental Kuznets Curve (EKC) hypothesis, which suggests that as per capita income increases, environmental quality initially deteriorates and then improves. The analysis is conducted using district-level data on environmental quality, economic output, and income inequality to estimate regression models and analyze the relationship between these variables.

## Objective

The main objective of this project is to test the Environmental Kuznets Curve hypothesis for Groundwater Quality in India. The Environmental Kuznets Curve is an economic theory that posits that as a country's income increases, its environmental impact will initially increase but eventually decrease.

## Dataset, Power Inequality, and Control Variables

The dataset used in this analysis spans 18 consecutive years, from 2000 to 2018. Power inequality variables are employed to analyze and measure the distribution of power in society, as they can affect the Environmental Kuznets Curve in various ways. Control variables are also included to account for the effects of other factors that may influence the relationship between income and environmental quality.

## Dataset Description

The repository includes the following datasets:

- Fertiliser: Contains data on fertiliser usage.
- Turnout: Contains data on voter turnout.
- Rainfall: Contains data on rainfall.
- Telecom: Contains data on telecommunications.
- Gini: Contains data on the Gini index.
- Ratioo: Contains ratio data.

## Code Overview

The code performs the following operations:

1. Reads the necessary CSV files into data frames.
2. Manipulates and cleans the data frames.
3. Performs data merging and transformation.
4. Builds a multilinear regression model using the `lm` function.
5. Provides model summaries and statistical analyses.
6. Conducts hypothesis testing and calculates standard errors.
7. Tests for equality of variances across state groups.
8. Performs the Chow test for structural breaks.

## Dependencies

The code requires the following R packages:

- `tidyr`
- `pacman`
- `data.table`
- `fixest`
- `stargazer`
- `dplyr`
- `magrittr`
- `car`

Please ensure that these packages are installed before running the code.

## Usage

To use the code, follow these steps:

1. Clone or download the repository.
2. Update the file paths in the code to point to the correct CSV files on your local machine.
3. Install the required R packages mentioned above.
4. Run the code in an R environment or an integrated development environment (IDE) of your choice.

## Tests and Estimations

Several tests and estimations are utilized in this project, including the Chow test, T-test, P-value test, Monte Carlo estimations, and Levene test. These tests help assess the significance and reliability of the findings.

## Conclusion

The analysis reveals a non-linear relationship between per capita income (SDP) and Environmental Quality Indicator (EQI) after reaching a certain threshold. A positive correlation is observed between them, suggesting a complex U-shaped pattern. Control variables such as fertilizer usage and annual rainfall are introduced to mitigate data bias and analyze the impact of power inequality variables on EQI.

Hypothesis testing indicates that some power inequality variables significantly affect EQI. By including control variables, a more accurate estimate of the SDP's effect on EQI is obtained, highlighting the importance of controlling for other factors. The analysis demonstrates that SDP has a negative impact on EQI, specifically on the amount of electric conductivity, indicating that as income increases, electric conductivity decreases, leading to improved groundwater quality.

Furthermore, the inclusion of state-wise Gini index in the enhanced model shows a higher positive correlation with EQI compared to the district-wise Gini index when other variables are held constant. This variation may be attributed to the lower granularity of state-wise data. The small standard errors for all variables indicate precise and reliable coefficient estimates.

Based on the observations, it can be concluded that there is a significant relationship between TeleDensity and Electric Conductivity. An increase in TeleDensity enhances communication and connectivity, which can positively impact future environmental quality. Conversely, a negative relationship is found between Voter Turnout Average and Electric Conductivity, suggesting that current priorities may focus more on demanding better infrastructure and education from the government rather than improving environmental conditions.

To test the equality of variances of the variable amount of electrical conductivity across four state groups (South, East, West, North), a Bartlett test is performed. The small p-value (e-16) associated with the test provides strong evidence to suggest that the variance of electrical conductivity differs across the state groups.

Please note that this project is based on the analysis of available data and should be interpreted within its limitations and assumptions.
