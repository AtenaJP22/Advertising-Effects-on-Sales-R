# Effects of Advertising on Sales: Television, Radio, and Newspaper Advertisements

## Project Overview
This project analyzes the impact of three advertising mediums—Television (TV), Radio, and Newspaper—on sales performance. The dataset includes 200 observations of advertising budgets and corresponding sales figures, measured in thousands of dollars and units, respectively. The goal is to determine which advertising methods significantly influence sales and to model these relationships using statistical techniques.

## Dataset
The dataset was sourced from [Kaggle](https://www.kaggle.com/datasets/ashydv/advertising-dataset) and contains the following variables:
- **TV**: Budget allocated to TV advertisements (in thousands of dollars).
- **Radio**: Budget allocated to Radio advertisements (in thousands of dollars).
- **Newspaper**: Budget allocated to Newspaper advertisements (in thousands of dollars).
- **Sales**: Resulting sales (in thousands of units).

## Methodology
The analysis includes the following steps:
1. **Exploratory Data Analysis (EDA)**: Visualizations (scatter plots, histograms, box plots) to understand relationships and distributions.
2. **Normality Tests**: Shapiro-Wilk tests to assess the normality of data distributions.
3. **Point Estimations and Confidence Intervals**: Estimation of population parameters (mean, standard deviation) using sample data.
4. **Hypothesis Testing**: 
   - One-sample and two-sample t-tests to compare means.
   - Chi-squared tests for independence and goodness-of-fit.
5. **Linear Regression**: Modeling the relationship between advertising budgets and sales, including residual analysis.
6. **Analysis of Variance (ANOVA)**: One-way and two-way ANOVA to test for significant differences in means and interactions between variables.
7. **Nonparametric Tests**: Wilcoxon signed-rank test and sign test for non-normal distributions.

## Key Findings
- **TV Advertising**: Exhibits a strong positive linear relationship with sales, confirmed by linear regression (p-value < 2.2e-16).
- **Radio Advertising**: Shows a weaker but still significant relationship with sales (p-value < 2e-16).
- **Newspaper Advertising**: No significant impact on sales (p-value = 0.954).
- **ANOVA Results**: Significant differences exist in the mean sales across advertising methods (p-value < 2e-16), with TV being the most influential.
- **Nonparametric Tests**: Confirmed significant differences in sales distributions before and after interventions (p-value < 0.05).

## Code Implementation
The analysis was conducted in R, using libraries such as `ggplot2`, `stats`, and `dplyr`. Below are some code snippets from the project:

### Linear Regression Model
```r
# Fit a linear regression model
lm_model <- lm(Sales ~ TV + Radio + Newspaper, data = advertising_data)
summary(lm_model)

# Hypothesis Testing (t-test)
# One-sample t-test for Radio budget
t_test_result <- t.test(SampleRadio, mu = 23, alternative = "greater")
print(t_test_result)

# ANOVA

# One-way ANOVA for advertising methods
anova_result <- aov(Sales ~ TV * Radio * Newspaper, data = advertising_data)
summary(anova_result)
'''

# Repository Structure

data/: Contains the dataset (advertising_data.csv).
scripts/: R scripts for data analysis, visualizations, and statistical tests.
results/: Outputs including plots, tables, and summaries.
README.md: This file, providing an overview of the project.

## How to Reproduce

1. Clone the repository:
'''bash
git clone https://github.com/yourusername/advertising-sales-analysis.git
'''

2. Install required R packages:
'''bash
install.packages(c("ggplot2", "dplyr", "stats"))
'''
3. Run the R scripts in the scripts/ folder to reproduce the analysis.

Course: Statistics in Engineering (INE2002)
For questions or feedback, please open an issue in the repository.
