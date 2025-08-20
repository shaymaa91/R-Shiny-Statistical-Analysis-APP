# R-Shiny-Statistical-Analysis-APP

![System Diagram](./App%20interface.png)

-[Application Link](https://fwks.shinyapps.io/stat503/)
- Application Link: https://fwks.shinyapps.io/stat503/
- Developed by: Fatimah Albahar, Kholoud Almutairi, Shaymaa Khalaf, Waiam Malibari
- Supervised by: Dr. Muhammad Riaz

## Introduction
Statistical inference can be difficult to grasp when moving from theory to practice. To address this gap, we developed an all-in-one R Shiny app that helps students and practitioners explore descriptive statistics, probability distributions, sampling, and hypothesis testing. The app was built as part of our coursework with the aim of making statistical concepts more interactive and approachable.
## Overview
The app is structured with five main modules using shiny and shinydashboard:
1.	Descriptive Statistics: Summarize data, compute measures of central tendency, and visualize using histograms, boxplots, and summary tables.
2.	Discrete Distributions: Explore Binomial, Geometric, Poisson and Hypergeometric distributions with exact, cumulative and inverse probability functions.
3.	Continuous Distributions: Explore Normal, Exponential, Weibull, and lognormal distributions with comulative, conditional, interval and inverse probability functions.
4.	Sampling Distributions: Explore Normal, t, f and chi-squared sampling distributions with comulative, interval and inverse probability functions.
5.	Statistical Inference (I, II, III): Perform Shapiro-Wilk normality test,  one population sample inference (location parameter, variance and proportion), two population sample inference (location parameter, variance and proportion), and three or more population sample inference (location parameter, variance and proportion).
Users can upload CSV files, input data manually, or use built-in datasets. Outputs include statistical summaries, test statistics, confidence intervals, and interpretation.
## Key Features
- Interactive UI with tab-based navigation.
- Support for both discrete and continuous distributions.
- Pedagogical design to highlight logic behind inference tests.
- Input flexibility: built-in datasets, CSV upload, or manual entry.
## Technical Details
- Languages & Libraries: R, Shiny, ShinyDashboard, BSDA, EnvStats.
- Data Flow: Reactive programming ensures smooth interactions across modules.
- Outputs: Numerical results, plots, and clear interpretations.
 
