library(dyplr)
library(lme4)
library(lmerTest)
library(car)
library(MuMIn)
library(report)
library(emmeans)
library(boot)
library(plotly)
library(lattice)
library(car)
library(mgcv)
library(randomForest)
library(pander)

# Volume Rate of Change (%): rate= ((Vx - Vx-1) / Vx-1) * 100 / Tx - Tx-1
# V = volume in mm3, T = days 

vol_output <- read.csv("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /DATA-PROCESSING/Final_params_Data.csv" ) |>
  mutate(Time = as.factor(Time), 
         Treatment = as.factor(Treatment), 
         #mean_Temperature = scale(mean_Temperature), 
         mean_Oxygen = scale(mean_Oxygen), 
         ar_in = log(ar_in),
         Day = factor(Day, levels = c("+ 85 days", "+ 134 days", "+ 183 days", "+ 274 days", "+ 358 days"))) |>
  filter(Rate_per_day > -1.8866990, 
         Rate_per_day < 0.9706308)
  
outliers <- boxplot(vol_output$Rate_per_day, plot=FALSE)$out
outliers

# DIFF DATA SETS 
test <- vol_output |>
  filter(!Rate_per_day < 0) |>
  mutate(Rate_per_day = -(Rate_per_day))

# n = 119 (diff. of 12 observations) --> would be removing outliers 

vol_output_6_months <- vol_output |>
  filter(Time != 4 & Time != 5)
# n = 102 

pH <- vol_output |>
  filter(Treatment != "Triple Stressor")
# n = 75 

triple_nipple <- vol_output |>
  filter(Treatment == "Triple Stressor" | Treatment == "Control" )
# n = 50

  # Raw data sheets if needed 
  # Environmental parameter data (monitored date + SysCO2 output)
  # envio_params <- read_csv("./Envio-Param+Carbonate-System_mean_by_tank-treatment.csv" )
  # Raw results from python CT scan image processing scripts
  # results_all <- read.csv("./raw_scan_data.csv")
  # Key for Months, Days, Dates
  # dates <- read_excel("./Tank-Parameters.xlsx", 
  #                     sheet = "Scanning_Dates") 
  # Sample Number to Treatment Key
  # treatment <- read_excel("./Tank-Parameters.xlsx", 
  #                         sheet = "Tank_Samples") 


(plt_vol_mean_time <- ggplot(test , aes(x = Day, y = Rate_per_day, fill = Treatment, colour = Treatment)) +
    geom_hline(yintercept = 0, color = "black", size = 0.2) +
    geom_boxplot(position = position_dodge(width = 0.8), width = 0.5) + 
    stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.3, position = position_dodge(width = 0.8), show.legend = FALSE) +  # Align error bars with the boxes
    #stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8), size = 1, shape = 3, color = "darkgrey") +
    labs(x = "Days", y = "Dissolution Rate (mm^3^ day^-1^") +
    geom_point(position = position_dodge(width = 0.8), alpha = 0.4) +   # Add individual sample points with jitter
    scale_fill_viridis_d() +
    scale_color_viridis_d() + 
    theme_minimal() +  
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      axis.line = element_line(color = "black"),  # Set color of axis lines to black
      legend.position = "bottom"  # Adjust legend position if needed
    ))

# DISTRIBUTION - RAW DATA

hist(vol_output$Rate_per_day) # actually normally distributed

hist(test$Rate_per_day) # left-skewed --> gamma distribution

(split_histo_trans1 <- ggplot(aes(scale(Rate_per_day)), data = test) + 
    geom_histogram(position = "dodge") +
    facet_wrap(~ Treatment) + 
    xlab("Time") + 
    ylab("Rate") +
    stat_function(fun = dnorm, color = "red", size = 1))

(split_histo_trans2 <- ggplot(aes(scale(Rate_per_day)), data = vol_output) + #--> possibly also just gaussian?
    geom_histogram(position = "dodge") +
    facet_wrap(~ Treatment) + 
    xlab("Time") + 
    ylab("Rate") +
    stat_function(fun = dnorm, color = "red", size = 1))

  # scaling does make the data distribution per treatment more normal



# ALL DATA

null_lm1 <- lm(scale(Rate_per_day) ~ 1 , data = vol_output)


null_nested1 <- lmer(scale(Rate_per_day) ~ 1 + (1|Tank/Sample), REML = FALSE , data = vol_output)
null_sample1 <- lmer(scale(Rate_per_day) ~ 1 + (1|Sample), REML = FALSE , data = vol_output)

null_tank1 <- lmer(scale(Rate_per_day) ~ 1 + (1|Tank), REML = FALSE , data = vol_output)

anova(null_sample1, null_tank1, null_nested1) # very similar models dif.AICs = 0.91. How do I choose which random effect is more suitable?
  # nested is not bad, second lowest AICc

lmm_interaction_nested1 <- lmer(scale(Rate_per_day) ~ Treatment * Time + (1|Tank/Sample), REML=FALSE, data = vol_output)
  
lmm_sep_nested1 <- lmer(scale(Rate_per_day) ~ Treatment + Time + (1|Tank/Sample), REML=FALSE, data = vol_output)
  
lmm_treatment_nested1 <- lmer(scale(Rate_per_day) ~ Treatment + (1|Tank/Sample), REML=FALSE, data = vol_output)
lmm_time_nested1 <- lmer(scale(Rate_per_day) ~ Treatment + (1|Tank/Sample), REML=FALSE, data = vol_output)

anova(lmm_interaction_nested1)
summary(lmm_interaction_nested1)
report(lmm_interaction_nested1)

vif(lmm_interaction_nested1) # GVIF < 5 --> no colinearity 

boxplot(scale(Rate_per_day) ~ Treatment, vol_output)

plot(resid(lmm_time_treatment_sep_sample1) ~ Time)

friedman.test(Rate_per_day ~ Treatment | Sample, data = vol_output)

# without nested effect, INCLUDING positive volume

lmm_time_sample1 <- lmer(scale(Rate_per_day) ~ Time + (1|Sample), REML=FALSE, data = vol_output)

lmm_treatment_sample1 <- lmer(scale(Rate_per_day) ~ Treatment + (1|Sample), REML=FALSE, data = vol_output)
  # converged
lmm_interaction_sample1 <- lmer(scale(Rate_per_day) ~ Treatment * Time + (1|Sample), REML=FALSE, data = vol_output)
# "fixed-effect model matrix is rank deficient so dropping 1 column / coefficient"
lmm_time_treatment_sep_sample1 <- lmer(scale(Rate_per_day) ~ Treatment + Time + (1|Sample), REML=FALSE, data = vol_output)
  # hella significnat

anova(null_sample1, lmm_time_sample1, lmm_interaction_sample1, lmm_time_treatment_sep_sample1)
  # lmm_time_sample1, lmm_time_treatment_sep_sample1, lmm_interaction_sample1 all signifncatly different to base-model
  # lmm_time_sample1 = best performing

lmm_time_tank1 <- lmer(scale(Rate_per_day) ~ Time + (1|Tank), REML=FALSE, data = vol_output)

lmm_treatment_tank1 <- lmer(scale(Rate_per_day) ~ Treatment + (1|Tank), REML=FALSE, data = vol_output)
  # converged
lmm_interaction_tank1 <- lmer(scale(Rate_per_day) ~ Treatment * Time + (1|Tank), REML=FALSE, data = vol_output)
 
lmm_time_treatment_sep_tank1 <- lmer(scale(Rate_per_day) ~ Treatment + Time + (1|Tank), REML=FALSE, data = vol_output)
  # converged

anova(null_tank1, lmm_time_tank1, lmm_interaction_tank1)
  # lmm_time_tank1, lmm_interaction_tank1 signifncatly different to base-model
  # lmm_time_tank1 = best performing 


# ASSUMPTIONS ----
# Plotting the residuals against the explanatory variable will indicate if the wrong model has been fitted (i.e.
# higher order terms are needed) or if there is some dependence on some other explanatory variable. If this
# is the case some obvious patterning will be visible in the plot.
# Plotting the residuals in order, any trend visible may indicate seasonal pattern or autocorrelation.
# Plotting the residuals against the fitted values will indicate if there is non-constant error variance, i.e. if the
# variance increases with the mean the residuals will fan out as the fitted value increases. Usually transforming
# the data, or using another distribution will help.

# The explanatory variables are related linearly to the response.
# The errors have constant variance.
# The errors are independent.
# The errors are Normally distributed.

# (add to Appendix)

# Residuals vs Fitted
plot(resid(lmm_interaction_tank1) ~ fitted(lmm_interaction_tank1), main = "Residuals vs Fitted")
abline(h = 0, col = "red")

plot(ph1,Rate_per_day,pch=20,main="",ylab="Exam Results %",xlab="Homework Results 1")

# Normal Q-Q plot
qqPlot(resid(lmm_interaction_tank1), main = "Normal Q-Q Plot")

# Scale-Location plot
plot(lmm_interaction_sample1, which = 3)

# Residuals vs Leverage plot
plot(lmm_interaction_sample1, which = 5)

# Shapiro-Wilk test for NORMALITY
shapiro.test(resid(lmm_interaction_tank1))

# Breusch-Pagan test for HOMOSCEDASTICITY
ncvTest(lmm_interaction_sample1)


# Durbin-Watson test for AUTOCORRELATION
durbinWatsonTest(resid(lmm_interaction_sample1))

  # Multi-COLINEARITY

# KRUSKAL-WALLIS: 
  # compare the distributions of a continuous variable among different groups defined by a categorical variable.
  # NOT FOR TWO FIXED CATEGORICAL EFFECTS

kruskal.test(resid(lmm_time_tank1) ~ Time, data = vol_output) # p-value = 0.443
kruskal.test(resid(lmm_treatment_tank1) ~ Treatment, data = vol_output) # converges and also non significant 
kruskal.test(resid(lmm_time_sample1) ~ Time, data = vol_output) # p-value = 0.414
kruskal.test(resid(lmm_treatment_sample1) ~ Treatment, data = vol_output) # converges

# Select Sample as random effect as this model takes into account the repeated sampling of the same fragments at each time point

# AICc
AICc(lmm_time_tank1, lmm_time_sample1, lmm_treatment_tank1, lmm_treatment_sample1, lmm_interaction_tank1, lmm_interaction_sample1 )
AICc(lmm_time_sample1, lmm_treatment_sample1, lmm_interaction_sample1 )
  # Having an interaction term uses more statistical power, thus lmm_interaction_sample1 will not have the lowest AICc

# RSQUARED 
  # R2m = the proportion of variance explained by the fixed effects alone.
  # R2c = the proportion of variance explained by both fixed and random effects together.

r.squaredGLMM(lmm_time_sample1)
  # R2m (sample) = 0.4406157
  # R2c (time) = 0.6025725

r.squaredGLMM(lmm_interaction_sample1)
  # model that explains most amount of variance 
  # R2m (sample) = 0.6673938 and (treatment*time) = 0.6949201

r.squaredGLMM(lmm_time_treatment_sep_sample1)
# R2m = 0.585537 R2c = 0.6069897

r.squaredGLMM(lmm_time_tank1)
# R2c (tank) = 0.2954011 --> much less (diff. R2c = 0.113498) variance is explained by Tank than Sample

# SIGNIFICANCE 
report(lmm_time_treatment_sep_sample1)

# EXCLUDING POSITIVES --> don't know how to transform this data 

glm_treatment1 <- glmer(Rate_per_day ~ Treatment + (1|Sample), family = Gamma(link = "log"), data = test)

glm_time1 <- glmer(Rate_per_day ~ Time + (1|Sample), family = Gamma(link = "log"), data = test)

glm_interaction1 <- glmer(Rate_per_day ~ Treatment * Time  + (1|Tank/Sample), family = Gamma(link = "log"), data = test)

glm_interaction1 <- glmer(Rate_per_day ~ Treatment + Time  + (1|Tank/Sample), family = Gamma(link = "log"), data = test)

summary(glm_interaction1)
report(glm_interaction1)

# 6MONTHS 

hist(vol_output_6_months$scl_Rate)

lmm_time_s2 <- lmer(scale(Rate_per_day) ~ Time + (1|Sample), REML=FALSE, data = vol_output_6_months)

  lmm_treatment_s2 <- lmer(scale(Rate_per_day) ~ Treatment + (1|Sample), REML=FALSE, data = vol_output_6_months)
  # converged
  lmm_interaction_s2 <- lmer(scale(Rate_per_day) ~ Treatment * Time + (1|Sample), REML=FALSE, data = vol_output_6_months)
  # converged
  lmm_time_treatment_sep_s2 <- lmer(scale(Rate_per_day) ~ Treatment + Time + (1|Sample), REML=FALSE, data = vol_output_6_months)
  # converged

anova(lmm_time_s2)
summary(lmm_time_s2)

# TRIPLE STRESSOR

hist(triple_nipple$scl_Rate)

lmm_time_s3 <- lmer(scale(Rate_per_day) ~ Time + (1|Sample), REML=FALSE, data = triple_nipple)

lmm_treatment_s3 <- lmer(scale(Rate_per_day) ~ Treatment + (1|Sample), REML=FALSE, data = triple_nipple)
# converged
lmm_interaction_s3 <- lmer(scale(Rate_per_day) ~ Treatment * Time + (1|Sample), REML=FALSE, data = triple_nipple)
# converged
lmm_time_treatment_sep_s3 <- lmer(scale(Rate_per_day) ~ Treatment + Time + (1|Sample), REML=FALSE, data = triple_nipple)
# converged



# PH

hist(vol_output$mean_pHT)

lmer_treatment2 <- lmer(Rate_per_day ~ scale(mean_pHT) + (1|Treatment), REML = FALSE, data = vol_output)

plot(lmer_treatment2)
anova(lmer_treatment2)

summary(lmer_treatment2)

shapiro.test(resid(lmer_treatment2))

# TEMPERATURE

hist(vol_output$mean_Temperature) #bi-modal distribution that is actually just a micture of normal distirbution bec of the two temperature extremes going on for the 

(split_histo_trans1 <- ggplot(aes(scale(mean_Temperature)), data = vol_output) + 
    geom_histogram(position = "dodge") +
    facet_wrap(~ Treatment) + 
    stat_function(fun = dnorm, color = "red", size = 1))

lmer_treatment1 <- lmer(Rate_per_day ~ scale(mean_Temperature) + (1|Treatment), REML = FALSE, data = vol_output)
lmer_tank1 <- lmer(Rate_per_day ~ mean_Temperature + (1|Tank), REML = FALSE, data = vol_output)

anova(lmer_treatment1)
summary(lmer_treatment1)
report(lmer_treatment1)
shapiro.test(resid(lmer_treatment1))

kruskal.test(lmer_treatment1)

lmer_treatment1 <- lmer(Rate_per_day ~ scale(mean_Oxygen) + (1|Treatment), REML = FALSE, data = vol_output)

