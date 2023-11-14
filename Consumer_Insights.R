# Load libraries

install.packages('corrplot')
install.packages('semPlot')
install.packages('scatterPlotMatrix')
library(tidyverse)
library(lubridate)
library(forcats)
library(corrplot) 
library(gplots)
library(nFactors) 
library(psych)  
library(RColorBrewer) 
library(GPArotation) 
library(stargazer)
library(semPlot)
library(scatterPlotMatrix)
library(car)

# Set WD & Import Dataset

survey_data <- read.csv('survey_data.csv')
survey_data <- data.frame(survey_data)

# --------- Part 1: Data Cleaning & Preprocessing ---------------------\

# 1.1 Initial Inspection -----

str(survey_data)
summary(survey_data)

# 1.2 Identify Missing -----
# No missing in the dataset

sum(is.na(survey_data))


# 1.3 Format Data Type (convert to categorical)-----

noncat_columns <- c('Age', 'PurchaseFreq', 'MoneySpent')

cat_columns <- names(survey_data)[!names(survey_data) %in% noncat_columns]

survey_data[cat_columns] <- lapply(survey_data[cat_columns], as.factor)

str(survey_data)


# 1.4 Check for outliers (numerical variables)

boxplot(survey_data$Age, main = 'Boxplot for Age')
boxplot(survey_data$PurchaseFreq, main = 'Boxplot for Purchase Frequency')
boxplot(survey_data$MoneySpent, main = 'Boxplot for Money Spent on Clothing')


# 1.5 Create new DF + Standardize variables

# [1] Dataframe on 'consumers lifestyle' (Col11 - Col23)

lifestyle_df <- (survey_data[ ,11:23])
lifestyle_df <- lapply(lifestyle_df, as.numeric)
lifestyle_df <- data.frame(lifestyle_df)
lifestyle_df <- scale(lifestyle_df, center = TRUE, scale = TRUE)

# [2] Dataframe on 'sustainable perception' (Col24 - Col34)

sustainable_df <- (survey_data[ ,24:34])
sustainable_df <- lapply(sustainable_df, as.numeric)
sustainable_df <- data.frame(sustainable_df)
sustainable_df <- scale(sustainable_df, center = TRUE, scale = TRUE)



# --------- Part 2: Data Analysis ---------------------

# 2.1 : Identify underlying factors on 'consumers lifestyle' - - - - - - - -

# [1] Initial exploration
head(lifestyle_df)

corrplot(cor(lifestyle_df), main = '\n\nCorrelation Plot on Consumers Lifestyles')


# [2] Identify whether data is suitable for 'factor analysis'? (Kaiser-Meyer-Olkin Test)

# Function for KMO Test 
# ( Testing the proportion of variability among variables that might be common variability) 

kmo <- function(x)
            {
              x <- subset(x, complete.cases(x))
              r <- cor(x)
              r2 <- r^2
              i <- solve(r)
              d <- diag(i)
              p2 <- (-i/sqrt(outer(d, d)))^2
              diag(r2) <- diag(p2) <- 0
              KMO <- sum(r2)/(sum(r2)+sum(p2))
              MSA <- colSums(r2)/(colSums(r2)+colSums(p2)) 
              return(list(KMO=KMO, MSA=MSA))
            }

kmo(lifestyle_df) # Pass Test: > 0.5


# [3] Decide on 'number' of factors using scree plot

scree(lifestyle_df, factors=TRUE, pc=TRUE,
      main = "Scree plot for EFA", hline=NULL, add=FALSE) 

nScree(lifestyle_df)

VSS.scree(lifestyle_df, main = "Scree plot for EFA") # suggest 4 factors

eigen(cor(lifestyle_df)) # suggest 4 factors


# [4] Factor Analysis

# without rotation
lifestyle_factor <- factanal(lifestyle_df, factors = 4, rotation ="none")
lifestyle_factor 

# with rotation (get better insights)
lifestyle_factorRO <- factanal(lifestyle_df, factors = 4, rotation ="varimax", scores="Bartlett")
lifestyle_factorRO 


# [5] Heatmap of Factors

heatmap.2(lifestyle_factorRO$loadings,
          col = brewer.pal(9, "YlOrRd"), trace= "none",dend="none",
          key = FALSE, cexCol = 1.2, Colv=FALSE,
          main = "\n\n\n\n\nUnderlying Factors of Consumers Lifestyle")


# [6] Plot Structure

semPaths(lifestyle_factorRO, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"),
         negCol=c("white", "red"), edge.label.cex=0.75, nCharNodes=7)


# [7] Factor Scores

lifestyle_scores <- data.frame(lifestyle_factorRO$scores) 

lifestyle_scores$AgeCategory <- survey_data$AgeCategory

head(lifestyle_scores)


# [8] Aggregate 'Factors Score' by 'Age Categories'
lifestyle_factor_mean <- aggregate(. ~ AgeCategory, data=lifestyle_scores, mean)
rownames(lifestyle_factor_mean) <- lifestyle_factor_mean[, 1]
lifestyle_factor_mean <- lifestyle_factor_mean[, -1]

names(lifestyle_factor_mean) <- c("Style Consumer", "Aesthetic Buyer", 
                                  "Prestige Seeker", "Functionality Shopper") 
rownames(lifestyle_factor_mean) <- c("Age: < 20", "Age: 21-30", "Age: 31-40", "Age: 41-50", "Age: 51-60", "Age: > 61")
lifestyle_factor_mean



heatmap(as.matrix(lifestyle_factor_mean),
        Colv = NA, 
        col = brewer.pal(9, "YlOrRd"), 
        cexCol = 1.4,
        keep.dendro = FALSE,
        main = "\n\nFactor Scores (mean) by Age Category",
        margins = c(15, 15))



# 2.2 : Identify underlying factors on 'Sustainable Clothing Perception' - - - - - - - -


# [1] Initial exploration
head(sustainable_df)
corrplot(cor(sustainable_df), order="hclust",
         main = '\n\nCorrelation Plot on Sustainability Perceptions')


# [2] Identify whether data is suitable for 'factor analysis'? (Kaiser-Meyer-Olkin Test)

# Function for KMO Test 
# ( Testing the proportion of variability among variables that might be common variability) 

kmo <- function(x)
{
  x <- subset(x, complete.cases(x))
  r <- cor(x)
  r2 <- r^2
  i <- solve(r)
  d <- diag(i)
  p2 <- (-i/sqrt(outer(d, d)))^2
  diag(r2) <- diag(p2) <- 0
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2)) 
  return(list(KMO=KMO, MSA=MSA))
}

kmo(sustainable_df) # Pass Test: > 0.5 (0.8193426)


# [3] Decide on 'number' of factors using scree plot

scree(sustainable_df, factors=TRUE, pc=TRUE,
      main = "Scree plot for EFA", hline=NULL, add=FALSE) 

nScree(sustainable_df) # suggest 3 factors

VSS.scree(sustainable_df, main = "Scree plot for EFA") # suggest 3-4 factors

eigen(cor(sustainable_df)) # suggest 3 factors


# [4] Factor Analysis

# without rotation
sustainable_factor <- factanal(sustainable_df, factors = 3, rotation ="none")
sustainable_factor 

# with rotation (get better insights)
sustainable_factorRO <- factanal(sustainable_df, factors = 3, rotation ="varimax", scores="Bartlett")
sustainable_factorRO 


# [5] Heatmap of Factors

heatmap.2(sustainable_factorRO$loadings,
          col = brewer.pal(9, "YlOrRd"), trace= "none",dend="none",
          key = FALSE, cexCol = 1.2, Colv=FALSE,
          main = "\n\n\n\n\nUnderlying Factors of Sustainable Perceptions")


# [6] Plot Structure

semPaths(sustainable_factorRO, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"),
         negCol=c("white", "red"), edge.label.cex=0.75, nCharNodes=7)


# [7] Factor Scores

sustainable_scores <- data.frame(sustainable_factorRO$scores) 

sustainable_scores$AgeCategory <- survey_data$AgeCategory

head(sustainable_scores)


# [8] Aggregate 'Factors Score' by 'Age Categories'
sustainable_factor_mean <- aggregate(. ~ AgeCategory, data= sustainable_scores, mean)
rownames(sustainable_factor_mean) <- sustainable_factor_mean[, 1]
sustainable_factor_mean <- sustainable_factor_mean[, -1]

names(sustainable_factor_mean) <- c("Eco Experts", "Eco Learners", "Beginners")

rownames(sustainable_factor_mean) <- c("Age: < 20", "Age: 21-30", "Age: 31-40", "Age: 41-50", "Age: 51-60", "Age: > 61")
sustainable_factor_mean



heatmap(as.matrix(lifestyle_factor_mean),
        Colv = NA, 
        col = brewer.pal(9, "YlOrRd"), 
        cexCol = 1.4,
        keep.dendro = FALSE,
        main = "\n\nFactor Scores (mean) by Age Category",
        margins = c(15, 15))







# 2.3 : Identify 'Purchase Intentions (PI)' & 'Willingness to Recommend (WTR)' for different education levels - - - - - - - -


# [1] Create new DF for this analysis: education level + cloth purchase behavior + PI1 + WTR

PIWTR_df <- survey_data[ ,c(4,7,35,37)]
PIWTR_df <- lapply(PIWTR_df, as.numeric)
PIWTR_df <- data.frame(PIWTR_df)

# [2] Initial exploration of new df

head(PIWTR_df)

plot(PIWTR_df)

corrplot(cor(PIWTR_df))


# [3] Create dummy variables for 'educational level'


PIWTR_df$Education <- as.factor(PIWTR_df$Education)
education_dummy <- model.matrix(~ Education -1, data = PIWTR_df)
education_dummy <- data.frame(education_dummy)
PIWTR_df <- cbind(PIWTR_df, education_dummy)

head(PIWTR_df)


# [4] ANOVA: Analysis whether 'PI differ across education levels?'

# Model 1: Full Model of PI1 - -

fullmodel_PI1 <- lm(PI1 ~ Education1 + Education2 + Education3,
                    data = PIWTR_df)

summary(fullmodel_PI1) # (p-value: 0.1016: no significant difference)

AIC(fullmodel_PI1) # (AIC = 471.5152)
BIC(fullmodel_PI1) # (BIC = 486.7645)


# Model 2: Reduced Model of PI1 - -

reducedmodel_PI1 <- lm(PI1 ~ 1, data = PIWTR_df)

summary(reducedmodel_PI1)

AIC(reducedmodel_PI1) # (AIC = 471.8732)
BIC(reducedmodel_PI1) # (BIC = 477.9729)


# Compare Model1 vs Model2

anova(fullmodel_PI1, reducedmodel_PI1) 

# Result: (Pr(>F): 0.1016) -> Full model is not better than reduced model



# [5] ANOVA: Analysis whether 'WTR differ across education levels?'


# Model 1: Full Model of WTR - -

fullmodel_WTR <- lm(WTR ~ Education1 + Education2 + Education3,
                    data = PIWTR_df)

summary(fullmodel_WTR) # (p-value: 0.1016: no significant difference)

AIC(fullmodel_WTR) # (AIC = 440.5472)
BIC(fullmodel_WTR) # (BIC = 455.7965)


# Model 2: Reduced Model of WTR - -

reducedmodel_WTR <- lm(WTR ~ 1, data = PIWTR_df)

summary(reducedmodel_WTR)

AIC(reducedmodel_WTR) # (AIC = 434.9635)
BIC(reducedmodel_WTR) # (BIC = 441.0632)


# Compare Model1 vs Model2 of WTR

anova(fullmodel_WTR, reducedmodel_WTR) # (P = 0.9388) Full model is not better




# [6] ANCOVA: Analysis whether 'PI differ across education levels?' (Controlling for 'Clothing Pur Behavior')


# Model 1: Full Model of PI1 - -

fullmodel_PI1_ctrl <- lm(PI1 ~ PurchaseFreq + Education1 + Education2 + Education3,
                    data = PIWTR_df)

summary(fullmodel_PI1_ctrl) # (p-value: 0.01848: no significant difference)

AIC(fullmodel_PI1_ctrl) # (AIC = 467.7071)
BIC(fullmodel_PI1_ctrl) # (BIC = 486.0062)


# Model 2: Reduced Model of PI1 - -

reducedmodel_PI1_ctrl <- lm(PI1 ~ PurchaseFreq, data = PIWTR_df)

summary(reducedmodel_PI1_ctrl)

AIC(reducedmodel_PI1_ctrl) # (AIC = 470.6107)
BIC(reducedmodel_PI1_ctrl) # (BIC = 479.7603)


# Compare Model1 vs Model2

anova(reducedmodel_PI1_ctrl, fullmodel_PI1_ctrl) # full model better


# check for 'moderation effect'

PI1_moderation <- lm(PI1 ~ PurchaseFreq + 
                       Education1 + Education2 + Education3 +
                       PurchaseFreq * Education1 + 
                       PurchaseFreq * Education2 + 
                       PurchaseFreq * Education3,
                     data = PIWTR_df)

summary(PI1_moderation)

anova(fullmodel_PI1_ctrl, PI1_moderation) # no significant difference


# [7] ANCOVA: Analysis whether 'PI differ across education levels?' (Controlling for 'Clothing Pur Behavior')


# Model 1: Full Model of WTR - -

fullmodel_WTR_ctrl <- lm(WTR ~ PurchaseFreq + Education1 + Education2 + Education3,
                         data = PIWTR_df)

summary(fullmodel_WTR_ctrl) # (p-value: 0.01848: no significant difference)

AIC(fullmodel_WTR_ctrl) # (AIC = 440.3439)
BIC(fullmodel_WTR_ctrl) # (BIC = 458.643)


# Model 2: Reduced Model of WTR - -

reducedmodel_WTR_ctrl <- lm(WTR ~ PurchaseFreq, data = PIWTR_df)

summary(reducedmodel_WTR_ctrl)

AIC(reducedmodel_WTR_ctrl) # (AIC = 434.5484)
BIC(reducedmodel_WTR_ctrl) # (BIC = 443.6979)


# Compare Model1 vs Model2

anova(reducedmodel_WTR_ctrl, fullmodel_WTR_ctrl) # Not reject H0: full model is not better


# check for 'moderation effect'

WTR_moderation <- lm(WTR ~ PurchaseFreq + 
                       Education1 + Education2 + Education3 +
                       PurchaseFreq * Education1 + 
                       PurchaseFreq * Education2 + 
                       PurchaseFreq * Education3,
                     data = PIWTR_df)

summary(WTR_moderation)

anova(fullmodel_WTR_ctrl, WTR_moderation) # no significant difference





# 2.4 : Analyze 'important drivers' for PI1 and WTR --------------------------

# [1] create new df for the purpose of analysis


# 1.1: Factor score on lifestyle
lifestyle_scores_df <- lifestyle_scores[,-5] # delete AgeCategory
names(lifestyle_scores_df) <- c("Style_Consumer", "Aesthetic_Buyer", 
                             "Prestige_Seeker", "Functionality_Shopper") 
lifestyle_scores_df$ID <- survey_data$ID # add ID col
lifestyle_scores_df$Gender <- survey_data['Gender'] # add Gender col (1 = Female)
lifestyle_scores_df$Gender <- ifelse(lifestyle_scores_df$Gender == 1,1,0)

head(lifestyle_scores_df) 
colnames(lifestyle_scores_df)
str(lifestyle_scores_df)


# 1.2: Factor score on sustainable perception
sustainable_scores_df <- sustainable_scores[,-4] # delete AgeCategory
names(sustainable_scores_df) <- c("Eco_Experts", "Eco_Learners", "Beginners")
sustainable_scores_df$ID <- survey_data$ID # add ID col

head(sustainable_scores_df)
colnames(sustainable_scores_df)
str(sustainable_scores_df)


# 1.3 Merge data: factor score + gender 
PIWTR_drivers <- inner_join(lifestyle_scores_df, sustainable_scores_df, by = 'ID')
PIWTR_drivers$PI1 <- as.numeric(as.character(survey_data$PI1))
PIWTR_drivers$WTR <- as.numeric(as.character(survey_data$WTR))
PIWTR_drivers$ID <- as.numeric(PIWTR_drivers$ID)

head(PIWTR_drivers)
str(PIWTR_drivers)


# 1.4 standardize dataframe

PIWTR_drivers_scale <- PIWTR_drivers %>%
  select(ID, PI1, WTR, Gender, everything())

scaled_col <- scale(PIWTR_drivers_scale[,4:11], center = TRUE, scale = TRUE)
PIWTR_drivers_scale <- cbind(PIWTR_drivers_scale[,1:3],scaled_col )

head(PIWTR_drivers_scale)
str(PIWTR_drivers_scale)

# [2] Multiple Regression: Analyze 'important drivers' for PI1

# Regression Model1: No Gender

PIWTR_drivers$PI1 <- as.numeric(as.character(PIWTR_drivers$PI1))
PI1_drivers_Nogen <- lm(PI1 ~ Style_Consumer + Aesthetic_Buyer + Prestige_Seeker + Functionality_Shopper +
                          Eco_Experts + Eco_Learners + Beginners,
                        data = PIWTR_drivers_scale)

summary(PI1_drivers_Nogen) # Significance = Prestige_Seeker 



# Regression Model2: with Gender


PI1_drivers_gen <- lm(PI1 ~ Style_Consumer + Aesthetic_Buyer + Prestige_Seeker + Functionality_Shopper +
                          Eco_Experts + Eco_Learners + Beginners +
                          Gender,
                        data = PIWTR_drivers_scale)

summary(PI1_drivers_gen) # Significance = 1) Prestige_Seeker 2) Gender  


# Check for 'Multicollinearity' (No multicollinearity)

VIF_PI1_drivers_gen <- vif(PI1_drivers_gen)
Tolerance_PI1_drivers_gen <- 1/(VIF_PI1_drivers_gen)

VIF_PI1_drivers_gen
Tolerance_PI1_drivers_gen

# Plot VIF
par(mar = c(5, 8, 4, 2) + 2)
barplot(VIF_PI1_drivers_gen, main = "VIF Values of PI1 Model", 
        horiz = TRUE, xlim = c(0,12), col = "red", las =1)
abline(v = 4, lwd=3, lty=2)
abline(v = 10, lwd = 3, lty = 2) 



# [3] Multiple Regression: Analyze 'important drivers' for WTR

# Regression Model1: No Gender

PIWTR_drivers$WTR <- as.numeric(as.character(PIWTR_drivers$WTR))
WTR_drivers_Nogen <- lm(WTR ~ Style_Consumer + Aesthetic_Buyer + Prestige_Seeker + Functionality_Shopper +
                          Eco_Experts + Eco_Learners + Beginners,
                        data = PIWTR_drivers_scale)

summary(WTR_drivers_Nogen) # Significance = Prestige_Seeker Only



# Regression Model2: with Gender


WTR_drivers_gen <- lm(WTR ~ Style_Consumer + Aesthetic_Buyer + Prestige_Seeker + Functionality_Shopper +
                        Eco_Experts + Eco_Learners + Beginners +
                        Gender,
                      data = PIWTR_drivers_scale)

summary(WTR_drivers_gen) # Significance = Prestige_Seeker Only  


# Check for 'Multicollinearity' (No multicollinearity)

VIF_WTR_drivers_gen <- vif(WTR_drivers_gen)
Tolerance_WTR_drivers_gen <- 1/(VIF_WTR_drivers_gen)

VIF_WTR_drivers_gen
Tolerance_WTR_drivers_gen

# Plot VIF
par(mar = c(5, 8, 4, 2) + 2)
barplot(VIF_WTR_drivers_gen, main = "VIF Values of WTR Model", 
        horiz = TRUE, xlim = c(0,12), col = "red", las =1)
abline(v = 4, lwd=3, lty=2)
abline(v = 10, lwd = 3, lty = 2)





# 2.5 : Analyze 'important drivers' from 2.4 BUT for different consumers characteristics  --------------------------

# [1] Check Impact of important drivers across different 'Money Spent' on clothing


# Add 'Money Spent' column to df

PIWTR_drivers_scale$MoneySpent <- scale(survey_data$MoneySpent, center = TRUE, scale = TRUE)

head(PIWTR_drivers_scale)


# Is 'MoneySpent' moderate the effect of Prestige_Seeker on PI1 ??


PI1_moderate <- lm(PI1 ~ Style_Consumer + Aesthetic_Buyer + Prestige_Seeker + Functionality_Shopper +
                     Eco_Experts + Eco_Learners + Beginners +
                     Gender+
                     MoneySpent + Prestige_Seeker * MoneySpent,
                   data = PIWTR_drivers_scale)

summary(PI1_moderate) # Ans: No moderation effect (not significant) !!



# [2] Check Impact of important drivers across different 'Education Level' on clothing

# Add 'education level dummy' into df
PIWTR_drivers_scale <- cbind(PIWTR_drivers_scale, education_dummy)
head(PIWTR_drivers_scale)


# Is 'education level'  moderate the effect of Prestige_Seeker on WTR ??

WTR_moderate <- lm(WTR ~ Style_Consumer + Aesthetic_Buyer + Prestige_Seeker + Functionality_Shopper +
                     Eco_Experts + Eco_Learners + Beginners +
                     Gender+
                     Education1 + Education2 + Education3 +
                     Prestige_Seeker * Education1 +
                     Prestige_Seeker * Education2 +
                     Prestige_Seeker * Education3,
                   data = PIWTR_drivers_scale)

summary(WTR_moderate) # Moderate effect: education2 (MBO – Intermediate Vocational Education)




















