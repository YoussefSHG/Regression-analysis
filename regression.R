ObesityDataSet <- read.csv("ObesityDataSet.csv")
View(ObesityDataSet)


#The attributes related with eating habits are:
#  FAVC : Frequent consumption of high caloric food

#FCVC : Frequency of consumption of vegetables

#NCP : Number of main meals

#CAEC : Consumption of food between meals

#CH2O : Consumption of water daily

#CALC : Consumption of alcohol

#The attributes related with the physical condition are:
#  SCC : Calories consumption monitoring

#FAF : Physical activity frequency

#TUE : Time using technology devices

#MTRANS : Transportation used

#The attributes related with eating habits are:
#FAVC : Frequent consumption of high caloric food

#FCVC : Frequency of consumption of vegetables

#NCP : Number of main meals

#CAEC : Consumption of food between meals

#CH2O : Consumption of water daily

#CALC : Consumption of alcohol

#The attributes related with the physical condition are:
#SCC : Calories consumption monitoring

#FAF : Physical activity frequency

#TUE : Time using technology devices

#MTRANS : Transportation used

##########################################################################


names(ObesityDataSet)[names(ObesityDataSet) == "NObeyesdad"] <- "Obesity_Type"

scale_mapper_forObesityType <- c('Insufficient_Weight' = 1,
                                 'Normal_Weight' = 2,
                                 'Overweight_Level_I' = 3,
                                 'Overweight_Level_II' = 4,
                                 'Obesity_Type_I' = 5,
                                 'Obesity_Type_II' = 6,
                                 'Obesity_Type_III' = 7)

# Map the values from the "Obesity_level" column using scale_mapper
ObesityDataSet$N_Obesity_Type <- scale_mapper_forObesityType[ObesityDataSet$Obesity_Type]
##########

scale_mapper_Gender = c('Male'=1,'Female'=0)

# Map the values from the "Obesity_level" column using scale_mapper
ObesityDataSet$N_Gender <- scale_mapper_Gender[ObesityDataSet$Gender]
##########

scale_mapper_SMOKE= c('yes'=1,'no'=0)
# Map the values from the "Obesity_level" column using scale_mapper
ObesityDataSet$N_SMOKE <- scale_mapper_SMOKE[ObesityDataSet$SMOKE]

##########
scale_mapper_family_history_with_overweight= c('yes'=1,'no'=0)
# Map the values from the "Obesity_level" column using scale_mapper
ObesityDataSet$N_family_history_with_overweight <- scale_mapper_family_history_with_overweight[ObesityDataSet$family_history_with_overweight]

##########
scale_mapper_FAVC= c('yes'=1,'no'=0)
# Map the values from the "Obesity_level" column using scale_mapper
ObesityDataSet$N_FAVC <- scale_mapper_FAVC[ObesityDataSet$FAVC]
##########
scale_mapper_N_SCC= c('yes'=1,'no'=0)
# Map the values from the "Obesity_level" column using scale_mapper
ObesityDataSet$N_SCC <- scale_mapper_N_SCC[ObesityDataSet$SCC]

##########
##still 
#unique(ObesityDataSet$CAEC)
###
scale_mapper_CAEC= c('Always'=3,"Frequently"=2,"Sometimes"=1,'no'=0)
# Map the values from the "Obesity_level" column using scale_mapper
ObesityDataSet$N_CAEC <- scale_mapper_CAEC[ObesityDataSet$CAEC]

##########

#unique(ObesityDataSet$CALC)

scale_mapper_CALC= c('Always'=3,"Frequently"=2,"Sometimes"=1,'no'=0)
# Map the values from the "Obesity_level" column using scale_mapper
ObesityDataSet$N_CALC <- scale_mapper_CALC[ObesityDataSet$CALC]
##########


unique(ObesityDataSet$MTRANS)
scale_mapper_MTRANS= c('Automobile'=5,
                       'Public_Transportation'=4,
                       'Motorbike'=3,
                       "Bike"=2,
                       "Walking"=1)

# Map the values from the "Obesity_level" column using scale_mapper
ObesityDataSet$N_MTRANS <- scale_mapper_MTRANS[ObesityDataSet$MTRANS]
##########
View(ObesityDataSet)




# Selecting numeric columns
numeric_ObesityDataSet <- ObesityDataSet[c('N_Gender', 'Age', 'Height', 'Weight',
                                           'N_family_history_with_overweight',
                                           'N_FAVC', 'FCVC','NCP', 'N_CAEC', 'N_SMOKE',
                                           'CH2O', 'N_SCC', 'FAF', 'TUE', 'N_CALC',
                                           'N_MTRANS', 'N_Obesity_Type')]


View(numeric_ObesityDataSet)

### Data Visualization Cleaning ###
### Box Plots Representing the Distribution of each numeric column ###
### Check for outliers ###

# It's rate, so dots that appear on the box plot won't be considered as outliers.
unique(numeric_ObesityDataSet$Age)

# box plot representing the distribution of Age
boxplot(
  numeric_ObesityDataSet$Age,
  main = "Distribution of Gender",
  col = "green",
  border = "black",
  horizontal = T,
  notch = F
)

# box plot representing the distribution of Height
boxplot(
  numeric_ObesityDataSet$Height,
  main = "Distribution of Height",
  col = "red",
  border = "black",
  horizontal = T,
  notch = F
)

# box plot representing the distribution of Weight
boxplot(
  numeric_ObesityDataSet$Weight,
  main = "Distribution of Weight",
  col = "purple",
  border = "black",
  horizontal = T,
  notch = F
)

# box plot representing the distribution of FCVC
boxplot(
  numeric_ObesityDataSet$FCVC,
  main = "Distribution of FCVC",
  col = "cyan",
  border = "black",
  horizontal = T,
  notch = F
)

# It's rate not exact, same as "NCP" column, so we won't consider values like 1 and 4 as outliers.
unique (numeric_ObesityDataSet$NCP)

# box plot representing the distribution of NCP
boxplot(
  numeric_ObesityDataSet$NCP,
  main = "Distribution of NCP",
  col = "cyan",
  border = "black",
  horizontal = F,
  notch = F
)

# box plot representing the distribution of CH2O
boxplot(
  numeric_ObesityDataSet$CH2O,
  main = "Distribution of CH2O",
  col = "cyan",
  border = "black",
  horizontal = F,
  notch = F
)

# box plot representing the distribution of FAF
boxplot(
  numeric_ObesityDataSet$FAF,
  main = "Distribution of FAF",
  col = "cyan",
  border = "black",
  horizontal = T,
  notch = F
)

# box plot representing the distribution of TUE
boxplot(
  numeric_ObesityDataSet$TUE,
  main = "Distribution of TUE",
  col = "cyan",
  border = "black",
  horizontal = F,
  notch = F
)


# Check if there is a duplicated rows in your data 
# & number of duplicated rows if it contains duplication 
sum(duplicated(numeric_ObesityDataSet))
# To remove duplicates
numeric_ObesityDataSet <- unique(numeric_ObesityDataSet)

sum(duplicated(numeric_ObesityDataSet))

# Check if there is a NA values in your Data
# & number of NA values 
sum(is.na(numeric_ObesityDataSet))
# There is no nulls in the data.


# Fit a simple model between the "Weight" and "N_Obesity_Type"
simple_model <- lm(N_Obesity_Type ~ Weight, data = numeric_ObesityDataSet)

summary(simple_model)

plot(numeric_ObesityDataSet$Weight, numeric_ObesityDataSet$N_Obesity_Type, xlab = 'Weight', ylab = 'N_Obesity_Type')
abline(simple_model, col = 'red', lwd = 2)

anova(simple_model)


# Fit a model using "Age", "Height", "FCVC", "NCP", "CH2O", "FAF",and "TUE" as X-variables.
model <- lm(N_Obesity_Type ~ N_Gender + Age + Height + Weight +
              N_family_history_with_overweight + N_FAVC + FCVC + 
              NCP + N_CAEC + N_SMOKE + CH2O + N_SCC + FAF + 
              TUE + N_CALC + N_MTRANS, data = numeric_ObesityDataSet)

summary(model)

#######################################################################################
lm(N_Obesity_Type ~ N_Gender + Age + Height + Weight +
     N_family_history_with_overweight + N_FAVC + FCVC + 
     NCP + N_CAEC + N_SMOKE + CH2O + N_SCC + FAF + 
     TUE + N_CALC + N_MTRANS, data = numeric_ObesityDataSet)
#######################################################################################
# Compute the correlation matrix
correlation_matrix <- cor(numeric_ObesityDataSet)

# Extract correlation of N_Obesity_Type with other variables
cor_with_obesity_type <- correlation_matrix["N_Obesity_Type", ]

# Print the correlation values
print(cor_with_obesity_type)


install.packages("gplots")

library(gplots)  # You need to install this package if you haven't already (install.packages("gplots"))

# Create heatmap
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),  # Color scheme
        scale = "none",  # No scaling
        symm = TRUE,  # Symmetrically scale the colors around zero
        margins = c(10, 10))  # Adjust the margins
################################################################33
library(car)
crPlots(model)
#######################################################################33


# After selecting those features having strong, positive or negative, relationship with N_Obesity_Type).
reduced_model1 <- lm(N_Obesity_Type ~ Age + Height + Weight +
                       N_family_history_with_overweight +
                       N_FAVC + FCVC + CH2O + N_SCC +
                       FAF + TUE, data = numeric_ObesityDataSet)



summary(reduced_model1)


#nrow(numeric_ObesityDataSet)

anova(reduced_model1,model)





lm(N_Obesity_Type ~ N_Gender + Age + Height + Weight +
     N_family_history_with_overweight + N_FAVC + FCVC + 
     NCP + N_CAEC + N_SMOKE + CH2O + N_SCC + FAF + 
     TUE + N_CALC + N_MTRANS, data = numeric_ObesityDataSet)##########################################

#plot(model)
e = resid(model)
print(e)

# Compute and print the fitted values
print(fitted(model))
print(predict(model))

# Predict the fitted value at certain x values

new <- data.frame(N_Gender = 0,  # Assuming 1 represents male and 2 represents female
                  Age = 20,
                  Height = 1.70,
                  Weight = 120,
                  N_family_history_with_overweight = 0,
                  N_FAVC = 1,
                  FCVC = 1,
                  NCP = 4,
                  N_CAEC = 3,
                  N_SMOKE = 0,
                  CH2O = 2,
                  N_SCC = 0,
                  FAF = 0,
                  TUE = 1,
                  N_CALC = 0,
                  N_MTRANS = 4
)
# predict new observation
print(predict(model, new, int='p', interval = 'confidence', level = 0.95))

# Print the Analysis of Variance Table
print(anova(model))

# Print the confidence interval of Bo and B1
print(confint(model, level = 0.95))

# Print the confidence interval for fitted values
print(predict(model, int = 'p'))