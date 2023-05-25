#########################
#####     CRABS     #####
#########################
#Sandra Fernández
#João Moreira
#Ksenija Vučković
#María Pérez

### First variable, which differentiates between the two species, represents a contrast
#between the carapace width relative to the width of the front lip and the depth of the body;
#the blue-form species has a greater relative carapace width than has the orange form.

### Second, which presents a contrast between the rear width and the carapace
#length, identifies males and females within each species; males have a greater relative carapace
#length than have females


#######################
###    OBJECTIVES   ###
#######################


# Set directory
setwd("C:/Users/semf2/Documents/Script2/Crabs")
setwd("C:/Users/59398/Documents/Master/IntroR/Crabs")
getwd()
library(MASS)
library(dplyr)
library(readxl)
library(dplyr) 
library(ggplot2)
library(car)
library(purrr)
library(gridExtra)
library(stats)

# Import database
# Databases are already in Mass package.
#data(package = "MASS", crabs)
#crabs

?crabs
crabs <- read.csv("crabs.csv")
as.data.frame(crabs)
View(crabs)

# Explore the file
class(crabs) #data.frame
mode(crabs) #list
typeof(crabs) #list

#Explore the dataset

dim(crabs)  
colnames(crabs)

#x=numbers
#sp=species= Blue |orange
#Sex=sex = M|F
#index 1:50 within each of the four groups.
#Fl=Frontal lobe size (mm)
#RW= Rear Width (mm)
#Cl=Carapace lenght (mm)
#CW=Carapace width(mm)
#BD=Body depth (mm)

# We have 9 variables, but MASS/crabs says it should have only 8 columns
#Index and X are the same, so we remove "X".
# Only characters are removable, so we transform it.

crabs$X <- as.character(crabs$X)
crabs<- crabs[-1]

summary(crabs)
str(crabs) 

# We change "index" to be character, because it is related to nº individuals.
# We change "sp" and "sex" to factors, to categorize this variables in levels.

crabs$index <- as.character(crabs$index)
crabs$sp <- as.factor(crabs$sp)
crabs$sex <- as.factor(crabs$sex)
str(crabs)

# We change variables names to make it more intuitive.


# Continue exploring...
results<-summarise(select(group_by(crabs,sp), FL, RW, CL, CW, BD), 
                   FL_min = min(FL, na.rm = TRUE), RW_min = min(RW, na.rm = TRUE),
                   CL_min = min(CL, na.rm = TRUE), CW_min = min(CW, na.rm = TRUE),
                   FL_max = max(FL, na.rm = TRUE), RW_max = max(RW, na.rm = TRUE),
                   CL_max = max(CL, na.rm = TRUE), CW_max = max(CW, na.rm = TRUE),
                   BD_min = min(BD, na.rm = TRUE), BD_max = max(BD, na.rm = TRUE),
                   BD_q25 = quantile(BD, c(0.25), na.rm =TRUE) , BD_q50 = quantile(BD, c(0.5), na.rm =TRUE),
                   BD_q75 = quantile(BD, c(0.75), na.rm =TRUE),
                   FL_q25 = quantile(FL, c(0.25), na.rm = TRUE), RW_q25 = quantile(RW, c(0.25), na.rm = TRUE),
                   CL_q25 = quantile(CL, c(0.25), na.rm = TRUE), CW_q25 = quantile(CW, c(0.25), na.rm = TRUE),
                   FL_q50 = quantile(FL, c(0.5), na.rm = TRUE), RW_q5 = quantile(RW, c(0.5), na.rm = TRUE),
                   CL_q50 = quantile(CL, c(0.5), na.rm = TRUE), CW_q5 = quantile(CW, c(0.5), na.rm = TRUE),
                   FL_q75 = quantile(FL, c(0.75), na.rm = TRUE), RW_q75 = quantile(RW, c(0.75), na.rm = TRUE),
                   CL_q75 = quantile(CL, c(0.75), na.rm = TRUE), CW_q75 = quantile(CW, c(0.75), na.rm = TRUE)
                   )

# Calculate mean

mean(crabs$FL)
mean(crabs$RW)
mean(crabs$CL)
mean(crabs$CW)
mean(crabs$BD)

#mean of all the variables
means <- sapply(crabs[, c("FL", "RW", "CL", "CW", "BD")], mean)
means

#mean of all the variables but selected by sp morpho

results_mean<-summarise(select(group_by(crabs, sp),  FL, RW, CL, CW,BD),
                        FL_mean = mean(FL, na.rm = TRUE), RW_mean = mean(RW, na.rm = TRUE),
                        CL_mean = mean(CL, na.rm = TRUE), CW_mean = mean(CW, na.rm = TRUE),
                        BD_mean = mean(BD, na.rm = TRUE))
results_mean

#Calculate the median

median(crabs$FL)
median(crabs$RW)
median(crabs$CL)
median(crabs$CW)
median(crabs$BD)

#median of all the variables

medians <- sapply(crabs[, c("FL", "RW", "CL", "CW", "BD")], median)
medians

#mdian of all the variables but selected by sp morpho

results_median<-summarise(select(group_by(crabs, sp),  FL, RW, CL, CW,BD),
                        FL_median = median(FL, na.rm = TRUE), RW_median = median(RW, na.rm = TRUE),
                        CL_median = median(CL, na.rm = TRUE), CW_median = median(CW, na.rm = TRUE),
                        BD_median = median(BD, na.rm = TRUE))

results_median

#Ranges 

range(crabs$FL)
range(crabs$RW)
range(crabs$CL)
range(crabs$CW)
range(crabs$BD)

#Ranges of all the variables
range <- sapply(crabs[, c("FL", "RW", "CL", "CW", "BD")], range)
range

#ranges of all the variables but selected by sp morpho
results_range<-summarise(select(group_by(crabs, sp),  FL, RW, CL, CW,BD),
                        FL_range = range(FL, na.rm = TRUE), RW_range = range(RW, na.rm = TRUE),
                        CL_range = range(CL, na.rm = TRUE), CW_range = range(CW, na.rm = TRUE))
results_range

# Standard deviations

sd(crabs$FL)
sd(crabs$RW)
sd(crabs$CL)
sd(crabs$CW)
sd(crabs$BD)

#Standard deviation of all the variables 

sd <- sapply(crabs[, c("FL", "RW", "CL", "CW", "BD")], sd)
sd

#standard deviation of all the variables but selected by sp morpho

results_sd<-summarise(select(group_by(crabs, sp),  FL, RW, CL, CW,BD),
                         FL_sd = sd(FL, na.rm = TRUE), RW_sd = sd(RW, na.rm = TRUE),
                         CL_sd = sd(CL, na.rm = TRUE), CW_sd = sd(CW, na.rm = TRUE))
results_sd

#Playing with tydiverse

# Select only the rows with one sex (F), one species, sex + species.
crabs_F <- crabs %>% filter(sex == "F")
crabs_F
crabs_spO <- crabs %>% filter (sp == "O")
crabs_spO
crabs_F_spO <- crabs %>% filter (sex == "F"& sp == "O")
crabs_F_spO
crabs_FB<- crabs %>% filter(sex == "F", sp=="B") 
crabs_FB
crabs_M <- crabs %>%filter(sex=="M")
crabs_M
crabs_mO<- crabs %>% filter(sex == "M", sp=="O") 
crabs_mO
crabs_MB<- crabs %>% filter(sex == "M", sp=="O")
crabs_MB

Sel1<-select(crabs,"sex","RW","CL")
Sel2 <- select(crabs, "sex")
Sel3<-select(crabs,"sp","CW","BD")


# Extract only morphological variables

Morfo<-select(crabs,"sp","FL", "RW", "CL", "CW", "BD")
Morfo

# 7. Change the scale of one measurement

crabs.RW.cm <- crabs %>% mutate(RW_cm = RW / 10)  %>%
  select(RW_cm, RW, everything())
str(crabs.RW.cm)

#### NORMAL & NON-NORMAL DISTRIBUTION ####

# Function to determine normality based on p-value threshold

is_normal <- function(p_value, alpha = 0.05) {
  if (p_value >= alpha) {
    print("Normal")
  } else {
    print("Non-Normal")
  }
}

# Perform Shapiro-Wilk test for each numeric variable in the data frame
#link with the function above

Nresults <- lapply(crabs, function(variable) {
  if (is.numeric(variable)) {
    test_result <- shapiro.test(variable)
    p_value <- test_result$p.value
    normality <- is_normal(p_value)
    
    print(data.frame(Variable = deparse(substitute(variable)),
                      P_Value = p_value,
                      Normality = normality))
  } else {
    print(NULL)  # Skip non-numeric variables
  }
})

#Print Null for variables that are not normal and for normal variables it prints
#the p value and the normality 

# Combine the results into a data frame
Nresults_Crabs <- do.call(rbind, Nresults)

# View the results of only the variables that were normal
View(Nresults_Crabs)

################################################################################
# In this updated code, before applying the Shapiro-Wilk test, a check is.numeric() 
#is performed to ensure that the variable is numeric. 
#If the variable is not numeric, the lapply() function skips it by returning NULL.
#By incorporating this check, the code will only apply the Shapiro-Wilk test to 
#numeric variables in the "Crabs" data frame. 
#The results will be combined into a data frame and displayed using View(Nresults_df).
###############################################################################

#OR

library(purrr)

# Perform Shapiro-Wilk test for each numeric variable in the data frame
Nresults_Crabs2 <- crabs %>%
  keep(is.numeric) %>%
  map_df(~{
    test_result <- shapiro.test(.x)
    p_value <- test_result$p.value
    normality <- ifelse(p_value >= 0.05, "Normal", "Non-Normal")
    
    tibble(
      Variable = names(.x),
      P_Value = p_value,
      Normality = normality
    )
  })

View(Nresults_Crabs2)

#### PLOTS WITH GGPLOT2###################################################

library(ggplot2)

##############
# Histogram###
##############

# Plot histograms for the variables (FL, RW, CL, CW, BD)

h1<-ggplot(crabs, aes(x = FL)) +
  geom_histogram(binwidth = 0.5, fill = "deepskyblue4", color = "black") +
  labs(x = "Frontal Lobe (mm)", y = "Frequency", title = "Histogram of Variable FL") + geom_density(alpha=.2, fill="black")
plot(h1)

h2<-ggplot(crabs, aes(x = RW)) +
  geom_histogram(binwidth = 0.5, fill = "pink", color = "black") +
  labs(x = "Rear Width (mm)", y = "Frequency", title = "Histogram of Variable RW") + geom_density(alpha=.2, fill="black")
plot(h2)

h3<-ggplot(crabs, aes(x = CL)) +
  geom_histogram(binwidth = 0.5, fill = "green", color = "black") +
  labs(x = "Carapace Length (mm)", y = "Frequency", title = "Histogram of Variable CL") + geom_density(alpha=.2, fill="black")
plot(h3)

h4<-ggplot(crabs, aes(x = CW)) +
  geom_histogram(binwidth = 0.5, fill = "yellow", color = "black") +
  labs(x = "Carapace Width (mm)", y = "Frequency", title = "Histogram of Variable CW") + geom_density(alpha=.2, fill="black")
plot(h4)

h5<-ggplot(crabs, aes(x = BD)) +
  geom_histogram(binwidth = 0.5, fill = "orange", color = "black") +
  labs(x = "Body Depth (mm)", y = "Frequency", title = "Histogram of Variable BD") + geom_density(alpha=.2, fill="black")
plot(h5)

library(gridExtra)

grid.arrange(h1,h2,h3,h4,h5, ncol=3, nrow = 2)

#################
# Scatter Plot###
#################


# Plot a scatter plot of two variables "CL" and "CW"
ggplot(crabs, aes(x = CL, y = CW)) + geom_point() +  labs(x = "Carapace Length (mm)", y = "Carapace Width (mm)", title = "Scatter Plot of CL and CW") + geom_point(aes(col=sp), size=3) + geom_smooth(method="lm")

############
# BOXPLOT###
############


ggplot(crabs, aes(y=CL, group=sp)) + geom_boxplot()+ labs(y= "Carapace Length (mm)")
ggplot(crabs, aes(y=CW, group=sp)) + geom_boxplot()+ labs(y= "Carapace Width (mm)")

ggplot(crabs, aes(y=BD)) + geom_boxplot()
ggplot(crabs, aes(y=BD)) + geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=T)
ggplot(crabs, aes(y=BD, color=sp)) + geom_boxplot()

ggplot(crabs, aes(x=CL, y=CW)) + geom_point(aes(col=sp), size=3) + geom_smooth(method="lm")

###############
# USING LOOPS##
###############

# Define the numeric variables
variables <- c("FL", "RW")  # Replace with the actual variable names

# Define the grouping variable
group_var <- "sex"  # Replace with "sex" or "species"

# Create an empty list to store the results
results <- list()

# Loop over unique values of the grouping variable
for (group_value in unique(crabs[[group_var]])) {
  # Subset the data based on the grouping variable
  subset_crabs <- subset(crabs, crabs[[group_var]] == group_value)
  
  # Calculate the mean, standard deviation, minimum, and maximum for each numeric variable
  variable_stats <- sapply(subset_crabs[variables], function(x) {
    c(Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE))
  })
  
  
  # Store the results in the list
  results[[as.character(group_value)]] <- variable_stats
}

# Print or access the results
for (group_value in names(results)) {
  cat("Group:", group_value, "\n")
  print(results[[group_value]])
  cat("\n")
}

###########
# ANOVA####
###########

# Load the necessary packages
library(stats)

# Convert factor variables to numeric

crabs$BD <- as.numeric(as.character(crabs$BD))

# Perform a linear model
lm_model <- lm(RW ~ CL, data = crabs)

# Print the summary of the linear model
summary(lm_model)

# Perform ANOVA
anova_result <- anova(lm_model)

# Print the ANOVA table
print(anova_result)


# Rename the columns
crabs<-crabs %>% 
  rename ("front_lobe"="FL", 
          "rear_width"="RW", 
          "cara_lenght"="CL",
          "cara_width"="CW", 
          "body_depth"="BD")
str(crabs)

