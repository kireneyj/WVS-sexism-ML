##### Data Science III: Final Project - Global Sexism -------------------------------------------------------------------
##### Irene Kwon, Political Science, Northwestern University

# packages
library(tidyverse)
library(modelr)
library(skimr)
library(janitor)
library(rsample)
library(corrplot)
library(naniar)
library(mice)
library(finalfit)
library(GGally)
library(DataExplorer)

# dataset
#wvs_timeseries <- read_rds("data/WVS.rds") %>% 
#  clean_names()
#countryname <- read_csv("data/countryname.csv")

#wvs_timeseries <- left_join(wvs_timeseries, countryname, by = "s003")
#wvs_timeseries$Countryname %>% table()

# wvs_timeseries <- read_rds("data/WVS.rds") %>% 
#   clean_names()
# country_continent <- read_csv("data/countryContinent.csv")
#wvs_timeseries <- left_join(wvs_timeseries, country_continent, by = "s003")

# write_rds(wvs_timeseries, "wvs_timeseries.rds")

wvs_timeseries <- read_rds("wvs_timeseries.rds")

## Missingness Re-coding (those below 0 are coded as missing)
cleanFun <- function(df){
  
  # set negative values as NA
  df[df < 0] <- NA
  
  # drop those columns
  return (df)
}

wvs_timeseries <- cleanFun(wvs_timeseries)

# with negative values coded as missing
# write_rds(wvs_timeseries, "clean_wvs_timeseries.rds")

wvs_timeseries <- read_rds("clean_wvs_timeseries.rds")

# Dependent Variable: sexism score - combine c001, d059, d060, d078, e233 
# combine and recode so that larger values denote more sexism 

## Sexism-related questions: 
# Q1. Jobs scarce: Men should have more right to a job than women 
wvs_timeseries$c001 %>% table() # 1: agree, 2: disagree, 3: neither 
# Q2. Men make better political leaders than women do. 
wvs_timeseries$d059 %>% table() # 1: agree strongly, 2: agree, 3: disagree, 4: strongly disagree
# Q3. University is more important for a boy than for a girl. 
wvs_timeseries$d060 %>% table() # 1: agree strongly, 2: agree, 3: disagree, 4: strongly disagree
# Q4. Men make better business executives than women do.
wvs_timeseries$d078 %>% table() # 1: agree strongly, 2: agree, 3: disagree, 4: strongly disagree
# Q5. Democracy: Women have the same right as men
wvs_timeseries$e233 %>% table() # 1: not an essential character of democracy vs. 10: an essential character of democracy
# Q6. If a woman earns more money than her husband, it’s almost certain to cause problems
wvs_timeseries$d066_b %>% table() # 1: agree, 2: neither, 3: disagree
# Q7. Pre-school child suffers with working mother 
wvs_timeseries$d061 %>% table() # 1: agree strongly, 2: agree, 3: disagree, 4: disagree strongly 

# Dependent variable re-coding (so that large values denote bigger sexism)
wvs_timeseries2 <- wvs_timeseries %>% 
  mutate(c001 = ifelse(c001==1, 3, 
                       ifelse(c001 == 2, 1, 
                              ifelse(c001 == 3, 2, c001))),
         d066_b = ifelse(d066_b ==1, 3, 
                         ifelse(d066_b == 2, 1, 
                                ifelse(d066_b == 3, 2, c001))),
         d059 = ifelse(d059 > 0, 5 - d059, d059), 
         d060 = ifelse(d060 > 0, 5 - d060, d060), 
         d078 = ifelse(d078 > 0, 5 - d078, d078), 
         e233 = ifelse(e233 > 0, 11 - e233, e233), 
         d061 = ifelse(d061 > 0, 5 - d061, d061)
  ) 


# wvs_timeseries2 <- cleanFun(wvs_timeseries2)
# wvs_timeseries2 %>% skim_without_charts()

# DV 1. numerical scale -> add the values
wvs_timeseries2 <- wvs_timeseries2 %>%
  mutate(
    sexism = d078 + d060 + d059 + e233 + c001 + d066_b + d061
  )

wvs_timeseries2$sexism %>% summary() # lowest: 6, highest : 28, mean = 14.23
wvs_timeseries2$sexism %>% table()

## Comment out ---------------------------------------------------------------
wvs_timeseries2 %>%
  ggplot(aes(x = sexism)) + 
  geom_histogram() + 
  geom_vline(xintercept = 14.23, color = "red")

# Create a binary coded variable of sexism: below 13.6 (mean), not much sexist vs. above 13.6: sexist 
wvs_timeseries2$sexism_b <- ifelse(wvs_timeseries2$sexism < 14.23, 0, 
                                   ifelse(wvs_timeseries2$sexism > 14.23, 1, wvs_timeseries2$sexism))
wvs_timeseries2$sexism_b %>% table() 
# alternative coding: probably use the third quartile to split sexists vs. non-sexists 

wvs_timeseries2 %>% 
  group_by(sexism) %>% 
  count(name = "n") %>% 
  filter(sexism < 14) # binary variable appropriately coded 

wvs_timeseries2 %>% 
  select(c001, d078, d060, d059, e233, d066_b, d061) %>% 
  na.omit() %>% 
  cor() %>% 
  corrplot(method = "color", order = "AOE") # mostly mild correlation 
## --------------------------------------------------------------------------------------
# Sort out & rename the relevant variables 
wvs_timeseries2 <- wvs_timeseries2 %>% 
  rename(wave = s002, 
         sex = x001, 
         age = x003,
         religion = f025, 
         education = x025, 
         ses = x045, 
         income = x047, 
         religion_important = a006, 
         religion_childquality = a040, 
         religiousmembership = a098, 
         attendreligion = f028, 
         oftenpray = f028b, 
         religiousperson = f034, 
         religionmeaning1 = f200, 
         religionmeaning2 = f201, 
         sciencevsreligion = f202, 
         onlyacceptablereligion = f203, 
         teachallreligion = f204, 
         differentreligionmoral = f205, 
         religiousauthoritylaw = e225, 
         neighbor_diffreligion = a124_12, 
         trust_diffreligion = g007_35_b, 
         toomuchsciencelessfaith = e220, 
         just_sexbeforemarriage = f135a, 
         neighbor_unmarriedcouple = a124_42, 
         just_homosexuality = f118, 
         neighbor_homosexual = a124_09, 
         traditionimportant = a198, 
         workingmomchildsuffer = d061, 
         divorcejustifiable = f121, 
         parentsproudlifegoal = d054, 
         housewifefulfilling = d057, 
         adventurerisk = a195, 
         behaveproperlyimpt = a196, 
         abortionjustifiable = f120, 
         politicsimportant = a004, 
         interestpolitics = e023, 
         politicalrightleft = e033, 
         happiness = a008, 
         health = a009, 
         satisfactionlife = a170, 
         men_more_right_jobs = c001, 
         women_making_more_problem = d066_b, 
         men_better_poli_leaders = d059, 
         men_better_biz_exec = d078, 
         uni_edu_boys = d060, 
         dem_women_same_right_men = e233, 
         respectforauthority = e018, 
         respectforelder = a208, 
         obedienceimportant = a042, 
         boss30yrold = a203, 
         godimportant = f063, 
         believeinhell = f053
         )

# filter out the 6th wave & select the necessary variables only 
wvs6_dat <- wvs_timeseries2 %>% 
  filter(wave == 6) %>% 
  select(respectforauthority, respectforelder, obedienceimportant, boss30yrold, godimportant, believeinhell, 
    countryname, continent, sub_region, sex, age, religion, education, ses, income, religion_important, religion_childquality, 
         religiousmembership, attendreligion, oftenpray, religiousperson, religionmeaning1, religionmeaning2, 
         sciencevsreligion, onlyacceptablereligion, teachallreligion, differentreligionmoral, religiousauthoritylaw, 
         neighbor_diffreligion, trust_diffreligion, toomuchsciencelessfaith, just_sexbeforemarriage, 
         neighbor_unmarriedcouple, just_homosexuality, neighbor_homosexual, traditionimportant, workingmomchildsuffer, 
         divorcejustifiable, parentsproudlifegoal, housewifefulfilling, adventurerisk, behaveproperlyimpt, abortionjustifiable, 
         politicsimportant, interestpolitics, politicalrightleft, happiness, health, satisfactionlife, men_more_right_jobs, 
         women_making_more_problem, men_better_poli_leaders, men_better_biz_exec, uni_edu_boys, dem_women_same_right_men, sexism) %>% 
  mutate(religion = as.factor(religion))


# missingness 
wvs6_dat %>% 
  missing_plot()
wvs6_dat %>% 
  gg_miss_var(show_pct = TRUE)
# self-positioning of political right and left: the most missing values -> because of "don't know"s
# multiple imputation? or simple listwise omission? 


# data split 
set.seed(7081991)
ss <- sample(1:3,size=nrow(wvs6_dat),replace=TRUE,prob=c(0.6,0.2,0.2))
train <- wvs6_dat[ss==1,]
test <- wvs6_dat[ss==2,]
eda <- wvs6_dat[ss==3,]

### Distribution of country variables (from EDA set)
a <- eda %>% 
  group_by(countryname) %>% 
  count(countryname) %>% 
  ggplot(aes(x = countryname, y = n)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45))

b <- train %>% 
  group_by(countryname) %>% 
  count(countryname) %>% 
  ggplot(aes(x = countryname, y = n)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45))

c <- test %>% 
  group_by(countryname) %>% 
  count(countryname) %>% 
  ggplot(aes(x = countryname, y = n)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45))

cowplot::plot_grid(a, b, c, ncol = 1) 

##### EDA -------------------------------------------------------------------------------------------------------------------
### Dependent Variable: Sexism Score --------------------------------------------------------
eda %>%
  filter(sexism != "NA") %>% 
  ggplot(aes(x = sexism)) + 
  geom_histogram(aes(y = ..density..), bins = 25) + 
  geom_vline(aes(xintercept = mean(sexism)), 
             color = "red", linetype = "dashed", size = 1) + 
  geom_density(alpha = .2, fill = "#1A5276") + 
  labs(
    title = "Distribution of Sexism", 
    subtitle = "Higher values indicate higher sexism.", 
    caption = "World Value Survey, Wave 6"
  )


# heatmap (correlation map) using the EDA dataset 
### correlation among sexism score constituent variables 
eda %>% 
  select(men_more_right_jobs, men_better_poli_leaders, men_better_biz_exec, 
         women_making_more_problem, uni_edu_boys, dem_women_same_right_men) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  corrplot(method = "color", order = "AOE")

eda %>% 
  select(men_more_right_jobs, men_better_poli_leaders, men_better_biz_exec, 
         women_making_more_problem, uni_edu_boys, dem_women_same_right_men) %>% 
  na.omit() %>% 
  cor() %>% 
  corrplot(method = "color", order = "AOE")


## PCA for the sexism score constituents 
sexism <- eda %>% 
  select(men_more_right_jobs, men_better_poli_leaders, men_better_biz_exec, 
         women_making_more_problem, uni_edu_boys, dem_women_same_right_men, 
         workingmomchildsuffer) %>% 
  na.omit()

sexism_pr_out <- sexism %>% 
  prcomp(scale = TRUE) # men_better_biz_exec, men_better_poli_leaders

# biplot(sexism_pr_out, scale = 0)

pve = tibble(component = 1:7,
             pve = sexism_pr_out$sdev^2/sum(sexism_pr_out$sdev^2)) 
ggplot(pve) + 
  geom_line(aes(component, pve)) # the two component, men_better_biz_exec, men_better_poli_leaders, seems to explain a lot


## Other variables ----------------------------------
# Distribution of Demographic Factors 
age <- eda %>% 
  filter(age > 0) %>% 
  ggplot(aes(x = age)) + 
  geom_histogram() + 
  ggtitle("Age") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  )

sex <- eda %>% 
  filter(sex > 0) %>% 
  group_by(sex) %>% 
  count(sex) %>% 
  ggplot(aes(x = as.factor(sex), y = n, fill = as.factor(sex))) + 
  geom_col() + 
  scale_x_discrete(
    name = "sex", 
    labels = c("Male", "Female")
  ) + 
  scale_fill_manual(
    name = NULL, 
    label = c("Male", "Female"), 
    values = c("2" = "#f5719d", "1" = "#7371f5")
  ) + 
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  ) + 
  ggtitle("Sex")

education <- eda %>% 
  filter(education > 0) %>% 
  group_by(education) %>% 
  count(education) %>% 
  ggplot(aes(x = as.factor(education), y = n)) + 
  geom_col() + 
  scale_x_discrete(
    labels = c("less than primary", "primary", "less than secondary (vocational)", 
               "secondary (vocational)", "less than secondary (academic)", "secondary (academic)", 
               "some university", "university and beyond")
  ) + 
  labs(
    x = NULL, 
    title = "Education"
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.text.x = element_text(angle = 30, vjust = 1)
  )

ses <- eda %>% 
  filter(ses > 0) %>% 
  group_by(ses) %>% 
  count(ses) %>% 
  ggplot(aes(x = as.factor(ses), y = n)) + 
  geom_col() + 
  scale_x_discrete(
    labels = c("Upper", "Upper middle", "Lower middle", "Working", "Lower")
  ) + 
  labs(
    x = NULL, 
    title = "Social Class (subjective)"
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
    axis.text.x = element_text(angle = 30, vjust = 1)
  )

cowplot::plot_grid(age, sex, education, ses, nrow = 2)

eda %>% 
  filter(age > 0) %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(stat = "density", binwidth = 1) + 
  facet_wrap(~continent) + 
  ggtitle("Age") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  )


#### Distribution of Sexism (color by) Country, by Gender, by Educational Level, etc. 

# Overall Distribution of Sexism by Region 
eda %>% 
  filter(!is.na(sexism)) %>% 
  ggplot(aes(x = fct_reorder(sub_region, sexism, .desc = TRUE), y = sexism, fill = sub_region)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(
    x = "Region", 
    y = "Sexism", 
    title = "Sexist Attitudes by Region", 
    subtitle = "Higher values for sexism indicate higher sexism."
  )


## (1) Distribution of sexism by gender 
sexism_by_gender <- eda %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(sexism)) %>% 
  ggplot(aes(x = as.factor(sex), y = sexism, fill = as.factor(sex))) + 
  geom_boxplot() +   
  scale_x_discrete(
    name = "sex", 
    labels = c("Male", "Female")
  ) + 
  scale_fill_manual(
    name = NULL, 
    label = c("Male", "Female"), 
    values = c("2" = "#f5719d", "1" = "#7371f5")
  )  
  
  
# as expected, men are more sexist than women 

sexism_by_gender_and_region <- eda %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(sexism)) %>% 
  ggplot(aes(x = as.factor(sex), y = sexism, fill = as.factor(sex))) + 
  geom_boxplot() + 
  scale_x_discrete(
    name = "sex", 
    labels = c("Male", "Female")
  ) + 
  scale_fill_manual(
    name = NULL, 
    label = c("Male", "Female"), 
    values = c("2" = "#f5719d", "1" = "#7371f5")
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  facet_wrap(~ continent) # pretty much all throughout different continents 

cowplot::plot_grid(sexism_by_gender, sexism_by_gender_and_region, nrow = 1)

## (2) distribution of sexism by educational level 
sexism_by_education <- eda %>% 
  filter(!is.na(education)) %>% 
  filter(!is.na(sexism)) %>% 
  ggplot(aes(x = as.factor(education), y = sexism, fill = as.factor(education))) + 
  geom_boxplot() +
  scale_x_discrete(
    labels = c("less than primary", "primary", "less than secondary (vocational)", 
               "secondary (vocational)", "less than secondary (academic)", "secondary (academic)", 
               "some university", "university and beyond")
  ) + 
  scale_fill_discrete(
    name = NULL, 
    labels = c("less than primary", "primary", "less than secondary (vocational)", 
               "secondary (vocational)", "less than secondary (academic)", "secondary (academic)", 
               "some university", "university and beyond")
  ) + 
  theme(
    axis.text.x = element_text(angle = 45),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) + 
  labs(
    x = NULL, 
    title = "Sexism by Education Level"
  )

sexism_by_education_region <- eda %>% 
  filter(!is.na(education)) %>% 
  filter(!is.na(sexism)) %>% 
  ggplot(aes(x = as.factor(education), y = sexism, fill = as.factor(education))) + 
  geom_boxplot() + 
  facet_wrap(~ continent) +
  scale_fill_discrete(
    name = NULL, 
    labels = c("less than primary", "primary", "less than secondary (vocational)", 
               "secondary (vocational)", "less than secondary (academic)", "secondary (academic)", 
               "some university", "university and beyond")
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) + 
  labs(
    x = NULL, 
    title = "Sexism by Education Level and by Region"
  )


sexism_by_education
sexism_by_education_region

## (3) Distribution of sexism by age (age group: in their 20's, 30's, 40's, 50's, 60's, 70's, 80's and more)
eda <- eda %>% 
  mutate(agegroup = ifelse(age < 25, "age1", 
                           ifelse(age < 35 & age >24, "age2", 
                                  ifelse(age < 45 & age > 34, "age3", 
                                         ifelse(age < 55 & age > 44, "age4", 
                                                ifelse(age < 65 & age > 54, "age5", "age6")))))) 

eda %>% 
  filter(!is.na(agegroup)) %>%
  filter(!is.na(sexism)) %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = agegroup, y = sexism, fill = agegroup)) + 
  geom_boxplot() + 
  scale_fill_discrete(
    name = "Age Group", 
    labels = c("15-24", "25-34", "35-44", 
               "45-54", "55-64", "65 and more")
  ) + 
  scale_x_discrete(
    name = NULL, 
    labels = c("15-24", "25-34", "35-44", 
               "45-54", "55-64", "65 and more")
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) + 
  ggtitle("Distribution of Sexism by Age Group")


eda %>% 
  filter(!is.na(agegroup)) %>%
  filter(!is.na(sexism)) %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = agegroup, y = sexism, fill = agegroup)) + 
  geom_boxplot() + 
  scale_fill_discrete(
    name = NULL, 
    labels = c("15-24", "25-34", "35-44", 
               "45-54", "55-64", "65 and more")
  ) + 
  scale_x_discrete(
    name = NULL, 
    labels = NULL
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    axis.ticks.x = element_blank()
  ) + 
  facet_wrap(~continent) + 
  ggtitle("Distribution of Sexism by Age Group and Region")


## (4) distribution of sexism by region 
eda %>% 
  filter(!is.na(sexism)) %>% 
  ggplot(aes(x = continent, y = sexism, fill = continent)) + 
  geom_boxplot()

## (5) distribution of sexism by religiosity 
eda %>% 
  filter(!is.na(religion_important)) %>% 
  filter(!is.na(sexism)) %>% 
  filter(!is.na(sex)) %>%
  mutate(gender = ifelse(sex == 1, "male", "female")) %>% 
  ggplot(aes(x = as.factor(religion_important), y = sexism, fill = as.factor(religion_important))) + 
  geom_boxplot() +
  facet_grid(gender ~ sub_region) + 
  scale_fill_discrete(
    name = "Importance of religion in life", 
    labels = c("Very important", "Rather important", "Not very important", "Not at all important")
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  labs(
    x = "Importance of Religion", 
    y = "sexism", 
    title = "Distribution of Sexism by Religiosity"
  )
  


## (6) distribution of sexism by political right-left 
eda %>% 
  filter(!is.na(politicalrightleft)) %>% 
  filter(!is.na(sexism)) %>%
  ggplot(aes(x = as.factor(politicalrightleft), y = sexism, 
             fill = as.factor(politicalrightleft))) + 
  geom_boxplot() + 
  scale_fill_discrete(
    name = "political left-right", 
    labels = c("Left", "2", "3", "4", "5", "6", "7", "8", "9", "right")
  ) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) + 
  labs(
    x = "Political Left-Right", 
    y = "sexism", 
    title = "Distribution of Sexism by Self-Positioning on Political Left-Right Scale"
  )

eda %>% 
  filter(!is.na(politicalrightleft)) %>% 
  filter(!is.na(sexism)) %>%
  ggplot(aes(x = as.factor(politicalrightleft), y = sexism, 
             fill = as.factor(politicalrightleft))) + 
  geom_boxplot() + 
  facet_wrap(~continent) + 
  scale_fill_discrete(
    name = "political left-right", 
    labels = c("Left", "2", "3", "4", "5", "6", "7", "8", "9", "right")
  ) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) + 
  labs(
    x = "Political Left-Right", 
    y = "sexism", 
    title = "Distribution of Sexism by Self-Positioning on Political Left-Right Scale\n(by Region)"
  )



eda %>% 
  filter(!is.na(politicalrightleft)) %>% 
  filter(!is.na(sexism)) %>% 
  ggplot(aes(x = as.factor(politicalrightleft), y = sexism, fill = as.factor(politicalrightleft))) + 
  geom_boxplot() + # 1: left, 10: right 
  facet_wrap(~sub_region)






### Heatmaps (relationship between variables): sexism, religiosity, political right-left, education, age, etc. 
## (those factors mentioned above)
## Probably generate separate heatmaps for different regions, so that we can have separate heatmaps 
## (this will enable us to see whether some correlation is more strong in some regions)

# attitudes toward tradition: divorcejustifiable, traditionimportant, parentsproudlifegoal, adventurerisk, behaveproperlyimpt, abortionjustifiable, housewifefulfilling
eda %>% 
  select(divorcejustifiable, traditionimportant, parentsproudlifegoal, adventurerisk, behaveproperlyimpt, abortionjustifiable, 
         housewifefulfilling, respectforauthority, respectforelder, obedienceimportant, boss30yrold, 
         just_sexbeforemarriage, just_homosexuality, neighbor_unmarriedcouple, neighbor_homosexual, 
         religion_important, religiousauthoritylaw, godimportant, believeinhell) %>%
  na.omit() %>%
  cor() %>% 
  corrplot(method = "color", order = "AOE")

# authoritarian personality: respectforauthority, respectforelder, obedienceimportant, boss30yrold
# social conservatism: just_sexbeforemarriage, just_homosexuality, neighbor_unmarriedcouple, neighbor_homosexual
# religiosity: religion_important, religiousauthoritylaw, godimportant, believeinhell



