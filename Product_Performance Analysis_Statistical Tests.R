# Data Preprocessing ------------------------------------------------------

library(tidyverse)
library(lubridate)

df<- read_csv(file="C:/Users/SpaceWalker/Desktop/Exercise_R.csv")

sapply(df, class)

names(df)<- tolower(names(df))

names(df)<- gsub(" ", "_", names(df))

df$service_start_date<- as.Date(df$service_start_date, "%d/%m/%Y")
class(df$service_start_date)

df$service_completion_date<- as.Date(df$service_completion_date, "%d/%m/%Y")
class(df$service_completion_date)

names(df)<- gsub("/", "_", names(df))

df$drs_change_improve<- gsub("%", "", as.character(df$drs_change_improve))
df$drs_change_improve<- as.numeric(df$drs_change_improve)

df$income<- gsub("\\$", "", as.character(df$income))
df$income<- gsub(",", "", as.character(df$income))
df$income<- as.numeric(df$income)

df$resident_cost_share<- gsub("%", "", as.character(df$resident_cost_share))
df$resident_cost_share<- as.numeric(df$resident_cost_share)

df$city_cost_share<- gsub("%", "", as.character(df$city_cost_share))
df$city_cost_share<- as.numeric(df$city_cost_share)

df$cost_of_installation<- gsub("\\$", "", as.character(df$cost_of_installation))
df$cost_of_installation<- as.numeric(df$cost_of_installation)

df$city_installation_cost<- gsub("\\$", "", as.character(df$city_installation_cost))
df$city_installation_cost<- as.numeric(df$city_installation_cost)

df$waste_management_cost<- gsub("\\$", "", as.character(df$waste_management_cost))
df$waste_management_cost<- as.numeric(df$waste_management_cost)

df$city_total_cost<- gsub("\\$", "", as.character(df$city_total_cost))
df$city_total_cost<- as.numeric(df$city_total_cost)

df$drs_change_per_dollar<- gsub("%", "", as.character(df$drs_change_per_dollar))
df$drs_change_per_dollar<- as.numeric(df$drs_change_per_dollar)


sapply(df, class)

write.csv(file="Exercise_DA_R.csv", df)

# Statistical Tests -------------------------------------------------------

library(dplyr)
library(tidyverse)
library(GGally)
library(ggplot2)

df<- read_csv(file="C:/Users/SpaceWalker/Desktop/Exercise_DA_R.csv")

df<- df[, -1]
table(is.na(df))
df2<- df%>%
  mutate(DRS_change_absolute=(drs_at_start)-(drs_at_completion))

df2$drs_change_improve<- (df2$drs_change_improve/100)
names(df2)

cor_data<- df2%>%
  select(service, duration, drs_at_start, drs_at_start, drs_change_improve, 
         DRS_change_absolute, `waste_produced_by_service_(lbs)`, 
         satisfaction_rating, people_in_household, 
         city_cost_share, resident_cost_share, income)


#ggpairs(cor_data, columns = 2:11, ggplot2::aes(colour=service))

cor_mat<- cor(cor_data[, 2:11])
#plot(cor_mat)
#install.packages("psych")
library(psych)
cor_test_p<- corr.test(cor_mat)$p

#ggpairs(df)
library(ggpubr)

###### Significance of Using Products: PP and FC


# First: DRS after complete

prd_drs<- df2%>%
  select(service, drs_at_completion)%>%
  group_by(drs_at_completion)

prd_drs%>%group_by(service) %>%
  summarise(
    count = n(),
    median = median(drs_at_completion, na.rm = TRUE),
    IQR = IQR(drs_at_completion, na.rm = TRUE))


ggboxplot(prd_drs, x= "service", y= "drs_at_completion", 
          fill= "service", palette = c("salmon", "steelblue"),
          ylab="DRS at End", title = "DRS Values After Using Different Services")

#Wilcox-Test:

prd_drs_hyp_test_1<- wilcox.test(drs_at_completion~service,
                                 data= prd_drs, exact= FALSE)
prd_drs_hyp_test_1

cat("We reject the Null Hypothesis (H0) since the p-value: ", prd_drs_hyp_test_1$p.value, " is <0.05 significance level, we chose the alternative that
    suggests, there's a significant difference in the distribution of DRS after
    using these service.")

# Normality test on DRS after completion for 
#Pest Prevention
prd_drs_pp<- prd_drs%>%
  filter(service=="Pest Prevention")

shapiro.test(prd_drs_pp$drs_at_completion)

plot(density(prd_drs_pp$drs_at_completion))

#Fire Control

prd_drs_fc<- prd_drs%>%
  filter(service=="Fire Control")

shapiro.test(prd_drs_fc$drs_at_completion)

plot(density(prd_drs_fc$drs_at_completion))

# combined density plot for DRS values
ggplot(prd_drs, aes(x = drs_at_completion, fill = service)) +
  geom_density(alpha = 0.5)+
  scale_fill_manual(values=c("Fire Control"= "salmon", "Pest Prevention"="steelblue"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="DRS After Using Services, DRS at Completion", 
       y="Frequency", title="Density Distribution of DRS After Using Services")

# Parametric T-test

prd_drs_para_t<- t.test(drs_at_completion~service, data= prd_drs)


#######################################################################_____

library(moments)

drs_hyp<-df2%>%
  select(income_bracket, drs_at_start, drs_at_completion)%>%
  group_by(income_bracket)
  
shapiro.test(drs_hyp$drs_at_start)
hist(drs_hyp$drs_at_start)
plot(density(drs_hyp$drs_at_start))
skewness(drs_hyp$drs_at_start)

shapiro.test(drs_hyp$drs_at_completion)
hist(drs_hyp$drs_at_completion)
skewness(drs_hyp$drs_at_completion)

plot(density(drs_hyp$drs_at_start))
plot(density(drs_hyp$drs_at_completion))

library(tidyr)
library(dplyr)
library(broom)
drs_hyp_long <- drs_hyp %>%
  pivot_longer(cols = c(drs_at_start, drs_at_completion), 
               names_to = "variable", values_to = "value")

ggboxplot(drs_hyp_long, x="income_bracket", y= "value",
          fill = "variable", 
          palette=c("salmon", "steelblue"), title = "DRS Values at Start and Completion \n by Income Brackets",
          xlab= "Income Brackets", ylab="DRS Values")+
  labs(fill = "DRS Stage") +  # Change the legend title to "Stage"
  scale_fill_manual(values = c("salmon", "steelblue"),
                    labels = c("DRS at Completion", "DRS at Start"))  # Rename the legend values

drs_hyp_norm_test<- drs_hyp%>%
  group_by(income_bracket)%>%
  summarize(
    normality_before= list(shapiro.test(drs_at_start)),
    normality_after= list(shapiro.test(drs_at_completion))
  )%>%
  mutate(
    normality_before= map(normality_before, tidy),
    normality_after=map(normality_after, tidy)
  )%>%
  unnest(cols=c(normality_before, normality_after), names_sep = "_")

names(drs_hyp_norm_test)

drs_hyp_norm_test_s<- drs_hyp_norm_test%>%
  select(income_bracket, normality_before_p.value, normality_after_p.value)%>%
  rename(DRS_before_p.value=normality_before_p.value,
         DRS_after_p.value=normality_after_p.value)


drs_hyp%>%group_by(income_bracket) %>%
  summarise(
    count = n(),
    median_DRS_start= median(drs_at_start, na.rm= TRUE),
    median_DRS_completion = median(drs_at_completion, na.rm = TRUE),
    IQR_DRS_start= IQR(drs_at_start, na.rm= TRUE),
    IQR_DRS_completion = IQR(drs_at_completion, na.rm = TRUE))

library(ggplot2)

ggplot(drs_hyp_long, aes(x= value, fill= variable))+
  geom_density(alpha=0.45)+
  facet_wrap(~income_bracket)+
  scale_fill_manual(values=c("salmon", "steelblue"), labels=c("DRS at Completion",
                                                              "DRS at Start"))+
                      labs(x = "DRS Value", y = "Density", 
                           fill = "DRS", 
                           title = "Density Plot of DRS Values Before and After by Income Bracket")+
  theme_minimal()

drs_hyp_stats<- drs_hyp_long%>%
  group_by(income_bracket, variable)%>%
  summarize(skewness= skewness(value),
            kurtosis= kurtosis(value),
            .groups= 'drop')
drs_hyp_stats

drs_hyp_above_stats<- drs_hyp%>%
  filter(income_bracket=="Above")

skewness(drs_hyp_above_stats$drs_at_start)
skewness(drs_hyp_above_stats$drs_at_completion)


drs_hyp_above_np<- drs_hyp%>%
  filter(income_bracket=="Above")

above_hyp_np<- wilcox.test(drs_hyp_above_np$drs_at_start, 
                           drs_hyp_above_np$drs_at_completion, paired=TRUE)

above_hyp_p<- t.test(drs_hyp_above_np$drs_at_start, drs_hyp_above_np$drs_at_completion, 
                     paired = TRUE)



drs_hyp_mid_np<- drs_hyp%>%
  filter(income_bracket=="Middle")

mid_hyp_np<- wilcox.test(drs_hyp_mid_np$drs_at_start, drs_hyp_mid_np$drs_at_completion,
                         paired=TRUE)

drs_hyp_under_np<- drs_hyp%>%
  filter(income_bracket=="Under")

under_hyp_np<- wilcox.test(drs_hyp_under_np$drs_at_start, drs_hyp_under_np$drs_at_completion, paired=TRUE)

################## CHI SQUARE TEST
#Service X Income_bracket

chi_sq_drs<- df2%>%
  select(service, income_bracket)

chi_sq_drs<- table(chi_sq_drs)

chisq.test(chi_sq_drs)

#Service X Satisfaction rating

chi_sq_drs2<- df2%>%
  select(service, satisfaction_rating)

chi_sq_drs2<- table(chi_sq_drs2)

chisq.test(chi_sq_drs2)

#Income_bracket X Satisfaction ratings

chi_sq_drs3<- df2%>%
  select(income_bracket, satisfaction_rating)

chi_sq_drs3<- table(chi_sq_drs3)

chisq.test(chi_sq_drs3)

#Service X Request_channel

chi_sq_drs4<- df2%>%
  select(service, `how_was_service_requested?`)

chi_sq_drs4<- table(chi_sq_drs4)

chisq.test(chi_sq_drs4)

################### Big density plot, Service X Income Bracket X DRS

drs_hyp_long <- drs_hyp %>%
  pivot_longer(cols = c(drs_at_start, drs_at_completion), 
               names_to = "variable", values_to = "value")
########
drs_dens<- df2%>%
  select(service, income_bracket, drs_at_start, drs_at_completion)

drs_dens2<- drs_dens%>%
  pivot_longer(cols=c(drs_at_start, drs_at_completion),
               names_to = "variable", values_to = "value")

drs_dens2_pp<- drs_dens2%>%
  filter(service=="Pest Prevention")

drs_dens2_pp$variable<- factor(drs_dens2_pp$variable, levels = c("drs_at_start", "drs_at_completion"))
labels <- c(drs_at_start = "Before Treatment", drs_at_completion = "After Treatment")
ggplot(drs_dens2_pp, aes(x=value, fill=income_bracket))+
  geom_density(alpha=0.45)+
  facet_grid(~variable, labeller = labeller(variable = labels))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="DRS Values", y="Density", fill= "Income Brackets", title = "DRS Values Before and After Pest Prevention\nby Income Brackets")+
  theme_minimal()

drs_dens2_pp_sk<- drs_dens2_pp[, -1]

drs_dens2_pp_sk2<-drs_dens2_pp_sk%>%
  group_by(income_bracket, variable)%>%
  summarize(skewness= skewness(value),
            kurtosis=kurtosis(value),
            .groups = 'drop')
drs_dens2_pp_sk2
## For Fire Control

drs_dens2_fc<- drs_dens2%>%
  filter(service=="Fire Control")

drs_dens2_fc_2<- drs_dens2_fc[, -1]



drs_dens2_fc$variable<- factor(drs_dens2_fc$variable, levels = c("drs_at_start", "drs_at_completion"))
labels <- c(drs_at_start = "Before Treatment", drs_at_completion = "After Treatment")


ggplot(drs_dens2_fc, aes(x=value, fill=income_bracket))+
  geom_density(alpha=0.45)+
  facet_grid(~variable, labeller = labeller(variable = labels))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="DRS Values", y="Density", fill= "Income Brackets", title = "DRS Values Before and After Fire Control\nby Income Brackets")+
  theme_minimal()

drs_dens2_fc_sk<-drs_dens2_fc_2%>%
  group_by(income_bracket, variable)%>%
  summarize(skewness= skewness(value),
            kurtosis= kurtosis(value),
            .groups = 'drop')

### Ratings Cluster


ggboxplot(df2, x= "income_bracket", y= "satisfaction_rating", 
          fill= "service", palette = c("salmon", "steelblue"),
          ylab="Satisfaction Ratings", xlab="",title = "Service Satisfaction Ratings by Income Brackets")



mean_data <- df2 %>%
  group_by(service) %>%
  summarise(mean_rating = mean(satisfaction_rating, na.rm = TRUE))

ggplot(df2, aes(x= satisfaction_rating, fill= income_bracket))+
  geom_density(alpha=0.40)+
  facet_wrap(~service)+
  labs(x = "Satisfaction Ratings", y = "Density", 
       fill = "Income Brackets", 
       title = "Density Plot of Service Satisfaction By Income Brackets")+
  theme_minimal()+
  geom_vline(
    data = mean_data,
    aes(xintercept = mean_rating, color = "Average Rating"),
    linetype = "dashed",
    size = 0.50
  ) +
  scale_color_manual(name = "Line Type", values = c("Average Rating" = "steelblue"))



ratings_measure<- df2%>%
  select(service, income_bracket, satisfaction_rating)%>%
  group_by(service, income_bracket)%>%
  summarize(skewness=skewness(satisfaction_rating),
            kurtosis=kurtosis(satisfaction_rating))
ratings_measure


ratings_normality_test<- df2%>%
  select(service, income_bracket, satisfaction_rating)%>%
  do(tidy(shapiro.test(.$satisfaction_rating)))


#Ratings Distribution

ggplot(df2, aes(x= satisfaction_rating, fill= service))+
  geom_density(alpha=0.40)+
  labs(x = "Satisfaction Ratings", y = "Density", 
       fill = "Services", 
       title = "Density Plot of Service Satisfaction")+
  theme_minimal()


only_ratings_measures<- df2%>%
  select(service, satisfaction_rating)%>%
  group_by(service)%>%
  summarize(skewness=skewness(satisfaction_rating),
            kurtosis=kurtosis(satisfaction_rating))
only_ratings_measures


ggboxplot(df2, x= "people_in_household", y= "duration", 
          fill= "income_bracket")+labs(y="Duration", 
          x="People in Household", title = "Services duration people", 
          fill="Income Bracket")+
  facet_grid(~service)