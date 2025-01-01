
# Number of Customers with Positive and Negative Lifetime Value -----------

library(tidyverse)
library(dplyr)
library(ggplot2)
claims_q1<- claims_df%>%
  group_by(customer_state)%>%
  filter(customer_lifetime_value<=0)%>%
  summarize(customers_with_negetive_value=n())

library(tidyverse)
library(dplyr)
library(ggplot2)
claims_q11<- claims_df%>%
  group_by(customer_state)%>%
  filter(customer_lifetime_value>=0)%>%
  summarize(customers_with_positive_value=n())

claims_q1f=merge(x= claims_q1, y= claims_q11, by = "customer_state", all = TRUE )
claims_q1final<-claims_q1f %>%
  mutate(percent_of_negative_customers=((customers_with_negetive_value)/(customers_with_negetive_value+customers_with_positive_value))*100)%>%
  arrange(desc(percent_of_negative_customers))
claims_q1final


# Customer Lifetime Value v/s Number of Claims ----------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)

claims_q22<- claims_df%>%
  group_by(customer_state, total_claims)%>%
  summarize(mean_customer_lifetime_value=mean(customer_lifetime_value), median_customer_lifetime_value=median(customer_lifetime_value),
            mean_salary= mean(income))%>%
  arrange(desc(mean_customer_lifetime_value))
claims_q22


# Policy Sales Channel By Value -------------------------------------------

claims_total<- claims_df%>%
  group_by(sales_channel)%>%
  summarise(total_revenue=sum(customer_lifetime_value))%>%
  arrange(desc(total_revenue))
claims_total
##
claims_success<- claims_df%>%
  group_by(sales_channel, coverage, policy)%>%
  summarize(total_value=(sum(customer_lifetime_value))/1000)%>%
  arrange(desc(total_value))
claims_success
ggplot(claims_success, aes(x= coverage, y= policy, fill=total_value))+
  geom_tile(lwd=1.5, linetype=1)+
  facet_wrap(~sales_channel)+
  labs(title = "Value Generated From Various Channels",
       fill="Total Value")+
  geom_text(aes(label=total_value), color="white", size=4)+
  scale_fill_gradient(low="orange", high="blue")


# Monthly Premiums v/s Total Claims by Vehicle Class ----------------------

claims_q4<- claims_df %>%
  group_by(total_claims, vehicle_class)%>%
  summarize(total_claims_amount, monthly_premium)
claims_q4
options(repr.plot.width =11, repr.plot.height =8)
ggplot(claims_q4, aes(x=monthly_premium, y=total_claims_amount, color=vehicle_class))+
  geom_point(alpha=0.5)+
  labs(title = "Claim Amount and Premium Price On Number of Claim by Vehicle Class",
       color="Vehicle Class")+
  facet_wrap(~total_claims, nrow=2, ncol=2)



# Total Lifetime Premium Paid v/s Total Claim by number of Claims  --------

claims_q4f<- claims_df %>%
  group_by(total_claims, vehicle_class)%>%
  mutate(total_paid= (monthly_premium*months_policy_active))%>%
  summarize(total_claims_amount, total_paid)
claims_q4f

ggplot(claims_q4f, aes(x=total_paid, y=total_claims_amount, color=vehicle_class))+
  geom_point()+
  facet_wrap(~total_claims, nrow=2, ncol=2)

claims_q4f2<- claims_q4f%>%
  group_by(vehicle_class)%>%
  summarise(number_of_units= n())
claims_q4f2


# Policy Coverage Demand by Customer Demographics -------------------------

claims_q6<- claims_df%>%
  group_by(coverage, marital_status, gender)%>%
  summarise(number_of_people=n())%>%
  arrange(desc(number_of_people))
claims_q6
ggplot(claims_q6, aes(x= coverage, y= number_of_people, fill=gender))+
  geom_col(position="dodge")+
  facet_wrap(~marital_status, nrow=2, ncol=2)
#swapping for profitable customers
claims_q62<- claims_df%>%
  filter(customer_lifetime_value>0)%>%
  group_by(coverage, marital_status, gender)%>%
  summarise(customer_lifetime_value)%>%
  arrange(desc(customer_lifetime_value))
claims_q62
ggplot(claims_q62, aes(x= coverage, y= customer_lifetime_value, fill=gender))+
  geom_col(position="dodge")+
  facet_wrap(~marital_status, nrow=2, ncol=2)



# Policy Profitability and Customer Lifetime Value by Customer Geo --------

claims_q7<- claims_df%>%
  filter(coverage=="Premium")%>%
  group_by(gender, marital_status)%>%
  summarise(customer_state, residence_type, customer_lifetime_value)%>%
  arrange(desc(customer_lifetime_value))
claims_q7
ggplot(claims_q7, aes(x= customer_state, y= customer_lifetime_value, fill= residence_type))+
  geom_boxplot()+
  labs(title = "Customer Lifetime Value Based On Locations",
       fill="Residence Type",
       y= "State",
       x="Customer Lifetime Value")+
  facet_grid(marital_status~gender)


# Customer Employment Status and Claim Amounts & Employment Status --------

claims_q8<- claims_df %>%
  filter(months_since_last_claim==0)%>%
  group_by(employment_status)%>%
  summarise(number_of_customers=n(),
            max_current_claim=max(current_claim_amount),
            median_current_claim=median(current_claim_amount),
            mean_current_claim=mean(current_claim_amount),
            min_current_claim=min(current_claim_amount))
claims_q8

