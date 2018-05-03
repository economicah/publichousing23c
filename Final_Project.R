# you can delete this after reading it, but HELLO
# I'm very hot right now. Don't like this heat.

#this is my test change

# how was your ice cream last night?
#it was so good. how was your lyft?

rm(list = ls()) #wipes out your environment
setwd("~/Documents/Harvard Extension School/Math E-23C/Data/")

#install.packages("doBy")
library(doBy)

#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("fiftystater")
library(fiftystater)
#install.packages("maps")
library(maps)
#install.packages("noncensus")
library(noncensus)

pha <- read.csv("PHA_2014.csv"); head(pha) #public housing authority data from 2014
pha[pha == -1] <- NA
pha[pha == -4] <- NA
pha[pha == -5] <- NA

# filtering to keep only "Housing Choice Voucher" programs (program = 3)
hcv <- filter(pha, pha$program == 3)

# add total_rent as a new variable
hcv <- mutate(hcv, total_rent  = rent_per_month + spending_per_month)
#mean(hcv$total_rent, na.rm = TRUE);min(hcv$total_rent, na.rm = TRUE);max(hcv$total_rent, na.rm = TRUE)

# add monthly income as a new variable
hcv <- mutate(hcv, income_monthly  = hh_income/12)
#mean(hcv$income_monthly, na.rm = TRUE);min(hcv$income_monthly, na.rm = TRUE);max(hcv$income_monthly, na.rm = TRUE)

# add rent_burden as a new variable (total rent/monthly income)
hcv <- mutate(hcv, rent_burden  = total_rent/income_monthly)
#mean(hcv$rent_burden, na.rm = TRUE);min(hcv$rent_burden, na.rm = TRUE);max(hcv$rent_burden, na.rm = TRUE)

hist(hcv$rent_burden, breaks = "FD", xlim = c(0,2))
max(hcv$rent_burden, na.rm = TRUE) 

# add census regions to data frame
data(states)
# keep only "State", "Region", and "Division" columns
states_fin <- states[, c(1, 3, 4)]
hcv <- left_join(hcv, states_fin, by = "state")

# load salary data
salaries <- read.csv("2014EXEC_COMP.csv"); head(salaries)
# keep only max salary for each PHA
# class of salary data is a problem though; need to remove $ and resulting comma
strip_dol <- function(x) as.numeric((gsub("\\,", "", gsub("\\$", "", x))))
colnames(salaries)
salaries[,4:10] <- sapply(salaries[,4:10], strip_dol)
salaries_max <- salaries %>% group_by(PHA.Code) %>% top_n(1, Total.Compensation) %>% 
  distinct(salaries, PHA.Code, Total.Compensation, .keep_all = TRUE)

# add salary data to HCV data frame
head(hcv$code)
salaries_max <- rename(salaries_max, code = PHA.Code)
hcv <- left_join(hcv, salaries_max, by = "code")
plot(hcv$hh_income, hcv$Total.Compensation)
# salaries by tenant income
ggplot(hcv) + geom_point(aes(x=hh_income, y=Total.Compensation)) + 
  scale_x_continuous(name="Income of Tenants") +
  scale_y_continuous(name="Salary of Highest PHA Employee")
# try to remove some of the higher salaries, to see if there is a trend?
ggplot(hcv) + geom_point(aes(x=hh_income, y=Total.Compensation)) + 
  scale_x_continuous(name="Income of Tenants", limits=c(0, 30000)) +
  scale_y_continuous(name="Salary of Highest PHA Employee", limits=c(0, 200000))
# looks more linear, as we expected
# segmented out by census region
ggplot(hcv) + geom_point(aes(x=hh_income, y=Total.Compensation)) + facet_wrap(~ region) +
  scale_x_continuous(name="Income of Tenants", limits=c(0, 30000)) +
  scale_y_continuous(name="Salary of Highest PHA Employee", limits=c(0, 200000))
# I THINK the NA are from the US territories that aren't captured in Census regions - do we want 
# to remove those? there actually seems to be a trend, with tenants in the terrorities being
# more rent burdened than those in other regions (did a boxplot elsewhere)
# there are only 81 observations of the territorities, so that would be a caveat of exploring that
# further, but could be interesting!
idx <- which(is.na(hcv$region) == TRUE)
hcv[idx, c(71, 77, 78)]

# also, it looks like tenants in the NE consistently have higher income than MW and S, slightly
# larger than the W


# plotted just to see what it looks like
ggplot(hcv) + geom_point(aes(x=rent_burden, y=Total.Compensation))


hist(hcv$Total.Compensation, breaks = "FD", freq = FALSE)
