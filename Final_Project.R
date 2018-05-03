#------------------------------------------------------------
#                           Misc
#------------------------------------------------------------

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

#------------------------------------------------------------
#                           Build Dataset
#------------------------------------------------------------

#public housing authority data from 2014
pha <- read.csv("PHA_2014.csv"); head(pha) 
pha[pha == -1] <- NA
pha[pha == -4] <- NA
pha[pha == -5] <- NA

# extract subset of only "Housing Choice Voucher" programs (program = 3)
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
#hist(hcv$rent_burden, breaks = "FD", xlim = c(0,2)) 
#liz: possible graphic?

# dataset of states by census regions
data(states)
# keep only "State", "Region", and "Division" columns
states_fin <- states[, c(1, 3, 4)]
# add census regions to HCV data frame
hcv <- left_join(hcv, states_fin, by = "state")

# load PHA executive salary data
salaries <- read.csv("2014EXEC_COMP.csv"); head(salaries)

# remove $ and resulting comma (class of salary data is a problem)
strip_dol <- function(x) as.numeric((gsub("\\,", "", gsub("\\$", "", x))))
colnames(salaries)
salaries[,4:10] <- sapply(salaries[,4:10], strip_dol)

# generate dataset of max salary for each PHA
salaries_max <- salaries %>% group_by(PHA.Code) %>% top_n(1, Total.Compensation) %>% 
  distinct(salaries, PHA.Code, Total.Compensation, .keep_all = TRUE)
salaries_max <- rename(salaries_max, code = PHA.Code)

# recode missing compensation data as NA rather than 0 and rename
salaries_max$Total.Compensation[salaries_max$Total.Compensation == 0] <- NA
salaries_max <- rename(salaries_max, largest_compensation = Total.Compensation)
#mean(salaries_max$Total.Compensation,na.rm = TRUE); min(salaries_max$Total.Compensation,na.rm = TRUE); max(salaries_max$Total.Compensation,na.rm = TRUE)

#dummy variable for "receieved bonus"
salaries_max <- mutate(salaries_max, receive_bonus  = Bonus > 0)
salaries_max$receive_bonus[is.na(salaries_max$largest_compensation)] <- NA
#table(salaries_max$receive_bonus)


# add max salary data to HCV data frame
hcv <- left_join(hcv, salaries_max, by = "code")
#plot(hcv$hh_income, hcv$largest_compensation)
#liz: possible graphic?

# salaries by tenant income
ggplot(hcv) + geom_point(aes(x=hh_income, y=largest_compensation)) + 
  scale_x_continuous(name="Income of Tenants") +
  scale_y_continuous(name="Salary of Highest PHA Employee")
# try to remove some of the higher salaries, to see if there is a trend?
ggplot(hcv) + geom_point(aes(x=hh_income, y=largest_compensation)) + 
  scale_x_continuous(name="Income of Tenants", limits=c(0, 30000)) +
  scale_y_continuous(name="Salary of Highest PHA Employee", limits=c(0, 200000))
# looks more linear, as we expected
# segmented out by census region
ggplot(hcv) + geom_point(aes(x=hh_income, y=largest_compensation)) + facet_wrap(~ region) +
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
ggplot(hcv) + geom_point(aes(x=rent_burden, y=largest_compensation))


hist(hcv$largest_compensation, breaks = "FD", freq = FALSE)
