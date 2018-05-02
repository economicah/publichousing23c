
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

# filtering to show only "Housing Choice Voucher" programs (program = 3)
hcv <- filter(pha, pha$program == 3)
# add total_rent as a new variable
hcv <- mutate(hcv, total_rent  = rent_per_month + spending_per_month)
min(hcv$total_rent, na.rm=TRUE)

# add monthly income as a new variable
hcv <- mutate(hcv, income_monthly  = hh_income/12)
# add rent_burden as a new variable (total rent/monthly income)
hcv <- mutate(hcv, rent_burden  = total_rent/income_monthly)
hist(hcv$rent_burden, breaks = "FD", xlim = c(0,2))
max(hcv$rent_burden, na.rm = TRUE) # no longer getting a random max of 24!

# add census regions to data frame
library(noncensus)
data(states)
states_fin <- states[, c(1, 3, 4)]
hcv <- left_join(hcv, states_fin, by = "state")

# load salary data
salaries <- read.csv("Desktop/MathE23c/Term Project/2014EXEC_COMP.csv"); head(salaries)
# keep only max salary for each PHA
salaries_max <- salaries %>% group_by(PHA.Code) %>% top_n(1, Total.Compensation)

# add salary data to HCV data frame
head(hcv$code)
salaries_max <- rename(salaries_max, code = PHA.Code)
hcv <- left_join(hcv, salaries_max, by = "code")
# dollar signs are a problem; need to remove $ and resulting comma
strip_dol <- function(x) as.numeric((gsub("\\,", "", gsub("\\$", "", x))))
colnames(hcv)
hcv[,81:87] <- sapply(hcv[,81:87], strip_dol)
head(hcv)
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
ggplot(hcv) + geom_boxplot(aes(x=division, y=rent_burden), na.rm = TRUE)
ggplot(hcv) + geom_boxplot(aes(x=division, y=tpoverty), na.rm = TRUE)


hist(hcv$Total.Compensation, breaks = "FD", freq = FALSE)