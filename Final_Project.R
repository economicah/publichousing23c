#------------------------------------------------------------
#                           Misc
#------------------------------------------------------------
#!diagnostics off
rm(list = ls()) #wipes out your environment
setwd("~/Documents/Harvard Extension School/Math E-23C/Data/")

#install.packages("reshape2")
library(reshape2)
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


#-------------------------------------------------------------
# *******************CLEAN & BUILD DATASET********************
#-------------------------------------------------------------

#------------------------------------------------------------
#          Read in Data                 
#------------------------------------------------------------

#public housing authority data from 2014
pha <- read.csv("PHA_2014.csv"); head(pha) 
pha[pha == -1] <- NA
pha[pha == -4] <- NA
pha[pha == -5] <- NA

# extract subset of only "Housing Choice Voucher" programs (program = 3)
hcv <- filter(pha, pha$program == 3)


# dataset of states by census regions
data(states)
# keep only "State", "Region", and "Division" columns
states_fin <- states[, c(1, 3, 4)]

# add census regions to HCV data frame
hcv <- left_join(hcv, states_fin, by = "state")
hcv$region <- as.character(hcv$region)
hcv$region[is.na(hcv$region)] <- "Island"
hcv$region <- as.factor(hcv$region)
table(hcv$region)

# load PHA executive salary data
salaries <- read.csv("2014EXEC_COMP.csv"); head(salaries)

# remove $ and resulting comma (class of salary data is a problem)
strip_dol <- function(x) as.numeric((gsub("\\,", "", gsub("\\$", "", x))))
colnames(salaries)
salaries[,4:10] <- sapply(salaries[,4:10], strip_dol)

# generate dataset of max salary for each PHA
salaries_max <- salaries %>% group_by(PHA.Code) %>% top_n(1, Total.Compensation) %>% 
  distinct(salaries, PHA.Code, Total.Compensation, .keep_all = TRUE)
# recode missing compensation data as NA rather than 0
salaries_max$Total.Compensation[salaries_max$Total.Compensation == 0] <- NA 
#mean(salaries_max$Total.Compensation,na.rm = TRUE); min(salaries_max$Total.Compensation,na.rm = TRUE); max(salaries_max$Total.Compensation,na.rm = TRUE)

#dummy variable for "received bonus"
salaries_max <- mutate(salaries_max, receive_bonus = Bonus > 0)
salaries_max$receive_bonus[is.na(salaries_max$Total.Compensation)] <- NA

# keep only "Total Comp", "PHA Code", and dummy for "received bonus"
salaries_max <- select(salaries_max, Total.Compensation, code = PHA.Code, receive_bonus)

# add max salary data to HCV data frame
hcv <- left_join(hcv, salaries_max, by = "code")
#plot(hcv$hh_income, hcv$Total.Compensation)
#liz: possible graphic?
# from Liz: there's a ggplot of this down below, that I need to add colors to still

#------------------------------------------------------------
#          Create Variables               
#------------------------------------------------------------


# add number of client households as a new variable
hcv <- mutate(hcv, num_hh = total_units * (pct_occupied/100))

# the census bureau designates census tracts with a poverty rate >=20% as "poverty areas"
hcv <- mutate(hcv,poverty_area=tpoverty >=20)
#head(hcv$poverty_area,hcv$tpoverty)

# micah: don't want to remove the below 3 lines, but the mutation is the same as two above 
# add number of client households as a new variable
# hcv <- mutate(hcv, num_hh = total_units * (pct_occupied/100))
#sum(hcv$num_hh, na.rm = TRUE)

# add number of female-headed households as a new variable
hcv <- mutate(hcv, num_fem = total_units * (pct_occupied/100) * (pct_female_head/100))

# add number of male-headed households as a new variable
hcv <- mutate(hcv, num_male = total_units * (pct_occupied/100) * (1-(pct_female_head/100)))

# add total_rent as a new variable
hcv <- mutate(hcv, total_rent  = rent_per_month + spending_per_month)
#mean(hcv$total_rent, na.rm = TRUE);min(hcv$total_rent, na.rm = TRUE);max(hcv$total_rent, na.rm = TRUE)

# add monthly income as a new variable
hcv <- mutate(hcv, income_monthly  = hh_income/12)
#mean(hcv$income_monthly, na.rm = TRUE);min(hcv$income_monthly, na.rm = TRUE);max(hcv$income_monthly, na.rm = TRUE)

# add rent_burden as a new variable (total rent/monthly income)
hcv <- mutate(hcv, rent_burden  = total_rent/income_monthly)
#mean(hcv$rent_burden, na.rm = TRUE);min(hcv$rent_burden, na.rm = TRUE);max(hcv$rent_burden, na.rm = TRUE)

table(hcv$receive_bonus,hcv$region)
#liz they're much more likely to get bonuses in the south looks like
#micah: I did a permutation test below to see if significant!

south_bonus <- hcv %>% filter(region == "South") %>% pull(receive_bonus)
sb_mean <- mean(south_bonus, na.rm = TRUE)

region_bonus <- hcv %>% filter(region != "South") %>% pull(receive_bonus)
rb_mean <- mean(region_bonus, na.rm = TRUE)

obs_diff <- sb_mean - rb_mean

N <- 10^5
diffs <- numeric(N)
for (i in 1:N) { 
  idx <- sample(length(hcv$region), size = length(south_bonus), replace = FALSE)
  diffs[i] <- mean(hcv$receive_bonus[idx], na.rm = TRUE) - mean(hcv$receive_bonus[-idx], na.rm = TRUE) 
}
mean(diffs)
pvalue_bonus <- (sum(diffs >= obs_diff)+1)/(N+1); pvalue_bonus
#micah - can you check what I did here? p-value is significant, but it seems too low to be true...

#-------------------------------------------------------------
# *************************GRAPHICS***************************
#-------------------------------------------------------------

#------------------------------------------------------------
#      BARPLOT (Reqd Graphical Displays #1)
#------------------------------------------------------------
hcv_collapse <- summaryBy(num_male+num_fem~region,data=hcv,FUN=sum,na.rm=TRUE)
hcv_collapse_melt <-melt(hcv_collapse,id.var="region")

#liz: trying to get males on top but not working.
#all set! I just switched their ordering in the hcv_collapse, and then switched ordering in the 
# ggplot below; I also ordered from highest to lowest
#hcv_collapse_melt$variable <- factor(hcv_collapse_melt$variable, levels = c("num_fem.sum"
                                                                         #   ,"num_male.sum"))

ggplot(data = hcv_collapse_melt, aes(x = reorder(region, -value), y = value, 
  fill = factor(variable))) + geom_bar(stat="identity") + theme_bw() + labs(x="Region")  +
  ggtitle("Households Receiving Subsidized Housing by Region") +
  scale_y_continuous(name="Count",labels=scales::comma) +
  theme(legend.position="bottom",legend.direction = "horizontal", legend.title=element_blank()) +
  scale_fill_manual(values=c("steelblue1", "lightpink"),labels=c("Male-Headed", "Female-Headed"))


#------------------------------------------------------------
#      HISTOGRAM (Reqd Graphical Displays #2)
#------------------------------------------------------------
#liz: is this done?
hist(hcv$months_waiting, breaks = "FD",
     ylim=c(0,300), xlim=c(0,150),
     col=rgb(0.2,0.8,0.5,0.5),border=F,
     main="Time Spent Waiting for a Home",
     xlab="Months",ylab="Number of HCV Programs")

#liz:-- do we want to look at these?
#I've made them look nicer, if we do! 
hist(hcv$Total.Compensation, breaks = "FD",
     col=rgb(0.1,0.5,0.8,0.5), main = "Total Compensation of PHA Executives",
     xlab = "Total Compensation", ylab = "Number of Executives")
hist(hcv$rent_burden, breaks = "FD", xlim = c(0.5,2),
     col=rgb(0.8,0.3,0.6,0.5), main = "Rent Burden of PHA Clients",
     xlab = "Rent Burden", ylab = "Number of Clients") 


# salaries by tenant income
ggplot(hcv, aes(x=hh_income, y=Total.Compensation), group=region) + 
  geom_point(aes(shape=region, color=region)) + 
  scale_x_continuous(name="Income of Tenants") +
  scale_y_continuous(name="Salary of Highest PHA Employee") +
  ggtitle("Max PHA employee salary by income of PHA tenants") +
  geom_smooth(method = 'lm')
# looks more linear, as we expected
# segmented out by census region
ggplot(hcv) + geom_point(aes(x=hh_income, y=Total.Compensation)) + facet_wrap(~ region) +
  scale_x_continuous(name="Income of Tenants", limits=c(0, 30000)) +
  scale_y_continuous(name="Salary of Highest PHA Employee", limits=c(0, 200000))


# plotted just to see what it looks like
ggplot(hcv) + geom_point(aes(x=rent_burden, y=Total.Compensation))

#-------------------------------------------------------------
# *************************Analysis***************************
#-------------------------------------------------------------

#------------------------------------------------------------
#      Permutation Test (Reqd Analysis #1)
#------------------------------------------------------------

PoorInd <- which(hcv$poverty_area); head(PoorInd) #indices for poverty_areas

# Total.Compensation
tapply(hcv$Total.Compensation, hcv$poverty_area, mean, na.rm=TRUE)
Total.Compensation <- hcv$Total.Compensation
Obs <- mean(Total.Compensation[PoorInd],na.rm=TRUE)-mean(Total.Compensation[-PoorInd],na.rm=TRUE); Obs  
# difference of $6,064--is this significant?
# Run a permutation test by scrambling the poverty_area vector 
N <-10000; diff <- numeric(N)
for (i in 1:N) {
  scramble <- sample(hcv$poverty_area, length(hcv$poverty_area))
  PoorInd <- which(scramble)
  diff[i] <- mean(Total.Compensation[PoorInd],na.rm=TRUE)-mean(Total.Compensation[-PoorInd],na.rm=TRUE)
}
head(diff)
hist(diff)
abline(v=Obs, col = "red") #far from the center of the distribution
pvalue <- mean(diff > Obs); pvalue #pval of 0. Significant
ptest
