#-----------------------------------------------------------
# ****************************MISC**************************
#-----------------------------------------------------------
#!diagnostics off
rm(list = ls()) #wipes out your environment
setwd("~/Documents/Harvard Extension School/Math E-23C/Data/")
setwd("~/Desktop/Term project") #for Liz! (I keep forgetting to do this)

#install.packages("reshape2")
library(reshape2)
#install.packages("doBy")
library(doBy)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("noncensus")
library(noncensus)
#install.packages("ggmap")
library(ggmap)
#install.packages("tidyr")
library(tidyr)
#install.packages("stats4")
library(stats4)
#install.packages(gridExtra)
library(gridExtra)

#-------------------------------------------------------------
# *******************CLEAN & BUILD DATASET********************
#-------------------------------------------------------------

#------------------------------------------------------------
#          Read in Data                 
#------------------------------------------------------------

# load public housing authority data from 2014
pha <- read.csv("PHA_2014.csv"); head(pha) 
pha[pha == -1] <- NA
pha[pha == -4] <- NA
pha[pha == -5] <- NA

# extract subset of only "Housing Choice Voucher" programs (program = 3)
hcv <- filter(pha, pha$program == 3)

# call dataset of states by census regions
data(states)
# keep only "State", "Region", and "Division" columns
states_fin <- states[, c(1, 3, 4)]

# add census regions to HCV data frame
hcv <- left_join(hcv, states_fin, by = "state")
# change region for US territories that were assigned "NA" to "Island"
hcv$region <- as.character(hcv$region)
hcv$region[is.na(hcv$region)] <- "Island"
hcv$region <- as.factor(hcv$region)
table(hcv$region)

# load PHA executive salary data
salaries <- read.csv("2014EXEC_COMP.csv"); head(salaries)

# remove $ and resulting comma (class of salary data is a problem)
# this contributes to "Professional-looking software engineering (#10)"
strip_dol <- function(x) as.numeric((gsub("\\,", "", gsub("\\$", "", x))))
colnames(salaries)
salaries[,4:10] <- sapply(salaries[,4:10], strip_dol)

# generate dataset of max salary for each PHA
# this contributes to "Professional-looking software engineering (#10)"
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

#------------------------------------------------------------
#          Create Variables               
#------------------------------------------------------------



# the census bureau designates census tracts with a poverty rate >=20% as "poverty areas"
hcv <- mutate(hcv, poverty_area = tpoverty >=20)
#head(hcv$poverty_area,hcv$tpoverty)

# add number of client households as a new variable
hcv <- mutate(hcv, num_hh = total_units * (pct_occupied/100))
#sum(hcv$num_hh, na.rm = TRUE)

# add number of female-headed households as a new variable
hcv <- mutate(hcv, num_fem = total_units * (pct_occupied/100) * (pct_female_head/100))

# add number of female-headed (with children) households as a new variable
hcv <- mutate(hcv, num_mother = total_units * (pct_occupied/100) * (pct_female_head_child/100))

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


#-------------------------------------------------------------
# *************************GRAPHICS***************************
#-------------------------------------------------------------

#------------------------------------------------------------
#      BARPLOT (Reqd Graphical Displays #1)
#------------------------------------------------------------

# create data frame with total number of female heads of households and male head of households
# by census region
hcv_collapse <- summaryBy(num_male+num_fem+num_mother~region,data=hcv,FUN=sum,na.rm=TRUE)
hcv_collapse <- mutate(hcv_collapse, num_nonmother  = num_fem.sum-num_mother.sum)
hcv_collapse <- select(hcv_collapse, region,num_male.sum, num_nonmother,num_mother.sum)

# melt that data frame
hcv_collapse_melt <-melt(hcv_collapse,id.var="region")

# create stacked barplot of heads of households by region
ggplot(data = hcv_collapse_melt, aes(x = reorder(region, -value), y = value, 
                                     fill = factor(variable))) + geom_bar(stat="identity") + theme_bw() + labs(x="Region")  +
  ggtitle("Households Receiving Subsidized Housing\n by Region") +
  scale_y_continuous(name="Count",labels=scales::comma) +
  theme(legend.position="bottom",legend.direction = "horizontal", legend.title=element_blank()) +
  scale_fill_manual(values=c("lightsalmon","mediumorchid3", "mediumorchid4"),
                    labels=c("Headed\n by a Man", "Headed by a\n Woman, No Children","Headed by a\n Woman with Children")) +
  theme(plot.title = element_text(hjust = 0.5))


#------------------------------------------------------------
#      HISTOGRAM (Reqd Graphical Displays #2)
#------------------------------------------------------------

# create histogram of Total Compensation
hist(hcv$Total.Compensation/1000, breaks = "FD", 
     col=rgb(0.1,0.5,0.8,0.5), main = "Earnings of the Top-Paid Employee at\n every Public Housing Authority",
     xlab = "Total Compensation (in thousands of dollars)", ylab = "Number of HCV Programs",las=1,xaxt="n",border=F,
     xlim = c(0, 260), ylim=c(0,300))
#axis(side=1, at=axTicks(1), 
#   labels=formatC(axTicks(1), format="d", big.mark=','))
# add axis tick marks
axis(side=1, at=seq(0,260,10))

# create histogram of average rent burden
hist(hcv$rent_burden, breaks = "FD", 
     col=rgb(0.8,0.3,0.6,0.5), xlim = c(0.5, 2.5), ylim=c(0,300),
     main = "Average Rent Burden",
     xlab = "Ratio of Pre-Subsidy Rent to Household Monthly Income", ylab = "Number of HCV Programs",border=F) 


#------------------------------------------------------------
#      PDF over HISTOGRAM (Reqd Graphical Displays #3)
#------------------------------------------------------------
# hist(hcv$months_waiting, breaks = "FD", 
#ylim=c(0,300), xlim=c(0,150),
#col=rgb(0.2,0.8,0.5,0.5),border=F,
#main="Time Spent Waiting for a Home",
#xlab="Months",ylab="Number of HCV Programs")

# create histogram of average months waiting
hist(hcv$months_waiting, breaks = "FD", prob = TRUE,
     ylim = c(0,.04),
     col=rgb(0.2,0.8,0.5,0.5),border=F,
     main="Time Spent Waiting for a Home",
     xlab="Average Months",ylab="Proportion of HCV Programs",xaxt="n")
axis(side=1, at=seq(0,200,10))
# add exponential distribution function
a <- 1/mean(hcv$months_waiting, na.rm = TRUE)
curve(dexp(x, a), add = TRUE)

#It makes sense that an exponential curve fits "number of months waiting" very well. 
#this curve arises when examining the length of time between events in Poisson processes. 


#------------------------------------------------------------
#      Contingency table (Reqd Graphical Displays #4)
#      Analysis of a contingency table (Reqd Analysis #3)
#------------------------------------------------------------
median(hcv$months_waiting, na.rm= TRUE)
max(hcv$months_waiting, na.rm = TRUE)
min(hcv$months_waiting, na.rm = TRUE)
mean(hcv$months_waiting, na.rm = TRUE)

# create bins of average months waiting
attach(hcv)
hcv$mw_bin[hcv$months_waiting < 6] <- "0 to < 6 mo."
hcv$mw_bin[hcv$months_waiting >= 6  & hcv$months_waiting < 12] <- "6 to < 12 mo."
hcv$mw_bin[hcv$months_waiting >= 12  & hcv$months_waiting < 18] <- "12 to < 18 mo."
hcv$mw_bin[hcv$months_waiting >= 18  & hcv$months_waiting < 24] <- "18 to < 24 mo."
hcv$mw_bin[hcv$months_waiting >= 24  & hcv$months_waiting < 30] <- "24 to < 3 mo."
hcv$mw_bin[hcv$months_waiting >= 30  & hcv$months_waiting < 36] <- "30 to < 36 mo."
hcv$mw_bin[hcv$months_waiting >= 36] <- "36+ mo."
detach(hcv)
table(hcv$mw_bin)
# re-order levels so they're chronological categories
hcv$mw_bin <- factor(as.factor(hcv$mw_bin), levels = 
                       c("0 to < 6 mo.", "6 to < 12 mo.", "12 to < 18 mo.",
                         "18 to < 24 mo.", "24 to < 3 mo.", "30 to < 36 mo.",
                         "36+ mo."))
table(hcv$mw_bin)
# create contingency table of months waiting crossed with poverty areas
pov_mo_tbl <- table(hcv$mw_bin, hcv$poverty_area); pov_mo_tbl
# Compare with the table that would be expected if the factors were independent
Expected <- outer(rowSums(pov_mo_tbl), colSums(pov_mo_tbl))/sum(pov_mo_tbl); Expected
# Check if the difference between expected and observed is significant
chisq.test(hcv$mw_bin, hcv$poverty_area)

# There is about a 1% chance that the observed contingency table arose by chance
# Logically this makes sense. Much fewer HCV programs located in poorer
# places (poverty_area = TRUE) experience short wait times (0-6 months)
# than we would expect, but many more experience wait times in every other
# bucket than we would expect if wait time was random. Many more HCV 
# programs than we would expect in non-poor areas experience short wait times, but
# many fewer experience wait times in every other bucket

#-------------------------------------------------------------
# *************************Analysis***************************
#-------------------------------------------------------------

#---------------------------------------------------------------------
#      Permutation Test (Reqd Analysis #1)
#      Comparison of analysis by classical methods (Reqd Analysis #4)
#      Permutation test works better than classical methods (#12)
#---------------------------------------------------------------------

# Permutation test #1-- High poverty areas by executive compensation
PoorInd <- which(hcv$poverty_area); head(PoorInd) #indices for poverty_areas (defined above
# as geographic areas where >20% of the population lives below poverty line)

# Total.Compensation = Compensation of highest paid employee at HCV Program
# display mean Total Comp for poverty areas vs non-poverty area (true and false, respectively)
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
pvalue <- sum((diff >= Obs)-1)
pvalue <- (sum(diff >= Obs)+1)/(N+1); pvalue 

# Unexpected thing #1: We expected that being located in a high-poverty area would be associated
# with LOWER executive pay. It would make sense that if a lot of the people in a town fall under
# the poverty line, which is calculated at a national level, that cost of living in the area
# (rent, food, gas, etc) would be lower for everyone in the town. In addition, job opportunities
# are probably scarcer so wages are lower. We were surprised to find that executives in 
# high poverty areas actually receive statistically significantly higher total compensation.

# comparison with classical methods (req'd analysis #4)
t.test(hcv$Total.Compensation~hcv$poverty_area)
# the classical method (t.test) and the simulation method both result in a significant
# pvalue; however, the permutation test returns a slightly higher p-value than the t.test;
# the permutation test is a better method at demonstrating the significant relationship
# between the two means

# Permutation Test #2-- Are southern executives more likely to receive bonuses?
table(hcv$receive_bonus,hcv$region)
south_bonus <- hcv %>% filter(region == "South") %>% pull(receive_bonus)
sb_mean <- mean(south_bonus, na.rm = TRUE)
region_bonus <- hcv %>% filter(region != "South") %>% pull(receive_bonus)
rb_mean <- mean(region_bonus, na.rm = TRUE)

obs_diff <- sb_mean - rb_mean; obs_diff

N <- 10^5
diffs <- numeric(N)
for (i in 1:N) { 
  idx <- sample(length(hcv$region), size = length(south_bonus), replace = FALSE)
  diffs[i] <- mean(hcv$receive_bonus[idx], na.rm = TRUE) - mean(hcv$receive_bonus[-idx], na.rm = TRUE) 
}
mean(diffs)
hist(diffs, xlim=(c(-.10,.15)))
abline(v=obs_diff, col = "red") 
# the line doesn't even fall on the histogram! significant difference
pvalue_bonus <- (sum(diffs >= obs_diff)+1)/(N+1); pvalue_bonus
# p-value is virtually 0; There is < 0.1% chance of this difference arising by chance.
# Southern executives are more likely to receive bonuses than the executives in the rest of the
# country.


# Permutation test #3 - difference between mean months waiting by poor/non-poor areas
mw <- hcv$months_waiting
obs_mw <- mean(mw[PoorInd], na.rm = TRUE) - mean(mw[-PoorInd], na.rm = TRUE); obs_mw

N <- 10^5
diffs <- numeric(N)
for (i in 1:N) { 
  scramble <- sample(hcv$poverty_area, length(hcv$poverty_area), replace = FALSE)
  index <- which(scramble)
  diffs[i] <- mean(mw[index],na.rm=TRUE)-mean(mw[-index],na.rm=TRUE)
}
mean(diffs)
hist(diffs, prob = TRUE)
abline(v=obs_mw, col="red") #looks like our observed differenced falls within the distribution
pvalue <- (sum(diffs >= obs_mw)+1)/(N+1); pvalue 
t.test(hcv$months_waiting ~ hcv$poverty_area)
# According to our permutation test, there's a 95% chance of the difference in mean waiting times between poor and 
# non poor areas occurring by change. However, according to the t-test, there's a 12% change of this occurring by
# chance. The permutation test is superior to the t-test and this difference in means is definitely not significant.

#------------------------------------------------------------
#      ggplot with linear regression (#11 and #14)
#      appropriate use of correlation (#16)
#------------------------------------------------------------

# plot and linear regression of Total Compensation as a function of percentage of households with 
# the race of the head of household a minority (Black, Native American, or Asian or Pacific 
# Islander, or the ethnicity is Hispanic)
summary(lm(Total.Compensation ~ pct_minority, data = hcv))
cor(hcv$Total.Compensation, hcv$pct_minority, use = "complete.obs")
# positive correlation between Total comp and % racial minority

ggplot(hcv, aes(x=pct_minority, y=Total.Compensation, color=pct_minority)) + 
  scale_x_continuous(name="Percent of Racial Minority Clients") + geom_point()+
  scale_y_continuous(name="Largest 'Total Compensation' @ PHA",labels=scales::comma) +
  ggtitle("Race and PHA Executive Compensation") +
  geom_smooth(method = 'lm',color='black') + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))+scale_color_gradientn(colours = rainbow(5))

# Unexpected thing #2-- for each additonal percentage point minority in the HCV Program,
# executive compensation increases by $414, and it's statistically significant! We expected
# these things to be unrelated.


# Another relationship demonstrated with ggplot, correlation, and linear regression
# Total compensation of PHA exec as a function of tenant income
cor(hcv$Total.Compensation, hcv$hh_income, use = "complete.obs")
# positive correlation between Total comp and tenant income
# with increasing average tenant household income, the largest total compensation observed
# in the PHA increases
summary(lm(Total.Compensation ~ hh_income, data = hcv))
# only 6% of the variability in Total Compensation can be explained by the income of the 
# PHA tenants. However, the relationship between Total Comp and tenant income is significant
# (p-value < 0.1%). There are other variables that likely contribute to the 
# variability in Total Comp. 

ggplot(hcv, aes(x=hh_income, y=Total.Compensation), 
       group=region) + 
  geom_point(aes(shape=region, color=region)) + 
  scale_x_continuous(name="Average Tenant Household Income",labels=scales::comma) +
  scale_y_continuous(name="Largest 'Total Compensation' @ PHA",labels=scales::comma) +
  ggtitle("Relationship between Tenant Income\nand PHA Executive Compensation") +
  geom_smooth(method = 'lm') + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# as we can see in the plot, the Island region is skewed to the left - could it 
# be affecting the fit of the model?
# data without Island outliers
hcv_mainland <- filter(hcv, hcv$region != "Island")
cor(hcv_mainland$Total.Compensation, hcv_mainland$hh_income, use = "complete.obs")
# positive correlation (as we saw above) and is basically the same correlation seen
# with all regions included
summary(lm(hh_income ~ Total.Compensation, data = hcv_mainland))
# very slightly higher R-squared value (0.067 vs 0.064), so virtually no change in the fit
# of the linear model

ggplot(hcv_mainland, aes(x=hh_income, y=Total.Compensation), 
       group=region) + 
  geom_point(aes(shape=region, color=region)) + 
  scale_x_continuous(name="Average Tenant Household Income",labels=scales::comma) +
  scale_y_continuous(name="Largest 'Total Compensation' @ PHA",labels=scales::comma) +
  ggtitle("Relationship between Tenant Income\nand PHA Executive Compensation") +
  stat_smooth(method = 'lm') + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# to see the trends in the regions separately (out of interest):
ggplot(hcv, aes(x=hh_income, y=Total.Compensation), group=region) + 
  geom_point(aes(shape=region, color=region, alpha = 0.3)) + 
  scale_x_continuous(name="Average Tenant Household Income",labels=scales::comma) +
  scale_y_continuous(name="Largest 'Total Compensation' @ PHA",labels=scales::comma) +
  ggtitle("Relationship between Tenant Income\nand PHA Executive Compensation, by Region") +
  geom_smooth(method = 'lm') + facet_wrap(~region) + theme_bw() + scale_alpha(guide = 'none')


#------------------------------------------------------------
#      Appropriate use of novel statistics (trimmed mean, #13)
#------------------------------------------------------------

# in an attempt to get a better fit of a linear model for the previously presented data
# (total comp by tenant income), we'll remove the outliers from the data
# first remove outliers from tenant income (hh_income)
hcv_no_out <- hcv[!hcv$hh_income %in% boxplot.stats(hcv$hh_income)$out,]

# look at difference on histogram
hist(hcv$hh_income, ylim = c(0, .00025), probability = TRUE,
     col = rgb(0.2, 0.4, 0.8, 0.3), main = "Income of Tenants at every 
     Public Housing Authority", xlab = "Annual Income (in dollars)", breaks = "FD")
hist(hcv_no_out$hh_income, col = rgb(0.1,0.7,0.2,0.5), probability = TRUE, breaks = "FD",add = TRUE)
# we see here that removing the outliers makes it much more likely to observe annual income
# close to the mean, and much less likely (probability = 0%) to see values above 20k or
# below 5k

# now remove the outliers of total comp from this dataset based off of original dataset
hcv_no_out <- hcv_no_out[!hcv_no_out$Total.Compensation %in% 
                           boxplot.stats(hcv$Total.Compensation)$out,]


ggplot(hcv_no_out, aes(x=hh_income, y=Total.Compensation), 
       group=region) + 
  geom_point(aes(shape=region, color=region)) + 
  scale_x_continuous(name="Average Tenant Household Income",labels=scales::comma) +
  scale_y_continuous(name="Largest 'Total Compensation' @ PHA",labels=scales::comma) +
  ggtitle("Relationship between Tenant Income \nand PHA Executive Compensation") +
  geom_smooth(method = 'lm') + theme_bw()
cor(hcv$Total.Compensation, hcv$hh_income, use = "complete.obs")
summary(lm(hh_income ~ Total.Compensation, data = hcv))
#Removing the ~300 outliers didn't change the fit of the linear model or the correlation

#------------------------------------------------------------------
#      Calculation and display of logistic regression curve (#15)
#------------------------------------------------------------------
# MICAH: do we want to keep this plot at all and just remove the logistic regression?
# or should we remove this entirely?
ggplot(hcv, aes(x=num_hh, y=Total.Compensation), group=region) + 
  geom_point(aes(shape=region, color=region)) + 
  scale_x_continuous(name="Number of Client Households",labels=scales::comma,limits=c(0,1000)) +
  scale_y_continuous(name="Largest 'Total Compensation' @ PHA",labels=scales::comma) +
  ggtitle("Relationship between Tenant Caseload and\n PHA Executive Compensation") +
  geom_smooth(method = 'lm') + theme_bw()
# looks relatively flat and potentially linear with the above x-limits

# removing the x-limits seems to show a different story
ggplot(hcv, aes(x=num_hh, y=Total.Compensation), group=region) + 
  geom_point(aes(shape=region, color=region)) + 
  scale_x_continuous(name="Number of Client Households",labels=scales::comma) +
  scale_y_continuous(name="Largest 'Total Compensation' @ PHA",labels=scales::comma) +
  ggtitle("Relationship between Tenant Caseload and\n PHA Executive Compensation") +
  geom_smooth(method = 'lm') + theme_bw()

# may be a good candidate for logistic regression


#MICAH: I made the outcome total.comp - thoughts on the below?

# attempt at logistic regression
# create binary variable for Total.Comp
hcv <- mutate(hcv, total_comp_bin = Total.Compensation >= 150000)
#plot total.comp bins as a function of number of households
plot(hcv$num_hh, hcv$total_comp_bin) 
idx <- which(is.na(hcv$num_hh))
hcv_logreg <- hcv[-idx,]
idx <- which(is.na(hcv_logreg$total_comp_bin))
hcv_logreg <- hcv_logreg[-idx,]
num_house <- hcv_logreg$num_hh
total_comp_bin <- hcv_logreg$total_comp_bin
MLL <- function(alpha, beta) {
  -sum(log(exp(alpha+beta*num_house)/(1+exp(alpha+beta*num_house)))*total_comp_bin
       + log(1/(1+exp(alpha+beta*num_house)))*(1-total_comp_bin))
}
results <- mle(MLL, start = list(alpha = 0, beta = 0))
results@coef
curve(exp(results@coef[1]+results@coef[2]*x)/ 
        (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
# Micah - I don't know how great this looks - what are your thoughts?
# I tried with bins of >= 100k and >= 200k, and those were worse
# any other indicators you'd want to try?

#plot total comp as function of total rent
plot(hcv$total_rent, hcv$total_comp_bin) 
idx <- which(is.na(hcv$total_rent))
hcv_logreg <- hcv[-idx,]
idx <- which(is.na(hcv_logreg$total_comp_bin))
hcv_logreg <- hcv_logreg[-idx,]
rent <- hcv_logreg$total_rent
tcomp <- hcv_logreg$total_comp_bin
MLL <- function(alpha, beta) {
  -sum(log(exp(alpha+beta*rent)/(1+exp(alpha+beta*rent)))*tcomp
       + log(1/(1+exp(alpha+beta*rent)))*(1-tcomp))
}
results <- mle(MLL, start = list(alpha = 0, beta = 0))
results@coef
curve(exp(results@coef[1]+results@coef[2]*x)/ 
        (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
# does this look better or worse than the other log reg curve?

#------------------------------------------------------------
#      Graphical display diff from class scripts (#19)
#------------------------------------------------------------

# citation (as requested by library(ggmap))
# D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R
# Journal, 5(1), 144-161. URL
# http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

# the below will create two difference maps, visualizing rent burden across the United States

# when retrieving the first map (below) from Google, you may get an error, saying you're 
# "OVER_QUERY_LIMIT" (Google blocks the query if you've reached your quota). If this occurs, 
# please try again a few more times, it will eventually work.
# The only solution around this is to get an API key from Google, but I'm not ready for that level
# of commitment for a one-off term project.
usa.map <- get_map(location = 'united states', zoom=4, maptype = "terrain",
                   source = 'google')

# pull out columns we're interested in
hcv_map <- select(hcv, longitude, latitude, rent_burden)
cont_coords <- function(x) (as.numeric(as.character(x)))
hcv_map[,1:2] <- sapply(hcv_map[,1:2], cont_coords)

attr(usa.map, "bb") #get correct limits of US map to add into plot
plot_all <- ggmap(usa.map) + geom_point(aes(x=longitude, y=latitude, colour=rent_burden), 
                                        data=hcv_map, size = 0.5, na.rm = TRUE)  + 
  scale_color_gradient("Rent\nBurden", low="blue", high="red") + 
  ggtitle("Rent Burden across the United States") +
  coord_map(projection="mercator",xlim=c(-124, -66), ylim=c(25, 50)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
plot_all
# easy to visualize data density and which specific areas that have low vs high rent burden

#different type of map, representing mean rent burden across the continental US
hcv_state <- hcv %>% group_by(states) %>% summarise(mean_rb = mean(rent_burden, na.rm = TRUE))
hcv_state <- hcv_state %>% separate(states, c("abbrev", "state_name"), " ", 
                                    remove = TRUE, fill = "right", extra = "merge")
hcv_state <- mutate(hcv_state, state = tolower(state_name))
hcv_state <- hcv_state[,3:4]

map <- map_data("state")
plot_mean <- ggplot(hcv_state, aes(fill = mean_rb)) + 
  geom_map(aes(map_id = state), map = map) + 
  expand_limits(x = map$long, y = map$lat) +
  scale_fill_gradient("Mean Rent\nBurden", low='grey', high='darkblue') + 
  ggtitle("Mean Rent Burden by US state") +
  coord_map(projection="mercator",xlim=c(-125, -66), ylim=c(25, 50)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
plot_mean
# this display allows you to see which states in the US have the highest or lower mean rent
# burden

# if you want to see the two maps together, please do the following:
grid.arrange(plot_all, plot_mean, nrow=2)