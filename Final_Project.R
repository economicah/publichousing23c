#-----------------------------------------------------------
# ****************************MISC**************************
#-----------------------------------------------------------
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
#install.packages("noncensus")
library(noncensus)
#install.packages("ggmap")
library(ggmap)


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

#------------------------------------------------------------
#          Create Variables               
#------------------------------------------------------------


# add number of client households as a new variable
hcv <- mutate(hcv, num_hh = total_units * (pct_occupied/100))
#sum(hcv$num_hh, na.rm = TRUE)

# the census bureau designates census tracts with a poverty rate >=20% as "poverty areas"
hcv <- mutate(hcv, poverty_area = tpoverty >=20)
#head(hcv$poverty_area,hcv$tpoverty)

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


#-------------------------------------------------------------
# *************************GRAPHICS***************************
#-------------------------------------------------------------

#------------------------------------------------------------
#      BARPLOT (Reqd Graphical Displays #1)
#------------------------------------------------------------
hcv_collapse <- summaryBy(num_male+num_fem~region,data=hcv,FUN=sum,na.rm=TRUE)
hcv_collapse_melt <-melt(hcv_collapse,id.var="region")

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

hist(hcv$Total.Compensation/1000, breaks = "FD", 
     col=rgb(0.1,0.5,0.8,0.5), main = "Earnings of the Top-Paid Employee at\n every Public Housing Authority",
     xlab = "Total Compensation (in thousands of dollars)", ylab = "Number of HCV Programs",las=1,xaxt="n",border=F,
     xlim = c(0, 260), ylim=c(0,300))
#axis(side=1, at=axTicks(1), 
  #   labels=formatC(axTicks(1), format="d", big.mark=','))
axis(side=1, at=seq(0,260,10))
#LIZ: are you ok making this a freq instead of a probability?

hist(hcv$rent_burden, breaks = "FD", 
     col=rgb(0.8,0.3,0.6,0.5), xlim = c(0.5, 2.5), ylim=c(0,300),
     main = "Average Rent Burden",
     xlab = "Ratio of Household Monthly Income to Pre-Subsidy Rent", ylab = "Number of HCV Programs",border=F) 
# we had decided we would plot rent_burden with a PDF on it, but the normal 
# distribution doesn't seem to fit, which is why I decided to do an exp dist. below
# commented out curve below, if you want to see:
#mu <- mean(hcv$rent_burden, na.rm = TRUE)
#sig <- sd(hcv$rent_burden, na.rm = TRUE)
#curve(dnorm(x, mu, sig), add= TRUE)
#LIZ: i agree with this decision! I made this one a freq also though, rather than prob. 
# i feel like that's easier to understand if we aren't overlaying a distribution

#------------------------------------------------------------
#      PDF over HISTOGRAM (Reqd Graphical Displays #3)
#------------------------------------------------------------
# hist(hcv$months_waiting, breaks = "FD", 
     #ylim=c(0,300), xlim=c(0,150),
     #col=rgb(0.2,0.8,0.5,0.5),border=F,
     #main="Time Spent Waiting for a Home",
     #xlab="Months",ylab="Number of HCV Programs")

hist(hcv$months_waiting, breaks = "FD", prob = TRUE,
     ylim = c(0,.04),
     col=rgb(0.2,0.8,0.5,0.5),border=F,
     main="Time Spent Waiting for a Home",
     xlab="Months",ylab="Proportion of HCV Programs")

# add exponential distribution function
a <- 1/mean(hcv$months_waiting, na.rm = TRUE)
curve(dexp(x, a), add = TRUE)


#------------------------------------------------------------
#      Contingency table (Reqd Graphical Displays #4)
#      Analysis of a contingency table (Reqd Analysis #3)
#------------------------------------------------------------
median(hcv$months_waiting, na.rm= TRUE)
max(hcv$months_waiting, na.rm = TRUE)
min(hcv$months_waiting, na.rm = TRUE)
mean(hcv$months_waiting, na.rm = TRUE)

attach(hcv)
hcv$mw_bin_2[hcv$months_waiting < 6] <- "0 to < 6 mo."
hcv$mw_bin_2[hcv$months_waiting >= 6  & hcv$months_waiting < 12] <- "6 to < 12 mo."
hcv$mw_bin_2[hcv$months_waiting >= 12  & hcv$months_waiting < 18] <- "12 to < 18 mo."
hcv$mw_bin_2[hcv$months_waiting >= 18  & hcv$months_waiting < 24] <- "18 to < 24 mo."
hcv$mw_bin_2[hcv$months_waiting >= 24  & hcv$months_waiting < 30] <- "24 to < 3 mo."
hcv$mw_bin_2[hcv$months_waiting >= 30  & hcv$months_waiting < 36] <- "30 to < 36 mo."
hcv$mw_bin_2[hcv$months_waiting >= 36] <- "36+ mo."
detach(hcv)
table(hcv$mw_bin_2)
# re-order levels so they're chronological categories
hcv$mw_bin_2 <- as.factor(hcv$mw_bin_2)
levels(hcv$mw_bin_2)
hcv$mw_bin_2 <- factor(hcv$mw_bin_2, levels = 
                         c("0 to < 6 mo.", "6 to < 12 mo.", "12 to < 18 mo.",
                           "18 to < 24 mo.", "24 to < 3 mo.", "30 to < 36 mo.",
                           "36+ mo."))
# contingency table of months waiting crossed with poverty areas
pov_mo_tbl <- table(hcv$mw_bin_2, hcv$poverty_area); pov_mo_tbl
# Compare with the table that would be expected if the factors were independent
Expected <- outer(rowSums(pov_mo_tbl), colSums(pov_mo_tbl))/sum(pov_mo_tbl); Expected
# Check if the difference between expected and observed is significant
chisq.test(hcv$mw_bin_2, hcv$poverty_area)

# There is about a 1% chance that the observed contingency table arose by chance
# Logically this makes sense. Much fewer HCV programs located in poorer
# places (poverty_area = TRUE) experience short wait times (0-6 months)
# than we would expect, but many more experience wait times in every other
#bucket than we would expect if wait time was random. Many more HCV 
# programs than we would expect in non-poor areas experience short wait times, but
# many fewer experience wait times in every other bucket

#-------------------------------------------------------------
# *************************Analysis***************************
#-------------------------------------------------------------

#------------------------------------------------------------
#      Permutation Test (Reqd Analysis #1)
#------------------------------------------------------------

#permutation test #1
PoorInd <- which(hcv$poverty_area); head(PoorInd) #indices for poverty_areas

# Total.Compensation
tapply(hcv$Total.Compensation, hcv$poverty_area, mean, na.rm=TRUE)
Total.Compensation <- hcv$Total.Compensation
Obs <- mean(Total.Compensation[PoorInd],na.rm=TRUE)-mean(Total.Compensation[-PoorInd],na.rm=TRUE); Obs  
# difference of $6,064--is this significant?

# Micah - if we want to spin this as our "unexepected" thing, do you think
# we should show that the difference is 7.5% of the mean for non-poverty area
# and is 7% of the mean for the poverty area (so it's rather small! and is about 
# the same proportion of each mean); see below:
6604/87240.89*100
6604/94427.60*100
#liz: where did 6604 come from?

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

#permutation test #2
table(hcv$receive_bonus,hcv$region)
#liz they're much more likely to get bonuses in the south looks like
#micah: I did a permutation test below to see if significant! (and it is)

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
#liz: what makes you think it's too low? because it's 0? i think it looks great

#------------------------------------------------------------
#      ggplot with linear regression (#11 and #14)
#------------------------------------------------------------

# total comp by tenant income

# with Island outliers
ggplot(hcv, aes(x=hh_income, y=Total.Compensation), 
       group=region) + 
  geom_point(aes(shape=region, color=region)) + 
  scale_x_continuous(name="Average Tenant Household Income",labels=scales::comma) +
  scale_y_continuous(name="Largest 'Total Compensation' @ PHA",labels=scales::comma) +
  ggtitle("Relationship between Tenant Income and\n PHA Executive Compensation") +
  geom_smooth(method = 'lm') + theme_bw()

cor(hcv$Total.Compensation, hcv$hh_income, use = "complete.obs")
# positive correlation between Total comp and tenant income
summary(lm(hh_income ~ Total.Compensation, data = hcv))
# NEED TO STATE HOW GOOD/BAD THIS FIT IS; POTENTIALLY TALK ABOUT RESIDUALS?
#LIZ: i think we should consider removing the island people. they clearly
#behave differently than everyone else and may be biasing our results in a way that
#is not relevant to analysis of the mainland. what do you think? we could even make a
#point of doing it and explaining it-- does that fit into any of the bonus point categories?


# trying to determine how to remove outliers: work in progress!
ggplot(hcv, aes(x=hh_income, y=Total.Compensation, group = region)) + 
  geom_boxplot() + facet_wrap(~ region)


# wihout Island outliers
hcv_mainland <- filter(hcv, hcv$region != "Island")
ggplot(hcv_mainland, aes(x=hh_income, y=Total.Compensation), 
       group=region) + 
  geom_point(aes(shape=region, color=region)) + 
  scale_x_continuous(name="Average Tenant Household Income",labels=scales::comma) +
  scale_y_continuous(name="Largest 'Total Compensation' @ PHA",labels=scales::comma) +
  ggtitle("Relationship between Tenant Income and\n PHA Executive Compensation") +
  stat_smooth(method = 'lm') + theme_bw()

cor(hcv_mainland$Total.Compensation, hcv_mainland$hh_income, use = "complete.obs")
# positive correlation between Total comp and tenant income
summary(lm(hh_income ~ Total.Compensation, data = hcv_mainland))

ggplot(hcv, aes(x=hh_income, y=Total.Compensation), group=region) + 
  geom_point(aes(shape=region, color=region, alpha = 0.3)) + 
  scale_x_continuous(name="Average Tenant Household Income",labels=scales::comma) +
  scale_y_continuous(name="Largest 'Total Compensation' @ PHA",labels=scales::comma) +
  ggtitle("Relationship between Tenant Income and\n PHA Executive Compensation, by Region") +
  geom_smooth(method = 'lm') + facet_wrap(~region) + theme_bw()

ggplot(hcv, aes(x=num_hh, y=Total.Compensation), group=region) + 
  geom_point(aes(shape=region, color=region)) + 
  scale_x_continuous(name="Number of Client Households",labels=scales::comma,limits=c(0,50000)) +
  scale_y_continuous(name="Largest 'Total Compensation' @ PHA",labels=scales::comma) +
  ggtitle("Relationship between Tenant Caseload and\n PHA Executive Compensation") +
  geom_smooth(method = 'lm') + theme_bw()
#LIZ: this is interesting. play with the x-axis limit. most are clumped <1,000 and it's relatively flat
#but then there's a really strong, positive relationship once the number of clients 
#gets above that, all the way till 40,000 or so. could this be a candidate for logistic regression?
#Micah - whoa! that's incredible; it would be worth a shot! I'm trying to think of how
# we'd make one of these a categorical; like <1000 households to >= 1000 households? actually,
# i think the outcome should be the compensation variable. so <100k to >= 100k?

# attempt at logistic regression
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
# Micah - I don't think this looks that great - what are your thoughts?
# I tried with bins of >= 100k and >= 200k, and those were worse
# any other indicators you'd want to try?


#------------------------------------------------------------
#      Graphical display diff from class scripts (#19)
#------------------------------------------------------------

# citation (as requested by library(ggmap))
# D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R
# Journal, 5(1), 144-161. URL
# http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

usa.map <- get_map(location = 'united states', zoom=4, maptype = "terrain",
                   source = 'google')

# pull out columns we're interested in
# MICAH - can choose a different variable for this, if you want!
hcv_map <- select(hcv, longitude, latitude, rent_burden)
cont_coords <- function(x) (as.numeric(as.character(x)))
hcv_map[,1:2] <- sapply(hcv_map[,1:2], cont_coords)
ggmap(usa.map) + geom_point(aes(x=longitude, y=latitude, colour=rent_burden), 
                            data=hcv_map, size = 0.5, na.rm = TRUE)  + 
  scale_color_gradient("Rent\nBurden", low="blue", high="red") + 
  ggtitle("Rent Burden Across the United States") +
  coord_map(projection="mercator",xlim=c(-124, -66), ylim=c(25, 50)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

attr(usa.map, "bb") #get correct limits of US map to add into plot

