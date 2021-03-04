library(readr)
library(dplyr)

merlion.raw = read_csv("merlion.csv",
                   col_types = cols(fund_mt = col_date(format = "%b-%y")))

# Removing duiplicate dataset
merlion.raw.distinct <- distinct(merlion.raw)

# Extracting duplicated rows
merlion.dup <- duplicated(merlion.raw)
data.bind <- cbind(merlion.raw, merlion.dup)
data.extract.dup <- filter(data.bind, merlion.dup == "TRUE")



# Extracting only dataset with discrepancies issues 
merlion.flag <- merlion.raw.distinct %>%
  mutate(Difference = round(ttl_pym - ttl_int_rec - ttl_pr_rec, digits = 2) - 251.51,
         loan_ratio = inc_ann / loan_amt,
         Diff.Loan_Amt_Ratio = Difference / loan_amt) %>%
  filter(Difference > 0)

#Filter out top 10% difference into a new dataframe
num_rows = nrow(merlion.flag)/10
toptenDifference <- merlion.flag[1:num_rows,]



# Comparing purpose
summary(factor(toptenDifference$purp))/(nrow(toptenDifference)/100)
summary(factor(merlion.flag$purp))/(nrow(merlion.flag)/100)
summary(factor(merlion.raw.distinct$purp))/(nrow(merlion.raw.distinct)/100)

# Creating a dataframe to compare the differences between the 3 dataset for comparing purpose
toptenDifferece.Purp = data.frame(summary(factor(toptenDifference$purp))/(nrow(toptenDifference)/100))
toptenDifferece.Purp['education',1] = 0
sort.toptenDifferece.Purp <- data.frame(toptenDifferece.Purp[ order(row.names(toptenDifferece.Purp)), ])
colnames(sort.toptenDifferece.Purp) = "toptenDifference"

toptenDifferece.Purp.abs = data.frame(summary(factor(toptenDifference$purp)))
toptenDifferece.Purp.abs['education',1] = 0
sort.toptenDifferece.Purp.abs <- data.frame(toptenDifferece.Purp.abs[ order(row.names(toptenDifferece.Purp.abs)), ])
colnames(sort.toptenDifferece.Purp.abs) = "toptenDifference.abs"

comparison_purpose = sort.toptenDifferece.Purp %>% 
 mutate(flag = summary(factor(merlion.flag$purp))/(nrow(merlion.flag)/100),
         raw = summary(factor(merlion.raw.distinct$purp))/(nrow(merlion.raw.distinct)/100)) %>%
  cbind(sort.toptenDifferece.Purp.abs) %>%
  mutate(flag.abs = summary(factor(merlion.flag$purp)),
         raw.abs = summary(factor(merlion.raw.distinct$purp)))
rownames(comparison_purpose) = names(summary(factor(merlion.flag$purp)))
View(comparison_purpose)
# We can see that the distribution of the 3 dataset according to their purpose are roughly the same, nothing out of the norm



# Comparing loan stat
summary(factor(toptenDifference$loan_stat))/(nrow(toptenDifference)/100)
summary(factor(merlion.flag$loan_stat))/(nrow(merlion.flag)/100)
summary(factor(merlion.raw.distinct$loan_stat))/(nrow(merlion.raw.distinct)/100)

summary(factor(toptenDifference$loan_stat))
summary(factor(merlion.flag$loan_stat))
summary(factor(merlion.raw.distinct$loan_stat))

# Creating a dataframe to compare the differences between the 3 dataset for purpose
toptenDifferece.loan_stat = data.frame(summary(factor(toptenDifference$loan_stat))/(nrow(toptenDifference)/100))
toptenDifferece.loan_stat[c('Current','Defaut'),1] = 0
colnames(toptenDifferece.loan_stat) = "toptenDifference"

toptenDifferece.loan_stat.abs = data.frame(summary(factor(toptenDifference$loan_stat)))
toptenDifferece.loan_stat.abs[c('Current','Defaut'),1] = 0
colnames(toptenDifferece.loan_stat.abs) = "toptenDifference.abs"

comparison_loan_stat = toptenDifferece.loan_stat %>% 
  mutate(flag = summary(factor(merlion.flag$loan_stat))/(nrow(merlion.flag)/100),
         raw = summary(factor(merlion.raw.distinct$loan_stat))/(nrow(merlion.raw.distinct)/100)) %>%
  cbind(toptenDifferece.loan_stat.abs) %>%
  mutate(flag.abs = summary(factor(merlion.flag$loan_stat)),
         raw.abs = summary(factor(merlion.raw.distinct$loan_stat)))
rownames(comparison_loan_stat) = names(summary(factor(merlion.flag$loan_stat)))
View(comparison_loan_stat)
# A huge jump in the distribution of "chargeoff" can be seen when compared between the three dataset.
# This is concerning to us and we decide to dive deeper to investigate the loan status of the dataset along with the discrepancies of 251.51



# Diving deeper into loan_stat "Chargeoff" with discrepancies
chargeoff_discrepancies <- merlion.raw.distinct %>%
  mutate(Difference = round(ttl_pym - ttl_int_rec - ttl_pr_rec, digits = 2) - 251.51,
         Total_Paid_Ratio = ttl_pym / loan_amt,
         Diff.Loan_Amt_Ratio = Difference / loan_amt) %>%
  filter(loan_stat == "Chargeoff",
         Difference > 0)
summary(chargeoff_discrepancies)
# We can see that the max value for both ""Total_Paid_Ratio" and "Diff.Loan_Amt_Ratio" is rather high for loans considered to be chargeoff.
# We decided to filter out entries that have already paid back more than their loan amount to see if we can find anything.

# filter total paid ratio >= 1
chargeoff_discrepancies_paid_ratio_more_1 <- filter(chargeoff_discrepancies, Total_Paid_Ratio >= 1)
summary(chargeoff_discrepancies_paid_ratio_more_1)

# % of loans with discrepancies that are classified as chargeoff despite having paid more than than the total amount
nrow(chargeoff_discrepancies_paid_ratio_more_1) / nrow(chargeoff_discrepancies)
# A total of 4.65% or 1,279 out of loans that are chargeoff and have discrepancies issues already paid back more than their loan amount.
# This is definitely strange as a reasonable man will assume that people that already paid back more than their loan amount to be classified as "Paid" instead of "Chargeoff".



# Extracting entire "Chargeoff" dataset
Chargeoff_only <- merlion.raw.distinct %>%
  mutate(Difference = round(ttl_pym - ttl_int_rec - ttl_pr_rec, digits = 2) - 251.51,
         Total_Paid_Ratio = ttl_pym / loan_amt,
         Diff.Loan_Amt_Ratio = Difference / loan_amt) %>%
  filter(loan_stat =="Chargeoff")

# filter total paid ratio >= 1
chargeoff_paid_ratio_more_1 <- filter(Chargeoff_only, Total_Paid_Ratio >= 1)
summary(chargeoff_paid_ratio_more_1)

# % of loans that are classified as chargeoff despite having paid more than than the total amount
nrow(chargeoff_paid_ratio_more_1) / nrow(Chargeoff_only)
# A total of 4.18% or 1,605 out of loans that are chargeoff already paid back more than their loan amount.
# Similar to the observation above, it is consider strange to be classify as "chargeoff" despite paying back more than their loan amount.



# Extracting entire "Current" dataset
Current_only <- merlion.raw.distinct %>%
  mutate(Difference = round(ttl_pym - ttl_int_rec - ttl_pr_rec, digits = 2) - 251.51,
         Total_Paid_Ratio = ttl_pym / loan_amt,
         Diff.Loan_Amt_Ratio = Difference / loan_amt) %>%
  filter(loan_stat =="Current")

# filter total paid ratio >= 1
current_paid_ratio_more_1 <- filter(Current_only, Total_Paid_Ratio >= 1)
summary(current_paid_ratio_more_1)

# % of loans that are classified as chargeoff despite having paid more than than the total amount
nrow(current_paid_ratio_more_1) / nrow(Current_only)
# A total of 7.08% or 8,762 out of loans that are "Current" already paid back more than their loan amount.
# Similar to the observation above.



# Extracting entire "Default" dataset
Default_only <- merlion.raw.distinct %>%
  mutate(Difference = round(ttl_pym - ttl_int_rec - ttl_pr_rec, digits = 2) - 251.51,
         Total_Paid_Ratio = ttl_pym / loan_amt,
         Diff.Loan_Amt_Ratio = Difference / loan_amt) %>%
  filter(loan_stat =="Default")

# filter total paid ratio >= 1
default_paid_ratio_more_1 <- filter(Default_only, Total_Paid_Ratio >= 1)
summary(Default_only)

# % of loans that are classified as chargeoff despite having paid more than than the total amount
nrow(default_paid_ratio_more_1) / nrow(Default_only)
# The observation of Default dataeset seems to be fine, as there is 0 entry classified as default despite paying back more than their loan amount.



# Extracting entire "Paid" dataset
Paid_only <- merlion.raw.distinct %>%
  mutate(Difference = round(ttl_pym - ttl_int_rec - ttl_pr_rec, digits = 2) - 251.51,
         Total_Paid_Ratio = ttl_pym / loan_amt,
         Diff.Loan_Amt_Ratio = Difference / loan_amt) %>%
  filter(loan_stat =="Paid")

# filter total paid ratio >= 1
paid_paid_ratio_more_1 <- filter(Paid_only, Total_Paid_Ratio >= 1)
summary(Paid_only)

# % of loans that are classified as chargeoff despite having paid more than than the total amount
nrow(paid_paid_ratio_more_1) / nrow(Paid_only)
# 74.80% of the "Paid" loans have paid back more than their loan amount.
# This means that 25.20% of the loans classified as "Paid" have not paid back more than their loan amounts.
# This is similar to the opposite of earlier observation, instead for the situation here, we should expect people that have not paid back more than their loan amount to be classify as "Chargeoff" or "Default" instead of "Paid".



# Compiling the loan_stat findings
Compiled_loan_stat <- rbind.data.frame(data.frame(nrow(chargeoff_paid_ratio_more_1) / nrow(Chargeoff_only),
                                                  nrow(current_paid_ratio_more_1) / nrow(Current_only),
                                                  nrow(default_paid_ratio_more_1) / nrow(Default_only),
                                                  nrow(paid_paid_ratio_more_1) / nrow(Paid_only)),
                                       1 - data.frame(data.frame(nrow(chargeoff_paid_ratio_more_1) / nrow(Chargeoff_only),
                                                                 nrow(current_paid_ratio_more_1) / nrow(Current_only),
                                                                 nrow(default_paid_ratio_more_1) / nrow(Default_only),
                                                                 nrow(paid_paid_ratio_more_1) / nrow(Paid_only))))
colnames(Compiled_loan_stat) <- c("Chargeoff", "Current", "Default", "Paid")
rownames(Compiled_loan_stat) <- c("Total Paid Ratio >= 1", "Total Paid Ratio < 0")
View(Compiled_loan_stat)
# Compilation of the aboe findings



# Extracting out the extreme anomalies in the classification of loan status for management review
# setting the input for the ratio
x <- 0.7
y <- 0.3

# filter "chargeoff" total paid ratio >= x
chargeoff_paid_ratio_more_x <- filter(Chargeoff_only, Total_Paid_Ratio >= x)

# filter "current" total paid ratio >= x
current_paid_ratio_more_x <- filter(Current_only, Total_Paid_Ratio >= x)

# filter "default" total paid ratio >= x
default_paid_ratio_more_x <- filter(Default_only, Total_Paid_Ratio >= x)

# filter "paid" total paid ratio <= y
paid_paid_ratio_less_y <- filter(Paid_only, Total_Paid_Ratio <= y)

# Combining the filtered dataset into one
dataset_with_anomalies <- rbind(chargeoff_paid_ratio_more_x,
                                current_paid_ratio_more_x,
                                default_paid_ratio_more_x,
                                paid_paid_ratio_less_y)

write.csv(dataset_with_anomalies, file="Dataset_with_Loanstat_anomalies.csv")



# Extract duplicated dataset to "Duplicated_Entries.csv"
write.csv(data.extract.dup, file="Duplicated_Entries.csv")

# Extract flag out dataset for discrepancies of more than 251.51 for amount paid to "Discrepancy_Flag.csv"
write.csv(merlion.flag, file="Discrepancy_Flag.csv")

# Extract Comparison Table for purpose to "Discrepancy_Purp.csv"
write.csv(comparison_purpose, "Discrepancy_Purp.csv")

# Extract Comparison Table for loan_stat to "Discrepancy_Loan_Stat.csv"
write.csv(comparison_loan_stat, "Discrepancy_Loan_Stat.csv")

# Extract drill down Chargeoff Discrepancies dataset to "Chargeoff_Discrepancy.csv"
write.csv(chargeoff_discrepancies, "Chargeoff_Discrepancy.csv")

# Extract loan status findings
write.csv(Compiled_loan_stat, "Loan_Stat_Findings.csv")
