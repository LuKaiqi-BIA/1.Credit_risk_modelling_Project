
#DEFINE DF AS ANY DATASET oF LOANS, THE MODEL WILL HANDLE THE REST
df = df %>% filter(loan_stat != "Current")

# Selecting variables available only at loan application
currents <- df %>% 
  select(-c(fund_mt,
            inc_ann_jt,
            app_typ,
            ttl_int_rec,
            ttl_pr_rec,
            ttl_pym,
            loan_stat))


# Ranked encode emp_l
the_dict <- c(10,0,3,2,9,1,8,4,7,6,5,-1)
names(the_dict) <- unique(currents$emp_l)
empl_c = unname(the_dict[currents$emp_l])
currents = currents  %>% select(-emp_l)
currents = currents %>% mutate(emp_l = empl_c)

# Combined model
# 1. Feed non-current data into Classification tree to predict final loan_stat("tagging")
current.class.pred = predict(rpart.tree, newdata = currents, type = "class")
currents.reg = currents %>% mutate(loan_stat = current.class.pred)

# 2. Feed tagged non-current data into regression model from previous part
reg.preds = predict(loan.reg, newdata = currents.reg)

# Expected profit (loss) with this combined model
sum(reg.preds - currents$loan_amt) 

# Expected profit(loss) without the combined model(the company's current status)
ttls = df %>% select(ttl_pym)
sum(ttls - currents$loan_amt) 



accuracy(reg.preds, df$ttl_pym)
