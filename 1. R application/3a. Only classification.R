
#DEFINE DF AS ANY DATASET oF LOANS, THE MODEL WILL HANDLE THE REST

df = df %>% filter(loan_stat != "Current")

currents <- df %>% 
  select(-c(fund_mt,
            inc_ann_jt,
            app_typ,
            ttl_int_rec,
            ttl_pr_rec,
            ttl_pym,
            loan_stat))


ttl_pym = df %>% filter(loan_stat != "Current") %>% select(ttl_pym)

the_dict <- c(10,0,3,2,9,1,8,4,7,6,5,-1)
names(the_dict) <- unique(currents$emp_l)


empl_c = unname(the_dict[currents$emp_l])
currents = currents  %>% select(-emp_l)
currents = currents %>% mutate(emp_l = empl_c)


current.class.pred = predict(rpart.tree, newdata = currents, type = "class")
current.combined = currents %>% cbind(ttl_pym) %>%
  mutate(loan_stat = current.class.pred,
         paid = ifelse(loan_stat == "Paid", ttl_pym, loan_amt))

sum(current.combined$paid - current.combined$loan_amt) #expected profit (loss) with this model

ttls = df %>% select(ttl_pym)
sum(ttls - currents$loan_amt) #expected profit(loss) without the model

