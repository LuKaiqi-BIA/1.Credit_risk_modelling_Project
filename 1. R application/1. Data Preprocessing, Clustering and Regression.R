library(dplyr)
library(readr)
library(cluster)
library(factoextra)
library(leaps)
library(forecast)
options(scipen = 999)

# Reading the file and filtering unnecessary data
df <- read_csv("merlion.csv")
df_clean <- df %>% filter(loan_stat != "Current") %>% select(-c(fund_mt,inc_ann_jt,app_typ,ttl_int_rec, ttl_pr_rec,loan_stat, ttl_pym)) 
df_ttl <- df %>% filter(loan_stat != "Current") %>% select(ttl_pym)
loan_df <- df %>% filter(loan_stat != "Current") %>% select(loan_stat)

# Ranked encode the emp_l data
the_dict <- c(10,0,3,2,9,1,8,4,7,6,5,-1)
names(the_dict) <- unique(df$emp_l)
empl_c = data.frame(unname(the_dict[df_clean$emp_l]))
df_clean = df_clean  %>% bind_cols(empl_c)
df_clean = df_clean %>% select(-emp_l)
colnames(df_clean)[colnames(df_clean)=="unname.the_dict.df_clean.emp_l.."] <- "emp_l"

# One hot encoding
df_clean.ienc <- df_clean %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric) 

df_clean.norm <- sapply(df_clean.ienc, scale)

# Generate elbow plot
set.seed(1)
k.max <- 15
wss <- (nrow(df_clean.norm)-1)*sum(apply(df_clean.norm,2,var))

for (i in 2:k.max) wss[i] <- sum(kmeans(df_clean.norm,
                                        centers=i,
                                        iter.max = 15, algorithm = "Hartigan-Wong")$withinss)


# Determine optimal number of clusters
pdf("project_cluster_elbow.pdf", width=8,height=6)
plot(1:k.max, wss, 
     type="b", 
     xlab="Number of Clusters K",
     ylab="Total within-cluster sum of squares",
     main="Elbow Plot to find Optimal Number of Clusters",
     pch=19, cex=1)

Ci <- 8

# --------------------------K-means---------------------------------------
#k means clustering with 8 clusters
km <- kmeans(df_clean.norm,Ci)


# Assign Cluster Numbers to dataset
df.clustn <- df_clean %>% bind_cols(data.frame(km$cluster))

# Generate cluster plot
pdf("project_cluster_factoextra.pdf", width=8,height=6)
fviz_cluster(km, data = df_clean.norm,
             ellipse.type="convex",
             outlier.color = "black",
             outlier.shape = 23)
dev.off()

df.clustn <- df.clustn %>%
  mutate_if(is.character, as.factor)
df.clustn <- df.clustn %>% bind_cols(loan_df)

df.clustn$loan_stat[df.clustn$loan_stat == "Chargeoff"] = "Default"

km.centers <- km$centers[km$cluster, ]
km.dist <- sqrt(rowSums((df_clean.norm - km.centers)^2)) 
mdist.km <- tapply(km.dist, km$cluster, mean )

distscore.km <- km.dist/(mdist.km[km$cluster])
distfact.km <- data.frame(distscore.km)
colnames(distfact.km) <- "DIST.FACTOR"

minmaxscore.km <- data.frame((distscore.km - min(distscore.km))/(max(distscore.km)-min(distscore.km)))
colnames(minmaxscore.km) <- "SCORE.MINMAX"

df.clustdn <- df.clustn %>%
  bind_cols(distfact.km, minmaxscore.km)

# Identify outliers
P <- 0.2

# Determine number of Outliers based on Distance Score % Criterion
x <- length( which( minmaxscore.km > P) )

# Outliers (Above distance score Percentile of 20%)
km.perc.order <- order(distscore.km, decreasing = TRUE)[1:x]
# Coerce outliers into dataframe
km.perc.outl <- data.frame(df.clustdn[km.perc.order,])


# --------------------------Multiple Linear Regression---------------------------------------

# Remove outliers from the original dataset and add back ttl_pym for regression
df_cleaned <- df.clustdn %>% cbind(df_ttl) %>% select(-c(km.perc.order)) %>% select(-c(km.cluster,DIST.FACTOR,SCORE.MINMAX)) 

# 70-30 split
split=0.7
train.index <- sample(1:nrow(df_cleaned),split*nrow(df_cleaned)) #70-30 split
train.mlm <- df_cleaned[train.index,]
test.mlm <- df_cleaned[-train.index,]

# Multiple regression with all variables
loan.reg <- lm(ttl_pym ~., data = train.mlm)
summary(loan.reg)
accuracy(loan.reg)
round(loan.reg$coefficients,2)
test.pred<- predict(loan.reg,test.mlm)
accuracy(test.pred,test.mlm$ttl_pym)

# Stepwise regression
loan.step<- step(loan.reg, direction = "both")
summary(loan.step)
test.step.pred <- predict(loan.step, test.mlm)
accuracy(test.step.pred,test.mlm$ttl_pym)
round(loan.step$coefficients,2)

# Backward elimination
loan.back<- step(loan.reg, direction = "backward")
summary(loan.back)
test.back.pred <- predict(loan.step, test.mlm)
accuracy(test.back.pred,test.mlm$ttl_pym)

# Forward selection
loan.forward<- step(loan.reg, direction = "backward")
summary(loan.forward)
test.forward.pred <- predict(loan.forward, test.mlm)
accuracy(test.forward.pred,test.mlm$ttl_pym)

