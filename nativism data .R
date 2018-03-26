library(foreign)
library(tidyverse)
dataset = read.spss("Populism_SPSS v3.sav", to.data.frame=TRUE)
dataset = as.tibble(dataset)
dataset_labels <- as.data.frame(attr(dataset, "variable.labels"))
getwd()

write.table(dataset_labels, file="dataset_labels.csv",sep=",",row.names=F)


country <- unique(dataset["qcountry"])
country <- data.frame(country)


summary(dataset$qcountry)

tail(dataset$qcountry)
plot(dataset$qcountry)




library(Amelia)
missmap(dataset, main = "Missing values vs observed", srt = 45)
colnames(dataset)



#### Transform from likert scale to numerical values
trans1 <- c('Much stronger'=5, 'Somewhat stronger'=4,
            'About the same'= 3, 'Somewhat weaker'= 2, "Much weaker" = 1)

trans2 <- c('Much better'=5, 'Slightly better'=4,
            'About the same'= 3, 'Slightly worse'= 2, 
            "Much worse" = 1, "Don't know" = 3)
trans3 = c("Strongly agree" = 1,
           "Somewhat agree" = 2,
           "Neither agree nor disagree" = 3,
           "Somewhat disagree" = 4,
           "Strongly disagree" = 5,
           "Don't know" = 3)

trans4 <- c("No confidence at all" = 1, "Not very much confidence" =2, 
            "A fair amount of confidence" = 3, "Complete confidence" =4, "Don't know" = 2.5)
trans5 <- c("I agree much more with statement A" = 1, "I tend to agree more with statement A" =2,
            "Agree with both equally" =3, "I tend to agree more with statement B" = 4,
            "I agree much more with statement B" = 5, " Don't know" = 3)
trans6 = c("Much more likely to vote for that party or leader" = 5, 
           "Slightly more likely to vote for that party or leader" =4,
           "Make no difference" = 3, 
           "Slightly less likely to vote for that party or leader" = 2,
           "Much less likely to vote for that party or leader" = 1,
           "Don't know" = 3)
trans7 = c("Satisfied" =1,
           "Dissatisfied" =2)
trans8 = c("Very good" =4, 
           "Somewhat good"=3,
           "Somewhat bad" =2,
           "Very bad" =1)
trans9 = c("7 \342\200\223 Very strong economy" =7, 
           "6"=6,"5" =5, "4" =4,"3" =3, "2" =2,
           "1 \342\200\223 Very weak economy" =1)


trans_B7 = trans1[dataset$B7]

## worse than parents
trans_Q7 = trans2[dataset$MW_Q7]
trans_Q8 = trans2[dataset$MW_Q8]

## nativism
trans_Q9_1 = trans3[dataset$MW_Q9_1]
trans_Q9_2 = trans3[dataset$MW_Q9_2]
trans_Q9_3 = trans3[dataset$MW_Q9_3]
trans_Q9_4 = trans3[dataset$MW_Q9_4]
trans_Q9_5 = trans3[dataset$MW_Q9_5]

## System is broken
trans_Q10_1 = trans3[dataset$MW_Q10_1]
trans_Q10_2 = trans3[dataset$MW_Q10_2]
trans_Q10_3 = trans3[dataset$MW_Q10_3]
trans_Q10_4 = trans3[dataset$MW_Q10_4]
trans_Q10_5 = trans3[dataset$MW_Q10_5]

## confidence level
trans_Q11_1 = trans4[dataset$MW_Q11_1]
trans_Q11_2 = trans4[dataset$MW_Q11_2]
trans_Q11_3 = trans4[dataset$MW_Q11_3]
trans_Q11_4 = trans4[dataset$MW_Q11_4]
trans_Q11_5 = trans4[dataset$MW_Q11_5]
trans_Q11_6 = trans4[dataset$MW_Q11_6]
trans_Q11_7 = trans4[dataset$MW_Q11_7]

## open or protection
trans_Q12 = trans5[dataset$MW_Q12_new]
trans_Q13 = trans5[dataset$MW_Q13_new]

## stranger in country
trans_Q14_1 = trans3[dataset$MW_Q14_1]

## gov should do...
trans_Q14_2 = trans3[dataset$MW_Q14_2]
trans_Q14_3 = trans3[dataset$MW_Q14_3]
trans_Q14_4 = trans3[dataset$MW_Q14_4]
trans_Q14_5 = trans3[dataset$MW_Q14_5]

## attributes of a party or leader
trans_Q19_1 = trans6[dataset$MW_Q19_1]
trans_Q19_2 = trans6[dataset$MW_Q19_2]
trans_Q19_3 = trans6[dataset$MW_Q19_3]
trans_Q19_4 = trans6[dataset$MW_Q19_4]
trans_Q19_5 = trans6[dataset$MW_Q19_5]
trans_Q19_6 = trans6[dataset$MW_Q19_6]
trans_Q19_7 = trans6[dataset$MW_Q19_7]
trans_Q19_8 = trans6[dataset$MW_Q19_8]
trans_Q19_9 = trans6[dataset$MW_Q19_9]
trans_Q19_10 = trans6[dataset$MW_Q19_10]

## Overall satisfied with the country?
trans_B2 = trans7[dataset$B2]

## rate the current economy
trans_B3 = trans8[dataset$B3]

## lcoal economy
trans_B6 = trans9[dataset$B6]
country = dataset$qcountry[drop = TRUE]


data_transformed = cbind.data.frame(country,
             trans_B2, trans_B3,trans_B6,trans_B7, trans_Q10_1, trans_Q10_2,
             trans_Q10_3, trans_Q10_4, trans_Q10_5, trans_Q11_1, trans_Q11_2,
             trans_Q11_3, trans_Q11_5, trans_Q11_4, trans_Q11_6, trans_Q11_7,trans_Q12,
             trans_Q13, trans_Q14_1, trans_Q14_2, trans_Q14_3, trans_Q14_4, trans_Q14_5,
             trans_Q19_1, trans_Q19_10, trans_Q19_2, trans_Q19_3, trans_Q19_4, trans_Q19_5,
             trans_Q19_6, trans_Q19_7, trans_Q19_8, trans_Q19_9, trans_Q7, trans_Q8,
             trans_Q9_1, trans_Q9_2, trans_Q9_3,trans_Q9_4, trans_Q9_5)
data_transformed = as.tibble(data_transformed)

data_transformed_nondup <- data_transformed[ , !duplicated(colnames(data_transformed))]

######################################## to perform nativism exploration
data_nativism <- data_numeric[, 35:39]
pc_nativism = prcomp(data_nativism)
biplot(pc_nativism)
ggpairs(data_nativism[-9,])


ggplot(data_nativism, aes(x= Q9_4, y = Q9_5))+
  geom_point()+
  geom_text(aes(label = rownames(data_nativism)), hjust = 1, alpha = 0.6)

##################find the outlier of Q3 Q5
ggplot(data_nativism, aes(x= Q9_3, y = Q9_5))+
  geom_point()+
  geom_text(aes(label = rownames(data_nativism)), hjust = 1, alpha = 0.6)


compare = c(worldmean = mean(data_nativism$Q9_3), 
            Sweden = data_nativism["Sweden", "Q9_3"])

##################find the outlier of Q3 Q4
ggplot(data_nativism, aes(x= Q9_3, y = Q9_4))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  geom_text(aes(label = rownames(data_nativism)), hjust = 1, alpha = 0.6)



ggplot(data_nativism, aes(x= Q9_1, y = Q9_4))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  geom_text(aes(label = rownames(data_nativism)), hjust = 1, alpha = 0.6)


g1 = ggplot(data_nativism, aes(x= Q9_2, y = Q9_4))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  geom_text(aes(label = rownames(data_nativism)), hjust = 1, alpha = 0.6)

data_numeric_noIndia = data_nativism[-9,]
g2 = ggplot(data_numeric_noIndia, aes(x= Q9_2, y = Q9_4))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  geom_text(aes(label = rownames(data_numeric_noIndia)), hjust = 1, alpha = 0.6)

######## kmeans
par(mfrow = c(1,1))
kmeans_out = kmeans(data_nativism, 5, nstart =20)

ls(pc_nativism)
sd_pc = pc_nativism$sdev
var_pc = (sd_pc^2) / sum(sd_pc^2)
plot(1:5, var_pc, type = "b",
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1))



plot(pc_nativism$x[, 1:2],
     col = kmeans_out$cluster,
     main = paste("k-means clustering of country with", 5, "clusters"),
     xlab = "pc1", ylab = "pc2")
text(pc_nativism$x[,1], pc_nativism$x[,2], 
     labels= rownames(data_nativism), 
     cex= 0.4, pos = 2)
abline(v = 0, h = 0)

data_nativism <- as.tibble(data_nativism)

data_nativism$nativism = (data_nativism$Q9_1 + data_nativism$Q9_2 + data_nativism$Q9_3+ data_nativism$Q9_5)/4

country <- rownames(data_nativism)
ggplot(data_nativism, aes(x = reorder((country), nativism), y = nativism))+
  geom_col()+
  coord_flip()

rownames(data_nativism)


#################### Individual countries answers
data_individualcountries <-  dataset %>%
  select(c(87:91,34)) %>%
  filter(qcountry %in% c("US","Sweden", "India", "Turkey", "France"))


data_individualcountries1 = data_individualcountries %>%
  group_by(qcountry, MW_Q9_5) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


ggplot(data_individualcountries1, aes(x = MW_Q9_5, y= freq * 100, fill = qcountry))+
  geom_bar(stat="identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1))+
  ylab("relative percentage of each answer") 




ggplot(mtcars, aes(x = factor(hp))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  ## scale_y_continuous(labels = percent_format()) #version 3.0.9
  scale_y_continuous(labels = percent)
table()


  





  
table(dataset %>%
        filter(qcountry == "Turkey") %>%
        select(MW_Q9_5))
  



















#########################################
## numerical data to perform PCA
data_numeric <- data_transformed %>%
  select(country, 
         trans_B2,trans_B3,trans_B6,trans_B7, trans_Q10_1, trans_Q10_2,
         trans_Q10_3, trans_Q10_4, trans_Q10_5, trans_Q11_1, trans_Q11_2,
         trans_Q11_3, trans_Q11_5, trans_Q11_4, trans_Q11_6, trans_Q11_7,trans_Q12,
         trans_Q13, trans_Q14_1, trans_Q14_2, trans_Q14_3, trans_Q14_4, trans_Q14_5,
         trans_Q19_1, trans_Q19_10, trans_Q19_2, trans_Q19_3, trans_Q19_4, trans_Q19_5,
         trans_Q19_6, trans_Q19_7, trans_Q19_8, trans_Q19_9, trans_Q7, trans_Q8,
         trans_Q9_1, trans_Q9_2, trans_Q9_3,trans_Q9_4, trans_Q9_5) %>%
  group_by(country) %>%
  summarize(B3 = mean(trans_B3), B6 = mean(trans_B6), B7 =mean(trans_B7), 
            Q10_1 = mean(trans_Q10_1), Q10_2 = mean(trans_Q10_2),
            Q10_3 = mean(trans_Q10_3), Q10_4 = mean(trans_Q10_4), 
            Q10_5 = mean(trans_Q10_5), Q11_1 = mean(trans_Q11_1), 
            Q11_2 = mean(trans_Q11_2), Q11_3 = mean(trans_Q11_3),
            Q11_5 = mean(trans_Q11_5), Q11_4 = mean(trans_Q11_4), 
            Q11_6 = mean(trans_Q11_6), Q11_7 = mean(trans_Q11_7),
            Q12 = mean(trans_Q12),
            Q13 = mean(trans_Q13),  Q14_1 = mean(trans_Q14_1), Q14_2 = mean(trans_Q14_2), 
            Q14_3 = mean(trans_Q14_3), Q14_4 = mean(trans_Q14_4), 
            Q14_5 = mean(trans_Q14_5),
            Q19_1 = mean(trans_Q19_1), Q19_10 = mean(trans_Q19_10), 
            Q19_2 = mean(trans_Q19_2), Q19_3 = mean(trans_Q19_3), Q19_4 = mean(trans_Q19_4), 
            Q19_5 = mean(trans_Q19_5),
            Q19_6 = mean(trans_Q19_6), Q19_7 =mean(trans_Q19_7), 
            Q19_8 = mean(trans_Q19_8), Q19_9 = mean(trans_Q19_9), 
            Q7 = mean(trans_Q7), Q8 = mean(trans_Q8),
            Q9_1 = mean(trans_Q9_1), Q9_2 = mean(trans_Q9_2), 
            Q9_3 = mean(trans_Q9_3), Q9_4 = mean(trans_Q9_4), Q9_5 = mean(trans_Q9_5)
            )

data_numeric = as.data.frame(data_numeric) %>%
  column_to_rownames(var = "country")


############################################################ PCA 
pc_out = prcomp(data_numeric, scale = TRUE)

#fit <- hclust(dist(pc_out$x[,1:3]), method="complete") # 1:3 -> based on 3 components
#groups <- cutree(fit, k=5)

ls(pc_out)

install.packages("plotly")
scores <- pc_out$x
x <- scores[,1]
y <- scores[,2]
z <- scores[,3]

# Loadings
loads <- pc_out$rotation

# Scale factor for loadings
scale.loads <- 10
# 3D plot
library(plotly)
p <- plot_ly() %>%
  add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="markers",
            marker = list(color=y, 
                          colorscale = c("#FFE1A1", "#683531"), 
                          opacity = 0.7)) 

for (k in 1:nrow(loads)) {
  x <- c(0, loads[k,1])*scale.loads
  y <- c(0, loads[k,2])*scale.loads
  z <- c(0, loads[k,3])*scale.loads
  p <- p %>% add_trace(x=x, y=y, z=z,
                       type="scatter3d", mode="lines",
                       line = list(width=8),
                       opacity = 0.6) 
}
print(p)
#########################################################




############################################
pc_var = pc_out$sdev ^ 2 
var_explained_per_pc = pc_var / sum(pc_var)

plot(var_explained_per_pc, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "o")

plot(cumsum(var_explained_per_pc), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

summary(pc_out)
biplot(pc_out, alpha = 0.3, alpha = 0.6) 
##### clustering
# Initialize total within sum of squares error: wss
wss <- 0
# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(data_numeric, centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

kmeans_out = kmeans(data_numeric, 5, nstart =20)

#### clustering on pca plots
plot(pc_out$x[, 1:2],
     col = kmeans_out$cluster,
     main = paste("k-means clustering of country with", 4, "clusters"),
     xlab = "pc1", ylab = "pc2")
text(pc_out$x[,1], pc_out$x[,2], 
     labels= rownames(data_numeric), 
     cex= 0.4, pos = 2, alpha = 0.7)
############################################
install.packages("GGally")
ggpairs(data_numeric)
cor(data_numeric)

################################
## Interpret the outliers
world_mean = apply(data_numeric, 2, mean)

data_numeric = data_numeric %>%
  mutate(nativism = (Q9_1+Q9_2+Q9_3+Q9_5)/4)
data_numeric = data_numeric %>% 
  mutate(sys_broke = (Q10_1+Q10_2+Q10_3+Q10_4+Q10_5)/5)

ggplot(data = data_numeric, aes(x = nativism, y=sys_broke))+
  geom_point(color = kmeans_out$cluster)+
  geom_text(aes(label=data_numeric[,1]),hjust=0, vjust=0, alpha = 0.6)+
  geom_hline(aes(yintercept = mean(data_numeric$sys_broke)), 
              color = "red", alpha = 0.6)+
  geom_vline(aes(xintercept = mean(data_numeric$nativism)),
             color = "red", alpha = 0.6)


mean(data_numeric$sys_broke)
##################################################################
ggplot(data_numeric, aes(x = Q19_1, y = Q19_2))+
  geom_point() +
  geom_text(aes(label = rownames(data_numeric)), 
            vjust = -1, alpha = 0.6)+
  xlab("leader stands for people")+
  ylab("leader change status quo")


ggplot(data_numeric, aes(x = Q14_5, y = Q14_3))+
  geom_point() +
  geom_text(aes(label = rownames(data_numeric)), 
            vjust = -1, alpha = 0.6)+
  xlab("gov control staples") +
              ylab("gov stop terrorism")


ggplot(data_numeric, aes(x = Q14_5, y = Q14_3))+
  geom_point() +
  geom_text(aes(label = rownames(data_numeric)), 
            vjust = -1, alpha = 0.6)+
  xlab("gov control staples") +
  ylab("gov stop terrorism")
##################################################################

data_w_demographic <-  cbind.data.frame(data_transformed, age = dataset$a3,
                                        gender = dataset$a2,
                                        education = dataset$a6all) %>%
  mutate(nativism = (trans_Q9_1+trans_Q9_2+trans_Q9_3+trans_Q9_5+ (6-trans_Q9_4))/5) %>% 
  mutate(sys_broke = (trans_Q10_1+trans_Q10_2+trans_Q10_3+trans_Q10_4+trans_Q10_5)/5)%>%
  mutate(confidence = (trans_Q11_1+ trans_Q11_2 +trans_Q11_3+trans_Q11_4+trans_Q11_5+trans_Q11_6+trans_Q11_7)/7) %>%
  mutate(open = (trans_Q12 + trans_Q13)/2) %>%
  mutate(gov_effect = (trans_Q14_2 + trans_Q14_3 + trans_Q14_4 +trans_Q14_5)/4)

sys_broke_cut = cut(data_w_demographic$sys_broke, breaks = 3)
levels(sys_broke_cut) <- c("disagree",
                      "neither agree or disagree",
                      "agree")
data_w_demographic$Sys_broke = sys_broke_cut

Sweden <- data_w_demographic %>%
  filter(country == "Sweden")

range(Sweden$sys_broke)
plot(Sweden$sys_broke)




### Model the System is broken
install.packages("Hmisc")
library(Hmisc)

Sweden$age <- as.numeric(Sweden$age)
Sweden_model <- polr(Sys_broke ~ (nativism + age + education + gender + confidence + open + gov_effect),
                     data = Sweden, Hess = TRUE)
summary(Sweden_model)

Sweden_coeff_table <- coef(summary(Sweden_model))
Sweden_p <- pnorm(abs(Sweden_coeff_table[, "t value"]), lower.tail = FALSE) * 2
Sweden_coeff_table <- cbind(Sweden_coeff_table, "p value" = Sweden_p)
Sweden_coeff_table <- cbind(Sweden_coeff_table, "Odds_ratio" = exp(Sweden_model$coefficients))
round(Sweden_coeff_table, 4)

addterm(Sweden_model, ~.^2, test = "Chisq")
Sweden_model2 <- stepAIC(Sweden_model, ~.^2)
Sweden_model2$anova
Sweden_Odds_ration <-  exp(Sweden_model2$coefficients)
plot(profile(Sweden_model2))

##########US data

US <- data_w_demographic %>%
  filter(country == "US")
US$age <- as.numeric(US$age)
##########regression on demographics

US_model_test1= lm(nativism ~ age + education + gender, data = US)

US_model_test2 = lm(nativism ~ age+education+gender+ confidence+open+gov_effect, data = US)
colnames(US)[4] = "local_economy"

US_model_test3 = lm(nativism ~ age+education+gender+ confidence+open+gov_effect+local_economy, data = US)
addterm(US_model_test3, ~.^2, test = "Chisq")
US_model_test4 <- stepAIC(US_model_test3, ~.^2)


US_model <- polr(Sys_broke ~ (nativism + age + education + gender + confidence + open + gov_effect),
                     data = US, Hess = TRUE)
summary(US_model)

US_coeff_table <- coef(summary(US_model))
US_p <- pnorm(abs(US_coeff_table[, "t value"]), lower.tail = FALSE) * 2
US_coeff_table <- cbind(US_coeff_table, "p value" = US_p)
US_coeff_table <- cbind(US_coeff_table, "Odds_ratio" = exp(US_model$coefficients))
round(US_coeff_table, 4)

addterm(US_model, ~.^2, test = "Chisq")
US_model2 <- stepAIC(US_model, ~.^2)
US_Odds_ration <-  exp(US_model2$coefficients)



US_nativism = US %>%
  select(trans_Q9_1, trans_Q9_2, trans_Q9_3, trans_Q9_4,
         trans_Q9_5)
##US_nativism_factor = factanal(US_nativism, factors = 2, rotation = "varimax", na.action = na.omit)
##US_nativism_factor$loadings
install.packages('psych')
library("psych")
install.packages('GPArotation')
library(GPArotation)
parallel <- fa.parallel(US_nativism, fm = 'minres', fa = 'fa')

factor_out <- fa(US_nativism,nfactors = 2,rotate = "oblimin",fm="minres")
factor_out
plot(factor_out$loadings)
text(factor_out$loadings, labels = colnames(US_nativism), pos = 2)

pr_out1 = prcomp(US_nativism)
biplot(pr_out1)
## for France data

France_nativism <- France %>%
  select(trans_Q9_1, trans_Q9_2, trans_Q9_3, trans_Q9_4,
         trans_Q9_5)
parallel <- fa.parallel(France_nativism, fm = 'minres', fa = 'fa', 
                        main = "France scree plots")
factor_out_France <- fa(France_nativism,nfactors = 2,rotate = "oblimin",fm="minres")
factor_out_France
plot(factor_out_France$loadings, main = "France_factor results")
text(factor_out_France$loadings, labels = colnames(US_nativism), pos = 2)

Turkey_nativism <- Turkey %>%
  select(trans_Q9_1, trans_Q9_2, trans_Q9_3, trans_Q9_4,
         trans_Q9_5)
parallel <- fa.parallel(Turkey_nativism, fm = 'minres', fa = 'fa', 
                        main = "Turkey scree plots")
factor_out_Turkey <- fa(Turkey_nativism,nfactors = 2,rotate = "oblimin",fm="minres")
plot(factor_out_Turkey$loadings, main = "Turkey_factor results")
text(factor_out_Turkey$loadings, labels = colnames(Turkey_nativism), pos = 2)


pr_out2 = prcomp(France_nativism)
biplot(pr_out2)
factor_out_Turkey

#############France data
options(scipen=0)
France <- data_w_demographic %>%
  filter(country == "France")
France$age <- as.numeric(France$age)
France_model <- polr(Sys_broke ~ (nativism + age + education + gender + confidence + open + gov_effect),
                 data = France, Hess = TRUE)
summary(France_model)

France_coeff_table <- coef(summary(France_model))
France_p <- pnorm(abs(France_coeff_table[, "t value"]), lower.tail = FALSE) * 2
France_coeff_table <- cbind(France_coeff_table, "p value" = France_p)
France_coeff_table <- cbind(France_coeff_table, "Odds_ratio" = exp(France_model$coefficients))
round(France_coeff_table, 4)
addterm(France_model, ~.^2, test = "Chisq")
France_model2 <- stepAIC(France_model, ~.^2)
France_ <-  exp(France_model2$coefficients)

#####################India data
India <- data_w_demographic %>%
  filter(country == "India")
India$age <- as.numeric(India$age)
India_model <- polr(Sys_broke ~ (nativism + age + education + gender + confidence + open + gov_effect),
                     data = India, Hess = TRUE)
summary(France_model)

India_coeff_table <- coef(summary(India_model))
India_p <- pnorm(abs(India_coeff_table[, "t value"]), lower.tail = FALSE) * 2
India_coeff_table <- cbind(India_coeff_table, "p value" = India_p)
India_coeff_table <- cbind(India_coeff_table, "Odds_ration" = exp(India_model$coefficients))
round(India_coeff_table, 4)


############### Turkey data
Turkey <- data_w_demographic %>%
  filter(country == "Turkey")
Turkey$age <- as.numeric(Turkey$age)
Turkey_model <- polr(Sys_broke ~ (nativism + age + education + gender + confidence + open + gov_effect),
                    data = Turkey, Hess = TRUE)

Turkey_coeff_table <- coef(summary(Turkey_model))
Turkey_p <- pnorm(abs(Turkey_coeff_table[, "t value"]), lower.tail = FALSE) * 2
Turkey_coeff_table <- cbind(Turkey_coeff_table, "p value" = India_p)
Turkey_coeff_table <- cbind(Turkey_coeff_table, "Odds_ration" = exp(Turkey_model$coefficients))
round(Turkey_coeff_table, 4)


write.table(Turkey_coeff_table, file = "Turkey.csv", sep = ",", quote = FALSE, row.names = F)

#######inverse code Q9_4
US_nativism$trans_Q9_4 <-  6 - US_nativism$trans_Q9_4
psych::alpha(US_nativism)


France_nativism$trans_Q9_4 <-  6 - France_nativism$trans_Q9_4
psych::alpha(France_nativism)

psych::alpha(Turkey_nativism)

Turkey_nativism$trans_Q9_4 <-  6 - Turkey_nativism$trans_Q9_4





###############################Global trend data
data_global = read.spss("Global Trends.sav", to.data.frame=TRUE)
dataset_labels_global <- as.data.frame(attr(dataset, "variable.labels"))
getwd()

write.table(dataset_labels_global, file="dataset_global_labels.csv",sep=",",row.names=F)
sample_index = sample(1:nrow(data_global), 500)
global_sample <-  data_global[sample_index, ]

missmap(global_sample, main = "Missing values vs observed", srt = 45)


wna = apply(global_sample, 2, anyNA)
wo_na = global_sample[!wna]

wo_na = wo_na[1:50, ]
trans_wo_na = as.data.frame(t(wo_na))


global_labels <- as.data.frame(attr(data_global, "variable.labels"))
write.table(global_labels[!wna], file="global_labels.csv",sep=",",row.names=F)












############################################################

TEST = dataset %>%
   select(MW_Q19_1)
table(TEST)
TEST2 = data_transformed %>%
   select(trans_Q19_1)

table(TEST[,1])
table(data_transformed$trans_Q19_1)
