##Cleaning
#remove NAs
Travis <- na.omit(Travis)
##Preliminary Analysis
#transit desert index
mean(Travis$td_index)
median(Travis$td_index)
sd(Travis$td_index)
IQR(Travis$td_index)
hist(Travis$td_index)
max(Travis$td_index)
min(Travis$td_index)
#poverty
mean(Travis$E_POV)
median(Travis$E_POV)
sd(Travis$E_POV)
IQR(Travis$E_POV)
hist(Travis$E_POV)
max(Travis$E_POV)
min(Travis$E_POV)
#unemployment
mean(Travis$E_UNEMP)
median(Travis$E_UNEMP)
sd(Travis$E_UNEMP)
IQR(Travis$E_UNEMP)
hist(Travis$E_UNEMP)
max(Travis$E_UNEMP)
min(Travis$E_UNEMP)
#minority
mean(Travis$E_MINRTY)
median(Travis$E_MINRTY)
sd(Travis$E_MINRTY)
IQR(Travis$E_MINRTY)
hist(Travis$E_MINRTY)
max(Travis$E_MINRTY)
min(Travis$E_MINRTY)
#vectors
td_index <- Travis$td_index
E_POV <- Travis$E_POV
E_UNEMP <- Travis$E_UNEMP
E_MINRTY <- Travis$E_MINRTY
#scatterplots
plot(E_POV, td_index, main="Poverty and Transit Score",
     xlab="Number of Impoverished People", ylab="Transit Desert Score")
plot(E_UNEMP, td_index, main="Unemployment and Transit Score",
     xlab="Number of Unemployed People", ylab="Transit Desert Score")
plot(E_MINRTY, td_index, main="Ethnic Minorities and Transit Score",
     xlab="Number of Ethnic Minorities", ylab="Transit Desert Score")
#log distributions
#poverty
log_pov <- log(Travis$E_POV)
hist(log_pov)
mean(log_pov)
median(log_pov)
sd(log_pov)
IQR(log_pov)
#unemployment
log_unemp <- log(Travis$E_UNEMP)
hist(log_unemp)
mean(log_unemp)
median(log_unemp)
sd(log_unemp)
IQR(log_unemp)
#minority
log_minrty <- log(Travis$E_MINRTY)
hist(log_minrty)
mean(log_minrty)
median(log_minrty)
sd(log_minrty)
IQR(log_minrty)
#log scatterplots
plot(log_pov, td_index, main="Poverty and Transit Score",
     xlab="Number of Impoverished People (log transformed)", ylab="Transit Desert Score")
plot(log_unemp, td_index, main="Unemployment and Transit Score",
     xlab="Number of Unemployed People (log transformed)", ylab="Transit Desert Score")
plot(log_minrty, td_index, main="Ethnic Minorities and Transit Score",
     xlab="Number of Ethnic Minorities (log transformed)", ylab="Transit Desert Score")
cor.test(td_index, log_pov)
cor.test(td_index, log_unemp)
cor.test(td_index, log_minrty)
##linear regression analysis
#poverty
pov_Travis <-lm(td_index ~ log_pov, data = Travis)
plot(pov_Travisel, which = 2)
plot(pov_Travisel, which = 1)
summary(pov_Travisel)
confint(pov_Travisel, level = 0.95)
summary(pov_Travisel)$r.squared
plot(log_pov, td_index,
     main = 'poverty vs transit score',
     xlab = 'poverty',
     ylab = 'transit score',
     pch = 20)
abline(pov_Travisel, col='blue')
#unemployment
unemp_Travis <-lm(td_index ~ log_unemp, data = Travis)
plot(unemp_Travisel, which = 2)
plot(unemp_Travisel, which = 1)
summary(unemp_Travisel)
confint(unemp_Travisel, level = 0.95)
summary(unemp_Travisel)$r.squared
plot(log_unemp, td_index,
     main = 'unemployment vs transit score',
     xlab = 'unemployment',
     ylab = 'transit score',
     pch = 20)
abline(unemp_Travisel, col='blue')
#minority
minrty_Travis <-lm(td_index ~ log_minrty, data = Travis)
plot(minrty_Travisel, which = 2)
plot(minrty_Travisel, which = 1)
summary(minrty_Travisel)
confint(minrty_Travisel, level = 0.95)
summary(minrty_Travisel)$r.squared
plot(log_minrty, td_index,
     main = 'minority vs transit score',
     xlab = 'minority',
     ylab = 'transit score',
     pch = 20)
abline(pov_Travisel, col='blue')
##GGplot Texas
install.packages("ggplot2")
library(ggplot2)
ggplot(TX, aes(x=log_pov, y=td_index, color=COUNTY)) +
        geom_point() + labs(title="Transit Score and Poverty for Texas Counties")
ggplot(TX, aes(x=log_unemp, y=td_index, color=COUNTY)) +
        geom_point() + labs(title="Transit Score and Unemployment for Texas Counties") +
        geom_smooth(method=lm)
ggplot(TX, aes(x=log_minrty, y=td_index, color=COUNTY)) +
        geom_point() + labs(title="Transit Score and Minotiry for Texas Counties")
##GGplot US
orig_log_minrty <- log(Original$E_MINRTY)
orig_log_pov <- log(Original$E_POV)
orig_log_unemp <- log(Original$E_UNEMP)
ggplot(Original, aes(x=orig_log_pov, y=td_index, color=ST_ABBR)) +
        geom_point() + labs(title="Transit Score and Poverty for US States")
ggplot(Original, aes(x=orig_log_unemp, y=td_index, color=ST_ABBR)) +
        geom_point() + labs(title="Transit Score and Unemployment for US States") +
        geom_smooth(method=lm)
ggplot(Original, aes(x=orig_log_minrty, y=td_index, color=ST_ABBR)) +
        geom_point() + labs(title="Transit Score and Minotiry for US States")