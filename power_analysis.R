rm(list=ls())
library(ggplot2)
library(tidyverse)
library(lmerTest)

# k
df <- read.csv("df_power_total.csv")
df[,1:6] <- df[,1:6]/100
df <- subset(df, K!='200000')
dfeg <- subset(df, Metric=="efficiency.gap")
out <- dfeg[,c(1,7,8,10,11,12)]
names(out)[1] <- "Power"
out$Epsilon <- as.factor(out$Epsilon)


ggplot(out, aes(x=K, y=Power, group=Epsilon, shape=Epsilon, col=Epsilon)) +
  geom_point(size=3) + 
  geom_line() + 
  theme_bw() + 
  scale_color_manual(values=c("red", "blue", "purple")) +
  scale_shape_manual(values=c(1, 2, 4)) +
  theme(axis.title=element_text(size=14),
        axis.text=element_text(size=13), 
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) + 
  theme(legend.position = c(0.7, 0.4)) + 
  xlab('k (in 1000s)') +
  scale_x_continuous(breaks=c(0, 50000, 100000,150000), labels=c(0, 50,100,150))

ggsave('plots/power_analysis/Power_EpsK.pdf', width=6, height=4, dpi=300)


mod <- lm(Power ~ Epsilon*K, data=out)
summary(mod)
#no interactions between epsilon and K

# asymptotic regression Power(k) = a - (a - b) exp(-c k), with k in 1000s, over all three epsilons.
# a is the maximal attainable power. Only k = 1000 lies below the plateau, so b and c are not
# well determined (b ends at its lower bound), but a is: it equals the mean power for k >= 10000.
out$k <- out$K/1000
asym <- nls(Power ~ a - (a - b)*exp(-c*k), data=out, start=list(a=0.7, b=0, c=0.5),
            algorithm="port", lower=c(0, -1, 0), upper=c(1, 1, 10))
summary(asym)
cat("maximal attainable power a =", round(coef(asym)["a"], 3),
    "; mean power for k >= 10000 =", round(mean(out$Power[out$k >= 10]), 3), "\n")

# epsilon
df <- read.csv("df_power_total.csv")
df[,1:6] <- df[,1:6]/100
df <- subset(df, K=='200000')
dfeg <- subset(df, Metric=="efficiency.gap")
dfmm <- subset(df, Metric=="mean.median")
dfpb <- subset(df, Metric=="partisan.bias")
dfpg <- subset(df, Metric=="partisan.gini")
dfss <- subset(df, Metric=="safe.seats")


out <- cbind.data.frame(Power=c(dfeg$efficiency.gap, dfmm$mean.median, dfpb$partisan.bias, dfpg$partisan.gini, dfss$safe.seats),
                        Metric=c(dfeg$Metric, dfmm$Metric, dfpb$Metric, dfpg$Metric, dfss$Metric),
                        Epsilon=c(dfeg$Epsilon, dfmm$Epsilon, dfpb$Epsilon, dfpg$Epsilon, dfss$Epsilon),
                        Year=as.factor(c(dfeg$PRES, dfmm$PRES, dfpb$PRES, dfpg$PRES, dfss$PRES)),
                        index=as.factor(c(dfeg$Index, dfmm$Index, dfpb$Index, dfpg$Index, dfss$Index)),
                        Party=c(dfeg$Party, dfmm$Party, dfpb$Party, dfpg$Party, dfss$Party))


data_means <- out[,c(1:3,4,6)] %>%
  group_by(Epsilon) %>%
  summarize(Mean_Power = mean(Power))

out$Epsilon <- as.factor(out$Epsilon)
mod <- lmer(Power ~ Epsilon+(1|index), data=out)
summary(mod)

# pairwise differences of the least-squares means of the epsilons. Their p-values are not
# adjusted for multiple comparisons, so this is not a Tukey HSD, despite the file name.
difflsmeans(mod, test.effs = "Epsilon", ddf="Kenward-Roger")

plot(difflsmeans(mod, test.effs="Epsilon"), cex.lab = 2)
ggsave('plots/power_analysis/HSDPlot.pdf', width=7, height=4, dpi=300)


# looking at metric, party, year

df <- read.csv("df_power_total.csv")
df[,1:6] <- df[,1:6]/100
df <- subset(df, K=='200000')
df <- subset(df, Epsilon=='0.001')
dfeg <- subset(df, Metric=="efficiency.gap")
dfmm <- subset(df, Metric=="mean.median")
dfpb <- subset(df, Metric=="partisan.bias")
dfpg <- subset(df, Metric=="partisan.gini")
dfss <- subset(df, Metric=="safe.seats")


out <- cbind.data.frame(Power=c(dfeg$efficiency.gap, dfmm$mean.median, dfpb$partisan.bias, dfpg$partisan.gini, dfss$safe.seats),
                        Metric=c(dfeg$Metric, dfmm$Metric, dfpb$Metric, dfpg$Metric, dfss$Metric),
                        Epsilon=c(dfeg$Epsilon, dfmm$Epsilon, dfpb$Epsilon, dfpg$Epsilon, dfss$Epsilon),
                        Year=as.factor(c(dfeg$PRES, dfmm$PRES, dfpb$PRES, dfpg$PRES, dfss$PRES)),
                        index=as.factor(c(dfeg$Index, dfmm$Index, dfpb$Index, dfpg$Index, dfss$Index)),
                        Party=c(dfeg$Party, dfmm$Party, dfpb$Party, dfpg$Party, dfss$Party))

map = data.frame(Metric=c("efficiency.gap", "mean.median","partisan.bias","partisan.gini",'safe.seats'), Metric_=c("Efficiency gap", "Mean median", "Partisan bias",'Partisan gini','Safe seats'))
out <- merge(out,map,by='Metric')
out$Metric <- out$Metric_
out$Metric_ <- NULL
print(out)

# ANOVA of the power of the 20 chains on metric, party and year (main effects)
anova(lm(Power ~ Metric + Party + Year, data=out))

# power tables: mean power over the chains of each metric, year and party, with the standard
# error of that mean. Each chain's power p is a proportion of n = 100 tests, so it has binomial
# variance p(1 - p)/n, and the mean of K chains has standard error sqrt(sum p(1 - p)/n) / K.
for (g in c("Metric", "Year", "Party")) {
  print(out %>% group_by(.data[[g]]) %>%
          summarize(K = n(), Mean_Power = mean(Power), SE = sqrt(sum(Power*(1 - Power)/100))/n(),
                    SD_between_chains = sd(Power)))
}

# Interaction plot

data_means <- out[,c(1:3,4,6)] %>%
  group_by(Metric, Party, Year, Epsilon) %>%
  summarize(Mean_Power = mean(Power))

data_means$Year <- as.numeric(as.character(data_means$Year))

ggplot(data_means, aes(x=Year, y=Mean_Power, col=Metric)) + geom_point(size=3) +
  facet_wrap(.~Party) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks=c(12, 16)) +
  ylab('Mean Power') + 
  theme(axis.text = element_text(size=12), 
        axis.title=element_text(size=14),
        strip.text.x = element_text(size=13),
        legend.title=element_text(size=13),
        legend.text=element_text(size=12)) 

ggsave('plots/power_analysis/InteractionPlots.pdf', width=7, height=4, dpi=300)


# Bias metrics

ggplot(out, aes(x=Metric, y=Power, fill=Metric)) + 
  geom_boxplot() +
  theme_bw() +
  scale_color_manual(values=c("red", "purple", "blue", "darkgreen", "orange"))  + 
  theme(axis.text = element_text(size=10), 
        axis.text.y = element_text(size=12), 
        axis.title=element_text(size=16),
        strip.text.x = element_text(size=20),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12)) 

ggsave('plots/power_analysis/PowerMetrics.pdf', width=7, height=4, dpi=300)
