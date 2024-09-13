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

# epsilon
df <- read.csv("df_power_total.csv")
df[,1:6] <- df[,1:6]/100
df <- subset(df, K=='200000')
df <- subset(df, PRES=="12")
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

difflsmeans(mod, test.effs = "Epsilon", ddf="Kenward-Roger")

plot(difflsmeans(mod, test.effs="TVset"), cex.lab = 2)
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
