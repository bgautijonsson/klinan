# Libraries
library(foreign)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(gridExtra)
library(dplyr)
library(tidyr)
library(broom)
library(stringr)
library(multcomp)
library(car)
source('apatheme.R')
library(jtools)


# Reading the data
df <- read.spss('A.2___Thunglyndi___Thunglyndisthankar_og_obeldi.sav', 
                  to.data.frame = TRUE)
 
unique(df$Abuse)

levels(df$Abuse) <- c('Engin Saga', 'Likamlegt og/eða Kynferðislegt',
                      'Andlegt ofbeldi')
names(df)
colnames(df)[2] <- 'Depression'
colnames(df)[1] <- 'Ofbeldi'
levels(df$Depression) <- c('Ekki saga', 'Fyrri saga')
df$RRStot <- df$RRSbrood + df$RRSrefle

# Exploratory Analysis

## Visualization

### Density Plots
g <- ggplot(data = df, aes(col = Depression, fill = Depression)) + apatheme +
    facet_grid(~Ofbeldi) + guides(fill=guide_legend(title="Þunglyndi"),
                                  col = guide_legend(title='Þunglyndi'))

g1 <- g + geom_density(aes(x=BDItot), alpha = 0.5) + labs(x='BDI', y='Dreyfing')
g2 <- g + geom_density(aes(x=BAItot), alpha = 0.5) + labs(x='BAI', y='Dreyfing')
g3 <- g + geom_density(aes(x=RRSbrood), alpha = 0.5) + labs(x='Brooding', y='Dreyfing')
g4 <- g + geom_density(aes(RRSrefle), alpha = 0.5) + labs(x='Reflection', y='Dreyfing')


save1 <- grid.arrange(g1, g2, nrow = 2)
#ggsave('samanburdurDreyfing1.pdf', plot = save1, device = 'pdf')

save2 <- grid.arrange(g3, g4, nrow=2)
#ggsave('samanburdurDreyfing2.pdf', plot = save2, device = 'pdf')


### RRSplots    
p1 <- ggplot(df, aes(x = Depression, y = RRSbrood)) + geom_boxplot() + apatheme +
    labs(x = 'Þunglyndi', y='Brooding skor')

p2 <- ggplot(df, aes(x = Depression, y = RRSrefle)) + geom_boxplot() + apatheme +
    labs(x = 'Þunglyndi', y='Reflection skor')

p3 <- ggplot(df, aes(x = RRSbrood, y = BDItot)) + geom_smooth(method = 'lm') + 
    apatheme + labs(x = 'Brooding skor', y = 'BDI skor') + geom_point()

p4 <- ggplot(df, aes(x = RRSrefle, y = BDItot)) + geom_smooth(method = 'lm') + 
    apatheme + labs(x = 'Reflection skor', y = 'BDI skor') + geom_point()

RRSplots <- grid.arrange(p1, p2, p3, p4, nrow=2)
#ggsave(filename = 'RRSplots.pdf', plot = RRSplots, device = 'pdf', 
       #width = 9, height = 7)

# Ofbeldi Plots

o1 <- ggplot(df, aes(x=Ofbeldi, fill = Depression)) + labs(y='Fjöldi') +
    geom_bar(stat='count', position = 'dodge') + apatheme +
    theme(legend.position = c(0.50, 0.85)) + 
    guides(fill=guide_legend(title="Þunglyndi")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
o1

o2 <- ggplot(df, aes(x=Ofbeldi, y = BDItot, col=Ofbeldi)) + geom_boxplot() + apatheme +
    geom_jitter() + labs(y='BDI Skor') + guides(col=FALSE) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
o2

Ofbeldiplots <- grid.arrange(o1, o2, nrow=1)
#ggsave(filename = 'Ofbeldiplots.pdf', plot = Ofbeldiplots, device = 'pdf', width = 9, height = 7)

# Öll plots


allplots <- grid.arrange(p1, p3, p2, p4, o2, o1, nrow=3)
ggsave(filename = 'allplots.pdf', plot = allplots, device = 'pdf', width = 9, height = 7)

# Boxline Plots

l1 <- ggline(data = df, x = 'Ofbeldi', y = 'RRSbrood', color = 'Depression',
       add = c('mean_se', 'dotplot'), palette = c("#00AFBB", "#E7B800")) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    ylab('Brooding') + apatheme + theme(legend.position = c(0.15, 0.85)) +
    guides(fill=guide_legend(title="Þunglyndi"), col = guide_legend(title='Þunglyndi'))
l2 <- ggline(data = df, x = 'Ofbeldi', y = 'RRSrefle', color = 'Depression',
             add = c('mean_se', 'dotplot'), palette = c("#00AFBB", "#E7B800")) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    ylab('Reflection') + apatheme + theme(legend.position = 'none') +
    guides(fill=guide_legend(title="Þunglyndi"), col = guide_legend(title='Þunglyndi'))
l3 <- ggline(data = df, x = 'Ofbeldi', y = 'BDItot', color = 'Depression',
             add = c('mean_se', 'dotplot'), palette = c("#00AFBB", "#E7B800")) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    ylab('BDI') + apatheme + theme(legend.position = 'none') +
    guides(fill=guide_legend(title="Þunglyndi"),col = guide_legend(title='Þunglyndi'))
l4 <- ggline(data = df, x = 'Ofbeldi', y = 'BAItot', color = 'Depression',
             add = c('mean_se', 'dotplot'), palette = c("#00AFBB", "#E7B800")) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
    ylab('BAI') + apatheme + theme(legend.position = 'none') +
    guides(fill=guide_legend(title="Þunglyndi"),col = guide_legend(title='Þunglyndi'))

lineplots <- grid.arrange(l1, l2, l3, l4)
#ggsave(filename = 'lineplots.pdf', plot = lineplots, device = 'pdf', width = 9, height = 7)

#

prop.table(table(df$Ofbeldi, df$Depression))
means <- colMeans(df[,5:8])
sd <- apply(df[,5:8], 2, sd)

gruppur <- df %>%
    group_by(Depression, Ofbeldi) %>%
    summarise(mean(BDItot), mean(BAItot), mean(RRSbrood), mean(RRSrefle),
              sd(BDItot), sd(BAItot), sd(RRSbrood), sd(RRSrefle))
gruppur

RRSbrood <- df %>%
    group_by(Ofbeldi) %>%
    do(tidy(t.test(RRSbrood ~ Depression, data=.)))
RRSbrood <- RRSbrood[,c(1,5,6,8,9)]
RRSbrood$prof <- 'RRSbrood'
RRSrefle <- df %>%
    group_by(Ofbeldi) %>%
    do(tidy(t.test(RRSrefle ~ Depression, data=.)))
RRSrefle <- RRSrefle[,c(1,5,6,8,9)]
RRSrefle$prof <- 'RRSrefle'
BDItot <- df %>%
    group_by(Ofbeldi) %>%
    do(tidy(t.test(BDItot ~ Depression, data=.)))
BDItot <- BDItot[,c(1,5,6,8,9)]
BDItot$prof <- 'BDItot'
BAItot <- df %>%
    group_by(Ofbeldi) %>%
    do(tidy(t.test(BAItot ~ Depression, data=.)))
BAItot <- BAItot[,c(1,5,6,8,9)]
BAItot$prof <- 'BAItot'

prof <- rbind(RRSbrood, RRSrefle, BDItot, BAItot)

options(scipen = 2)
pdf(file = 'DepressionVsNotTafla.pdf', width = 8)
grid.table(prof, rows=NULL)
dev.off()


# ANOVA

anovaBDI <- aov(BDItot ~ Depression + Ofbeldi + Depression:Ofbeldi, data = df)
anovaBAI <- aov(BAItot ~ Depression + Ofbeldi + Depression:Ofbeldi, data = df)
anovaBrood <- aov(RRSbrood ~ Depression + Ofbeldi + Depression:Ofbeldi, data = df)
anovaRefle <- aov(RRSrefle ~ Depression + Ofbeldi + Depression:Ofbeldi, data = df)
summary(anovaBDI)
summary(anovaBAI)
summary(anovaBrood)
summary(anovaRefle)

capture.output(summary(anovaBDI), summary(anovaBAI), summary(anovaBrood),
               summary(anovaRefle), file = 'anova.doc')


options(scipen = 999)
anovas <- data.frame('Refle' = summary(anovaRefle)[[1]]['Pr(>F)'],
                     'Brood' = summary(anovaBrood)[[1]]['Pr(>F)'],
                     'BDI' = summary(anovaBDI)[[1]]['Pr(>F)'],
                     'BAI' = summary(anovaBAI)[[1]]['Pr(>F)'])
anovas <- anovas[1:3,]

names(anovas) <- c('RRSrefle', 'RRSbrood', 'BDItot', 'BAItot')

# Samanburður á ofbeldissögu
TukeyHSD(anovaBrood, which='Ofbeldi')
TukeyHSD(anovaRefle, which='Ofbeldi')

# Skoðun á samverkandi áhrifum í RRSbrood
TukeyHSD(anovaBrood, conf.level = 0.05, which='Depression:Ofbeldi')

# Skoða normaldreyfingu

## RRSbrood ekki normaldreyft eftir Ofbeldi og Depression
leveneTest(RRSbrood ~ Ofbeldi*Depression, data = df)
shapiro.test(x = residuals(object=anovaBrood))
plot(anovaBrood, 2)
prop.table(table(df$Ofbeldi, df$Depression))

Anova(anovaBrood, type='II')
summary(aov(RRSbrood ~ Depression:Ofbeldi, data = df))
options(scipen = 999)
summary(lm(RRSbrood ~ Depression + Ofbeldi - 1 + Depression*Ofbeldi , 
           df))$coef


# Predictive Model

# Þunglyndisgreining próf
fit <- glm(Depression ~ Ofbeldi + RRSbrood + RRSrefle, data=df, family = 'binomial')
fit2 <- glm(Depression ~ Ofbeldi, data = df, family = 'binomial')
fit3 <- glm(Depression ~ RRSbrood + RRSrefle, data = df, family = 'binomial')
fit4 <- glm(Depression ~ RRStot + Ofbeldi + RRStot*Ofbeldi, data = df, family = 'binomial')
summary(fit4)
exp(fit4$coefficients)
exp(confint(fit4))
anova(fit, test = 'Chisq')
anova(fit2, fit, test = 'Chisq')
anova(fit3, fit, test = 'Chisq')

plot(fit, 1)
plot(fit, 2)

# BDI skor próf
lm <- lm(BDItot ~ Ofbeldi + RRSbrood + RRSrefle, data = df)
lm2 <- lm(BDItot ~ Ofbeldi, data = df)
lm3 <- lm(BDItot ~ RRSbrood + RRSrefle, data = df)
lm4 <- lm(BDItot ~ RRStot + Ofbeldi + RRStot*Ofbeldi, data = df)
summary(lm4)
confint(lm4)
anova(lm4, test='Chisq')
anova(lm2, lm, test = 'Chisq')
anova(lm3, lm, test = 'Chisq')



qplot(lm$fitted.values, lm$residuals) + apatheme + geom_smooth(method = 'lm') +
    labs(x='Spágildi', y='Leif')

qplot(fit$fitted.values, fit$residuals) + apatheme + geom_smooth(method = 'lm') +
    labs(x='Spágildi', y='Leif')

