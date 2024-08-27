library(tidyverse)
library(lattice)
library(corrplot)
library(lme4)
library(lmerTest)
library(effects)

# Dundee
path_dundee <- "../data/Dundee.csv"
df_dundee <- read.csv(path_dundee, header = TRUE, sep = ",")
df_dundee$prev_surp <- c(0, head(df_dundee$surp, -1))
df_dundee$prev_freq <- c(0, head(df_dundee$freq, -1))
df_dundee$prev_app <- c(0, head(df_dundee$app, -1))
df_dundee$prev_comp <- c(0, head(df_dundee$comp, -1))
df_dundee$prev_t <- c(0, head(df_dundee$t, -1))
df_dundee$prev_nodecount <- c(0, head(df_dundee$nodecount, -1))
df_dundee <- df_dundee[df_dundee$RT != 0 & df_dundee$is_divided == 0, ]

df_dundee <- df_dundee %>%
  mutate_at(
    vars(
      prev_is_fixed,
      article,
      subject
    ),
    as.factor
  )

df_dundee <- df_dundee %>%
  mutate_at(
    vars(
      app,
      comp,
      t,
      nodecount,
      num_of_word,
      segmentN,
      screenN,
      lineN,
      wlen,
      dep,
      freq,
      surp,
      prev_freq,
      prev_surp,
      prev_app,
      prev_comp,
      prev_t,
      prev_nodecount,
    ),
    scale
  )
length(df_dundee$RT)
plot(density(df_dundee$RT), main = "Dundee Total Reading Times without 0ms", xlab = "RTs (ms)")
plot(density(log(df_dundee$RT)), main = "Dundee Log-Transformed Total Reading Times without 0ms", xlab = "log(RTs (ms))")

nrow(df_dundee)
ncol(df_dundee)
names(df_dundee)
Nsubj <- length(unique(df_dundee$subject))
Ntr <- nrow(df_dundee) / Nsubj

table(df_dundee$subject, df_dundee$article)

hist(with(df_dundee, tapply(RT, subject, mean)),
  main = "Between subject variability",
  xlab = "mean RTs", freq = FALSE
)
hist(with(df_dundee, tapply(RT, article, mean)),
  main = "Between article variability",
  xlab = "mean RTs", freq = FALSE
)

ggplot(data.frame(data = log(df_dundee$RT)), aes(sample = data)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot of Reading Times")

dundee_cor <- df_dundee[, c("app", "comp", "t", "nodecount", "dep", "wlen", "num_of_word", "freq", "prev_freq", "surp")]
corrplot(cor(dundee_cor), tl.col = "black", addCoef.col = "black", method = "circle", type = "upper")


xyplot(log(RT) ~ app | subject,
  group = subject,
  data = df_dundee,
  col = (c("black", "darkgray")),
  xlab = "App", ylab = "Log(RT)"
)

xyplot(log(RT) ~ comp | subject,
  group = subject,
  data = df_dundee,
  col = (c("black", "darkgray")),
  xlab = "Comp", ylab = "Log(RT)"
)

xyplot(log(RT) ~ t | subject,
  group = subject,
  data = df_dundee,
  col = (c("black", "darkgray")),
  xlab = "TR", ylab = "Log(RT)"
)

xyplot(log(RT) ~ nodecount | subject,
  group = subject,
  data = df_dundee,
  col = (c("black", "darkgray")),
  xlab = "NodeCount", ylab = "Log(RT)"
)


baseline <- lmer(
  log(RT) ~ dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_dundee, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
df_dundee <- df_dundee[scale(resid(baseline)) < 3.0, ]
baseline <- lmer(
  log(RT) ~ dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_dundee, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
length(df_dundee$RT)
summary(baseline)

App <- lmer(
  log(RT) ~ app + prev_app + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_dundee, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(App)
eff_app <- Effect("app", App)
plot(eff_app)

Comp <- lmer(
  log(RT) ~ comp + prev_comp + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_dundee, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(Comp)
eff_comp <- Effect("comp", Comp)
plot(eff_comp)

TR <- lmer(
  log(RT) ~ t + prev_t + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_dundee, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(TR)
eff_t <- Effect("t", TR)
plot(eff_t)

NodeCount <- lmer(
  log(RT) ~ nodecount + prev_nodecount + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_dundee, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(NodeCount)
eff_nodecount <- Effect("nodecount", NodeCount)
plot(eff_nodecount)


summary(App)$coefficients[2, ]
summary(Comp)$coefficients[2, ]
summary(TR)$coefficients[2, ]
summary(NodeCount)$coefficients[2, ]

app_p_value <- summary(App)$coefficients[2, 5]
comp_p_value <- summary(Comp)$coefficients[2, 5]
t_p_value <- summary(TR)$coefficients[2, 5]
nodecount_p_value <- summary(NodeCount)$coefficients[2, 5]
p_values <- c(app_p_value, comp_p_value, t_p_value, nodecount_p_value)
corrected_p_values <- p.adjust(p_values, method = "holm")
corrected_p_values


AppComp <- lmer(
  log(RT) ~ app + prev_app + comp + prev_comp + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_dundee, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
CompTR <- lmer(
  log(RT) ~ comp + prev_comp + t + prev_t + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_dundee, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
AppTR <- lmer(
  log(RT) ~ app + prev_app + t + prev_t + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_dundee, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
AppCompTR <- lmer(
  log(RT) ~ app + prev_app + comp + prev_comp + t + prev_t + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_dundee, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)

anova(baseline, App)
anova(Comp, AppComp)
anova(TR, AppTR)
anova(CompTR, AppCompTR)

anova(baseline, Comp)
anova(App, AppComp)
anova(TR, CompTR)
anova(AppTR, AppCompTR)

anova(baseline, TR)
anova(App, AppTR)
anova(Comp, CompTR)
anova(AppComp, AppCompTR)



# BCCWJ
path_bccwj <- "../data/BCCWJ-EyeTrack.csv"
df_bccwj <- read.csv(path_bccwj, header = TRUE, sep = ",")
df_bccwj$prev_surp <- c(0, head(df_bccwj$surp, -1))
df_bccwj$prev_freq <- c(0, head(df_bccwj$freq, -1))
df_bccwj$prev_app <- c(0, head(df_bccwj$app, -1))
df_bccwj$prev_comp <- c(0, head(df_bccwj$comp, -1))
df_bccwj$prev_t <- c(0, head(df_bccwj$t, -1))
df_bccwj$prev_nodecount <- c(0, head(df_bccwj$nodecount, -1))
df_bccwj <- df_bccwj[df_bccwj$RT != 0 & df_bccwj$surp != 0, ]
df_bccwj <- df_bccwj %>%
  mutate_at(
    vars(
      prev_is_fixed,
      article,
      subject
    ),
    as.factor
  )

df_bccwj <- df_bccwj %>%
  mutate_at(
    vars(
      app,
      comp,
      t,
      nodecount,
      num_of_word,
      segmentN,
      screenN,
      lineN,
      wlen,
      dep,
      freq,
      surp,
      prev_freq,
      prev_surp,
      prev_app,
      prev_comp,
      prev_t,
      prev_nodecount,
    ),
    scale
  )
length(df_bccwj$RT)
plot(density(df_bccwj$RT), main = "BCCWJ Total Reading Times without 0ms", xlab = "RTs (ms)")
plot(density(log(df_bccwj$RT)), main = "BCCWJ Log-Transformed Total Reading Times without 0ms", xlab = "log(RTs (ms))")

nrow(df_bccwj)
ncol(df_bccwj)
names(df_bccwj)
Nsubj <- length(unique(df_bccwj$subject))
Ntr <- nrow(df_bccwj) / Nsubj
table(df_bccwj$subject, df_bccwj$article)

hist(with(df_bccwj, tapply(RT, subject, mean)),
  main = "Between subject variability",
  xlab = "mean RTs", freq = FALSE
)
hist(with(df_bccwj, tapply(RT, article, mean)),
  main = "Between article variability",
  xlab = "mean RTs", freq = FALSE
)

ggplot(data.frame(data = log(df_bccwj$RT)), aes(sample = data)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot of Reading Times")

bccwj_cor <- df_bccwj[, c("app", "comp", "t", "nodecount", "dep", "wlen", "surp", "freq", "prev_freq", "num_of_word")]
corrplot(cor(bccwj_cor), tl.col = "black", addCoef.col = "black", method = "circle", type = "upper")

xyplot(log(RT) ~ app | subject,
  group = subject,
  data = df_bccwj,
  col = (c("black", "darkgray")),
  xlab = "App", ylab = "Log(RT)"
)
xyplot(log(RT) ~ comp | subject,
  group = subject,
  data = df_bccwj,
  col = (c("black", "darkgray")),
  xlab = "Comp", ylab = "Log(RT)"
)
xyplot(log(RT) ~ t | subject,
  group = subject,
  data = df_bccwj,
  col = (c("black", "darkgray")),
  xlab = "TR", ylab = "Log(RT)"
)
xyplot(log(RT) ~ nodecount | subject,
  group = subject,
  data = df_bccwj,
  col = (c("black", "darkgray")),
  xlab = "NodeCount", ylab = "Log(RT)"
)


baseline <- lmer(
  log(RT) ~ dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_bccwj, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
df_bccwj <- df_bccwj[scale(resid(baseline)) < 3.0, ]
baseline <- lmer(
  log(RT) ~ dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_bccwj, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
length(df_bccwj$RT)
summary(baseline)

App <- lmer(
  log(RT) ~ app + prev_app + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_bccwj, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(App)
eff_app <- Effect("app", App)
plot(eff_app)

Comp <- lmer(
  log(RT) ~ comp + prev_comp + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_bccwj, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(Comp)
eff_comp <- Effect("comp", Comp)
plot(eff_comp)

TR <- lmer(
  log(RT) ~ t + prev_t + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_bccwj, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(TR)
eff_t <- Effect("t", TR)
plot(eff_t)

NodeCount <- lmer(
  log(RT) ~ nodecount + prev_nodecount + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_bccwj, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
summary(NodeCount)
eff_nodecount <- Effect("nodecount", NodeCount)
plot(eff_nodecount)


summary(App)$coefficients[2, ]
summary(Comp)$coefficients[2, ]
summary(TR)$coefficients[2, ]
summary(NodeCount)$coefficients[2, ]

app_p_value <- summary(App)$coefficients[2, 5]
comp_p_value <- summary(Comp)$coefficients[2, 5]
t_p_value <- summary(TR)$coefficients[2, 5]
nodecount_p_value <- summary(NodeCount)$coefficients[2, 5]
p_values <- c(app_p_value, comp_p_value, t_p_value, nodecount_p_value)
corrected_p_values <- p.adjust(p_values, method = "holm")
corrected_p_values


AppComp <- lmer(
  log(RT) ~ app + prev_app + comp + prev_comp + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_bccwj, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
CompTR <- lmer(
  log(RT) ~ comp + prev_comp + t + prev_t + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_bccwj, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
AppTR <- lmer(
  log(RT) ~ app + prev_app + t + prev_t + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_bccwj, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)
AppCompTR <- lmer(
  log(RT) ~ app + prev_app + comp + prev_comp + t + prev_t + dep + wlen + surp + freq + prev_freq + num_of_word + prev_is_fixed + segmentN + lineN + screenN
    + (1 | article)
    + (1 | subject),
  data = df_bccwj, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)

anova(baseline, App)
anova(Comp, AppComp)
anova(TR, AppTR)
anova(CompTR, AppCompTR)

anova(baseline, Comp)
anova(App, AppComp)
anova(TR, CompTR)
anova(AppTR, AppCompTR)

anova(baseline, TR)
anova(App, AppTR)
anova(Comp, CompTR)
anova(AppComp, AppCompTR)