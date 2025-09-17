# MOBILISENSE ANALYSIS 
# LMM & SENSITIVITY ANALYSIS FIGURES (LINE 43) AND TABLES (LINE 91)
# PUBLISHED ON 17/09/2025
# Code written by Anais Teyton

library(tidyr)
library(ggpubr)
library(ggplot2)
library(rempsyc)
library(dplyr)

##### PREPARE RESULTS FOR VISUALIZATION
dbp<-read.csv("DBP RESULTS CSV LOCATION HERE.csv")
sbp<-read.csv("SBP RESULTS CSV LOCATION HERE.csv")

sbp$order <- rep(1:(nrow(sbp)/2), each = 2)
dbp$order <- rep(1:(nrow(dbp)/2), each = 2)
dbp <- dbp %>%
  separate(X95..CI, into = c('lci', 'uci'), sep = ', ')
sbp <- sbp %>%
  separate(X95..CI, into = c('lci', 'uci'), sep = ', ')

dbp$lci<-as.double(dbp$lci)
dbp$uci<-as.double(dbp$uci)
sbp$lci<-as.double(sbp$lci)
sbp$uci<-as.double(sbp$uci)
sbp$Value<-as.double(sbp$Value)
dbp$Value<-as.double(dbp$Value)

sbp_sd <- subset(sbp, grepl("SD$", exposure))
sbp_no_sd <- subset(sbp, !grepl("SD$", exposure))
dbp_sd <- subset(dbp, grepl("SD$", exposure))
dbp_no_sd <- subset(dbp, !grepl("SD$", exposure))

sbp <- sbp %>%
  mutate(Group = ifelse(grepl('SD', exposure), 'Variability Metric', 'Average'))
dbp <- dbp %>%
  mutate(Group = ifelse(grepl('SD', exposure), 'Variability Metric', 'Average'))

sbp$exposure <- sub(" SD", "", sbp$exposure)
dbp$exposure <- sub(" SD", "", dbp$exposure)

##### FIGURES
all<- bind_rows(sbp, dbp)
avg<-all[all$Group == "Average", ]
custom_order <- c( "2 Hour", "1 Hour", "30 Min", "15 Min", "10 Min", "5 Min")
avg$exposure <- factor(avg$exposure, levels = custom_order)
sd<-all[all$Group == "Variability Metric", ]
custom_order <- c( "2 Hour", "1 Hour", "30 Min", "15 Min", "10 Min", "5 Min")
sd$exposure <- factor(sd$exposure, levels = custom_order)

avg_plot <- ggplot(avg, aes(Value, exposure)) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="solid", color='black') +
  geom_point(size = 2) +
  labs(x = "Coefficient Estimate") +
  geom_errorbar(aes(xmin = lci, xmax = uci), width = 0.1, size = 1) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 0.5) +
  ggpubr::theme_pubclean(flip = TRUE) +
  facet_wrap(~outcome, labeller = label_wrap_gen(width = 22), nrow = 1)+   
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0)) +  
  ggtitle("Average Temperature Exposure Window")  
avg_plot

sd_plot <- ggplot(sd, aes(Value, exposure)) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="solid", color='black') +
  geom_point(size = 2) +
  labs(x = "Coefficient Estimate") +
  geom_errorbar(aes(xmin = lci, xmax = uci), width = 0.1, size = 1) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 0.5) +
  ggpubr::theme_pubclean(flip = TRUE) +
  facet_wrap(~outcome, labeller = label_wrap_gen(width = 22), nrow = 1) + 
  theme(
    axis.title.y = element_blank(),  
    axis.title.x = element_text(size = 10),  
    plot.title = element_text(size = 10, hjust = 0)  
  ) +
  ggtitle("Temperature Variability Metric Exposure Window")  
sd_plot

fig_final <- ggarrange(
  avg_plot,
  sd_plot,
  align = "h",
  ncol = 1,  
  nrow = 2  
)
fig_final

##### TABLES
desired_order <- c("5 Min", "10 Min", "15 Min", "30 Min", "1 Hour", "2 Hour", "5 Min SD", "10 Min SD", "15 Min SD", "30 Min SD", "1 Hour SD", "2 Hour SD")

subset_dbp <- dbp %>%
  select(exposure, Value, lci, uci) 

subset_dbp$exposure <- factor(subset_dbp$exposure, levels = desired_order)

subset_dbp <- subset_dbp %>%
  rename(Exposure=exposure, Coefficient=Value, LCI=lci, UCI=uci) 

subset_sbp <- sbp %>%
  select(exposure, Value, lci, uci) 

subset_sbp$exposure <- factor(subset_sbp$exposure, levels = desired_order)

subset_sbp <- subset_sbp %>%
  rename(Exposure=exposure, Coefficient=Value, LCI=lci, UCI=uci) 

subset_dbptable<-nice_table(subset_dbp)
subset_sbptable<-nice_table(subset_sbp)

subset_dbptable
subset_sbptable

dbp_sens<-read.csv("DBP SENSITIVITY RESULTS CSV LOCATION HERE.CSV")
sbp_sens<-read.csv("SBP SENSITIVITY RESULTS CSV LOCATION HERE.CSV")

dbp_sens <- dbp_sens %>%
  separate(X95..CI, into = c('LCI', 'UCI'), sep = ', ')
sbp_sens <- sbp_sens %>%
  separate(X95..CI, into = c('LCI', 'UCI'), sep = ', ')

dbp_sens <- dbp_sens %>%
  rename(Sensitivity=sensitivity) 
sbp_sens <- sbp_sens %>%
  rename(Sensitivity=sensitivity) 

sbp_sens <- sbp_sens %>%
  select(Sensitivity, Value, LCI, UCI) 
dbp_sens <- dbp_sens %>%
  select(Sensitivity, Value, LCI, UCI)
  
subset_dbptable_sens<-nice_table(dbp_sens)
subset_sbptable_sens<-nice_table(sbp_sens)

subset_dbptable_sens
subset_sbptable_sens
