## INPUTS
## -----------
## outcomes_blckgrp_clean_popweighted.csv

## OUTPUTS
## -----------
## Figure6.pdf

## ==========================
## 1. Loading Libraries
## ==========================

# Function to check and install any missing packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    cat(sprintf("Installing package: '%s'\n", package))
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  } else {
    cat(sprintf("Package '%s' is already installed.\n", package))
  }
}

# List of required packages
packages <- c("ggplot2", "dplyr", "dineq", "patchwork", "reshape", "reshape2", 
              "tidyr", "GGally")

# Apply the function to each package
invisible(sapply(packages, install_and_load))

## ==========================
## Set Main Path (User should modify this only)
## ==========================
main_path <- "path/to/replication"  # User should replace this with the path to the 'replication' folder


# ==========================
## 2. Load Data
## ==========================

# Set working directory to main path
setwd(main_path)

# Define paths to data files
idx_path <- file.path(main_path, "data", "clean", "outcomes_blckgrp_clean_popweighted.csv")

# Read the datasets.
idx = read.csv(idx_path)

#Define Color Palette
colors=c("#d8abe7","#9a024e","#f25d46","#e4ca23","#096f53","#075db4")

## Rename Variables
idx = idx%>%dplyr::rename('median_income' = 'cs_b19013e1')
idx = idx%>%dplyr::rename('total_population' = 'cs_b01001e1')

## subset to just block groups with income information
idx = idx%>%drop_na(median_income) 

## calculate income quartiles, deciles and percentiles for binning block groups by income
no_bins = 11
b_usage = c(unname(quantile(idx$usage_pct_all_cats_home_bg_based*100,(1:no_bins)/no_bins)))
no_bins = 10
b_access = c(unname(quantile(idx$access_idx_crosscity_wt*100,(0:no_bins)/no_bins)))

idx = idx%>%arrange(median_income)%>%group_by(urb_geoid)%>%
    mutate(rk_pctile=cumsum(total_population)/sum(total_population),
           rk_quartile = dineq::ntiles.wtd(median_income,4,total_population),
           rk_decile = dineq::ntiles.wtd(median_income,10,total_population),
           usage_decile = cut(usage_pct_all_cats_home_bg_based*100,breaks = b_usage, include.lowest=TRUE,labels=c(1,2,3,4,5,6,7,8,9,10)),
           access_decile = cut(access_idx_crosscity_wt*100,breaks = b_access,include.lowest=TRUE,labels=c(1,2,3,4,5,6,7,8,9,10)))

idx$psi_100 = idx$psi*100 ## rescale to between 0 and 100
idx = idx%>%filter(!is.na(psi))

## collapse the dataframe so that each block group has a row with within-15 minute PSI and a row with outside-15 minute PSI
psi_inout = idx %>% ungroup() %>% select(c('rk_decile','rk_pctile','rk_quartile','psi_within15', 'psi_outside15', 'bg_geoid'))
psi_inout = reshape::melt(as.data.frame(psi_inout), id=c('rk_decile','rk_pctile','rk_quartile','bg_geoid'))
psi_inout$rk_pctile_10 = psi_inout$rk_pctile*10
psi_inout = psi_inout%>%na.omit()
psi_inout$value_100 = psi_inout$value*100

lowinc_within = mean((psi_inout%>%filter(rk_decile==1)%>%filter(variable == 'psi_within15'))$value)
lowinc_without = mean((psi_inout%>%filter(rk_decile==1)%>%filter(variable == 'psi_outside15'))$value)
hiinc_within = mean((psi_inout%>%filter(rk_decile==10)%>%filter(variable == 'psi_within15'))$value)
hiinc_without = mean((psi_inout%>%filter(rk_decile==10)%>%filter(variable == 'psi_outside15'))$value)
lowestinc_pctdif_withinwithout = (lowinc_within-lowinc_without)/lowinc_within
highestinc_pctdif_withinwithout = (hiinc_within-hiinc_without)/hiinc_within

lowuse_lowinc = mean((idx%>%filter(access_decile==1)%>%filter(rk_quartile == 1))$psi)
hiuse_lowinc = mean((idx%>%filter(access_decile==10)%>%filter(rk_quartile == 1))$psi)
lowuse_hiinc = mean((idx%>%filter(access_decile==1)%>%filter(rk_quartile == 4))$psi)
hiuse_hiinc = mean((idx%>%filter(access_decile==10)%>%filter(rk_quartile == 4))$psi)
lowacc_lowinc = mean((idx%>%filter(usage_decile==1)%>%filter(rk_quartile == 1))$psi)
hiacc_lowinc = mean((idx%>%filter(usage_decile==10)%>%filter(rk_quartile == 1))$psi)
lowacc_hiinc = mean((idx%>%filter(usage_decile==1)%>%filter(rk_quartile == 4))$psi)
hiacc_hiinc = mean((idx%>%filter(usage_decile==10)%>%filter(rk_quartile == 4))$psi)
lowestinc_pctdif_usage = (hiuse_lowinc-lowuse_lowinc)/lowuse_lowinc
highestinc_pctdif_usage = (hiuse_hiinc-lowuse_hiinc)/lowuse_hiinc
lowestinc_pctdif_access = (hiuse_lowinc-lowuse_lowinc)/lowacc_lowinc
highestinc_pctdif_access = (hiacc_hiinc-lowacc_hiinc)/lowacc_hiinc

print(paste('In the lowest income quartile, as usage goes from lowest to highest, PSI changes by: ', round(lowestinc_pctdif_usage,3)*100,'%'))
print(paste('In the highest income quartile, as usage goes from lowest to highest, PSI changes by: ', round(highestinc_pctdif_usage,3)*100,'%'))
print(paste('In the lowest income quartile, the difference between within-15 and outside-15 PSI is: ', round(lowestinc_pctdif_withinwithout,3)*100,'%'))
print(paste('In the highest income quartile, the difference between within-15 and outside-15 PSI is: ', round(highestinc_pctdif_withinwithout,3)*100,'%'))


f <- function(x) {
  r <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  r[3] = mean(x)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


p1 <- ggplot(psi_inout, aes(as.factor(rk_decile),value*100, colour=as.factor(variable), fill=as.factor(variable), shape = as.factor(variable), group=interaction(as.factor(variable),as.factor(rk_decile)))) + 
   ## line connecting binned scatter points
  stat_summary(fun='mean',position=position_dodge2(width=.7,padding=.2),
               size=.5,alpha=1, geom='path', aes(group=as.factor(variable)))+
  ## box plots
  stat_summary( fun.data=f, geom = 'boxplot', position=position_dodge2(width=.7,padding=.2), 
                width=.6, size=.1, alpha=.1,  na.rm=TRUE, aes(group = (interaction(as.factor(variable),as.factor(rk_decile))))) + ## confidence intervals
  ## binned scatter
  stat_summary(fun='mean', position=position_dodge2(width=.7,padding=.2),
               size=1.5,alpha=1, geom='point')+ ## binned scatter
  scale_fill_manual(name = 'version',values=c("#096f53","#f25d46"), labels = c("Trips < 15 minutes away","Trips > 15 minutes away")) +
  scale_color_manual(name = 'version',values=c("#096f53","#f25d46"), labels = c("Trips < 15 minutes away","Trips > 15 minutes away")) +
  scale_shape_manual(name = 'version',values=c(15,16), labels = c("Trips < 15 minutes away","Trips > 15 minutes away")) +
  theme_bw() + 
  labs(
    x = "Income decile",
    y = "Experienced segregation",
  ) +
  theme(legend.position=c(0.99,0.01),
        legend.justification=c("right","bottom"),
        legend.spacing.x = unit(0, 'cm'),
        legend.text=element_text(size=9),
        legend.background = element_rect(fill=alpha('white', .5)),
        legend.title.align=0.5,
        legend.margin=margin(0,0,0,0),
        text=element_text(size=18),
        legend.title = element_blank(),#element_text(color = '#666666', size=16),
        axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        
        plot.margin = margin(
          t = 20, r = 10, b = 10, l = 10, 
          unit = "points"), plot.tag = element_text(size = 20,face='bold')
  )

## downselect to only necessary columns
idx_sub = idx%>%ungroup()%>%select(c(urb,psi,pii,usage_decile,access_decile,usage_pct_all_cats_home_bg_based,access_idx_crosscity_wt,rk_quartile,median_income,total_population))%>%tidyr::drop_na(usage_pct_all_cats_home_bg_based)

no_bins = 11
## identify bin breaks


p2 <- ggplot(idx_sub, aes(usage_decile,psi*100,colour=as.factor(rk_quartile), shape = as.factor(rk_quartile),  fill = as.factor(rk_quartile))) +
  ## line connecting binned scatter points
  stat_summary(fun='mean',position=position_dodge2(width=.7,padding=.3),
               size=.5,alpha=1, geom='path', aes(group=as.factor(rk_quartile)))+
  ## box plots
  stat_summary( fun.data=f, position=position_dodge2(width=.7,padding=.3), 
                width = .6, geom='boxplot', size=.1, alpha=.1,  na.rm=TRUE, aes(group = (interaction(as.factor(rk_quartile),as.factor(usage_decile))))) + ## confidence intervals
  ## binned scatter
  stat_summary(fun='mean', position=position_dodge2(width=.7,padding=.3),
               size=1.5,alpha=1, geom='point')+ ## binned scatter
  scale_fill_manual(name = 'Income rank', values=c("#f25d46","#9a024e","#096f53","#075db4"), labels = c("Low income","Low-moderate","Moderate-high","High income")) +
  scale_color_manual(name = 'Income rank', values=c("#f25d46","#9a024e","#096f53","#075db4"), labels = c("Low income","Low-moderate","Moderate-high","High income")) +
  scale_shape_manual(name = 'Income rank', values = c(1,16,0,15),labels = c("Low income","Low-moderate","Moderate-high","High income")) +
  theme_bw() + 
  labs(
    x = "Usage decile",
    y = "Experienced segregation",
  ) +
  theme(legend.position=c(0.99,0.01),
        legend.justification=c("right","bottom"),
        legend.spacing.x = unit(0, 'cm'),
        legend.text=element_text(size=9),
        legend.background = element_rect(fill=alpha('white', .5)),
        legend.title.align=0.5,
        legend.margin=margin(0,0,0,0),
        text=element_text(size=18),
        legend.title = element_blank(),#(color = '#666666', size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=16),
        plot.margin = margin(
          t = 20, r = 10, b = 10, l = 10, 
          unit = "points"), plot.tag = element_text(size = 20,face='bold')
  )

no_bins = 10
## identify bin breaks
p3 <- ggplot(idx_sub, aes(access_decile,psi*100,colour=as.factor(rk_quartile), shape = as.factor(rk_quartile),  fill = as.factor(rk_quartile))) +
  ## line connecting binned scatter points
  stat_summary(fun='mean',position=position_dodge2(width=.7,padding=.3),
               size=.5,alpha=1, geom='path', aes(group=as.factor(rk_quartile)))+
  stat_summary( fun.data=f, position=position_dodge2(width=.7,padding=.3), 
                width = .6, geom='boxplot', size=.1, alpha=.1,  na.rm=TRUE, aes(group = (interaction(as.factor(rk_quartile),as.factor(access_decile))))) + ## confidence intervals
  stat_summary(fun='mean', position=position_dodge2(width=.7,padding=.3),
               size=1.5,alpha=1, geom='point')+ ## binned scatter
  scale_fill_manual(name = 'Income rank', values=c("#f25d46","#9a024e","#096f53","#075db4"), labels = c("Low income","Low-moderate","Moderate-high","High income")) +
  scale_color_manual(name = 'Income rank', values=c("#f25d46","#9a024e","#096f53","#075db4"), labels = c('Low income',"Low-moderate","Moderate-high","High income")) +
  scale_shape_manual(name = 'Income rank', values = c(1,16,0,15),labels = c('Low income',"Low-moderate","Moderate-high","High income")) +
  theme_bw() + 
  labs(
    x = "Access decile",
    y = "Experienced segregation",
  ) +
  theme(legend.position=c(0.99,0.01),
        legend.justification=c("right","bottom"),
        legend.spacing.x = unit(0, 'cm'),
        legend.text=element_text(size=9),
        legend.box.just="right",
        legend.background = element_rect(fill=alpha('white', .5)),
        legend.title.align=0.5,
        legend.margin=margin(0,0,0,0),
        text=element_text(size=18),
        legend.title = element_blank(),#(color = '#666666', size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=16),
        plot.margin = margin(
          t = 20, r = 10, b = 10, l = 10, 
          unit = "points"), plot.tag = element_text(size = 20,face='bold'))

p3 + p2 + p1 + 
  plot_annotation(tag_levels = 'a') 
ggsave(paste('output/figure_6.pdf',sep=''),width=12,height=5,dpi=300,device='pdf')



