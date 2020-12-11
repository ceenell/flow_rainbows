
# prelims -----------------------------------------------------------------

library(tidyverse);library(scico);library(lubridate);library(stringr);library(colorspace)
library(readxl);library(dataRetrieval)
library(reshape2);library(zoo)
library(vegan)

theme_set(theme_classic())

# read in data ------------------------------------------------------------

gage_data<-read.csv('data/ref_gages.csv')%>%
  mutate(site_no = str_pad(site_no, 8, side='left', pad=0))

# limit to 100 yr gages
gage_good<-gage_data%>%filter(years_all >= 100)


# clean data --------------------------------------------------------------

## daily discharge data from NWIS for all HCDN gages 
## calcualte rolling means for 3 and 7 day periods
flow <- readRDS('data/gage_flow.rds')%>% 
  mutate(site_no = str_pad(as.character(site_no), 8, side = "left", pad = "0"))%>%
  filter(year_water < 2021 & year_water >= 1920)%>%
  group_by(site_no) %>%
  mutate(flow_7 = rollmean(Flow, k = 7, fill = NA),
         flow_3 = rollmean(Flow, k = 3, fill = NA)) %>%
  ungroup()
str(flow)

## prep for plotting
flow <- flow %>%
  mutate(decade = paste0(substr(year_water, 1, 3), 0))%>%
  mutate(ano = case_when(
    year_water >= 2001 ~ "2020",
    year_water < 2001 & year_water >=1981 ~ "2000",
    year_water < 1981 & year_water >=1961 ~ "1980",
    year_water < 1961 & year_water >=1941 ~ "1960",
    year_water < 1941 & year_water >=1921 ~ "1940"
  ))%>% 
  filter(!is.na(decade)) %>% data.frame() %>%
  transform(yr20 = factor(ano))%>%
  transform(yr20 = fct_relevel(yr20, sort))%>%
  left_join(gage_data, by="site_no")%>%
  filter(site_no %in% gage_good$site_no)
str(flo_raw)
unique(flo_raw$yr20)


## flow extremes based on rolling mean
## 7-day high and low flow 
flow_extreme <- flow %>% 
  group_by(site_no,year_water,yr20, j)%>%
  summarize(flow_high = max(flow_7, na.rm=TRUE), 
         flow_low = min(flow_7, na.rm=TRUE))%>%
  left_join(gage_data, by="site_no")%>%
  filter(site_no %in% gage_good$site_no)

# 7-day high flow for each year
flow_high <- flow %>% 
  group_by(site_no,year_water,yr20, j)%>%
  mutate(flow_high = max(flow_7, na.rm=TRUE))%>%
  left_join(gage_data, by="site_no")%>%
  filter(site_no %in% gage_good$site_no)

# rainbow plots -----------------------------------------------------------

## flow with  7 day conditional mean
flow_rainbows <- function(fpath, color_list){
  
  ggplot(data=flow, aes(x=j, y=Flow)) +
    geom_smooth(method="loess", aes(fill=yr20, group=yr20), color=NA, size=1, alpha=.8, se=T, n=52) +
    scale_fill_manual(values=color_list)+
    theme_void()+
    theme(legend.position="top", plot.margin=margin(0,0,0,0, "pt"), 
          plot.background=element_rect(fill=NA, color=NA),
          strip.text = element_blank(), panel.spacing.x = unit(0, "lines"), panel.spacing.y = unit(-1, "lines"))+
    facet_wrap(~site_no, scales="free_y", ncol=6)
  
  ggsave(paste0('viz/flow_rainbows_',fpath,'.png'), width=12, height=10)
  #ggsave(paste0('viz/flow_rainbows_',fpath,'.svg'), width=16, height=9)
  
}

flow_rainbows('batlow_pink',scico(5, palette="batlow"))
flow_rainbows('hawaii',scico(5, palette="hawaii"))
flow_rainbows('roma',scico(5, palette="roma"))


## plot max flow
flow_rainbows_ext <- function(fpath, color_list){
  
  ggplot(data=flow_extreme, aes(x=j, y=flow_high)) +
    geom_smooth(method="loess", aes(fill=yr20, group=yr20), color=NA, size=1, alpha=.8, se=T, n=365) +
    scale_fill_manual(values=color_list)+
    theme_void()+
    theme(legend.position="top", plot.margin=margin(0,0,0,0, "pt"), 
          plot.background=element_rect(fill=NA, color=NA),
          strip.text = element_blank(), panel.spacing.x = unit(0, "lines"), panel.spacing.y = unit(-1, "lines"))+
    facet_wrap(~site_no, scales="free_y", ncol=6)
  
  ggsave(paste0('viz/flow_rainbows_ext_',fpath,'.png'), width=12, height=10)
  #ggsave(paste0('viz/flow_rainbows_',fpath,'.svg'), width=16, height=9)
  
}
flow_rainbows_ext('batlow',scico(5, palette="batlow"))
flow_rainbows_ext('hawaii',scico(5, palette="hawaii"))
flow_rainbows_ext('roma',scico(5, palette="roma"))


## plot max annual flow trend
flow_rainbows_high <- function(fpath, color_list){
  
  ggplot(data=flow_high, aes(x=j, y=flow_high)) +
    geom_smooth(method="loess", aes(fill=yr20, group=yr20), color=NA, size=1, alpha=.8, se=T, n=365) +
    scale_fill_manual(values=color_list)+
    theme_void()+
    theme(legend.position="top", plot.margin=margin(0,0,0,0, "pt"), 
          plot.background=element_rect(fill=NA, color=NA),
          strip.text = element_blank(), panel.spacing.x = unit(0, "lines"), panel.spacing.y = unit(-1, "lines"))+
    facet_wrap(~site_no, scales="free_y", ncol=6)
  
  ggsave(paste0('viz/flow_rainbows_high_',fpath,'.png'), width=12, height=10)
  #ggsave(paste0('viz/flow_rainbows_',fpath,'.svg'), width=16, height=9)
  
}
flow_rainbows_ext('batlow',scico(5, palette="batlow"))
flow_rainbows_ext('hawaii',scico(5, palette="hawaii"))
flow_rainbows_ext('roma',scico(5, palette="roma"))


