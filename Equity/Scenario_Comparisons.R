#===========
#Scenario_Comparisons.R
#===========
#===========
# Description: 
#     This script will use extracted bzone-level data to compare outcomes across equity indices
#     Vtrans methodology: https://icfbiometrics.blob.core.windows.net/vtrans/assets/docs/2020_VTrans_Mid-term_Needs_DRAFT_Technical_Guide.pdf
#     MWCOG methodology: https://www.mwcog.org/assets/1/6/methodology.pdf

#===========


if(!exists('full_census_table_TAZ')){
  source("Data Prep/config.R")
  source("Equity/download_Census_data.R")
}

# Install/Load libraries --------------
source("Data Prep/get_packages.R")
source("Equity/Vtrans_EEA.R")

library(dplyr)
library(tidyverse)
library(tidycensus)
library(viridis)
library(leaflet)
library(readr)
library(sp)
library(sf)
library(rgeos)
library(rlist)
library(ggplot2)
library(RColorBrewer)

# Load data

base_df <- read.csv("Example Data/VDOT_HH_2019_Base_Scenario.csv")
future_df <- read.csv("Example Data/VDOT_HH_2045_Base_Scenario.csv")
future_telework_df <- read.csv("Example Data/VDOT_HH_2045_Telework_Scenario.csv")


base_df_bzone <- base_df %>% 
  group_by(Bzone) %>% 
  summarise(DailyGGE = sum(DailyGGE),
            DailyCO2e = sum(DailyCO2e),
            DVMT = sum(Dvmt), 
            WalkTrips = sum(WalkTrips),
            BikeTrips = sum(BikeTrips),
            TransitTrips = sum(TransitTrips), 
            VehicleTrips = sum(VehicleTrips), 
            Population = sum(HhSize)
  )


future_df_bzone <- future_df %>% 
  group_by(Bzone) %>% 
  summarise(DailyGGE = sum(DailyGGE),
            DailyCO2e = sum(DailyCO2e),
            DVMT = sum(Dvmt), 
            WalkTrips = sum(WalkTrips),
            BikeTrips = sum(BikeTrips),
            TransitTrips = sum(TransitTrips), 
            VehicleTrips = sum(VehicleTrips), 
            Population = sum(HhSize)
  )

future_telework_df_bzone <- future_telework_df %>% 
  group_by(Bzone) %>% 
  summarise(DailyGGE = sum(DailyGGE),
            DailyCO2e = sum(DailyCO2e),
            DVMT = sum(Dvmt), 
            WalkTrips = sum(WalkTrips),
            BikeTrips = sum(BikeTrips),
            TransitTrips = sum(TransitTrips), 
            VehicleTrips = sum(VehicleTrips), 
            Population = sum(HhSize)
  )


# Pull variables of interest, join tables

base_df_bzone_slim <- base_df_bzone  %>%
  mutate(DailyCO2e_2019 = DailyCO2e, 
         DVMT_2019 = DVMT)%>% select(Bzone, DailyCO2e_2019, DVMT_2019)
future_df_bzone_slim <- future_df_bzone%>%
  mutate(DailyCO2e_2045_base = DailyCO2e, 
         DVMT_2045_base = DVMT) %>% select(Bzone, DailyCO2e_2045_base, DVMT_2045_base)
future_telework_df_bzone_slim <- future_telework_df_bzone %>%
  mutate(DailyCO2e_2045_telework = DailyCO2e, 
         DVMT_2045_telework = DVMT)%>% select(Bzone, DailyCO2e_2045_telework, DVMT_2045_telework)

combined_df <- base_df_bzone_slim %>% merge(future_df_bzone_slim) %>%
  merge(future_telework_df_bzone_slim)


combined_df <- left_join(base_df_bzone_slim, future_df_bzone_slim, 
                           by = c("Bzone" = "Bzone"))

combined_df <- left_join(combined_df, future_telework_df_bzone_slim, 
                         by = c("Bzone" = "Bzone"))


# Add VDOT EEA Indices

combined_df <- left_join(combined_df, vtrans_final_table, 
                         by = c("Bzone" = "Bzone"))



write.csv(combined_df, "Example Data/VDOT_scenarios.csv")

# Plotting ---- 

plot_df <- rbind(
  base_df_bzone %>%
    mutate(Year = 2019),
  
  future_df_bzone %>%
    mutate(Year = 2045)

  ) %>%
  left_join(vtrans_final_table)

# with telework :   future_telework_df_bzone %>% mutate(Year = '2045_telework')

plot_df <- plot_df %>%
  group_by(Year, EEA) %>%
  summarize(mean_DailyCO2e = mean(DailyCO2e),
            mean_DVMT = mean(DVMT),
            mean_TransitTrips = mean(TransitTrips),
            sd_DailyCO2e = sd(DailyCO2e),
            sd_DVMT = sd(DVMT),
            sd_TransitTrips = sd(TransitTrips)
  ) %>%
  mutate(EEA = factor(EEA, labels = c('Not EEA', 'EEA')))

ylim_co2 = c(6300, 14510)
ylim_dvmt = c(26000, 34000)
ylim_transit = c(283, 870)


# Base
gp1 <- ggplot(plot_df, aes(x = Year, y = mean_DailyCO2e, color = EEA)) +
  geom_line(size = 2) +
  ylim(ylim_co2) +
  theme(legend.position = "none") +
  ylab('Average daily CO[2]e per household') +
  ggtitle('Daily household CO[2]e emissions \n for VTrans area by EEA')


gp2 <- ggplot(plot_df, aes(x = Year, y = mean_DVMT, color = EEA)) +
  geom_line(size = 2) +
  ylim(ylim_dvmt) + 
  theme(legend.position = "none") +
  ylab('Average DVMT per household') +
  ggtitle('Daily VMT \n for VTrans area by EEA')

gp3 <- ggplot(plot_df, aes(x = Year, y = mean_TransitTrips, color = EEA)) +
  geom_line(size = 2) +
  ylim(ylim_transit) +
  ylab('Average transit trips per household') +
  ggtitle('Daily transit trips \n for VTrans area by EEA')

all_base <- egg::ggarrange(gp1, gp2, gp3, ncol = 3)
ggsave(plot = all_base, filename = 'VTrans_EEA_Scenario_Base.jpeg',
       width = 12, height = 6)


# Telework


plot_df <- rbind(
  base_df_bzone %>%
    mutate(Year = 2019),
  
  future_telework_df_bzone %>%
    mutate(Year = 2045)
  
) %>%
  left_join(vtrans_final_table)


plot_df <- plot_df %>%
  group_by(Year, EEA) %>%
  summarize(mean_DailyCO2e = mean(DailyCO2e),
            mean_DVMT = mean(DVMT),
            mean_TransitTrips = mean(TransitTrips),
            sd_DailyCO2e = sd(DailyCO2e),
            sd_DVMT = sd(DVMT),
            sd_TransitTrips = sd(TransitTrips)
  ) %>%
  mutate(EEA = factor(EEA, labels = c('Not EEA', 'EEA')))

gp1 <- ggplot(plot_df, aes(x = Year, y = mean_DailyCO2e, color = EEA)) +
  geom_line(size = 2) +
  ylim(ylim_co2) +
  ylab('Average daily CO[2]e per household') +
  theme(legend.position = "none") +
  ggtitle('Daily household CO[2]e emissions \n for VTrans area by EEA')


gp2 <- ggplot(plot_df, aes(x = Year, y = mean_DVMT, color = EEA)) +
  geom_line(size = 2) +
  ylim(ylim_dvmt) +
  ylab('Average DVMT per household') +
  theme(legend.position = "none") +
  ggtitle('Daily VMT \n for VTrans area by EEA')

gp3 <- ggplot(plot_df, aes(x = Year, y = mean_TransitTrips, color = EEA)) +
  geom_line(size = 2) +
  ylim(ylim_transit) +
  ylab('Average transit trips per household') +
  ggtitle('Daily transit trips \n for VTrans area by EEA')

all_base <- egg::ggarrange(gp1, gp2, gp3, ncol = 3)
ggsave(plot = all_base, filename = 'VTrans_EEA_Scenario_Telework.jpeg',
       width = 12, height = 6)

