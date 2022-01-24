# VERSPM Equity Analysis
library(tidyverse)
library(reshape2)
# 1. Join metrics with VERSPM households by bzones. Extract with VERSPM_SCenarios_Extract_For_Equity.R and then move the results to the workign dir here.

# 2. Produce initial figures and plots

if(!exists('geo')){
  source("Data Prep/config.R")
}

# Read in equity measures

equity_meas <- dir(working_dir)[grep('_Equity_Bzones.csv', dir(working_dir))]

equity_meas_compile <- vector()

for(e in equity_meas){
  ex <- read.csv(file.path(working_dir, e))
  
  if(e == equity_meas[1]){
    equity_meas_compile = ex
  } else {
    equity_meas_compile = left_join(equity_meas_compile, ex, by = 'Bzone')
  }
  
}
head(equity_meas_compile)

# Join with household data ----
# hh is the household level output

# Output from `Extract_For_Equity.R`
load(file.path(working_dir, "Single_Run_Complete.RData"))

equity_meas_compile <- equity_meas_compile %>% 
  rename(SVI = RPL_THEMES_clean) %>%
  select(Bzone, SVI, Vtrans_EEA, MWCOG_EEA, PBOT_index) %>%
  mutate(Bzone = as.character(Bzone))

hh <- hh %>%
  left_join(equity_meas_compile %>% select(Bzone, SVI, Vtrans_EEA, MWCOG_EEA, PBOT_index))

hh <- hh %>%
  mutate(Pct_Own_Cost = OwnCost / Income,
         Pct_Own_Cost = ifelse(Pct_Own_Cost > 5, NA, Pct_Own_Cost))

# Summarize by each index

gp <- ggplot(hh, aes(x = SVI, y = Income)) +
  geom_point()

ggsave(filename = 'SVI_Income.jpeg', plot = gp)

gp <- ggplot(hh, aes(x = PBOT_index, y = TransitTrips)) +
  geom_point(alpha = 0.1)

ggsave(filename = 'PBOT_Transit.jpeg', plot = gp)



# Correlation plot ----

d <- equity_meas_compile %>% 
  select(SVI, Vtrans_EEA, MWCOG_EEA, PBOT_index) %>%
  filter(!is.na(SVI)) %>%
  filter(!is.na(Vtrans_EEA))
  
cormat <- round(cor(d),2)

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat, diag = T)]<- NA
  return(cormat)
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)


upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

