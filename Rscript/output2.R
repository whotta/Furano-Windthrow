# ----------------------------------
# Make result figures with errorbar
# W.Hotta
# Date: 2020/11/11
# ----------------------------------

# Load packages
library(tidyverse)
library(raster)
library(rgdal)
library(viridis)
library(ggthemes)
library(patchwork)

total_biom0 <- raster('111_current__historicWindthrow__WT/1/OutputMaps/biomass/biomass-TotalBiomass-0.img')
total_biom0_df <- as.data.frame(total_biom0, xy=TRUE)
total_biom0_df %>% 
  filter(biomass.TotalBiomass.0 != 0) %>% 
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = biomass.TotalBiomass.0)) +
  theme_bw() +
  coord_fixed()

cases <- c('111_current__historicWindthrow__WT', '112_current__historicWindthrow__SL', '113_current__historicWindthrow__SLSC',
           '121_current__frequent__WT', '122_current__frequent__SL', '123_current__frequent__SLSC',
           '131_current__intense__WT', '132_current__intense__SL', '133_current__intense__SLSC',
           '141_current__frequentintense__WT', '142_current__frequentintense__SL', '143_current__frequentintense__SLSC',
           '211_CSIRO-Mk3-6-0_RCP2.6__historicWindthrow__WT', '212_CSIRO-Mk3-6-0_RCP2.6__historicWindthrow__SL', '213_CSIRO-Mk3-6-0_RCP2.6__historicWindthrow__SLSC',
           '221_CSIRO-Mk3-6-0_RCP2.6__frequent__WT', '222_CSIRO-Mk3-6-0_RCP2.6__frequent__SL', '223_CSIRO-Mk3-6-0_RCP2.6__frequent__SLSC',
           '231_CSIRO-Mk3-6-0_RCP2.6__intense__WT', '232_CSIRO-Mk3-6-0_RCP2.6__intense__SL', '233_CSIRO-Mk3-6-0_RCP2.6__intense__SLSC',
           '241_CSIRO-Mk3-6-0_RCP2.6__frequentintense__WT', '242_CSIRO-Mk3-6-0_RCP2.6__frequentintense__SL', '243_CSIRO-Mk3-6-0_RCP2.6__frequentintense__SLSC',
           '311_GFDL-CM3_RCP8.5__historicWindthrow__WT', '312_GFDL-CM3_RCP8.5__historicWindthrow__SL','313_GFDL-CM3_RCP8.5__historicWindthrow__SLSC',
           '321_GFDL-CM3_RCP8.5__frequent__WT', '322_GFDL-CM3_RCP8.5__frequent__SL', '323_GFDL-CM3_RCP8.5__frequent__SLSC',
           '331_GFDL-CM3_RCP8.5__intense__WT', '332_GFDL-CM3_RCP8.5__intense__SL', '333_GFDL-CM3_RCP8.5__intense__SLSC',
           '341_GFDL-CM3_RCP8.5__frequentintense__WT', '342_GFDL-CM3_RCP8.5__frequentintense__SL', '343_GFDL-CM3_RCP8.5__frequentintense__SLSC')

# Read log files ----------------------------------------------------------------------------------
necn_sum_dfs <- list()
necn_dfs <- list()
spp_dfs <- list()
regen_dfs <- list()
salvage_dfs <- list()
iter <- 1

for (case in cases) {
  dir_names <- list.dirs(path = file.path(case), recursive = F)
  for (dir_name in dir_names) {
    # necn_sum_dfs[[iter]] <- read_csv(paste0(dir_name, '/NECN-succession-log-short.csv')) %>%
    #   mutate(scenario = case,
    #          dir_name = dir_name)
    necn_dfs[[iter]] <- read_csv(paste0(dir_name, '/NECN-succession-log.csv')) %>%
      mutate(scenario = case,
             dir_name = dir_name)
    # spp_dfs[[iter]] <- read_csv(paste0(dir_name, '/spp-biomass-log.csv')) %>%
    #   mutate(scenario = case,
    #          dir_name = dir_name)
    # regen_dfs[[iter]] <- read_csv(paste0(dir_name, '/NECN-reproduction-log.csv')) %>% 
    #   mutate(scenario = case,
    #          dir_name = dir_name)
    # salvage_dfs[[iter]] <- read_csv(paste0(dir_name, '/biomass-harvest-summary-log.csv')) %>% 
    #   mutate(scenario = case,
    #          dir_name = dir_name)
    iter <- iter + 1
  }
  
}

# Read raster files ----------------------------------------------------------------------------------
wind_dfs <- list()
biomrem_dfs <- list()
biom_dfs <- list()
iter <- 1
for (case in cases) {
  i2 <- 1
  wind_ras <- list()
  wind_ras_dfs <- list()
  biomrem_ras <- list()
  biomrem_ras_dfs <- list()
  for (i in c(15, 40, 65, 90)) {
    id <- as.data.frame(raster(paste0(case, '/1/OutputMaps/harvest/prescripts-1.img')), xy = TRUE) %>% 
      dplyr::select(-starts_with('prescript'))
    wind_ras[[i2]] <- raster(paste0(case, '/1/OutputMaps/harvest/prescripts-', i, '.img'))
    wind_ras_dfs[[i2]] <- as.data.frame(wind_ras[[i2]])
    biomrem_ras[[i2]] <- raster(paste0(case, '/1/OutputMaps/harvest/biomass-removed-', i, '.img'))
    biomrem_ras_dfs[[i2]] <- as.data.frame(biomrem_ras[[i2]])
    i2 <- i2 + 1
  }
  wind_dfs[[iter]] <- bind_cols(id, wind_ras_dfs) %>% 
    mutate(scenario = case)
  biomrem_dfs[[iter]] <- bind_cols(id, biomrem_ras_dfs) %>% 
    mutate(scenario = case)
  
  i3 <- 1
  biom_ras <- list()
  biom_ras_dfs <- list()
  for (j in 1:105) {
    id <- as.data.frame(raster(paste0(case, '/1/OutputMaps/biomass/biomass-TotalBiomass-1.img')), xy = TRUE) %>%
      dplyr::select(-starts_with('biomass'))
    biom_ras[[i3]] <- raster(paste0(case, '/1/OutputMaps/biomass/biomass-TotalBiomass-', j, '.img'))
    biom_ras_dfs[[i3]] <- as.data.frame(biom_ras[[i3]])
    i3 <- i3 + 1
  }
  biom_dfs[[iter]] <- bind_cols(id, biom_ras_dfs) %>%
    mutate(scenario = case)
  
  # necn_sum_dfs[[iter]] <- read_csv(paste0(case, '/NECN-succession-log-short.csv')) %>% 
  #   mutate(scenario = case)
  # necn_dfs[[iter]] <- read_csv(paste0(case, '/NECN-succession-log.csv')) %>% 
  #   mutate(scenario = case)
  # spp_dfs[[iter]] <- read_csv(paste0(case, '/spp-biomass-log.csv')) %>% 
  #   mutate(scenario = case)
  iter <- iter + 1
}

# Windthrow area ---------------------------------------------------------------------------
wind_df <- bind_rows(wind_dfs) %>% 
  gather(key = windyear, value = wind, 
         starts_with('prescripts')) %>% 
  mutate(windyear = str_replace(windyear, pattern = 'prescripts.', replacement = ''),
         windthrow = case_when(wind == 1 ~ 'No',
                               wind == 2 ~ 'Yes',
                               TRUE ~ 'nonactive'))
wind_df$windyear <- factor(wind_df$windyear,
                           levels = c('15', '40', '65', '90'))
wind_df$wind <- as.factor(wind_df$wind)
wind_df$windthrow <- factor(wind_df$windthrow,
                            levels = c('Yes', 'No', 'nonactive'))

wind_df %>% 
  filter(wind != 0) %>% 
  filter(scenario %in% c('111_current__historicWindthrow__WT', '112_current__historicWindthrow__SL', '113_current__historicWindthrow__SLSC',
                         '121_current__frequent__WT', '122_current__frequent__SL', '123_current__frequent__SLSC',
                         '131_current__intense__WT', '132_current__intense__SL', '133_current__intense__SLSC',
                         '141_current__frequentintense__WT', '142_current__frequentintense__SL', '143_current__frequentintense__SLSC')) %>% 
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = windthrow)) +
  facet_grid(windyear~scenario) +
  theme_bw() +
  theme(strip.text.x = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(color = 'black', fill = 'white')) +
  coord_fixed() +
  scale_fill_viridis(discrete = T)
#scale_fill_colorblind()
#scale_fill_pander()
#scale_fill_brewer(palette = 'Set1')

windthrow_area <- wind_df %>% 
  mutate(count = 1) %>% 
  group_by(scenario, windyear, wind) %>% 
  summarise(area_ha = sum(count)) %>% 
  spread(key = wind, value = area_ha) %>% 
  mutate(wind_ratio = `2` / (`1`+`2`))

# biomass removed ---------------------------------------------------------------------------
biomrem_df <- bind_rows(biomrem_dfs) %>% 
  gather(key = windyear, value = biomassremoved, 
         starts_with('biomass')) %>% 
  mutate(windyear = str_replace(windyear, pattern = 'biomass.removed.', replacement = ''),
         biomassremoved = biomassremoved/100)
biomrem_df$windyear <- factor(biomrem_df$windyear,
                              levels = c('15', '40', '65', '90'))

biomrem_df %>% 
  #filter(wind != 0) %>% 
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = biomassremoved)) +
  facet_grid(windyear~scenario) +
  theme_bw() +
  coord_fixed() +
  scale_fill_viridis(discrete = F)
#scale_fill_colorblind()
#scale_fill_pander()
#scale_fill_brewer(palette = 'Set3')

# biomass total ---------------------------------------------------------------------------
biom_df <- bind_rows(biom_dfs) %>% 
  gather(key = year, value = biomass, 
         starts_with('biomass')) %>% 
  mutate(year = str_replace(year, pattern = 'biomass.TotalBiomass.', replacement = ''),
         biomass = biomass/100)

biom_df$year <- as.numeric(biom_df$year)

biom_df %>% 
  filter(biomass != 0,
         year %in% c(14, 39, 64, 89, 105)) %>% 
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = biomass)) +
  facet_grid(year~scenario) +
  theme_bw() +
  coord_fixed() +
  scale_fill_viridis(discrete = F)
#scale_fill_colorblind()
#scale_fill_pander()
#scale_fill_brewer(palette = 'Set3')

# spp log ----------------------------------------------------------------------
spp_df<- bind_rows(spp_dfs) %>% 
  gather(key = sppname, val = agb, starts_with('AboveGroundBiomass_')) %>% 
  filter(NumActiveSites != 0) %>% 
  mutate(sppname = str_replace(sppname, pattern = 'AboveGroundBiomass_', replacement = ''),
         agbtotal = as.numeric(agb) * NumActiveSites) %>% 
  group_by(scenario, dir_name, Time, sppname) %>% 
  summarise(AGB = sum(agbtotal)/12169) %>% 
  ungroup() %>% 
  group_by(scenario, Time, sppname) %>% 
  summarise(meanAGB = mean(AGB)/100,
            sdAGB = sd(AGB/100),
            seAGB = sd(AGB/100)/sqrt(length(AGB)))


# Make scenario index
scenario_index <- str_split(spp_df$scenario, pattern = "__", simplify = TRUE)
scenario_index_df <- as.data.frame(scenario_index)
scenario_index2 <- str_split(scenario_index_df$V1, pattern = "_", simplify = TRUE, n = 2)
scenario_index2_df <- as.data.frame(scenario_index2)
index_df <- bind_cols(scenario_index_df, scenario_index2_df) %>% 
  dplyr::select(V1...4, V2...5, V2...2, V3) %>% 
  rename(scenario_no = V1...4,
         Climate = V2...5,
         WindthrowRegime = V2...2,
         PostWindthrowManagement = V3)
# merge spp_df & scenario index
spp_df2 <- bind_cols(spp_df, index_df) %>% 
  mutate(sppname2 = case_when(sppname %in% c('Picejezo', 'Picegleh') ~ 'Picea spp.',
                              sppname %in% c('Betuerma', 'Betumaxi') ~ 'Betula spp.',
                              TRUE ~ sppname))
spp_df2$WindthrowRegime <- factor(spp_df2$WindthrowRegime,
                                  levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'),
                                  labels = c('Historical', 'Frequent', 'Intense', 'Frequent & Intense'))
spp_df2$PostWindthrowManagement <- factor(spp_df2$PostWindthrowManagement,
                                          levels = c('WT', 'SL', 'SLSC'))
spp_df2$sppname2 <- factor(spp_df2$sppname2, 
                         levels = c("Abiesach", "Picea spp.", "Betula spp.",
                                    "Quermong", "Aceritaya", "Tilijapo", "Fraxmand", "Kalopict", 
                                    "Ulmulaci", "sasa_spp"),
                         labels = c('Abies sachalinensis', 'Picea spp.',
                                    'Betula spp.', 
                                    'Quercus crispula', 'Acer pictum',
                                    'Tilia japonica', 'Fraxinus mandshurica', 'Kalopanax septemlobus', 'Ulmus laciniata', 'Sasa senanensis'))
spp_df2$sppname <- factor(spp_df2$sppname, 
                           levels = c("Abiesach", "Picejezo", "Picegleh", "Betuerma", "Betumaxi",
                                      "Fraxmand", "Kalopict", "Quermong", "Aceritaya", "Tilijapo", 
                                      "Ulmulaci", "sasa_spp"),
                          labels = c('Abies sachalinensis', 'Picea jezoensis', 'Picea glehnii',
                                     'Betula ermanii', 'Betula maximowicziana', 'Fraxinus mandshurica',
                                     'Kalopanax septemlobus', 'Quercus crispula', 'Acer pictum',
                                     'Tilia japonica', 'Ulmus laciniata', 'Sasa senanensis'))
spp_df2$Climate <- factor(spp_df2$Climate, 
                          levels = c("current", "CSIRO-Mk3-6-0_RCP2.6", "GFDL-CM3_RCP8.5"),
                          labels = c("Current", "RCP2.6 (CSIRO-Mk3-6-0)", "RCP8.5 (GFDL-CM3)"))
# Figure species composition
s1 <- spp_df2 %>% 
  filter(Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = sppname), 
           position = 'stack', stat = 'identity', alpha = 1) +
  #geom_area(aes(x = PostWindthrowManagement, y = meanAGB, fill = sppname), color = "black", alpha = 0.7) +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(legend.text = element_text(face = "italic"),
        legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        axis.title.x = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = 'Set3') +
  #ggtitle('2130') +
  ylab(expression('Aboveground biomass (Mg' ~ ha^-1* ')')) +
  ylim(0,300)

ggsave('spp_agb_2130.tiff', s1, dpi = 600, width = 11.32, height = 7)

s2 <- spp_df2 %>% 
  filter(Time == 14, PostWindthrowManagement == 'WT',
         WindthrowRegime == 'Historical') %>% 
  ggplot() +
  geom_bar(aes(x = NA, y = meanAGB, fill = sppname), 
           position = 'stack', stat = 'identity', alpha = 1) +
  #geom_area(aes(x = PostWindthrowManagement, y = meanAGB, fill = sppname), color = "black", alpha = 0.7) +
  facet_grid(Climate ~ Time) +
  theme_bw(base_size = 15) +
  theme(legend.position = 'none',
        strip.text.x = element_text(colour = 'white'),
        strip.text.y = element_blank(),
        strip.background = element_rect(fill = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = 'Set3') +
  ggtitle('2029')  +
  ylab(expression('Aboveground biomass (Mg' ~ ha^-1* ')')) +
  ylim(0,300)

s12 <- s2 + s1 +
  plot_layout(ncol = 2, widths = c(1,8))

s3 <- spp_df2 %>% 
  filter(Time == 14, PostWindthrowManagement == 'WT',
         WindthrowRegime == 'Historical') %>% 
  ggplot() +
  geom_bar(aes(x = NA, y = meanAGB, fill = sppname), 
           position = 'fill', stat = 'identity', alpha = 0.8, color = 'grey20') +
  #geom_area(aes(x = PostWindthrowManagement, y = meanAGB, fill = sppname), color = "black", alpha = 0.7) +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(legend.position = 'none',
        strip.text.x = element_text(colour = 'white'),
        strip.text.y = element_blank(),
        strip.background = element_rect(fill = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = 'Set3') +
  ggtitle('2029') +
  ylab('Percentage of landscape (%)')

s4 <- spp_df2 %>% 
  filter(Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = sppname), 
           position = 'fill', stat = 'identity', alpha = 0.8, color = 'grey20') +
  #geom_area(aes(x = PostWindthrowManagement, y = meanAGB, fill = sppname), color = "black", alpha = 0.7) +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(legend.text = element_text(face = "italic"),
        legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        axis.title.x = element_blank(),
        panel.grid = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = 'Set3') +
  #ggtitle('2130') +
  ylab('Percentage of landscape (%)')

ggsave('spp_dominant_grid_2130.tiff', s4, dpi = 600, width = 11.9, height = 6)


s34 <- s3 + s4 +
  plot_layout(ncol = 2, widths = c(1,8))

sfull <- 
  s12 / s34
  #plot_layout(ncol = 2) +
  #plot_annotation(#theme = theme(plot.title = element_text(size = 20)),
                  #tag_levels = "a"
                  #tag_prefix = "",
                  #tag_suffix = ":"
                  #)

ggsave('landscape_sppcomp2.tiff', sfull, 
       width = 11.32, height = 11.32, dpi = 600)
?ggsave
# Each spp line -----------------------------
spp_df3<- bind_rows(spp_dfs) %>% 
  gather(key = sppname, val = agb, starts_with('AboveGroundBiomass_')) %>% 
  mutate(sppname = str_replace(sppname, pattern = 'AboveGroundBiomass_', replacement = ''),
         agbtotal = agb*NumActiveSites) %>% 
  mutate(sppname2 = case_when(sppname %in% c('Picejezo', 'Picegleh') ~ 'Picea spp.',
                              sppname %in% c('Betuerma', 'Betumaxi') ~ 'Betula spp.',
                              TRUE ~ sppname)) %>%
  group_by(scenario, dir_name, Time, sppname2) %>% 
  filter(!is.nan(agb)) %>% 
  summarise(AGB = sum(agbtotal)/12169) %>% 
  ungroup()

# Make scenario index
scenario_index <- str_split(spp_df3$scenario, pattern = "__", simplify = TRUE)
scenario_index_df <- as.data.frame(scenario_index)
scenario_index2 <- str_split(scenario_index_df$V1, pattern = "_", simplify = TRUE, n = 2)
scenario_index2_df <- as.data.frame(scenario_index2)
index_df <- bind_cols(scenario_index_df, scenario_index2_df) %>% 
  dplyr::select(V1...4, V2...5, V2...2, V3) %>% 
  rename(scenario_no = V1...4,
         Climate = V2...5,
         WindthrowRegime = V2...2,
         PostWindthrowManagement = V3)
# merge spp_df & scenario index
spp_df4 <- bind_cols(spp_df3, index_df) %>% 
  group_by(WindthrowRegime, PostWindthrowManagement, Time, sppname2) %>% 
  summarise(meanAGB = mean(AGB/100),
            maxAGB = max(AGB/100),
            minAGB = min(AGB/100),
            sdAGB = sd(AGB/100))
  
spp_df4$WindthrowRegime <- factor(spp_df4$WindthrowRegime,
                                  levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'),
                                  labels = c('Historical', 'Frequent', 'Intense', 'Frequent & Intense'))
spp_df4$PostWindthrowManagement <- factor(spp_df4$PostWindthrowManagement,
                                          levels = c('WT', 'SL', 'SLSC'))
spp_df4$sppname2 <- factor(spp_df4$sppname2, 
                           levels = c("Abiesach", "Picea spp.", "Betula spp.",
                                      "Quermong", "Aceritaya", "Tilijapo", "Fraxmand", "Kalopict", 
                                      "Ulmulaci", "sasa_spp"),
                           labels = c('Abies sachalinensis', 'Picea spp.',
                                      'Betula spp.', 
                                      'Quercus crispula', 'Acer pictum',
                                      'Tilia japonica', 'Fraxinus mandshurica', 'Kalopanax pictus', 'Ulmus laciniata', 'Sasa senanensis'))

# Figure species composition
spp_df4 %>% 
  filter(sppname2 %in% c('Abies sachalinensis', 'Picea spp.', 'Betula spp.', 'Quercus crispula', 'Sasa senanensis')) %>% 
  ggplot() +
  geom_ribbon(aes(x = Time, ymin = minAGB, ymax = maxAGB, fill = PostWindthrowManagement), alpha = 0.5) +
  geom_line(aes(x = Time, y = meanAGB, color = PostWindthrowManagement), size = 1) +
  geom_hline(yintercept = 0) +
  facet_grid(sppname2 ~ WindthrowRegime, scales = 'free_y') +
  theme_bw(base_size = 20) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0, face = "italic"),
        strip.background = element_rect(fill = 'white')) +
  ggtitle('Each species biomass in landscape') +
  ylab('Aboveground biomass (Mg/ha)') +
  scale_colour_hc() +
  scale_fill_hc()


# Each spp bar plot (115yr) -----------------------------
spp_df3<- bind_rows(spp_dfs) %>% 
  gather(key = sppname, val = agb, starts_with('AboveGroundBiomass_')) %>% 
  mutate(sppname = str_replace(sppname, pattern = 'AboveGroundBiomass_', replacement = ''),
         agbtotal = agb*NumActiveSites) %>% 
  mutate(sppname2 = case_when(sppname %in% c('Picejezo', 'Picegleh') ~ 'Picea spp.',
                              sppname %in% c('Betuerma', 'Betumaxi') ~ 'Betula spp.',
                              TRUE ~ sppname)) %>%
  group_by(scenario, dir_name, Time, sppname2) %>% 
  filter(!is.nan(agb)) %>% 
  summarise(AGB = sum(agbtotal)/12169) %>% 
  ungroup()

# Make scenario index
scenario_index <- str_split(spp_df3$scenario, pattern = "__", simplify = TRUE)
scenario_index_df <- as.data.frame(scenario_index)
scenario_index2 <- str_split(scenario_index_df$V1, pattern = "_", simplify = TRUE, n = 2)
scenario_index2_df <- as.data.frame(scenario_index2)
index_df <- bind_cols(scenario_index_df, scenario_index2_df) %>% 
  dplyr::select(V1...4, V2...5, V2...2, V3) %>% 
  rename(scenario_no = V1...4,
         Climate = V2...5,
         WindthrowRegime = V2...2,
         PostWindthrowManagement = V3)
# merge spp_df & scenario index
spp_df5 <- bind_cols(spp_df3, index_df) %>% 
  group_by(WindthrowRegime, PostWindthrowManagement, Climate, Time, sppname2) %>% 
  summarise(meanAGB = mean(AGB/100),
            maxAGB = max(AGB/100),
            minAGB = min(AGB/100),
            sdAGB = sd(AGB/100))

spp_df5$WindthrowRegime <- factor(spp_df5$WindthrowRegime,
                                  levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'),
                                  labels = c('Historical', 'Frequent', 'Intense', 'Frequent & Intense'))
spp_df5$PostWindthrowManagement <- factor(spp_df5$PostWindthrowManagement,
                                          levels = c('WT', 'SL', 'SLSC'))
spp_df5$sppname2 <- factor(spp_df5$sppname2, 
                           levels = c("Abiesach", "Picea spp.", "Betula spp.",
                                      "Quermong", "Aceritaya", "Tilijapo", "Fraxmand", "Kalopict", 
                                      "Ulmulaci", "sasa_spp"),
                           labels = c('Abies sachalinensis', 'Picea spp.',
                                      'Betula spp.', 
                                      'Quercus crispula', 'Acer pictum',
                                      'Tilia japonica', 'Fraxinus mandshurica', 'Kalopanax pictus', 'Ulmus laciniata', 'Sasa senanensis'))
spp_df5$Climate <- factor(spp_df5$Climate, 
                            levels = c("current", "CSIRO-Mk3-6-0_RCP2.6", "GFDL-CM3_RCP8.5"),
                            labels = c("Current", "RCP2.6 (CSIRO-Mk3-6-0)", "RCP8.5 (GFDL-CM3)"))
spp_df5 <- spp_df5 %>% 
  ungroup()
dominant_df <- spp_df5 %>% 
  group_by(WindthrowRegime) %>% 
  summarise(AGB = sum(meanAGB))

# Figure species composition
spp_df5 %>% 
  filter(sppname2 %in% c('Abies sachalinensis', 'Picea spp.', 'Betula spp.', 
                         'Quercus crispula', 'Sasa senanensis'),
         Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = sppname2, y = meanAGB, fill = PostWindthrowManagement), 
           position = 'dodge', stat = 'identity') +
  #coord_flip() +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(panel.spacing=unit(.2,"lines"),
        plot.margin = unit(c(1, 1, 1, 3), "lines"),
        legend.position = 'bottom',
        legend.title = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
        #axis.ticks.x = element_blank()
        ) +
  #ggtitle('Each species biomass in landscape') +
  ylab(expression('Aboveground biomass (Mg' ~ ha^-1* ')')) +
  scale_colour_hc() +
  scale_fill_hc()

#install.packages('ggh4x')
#library(ggh4x)

a <- spp_df5 %>% 
  filter(sppname2 %in% c('Abies sachalinensis'),
         Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = PostWindthrowManagement), 
           position = 'dodge', stat = 'identity') +
  #coord_flip() +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(panel.spacing=unit(.2,"lines"),
        legend.position = 'none',
        legend.title = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle(expression(italic('Abies sachalinensis'))) +
  ylab(expression('Aboveground biomass (Mg' ~ ha^-1* ')')) +
  scale_colour_hc() +
  scale_fill_hc() 


b <- spp_df5 %>% 
  filter(sppname2 %in% c('Picea spp.'),
         Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = PostWindthrowManagement), 
           position = 'dodge', stat = 'identity') +
  #coord_flip() +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(panel.spacing=unit(.2,"lines"),
        legend.position = 'none',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle(expression(paste(italic('Picea'), ' spp.'))) +
  ylab(expression('Aboveground biomass (Mg' ~ ha^-1* ')')) +
  scale_colour_hc() +
  scale_fill_hc() 

c <- spp_df5 %>% 
  filter(sppname2 %in% c('Betula spp.'),
         Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = PostWindthrowManagement), 
           position = 'dodge', stat = 'identity') +
  #coord_flip() +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(panel.spacing=unit(.2,"lines"),
        legend.position = 'none',
        legend.title = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_rect(fill = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle(expression(paste(italic('Betula'), ' spp.'))) +
  ylab(expression('Aboveground biomass (Mg' ~ ha^-1* ')')) +
  scale_colour_hc() +
  scale_fill_hc() 

d <- spp_df5 %>% 
  filter(sppname2 %in% c('Quercus crispula'),
         Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = PostWindthrowManagement), 
           position = 'dodge', stat = 'identity') +
  #coord_flip() +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(panel.spacing=unit(.2,"lines"),
        legend.position = 'none',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle(expression(italic('Quercus crispula'))) +
  ylab(expression('Aboveground biomass (Mg' ~ ha^-1* ')')) +
  scale_colour_hc() +
  scale_fill_hc() 

e <- spp_df5 %>% 
  filter(sppname2 %in% c('Sasa senanensis'),
         Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = PostWindthrowManagement), 
           position = 'dodge', stat = 'identity') +
  #coord_flip() +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(panel.spacing=unit(.2,"lines"),
        legend.position = 'none',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle(expression(italic('Sasa senanensis'))) +
  ylab('Aboveground biomass (Mg/ha)') +
  scale_colour_hc() +
  scale_fill_hc()

full <- 
  a  +  theme(panel.grid = element_blank())+
  b +  theme(panel.grid = element_blank())+
  `c` +  theme(panel.grid = element_blank())+
  d +  theme(panel.grid = element_blank())+
  plot_spacer() + 
  e +  theme(panel.grid = element_blank())+
  plot_layout(ncol = 2)
ggsave('eachspp_agb.tiff', full, width = 16, height = 10, dpi = 600)

asp <- spp_df5 %>% 
  filter(sppname2 %in% c('Abies sachalinensis'),
         Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = PostWindthrowManagement), 
           position = 'dodge', stat = 'identity') +
  #coord_flip() +
  facet_nested(Climate ~ sppname2 + WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(panel.spacing=unit(.2,"lines"),
        legend.position = 'none',
        legend.title = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_rect(fill = 'white'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  #ggtitle('Abies sachalinensis') +
  ylab('Aboveground biomass (Mg/ha)') +
  scale_colour_hc() +
  scale_fill_hc()

bp <- spp_df5 %>% 
  filter(sppname2 %in% c('Betula spp.'),
         Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = PostWindthrowManagement), 
           position = 'dodge', stat = 'identity') +
  #coord_flip() +
  facet_nested(Climate ~ sppname2 + WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(panel.spacing=unit(.2,"lines"),
        legend.position = 'none',
        legend.title = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  #ggtitle('Abies sachalinensis') +
  ylab('Aboveground biomass (Mg/ha)') +
  scale_colour_hc() +
  scale_fill_hc()

pp <- spp_df5 %>% 
  filter(sppname2 %in% c('Picea spp.'),
         Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = PostWindthrowManagement), 
           position = 'dodge', stat = 'identity') +
  #coord_flip() +
  facet_nested(Climate ~ sppname2 + WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(panel.spacing=unit(.2,"lines"),
        legend.position = 'none',
        legend.title = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  #ggtitle('Abies sachalinensis') +
  ylab('Aboveground biomass (Mg/ha)') +
  scale_colour_hc() +
  scale_fill_hc()

qp <- spp_df5 %>% 
  filter(sppname2 %in% c('Quercus crispula'),
         Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = PostWindthrowManagement), 
           position = 'dodge', stat = 'identity') +
  #coord_flip() +
  facet_nested(Climate ~ sppname2 + WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(panel.spacing=unit(.2,"lines"),
        legend.position = 'none',
        legend.title = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  #ggtitle('Abies sachalinensis') +
  ylab('Aboveground biomass (Mg/ha)') +
  scale_colour_hc() +
  scale_fill_hc()

sp <- spp_df5 %>% 
  filter(sppname2 %in% c('Sasa senanensis'),
         Time == 115) %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = meanAGB, fill = PostWindthrowManagement), 
           position = 'dodge', stat = 'identity') +
  #coord_flip() +
  facet_nested(Climate ~ sppname2 + WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(panel.spacing=unit(.2,"lines"),
        legend.position = 'none',
        legend.title = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_rect(fill = 'white'),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  #ggtitle('Abies sachalinensis') +
  ylab('Aboveground biomass (Mg/ha)') +
  scale_colour_hc() +
  scale_fill_hc()

# patchwork
asp + pp + bp + qp + sp +
  plot_layout(ncol = 2) +
  plot_annotation()


# reproduction log ---------------------------------------
regen_df <- bind_rows(regen_dfs)
# Make scenario index
scenario_id <- str_split(regen_df$scenario, pattern = "__", simplify = TRUE)
scenario_id_df <- as.data.frame(scenario_id)
scenario_id2 <- str_split(scenario_id_df$V1, pattern = "_", simplify = TRUE, n = 2)
scenario_id2_df <- as.data.frame(scenario_id2)
index_df <- bind_cols(scenario_id_df, scenario_id2_df) %>% 
  dplyr::select(V1...4, V2...5, V2...2, V3) %>% 
  rename(scenario_no = V1...4,
         Climate = V2...5,
         WindthrowRegime = V2...2,
         PostWindthrowManagement = V3)
regen_df2 <- bind_cols(regen_df, index_df)
regen_df2_sum <- regen_df2 %>% 
  group_by(Climate, WindthrowRegime, PostWindthrowManagement,
           Time, SpeciesName) %>% 
  summarise(CohortsSurface = mean(NumCohortsSurface),
            CohortsNlog = mean(NumCohortsNlog)) %>% 
  mutate(Cohorts = CohortsSurface + CohortsNlog)
# 順番ラベル変更
regen_df2_sum$WindthrowRegime <- factor(regen_df2_sum$WindthrowRegime,
                                  levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'),
                                  labels = c('Historical', 'Frequent', 'Intense', 'Frequent & Intense'))
regen_df2_sum$PostWindthrowManagement <- factor(regen_df2_sum$PostWindthrowManagement,
                                          levels = c('WT', 'SL', 'SLSC'))
regen_df2_sum$SpeciesName <- factor(regen_df2_sum$SpeciesName,
                           levels = c("Abiesach", "Picejezo", "Picegleh", "Betuerma", "Betumaxi", 
                                      "Fraxmand", "Kalopict", "Quermong", "Aceritaya", "Tilijapo", 
                                      "Ulmulaci", "sasa_spp"),
                           labels = c('Abies sachalinensis', 'Picea jezoensis', 'Picea glehnii',
                                      'Betula ermanii', 'Betula maximowicziana', 'Fraxinus mandshurica', 'Kalopanax septemlobus', 
                                      'Quercus crispula', 'Acer pictum', 'Tilia japonica', 
                                      'Ulmus laciniata', 'Sasa senanensis'))
regen_df2_sum$Climate <- factor(regen_df2_sum$Climate, 
                          levels = c("current", "CSIRO-Mk3-6-0_RCP2.6", "GFDL-CM3_RCP8.5"),
                          labels = c("Current", "RCP2.6 (CSIRO-Mk3-6-0)", "RCP8.5 (GFDL-CM3)"))
pr <- regen_df2_sum %>% 
  group_by(Climate, WindthrowRegime, PostWindthrowManagement,
           SpeciesName) %>% 
  summarise(TotalCohortsSurface = sum(CohortsSurface),
            TotalCohortsNlog = sum(CohortsNlog),
            TotalCohorts = sum(Cohorts)) %>% 
  filter(SpeciesName !='Sasa senanensis') %>% 
  ggplot() +
  geom_bar(aes(x = PostWindthrowManagement, y = TotalCohorts, fill = SpeciesName), stat = 'identity', position = 'stack') +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        legend.text = element_text(face = 'italic'),
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = 'Set3') +
  #ggtitle('Total cohorts established in landscape') +
  ylab('The number of cohorts')
  
ggsave('regen_cohorts.tiff', pr, width = 11.32, height = 7, dpi = 600)

# NECN log ----------------------------------------------------------------------
necn_df<- bind_rows(necn_dfs) %>% 
  mutate(NEP = -NEEC,
         NPP = AG_NPPC + BG_NPPC,
         Rh = NPP - NEP,
         npptotal = NPP*NumSites,
         rhtotal = Rh*NumSites,
         neptotal = NEP*NumSites) %>% 
  group_by(scenario, dir_name, Time) %>% 
  summarise(NPP = sum(npptotal)/12169,
            Rh = -sum(rhtotal)/12169,
            NEP = sum(neptotal)/12169) %>% 
  ungroup()

# Make scenario index
scenario_index_cf <- str_split(necn_df$scenario, pattern = "__", simplify = TRUE)
scenario_index_cf_df <- as.data.frame(scenario_index_cf)
scenario_index2_cf <- str_split(scenario_index_cf_df$V1, pattern = "_", n = 2, simplify = TRUE)
scenario_index2_cf_df <- as.data.frame(scenario_index2_cf)
index_cf_df <- bind_cols(scenario_index_cf_df, scenario_index2_cf_df) %>% 
  dplyr::select(V1...4, V2...5, V2...2, V3) %>% 
  rename(scenario_no = V1...4,
         Climate = V2...5,
         WindthrowRegime = V2...2,
         PostWindthrowManagement = V3)
# merge spp_df & scenario index
necn_df2 <- bind_cols(necn_df, index_cf_df) %>% 
  gather(key = type, value = flux,
         NPP, Rh, NEP) %>% 
  group_by(WindthrowRegime, PostWindthrowManagement, type, Time) %>% 
  summarise(meanflux = mean(flux),
            maxflux = max(flux),
            minflux = min(flux),
            sdflux = sd(flux))
  
# nep_df <- bind_cols(necn_df, index_cf_df) %>% 
#   mutate(NEP_pos = case_when(meanNEP >= 0 ~ meanNEP,
#                              TRUE ~ 0),
#          NEP_neg = case_when(meanNEP < 0 ~ meanNEP,
#                              TRUE ~ 0))
necn_df2$WindthrowRegime <- factor(necn_df2$WindthrowRegime,
                                   levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'),
                                   labels = c('Historical', 'Frequent', 'Intense', 'Frequent & Intense'))
necn_df2$PostWindthrowManagement <- factor(necn_df2$PostWindthrowManagement,
                                           levels = c('WT', 'SL', 'SLSC'))
necn_df2$type <- factor(necn_df2$type,
                        labels = c('NEP', 'NPP', 'Rh'))
# Flux figure -----------------------------------------------------------------------------------------------
cf <- necn_df2 %>% 
  filter(Time != 0,
         type != 'NEP') %>%
  ggplot() +
  geom_line(aes(x = Time+2015, y = meanflux, color = PostWindthrowManagement), size = 1) +
  geom_ribbon(aes(x = Time+2015, ymin = minflux, ymax = maxflux, fill = PostWindthrowManagement), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(type ~ WindthrowRegime, scales = 'free_y') +
  theme_bw(base_size = 15) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        #axis.title = element_text(size = 15),
        #axis.text = element_text(size = 15)
        ) +
  #ggtitle('Changes in carbon flux in landscape') +
  labs(x = 'Simulation year', 
       y = expression(paste("Carbon flux (gC ", {m^-2}, " ", {year^-1}, ")", sep="")),
       #caption = 'Shaded areas represent the ranges of maximum and minimum values among all climate scenarios.'
       ) +
  scale_colour_hc() +
  scale_fill_hc() +
  scale_x_continuous(breaks=seq(2030, 2130,length=5), limits=c(2015,2130))
  # scale_colour_viridis(discrete = T) +
  # scale_fill_viridis(discrete = T)
  # scale_color_manual(values = c('NEP' = 'grey70', 'NPP' = 'palegreen3', 'Rh' = 'lightcoral'),
  #                    labels = c('NEP', 'NPP', 'Rh')) +

ggsave(file = "cflux.tiff", cf, dpi = 600, width = 12.84, height = 7.8)

# Carbon stock -----
necn_cdf<- bind_rows(necn_dfs) %>% 
  dplyr::select(scenario, dir_name, Time, ClimateRegionIndex, NumSites,
                starts_with('C_Live'), starts_with('C_Dead'), starts_with('C_SOM')) %>% 
  mutate(live = C_LiveLeaf + C_LiveFRoot + C_LiveWood + C_LiveCRoot,
         dead = C_DeadWood + C_DeadCRoot + C_DeadLeaf_Struc + C_DeadLeaf_Meta + C_DeadFRoot_Struc + C_DeadFRoot_Meta,
         soil = C_SOM1surf + C_SOM1soil + C_SOM2 + C_SOM3) %>% 
  dplyr::select(-starts_with('C_Live'), -starts_with('C_Dead'), -starts_with('C_SOM')) %>%
  mutate(live = live*NumSites,
         dead = dead*NumSites,
         soil = soil*NumSites) %>% 
  group_by(scenario, dir_name, Time) %>% 
  summarise(Live = sum(live)/12169/100,
            Dead = sum(dead)/12169/100,
            Soil = sum(soil)/12169/100) %>% 
  ungroup()

necn_cdf2 <- bind_cols(necn_cdf, index_cf_df) %>% 
  mutate(Total = Live + Dead + Soil) %>%  
  gather(key = type, value = cstock,
         Live, Dead, Soil, Total) %>% 
  group_by(WindthrowRegime, PostWindthrowManagement, type, Time) %>% 
  summarise(meanCstock = mean(cstock),
            maxCstock = max(cstock),
            minCstock = min(cstock),
            sdCstock = sd(cstock))
  
necn_cdf2$WindthrowRegime <- factor(necn_cdf2$WindthrowRegime,
                                    levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'),
                                    labels = c('Historical', 'Frequent', 'Intense', 'Frequent & Intense'))
necn_cdf2$PostWindthrowManagement <- factor(necn_cdf2$PostWindthrowManagement,
                                            levels = c('WT', 'SL', 'SLSC'))
necn_cdf2$type <- factor(necn_cdf2$type,
                         levels = c('Live', 'Dead', 'Soil', 'Total'),
                         labels = c('Live trees & Sasa', 'Coarse Woody Debris', 'Soil Organic Materials', 'Total'))

# Figure carbon stock ----------------------------------------------------
cs <- necn_cdf2 %>% 
  #filter(type != 'Total') %>% 
  ggplot() +
  geom_ribbon(aes(x = Time+2015, ymin = minCstock, ymax = maxCstock, fill = PostWindthrowManagement), alpha = 0.5) +
  geom_line(aes(x = Time+2015, y = meanCstock, color = PostWindthrowManagement), size = 1) +
  geom_hline(yintercept = 0) +
  facet_grid(type ~ WindthrowRegime, scales = 'free_y') +
  theme_bw(base_size = 15) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_blank()) +
  scale_colour_hc() +
  scale_fill_hc() +
  scale_x_continuous(breaks=seq(2030, 2130,length=5), limits=c(2015,2130)) +
  # scale_colour_viridis(discrete = T) +
  # scale_fill_viridis(discrete = T) +
  # scale_color_brewer(palette = 'Set2') +
  # scale_fill_brewer(palette = 'Set2') +
  #ggtitle('Changes in carbon stocks in landscape') +
  xlab('Simulation year') +
  ylab(expression('Carbon stocks (Mg ' ~ha^-1* ')')) +
  labs(caption = 'Shaded areas represent the ranges of maximum and minimum values among all climate scenarios.')

ggsave(file = "cstock.tiff", cs, dpi = 600, width = 12.84, height = 7.8)


cfcs <- cf / cs + 
  plot_annotation(tag_levels = 'a')

ggsave(file = "cfluxstock.tiff", cfcs, dpi = 600, width = 12.84, height = 15.6)








# NECN summary log ---------------------------------------------------------------------
necn_sum_df <- bind_rows(necn_sum_dfs) %>% 
  mutate(NEP = -NEEC) %>% 
  group_by(scenario, Time) %>% 
  summarise(meanAGB = mean(AGB),
            sdAGB = sd(AGB),
            meanNEP = mean(NEP),
            sdNEP = sd(NEP)) %>% 
  mutate(sinksource = case_when(meanNEP >= 0 ~ 'Sink',
                                TRUE ~ 'Source'))

# C stock
necn_c_df <- bind_rows(necn_sum_dfs) %>% 
  mutate(NEP = -NEEC) %>% 
  group_by(scenario, Time) %>% 
  summarise(meanAGB = mean(AGB),
            `LiveTrees&Sasa` = mean(AGB/100/2),
            `DeadWood` = mean(C_DeadWood/100),
            `SoilOrganicMaterials` = mean(SOMTC/100)) %>% 
  ungroup() %>% 
  gather(key = type, value = CarbonStock,
         `LiveTrees&Sasa`, `DeadWood`, `SoilOrganicMaterials`)

necn_c_df$type <- factor(necn_c_df$type,
                         levels = c('LiveTrees&Sasa', 'DeadWood', 'SoilOrganicMaterials'))
# Make scenario index
scenario_index_c <- str_split(necn_c_df$scenario, pattern = "__", simplify = TRUE)
scenario_index_c_df <- as.data.frame(scenario_index_c)
scenario_index2_c <- str_split(scenario_index_c_df$V1, pattern = "_", simplify = TRUE)
scenario_index2_c_df <- as.data.frame(scenario_index2_c)
index_c_df <- bind_cols(scenario_index_c_df, scenario_index2_c_df) %>% 
  dplyr::select(V1...4, V2...5, V2...2, V3...3) %>% 
  rename(scenario_no = V1...4,
         Climate = V2...5,
         WindthrowRegime = V2...2,
         PostWindthrowManagement = V3...3)
# merge spp_df & scenario index
necn_c_df2 <- bind_cols(necn_c_df, index_c_df)
necn_c_df2$WindthrowRegime <- factor(necn_c_df2$WindthrowRegime,
                                     levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'))
necn_c_df2$PostWindthrowManagement <- factor(necn_c_df2$PostWindthrowManagement,
                                             levels = c('WT', 'SL', 'SLSC'))

# Figure carbon stock
necn_c_df2 %>% 
  filter(scenario %in% c('311_GFDL-CM3_RCP8.5__historicWindthrow__WT', '312_GFDL-CM3_RCP8.5__historicWindthrow__SL','313_GFDL-CM3_RCP8.5__historicWindthrow__SLSC',
                         '321_GFDL-CM3_RCP8.5__frequent__WT', '322_GFDL-CM3_RCP8.5__frequent__SL', '323_GFDL-CM3_RCP8.5__frequent__SLSC',
                         '331_GFDL-CM3_RCP8.5__intense__WT', '332_GFDL-CM3_RCP8.5__intense__SL', '333_GFDL-CM3_RCP8.5__intense__SLSC',
                         '341_GFDL-CM3_RCP8.5__frequentintense__WT', '342_GFDL-CM3_RCP8.5__frequentintense__SL', '343_GFDL-CM3_RCP8.5__frequentintense__SLSC')) %>% 
  ggplot() +
  geom_line(aes(x = Time, y = CarbonStock, group = type), position = 'stack') +
  geom_area(aes(x = Time, y = CarbonStock, fill = type), color = "black", alpha = 0.7) +
  facet_grid(WindthrowRegime ~ PostWindthrowManagement) +
  theme_bw(base_size = 20) +
  theme(legend.position = 'bottom',
        strip.text.y = element_text(angle = 0)) +
  scale_fill_brewer(palette = 'Set2',
                    labels = c('Live trees & Sasa', 'Coarse Woody Debris', 'Soil Organic Carbon')) +
  ggtitle('Changes in carbon stocks in landscape (GFDL-CM3 RCP8.5)') +
  ylab('Carbon stocks (Mg/ha)') +
  ylim(0,300)




# NEP
nep_df <- necn_sum_df %>% 
  mutate(NEP_pos = case_when(meanNEP >= 0 ~ meanNEP,
                             TRUE ~ 0),
         NEP_neg = case_when(meanNEP < 0 ~ meanNEP,
                             TRUE ~ 0))

nep_df %>% 
  filter(Time != 0) %>% 
  ggplot() +
  geom_line(aes(x = Time, y = meanNEP)) +
  #geom_area(aes(x = Time, y = meanNEP), alpha = 0.7) +
  geom_area(aes(x = Time, y = NEP_pos), fill = 'blue', alpha = 0.7) +
  geom_area(aes(x = Time, y = NEP_neg), fill = 'gold', alpha = 0.7) +
  geom_ribbon(aes(x = Time, ymin = meanNEP - sdNEP, ymax = meanNEP + sdNEP)) +
  geom_hline(yintercept = 0, size = 1) +
  facet_wrap(~scenario, ncol = 6) +
  theme_bw() +
  ggtitle('Changes in NEP in landscape') +
  labs(x = 'Year', y = expression(paste("Net Ecosystem Productions (g/ ", {m^2}, "/year)", sep="")))

# AGB
necn_sum_df %>% 
  filter(Time != 0) %>% 
  ggplot() +
  geom_line(aes(x = Time, y = meanAGB)) +
  geom_ribbon(aes(x = Time, ymin = meanAGB - sdAGB, ymax = meanAGB + sdAGB)) +
  geom_hline(yintercept = 0, size = 1) +
  facet_wrap(~scenario) +
  theme_bw() +
  ggtitle('Changes in Aboveground biomass in landscape') +
  labs(x = 'Year', y = expression(paste("Aboveground biomass (Mg/ha)")))


necn_sum_df2 <- bind_rows(necn_sum_dfs) %>% 
  mutate(NEP = -NEEC)
necn_sum_df2 %>% 
  filter(Time != 0) %>% 
  ggplot() +
  geom_line(aes(x = Time, y = AGB, color = dir_name)) +
  #geom_ribbon(aes(x = Time, ymin = meanAGB - sdAGB, ymax = meanAGB + sdAGB)) +
  geom_hline(yintercept = 0, size = 1) +
  facet_wrap(~scenario) +
  theme_bw() +
  theme(legend.position = 'none') +
  ggtitle('Changes in Aboveground biomass in landscape') +
  labs(x = 'Year', y = expression(paste("Aboveground biomass (Mg/ha)")))