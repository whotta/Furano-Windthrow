


root_path <- 'D:/Project_Furano_Windthrow_201201'
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
           '341_GFDL-CM3_RCP8.5__frequentintense__WT', '342_GFDL-CM3_RCP8.5__frequentintense__SL', '343_GFDL-CM3_RCP8.5__frequentintense__SLSC'
)


# Figure S9 Read establishment log file --------
ReadEstLog <- function(fname) {
  buf <- str_split(fname, pattern = '/')[[1]]
  foldernames <- buf[(length(buf) - 3):(length(buf) - 1)]
  management <- str_remove(foldernames[1], pattern = 'BaU_')
  climate <- foldernames[2]
  iter <- as.numeric(str_remove(foldernames[3], pattern = 'reg1_iter'))
  # Read file
  est_df <- read_csv(fname, col_types = cols()) %>% 
    pivot_longer(cols = c(-'Time', -'SpeciesName', -'ClimateRegion', -'NumberAttempts')) %>% 
    mutate(Species = fct_relevel(Species, spplevels),
           climate = climate, 
           management = management, iter = iter)
  return(est_df)
}

# Read all est-log.csv files
est_fnames <-list()
iter <- 1
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    e <- read_csv(paste0(dir_name, '/NECN-prob-establish-log.csv')) %>% select(-...12) %>% 
      filter(AvgTempMult != NaN)
    e$AvgMinJanTempMult <- as.numeric(e$AvgMinJanTempMult)
    e$AvgTempMult <- as.numeric(e$AvgTempMult)
    e$AvgSoilMoistureMult <- as.numeric(e$AvgSoilMoistureMult)
    e$AvgProbEst <- as.numeric(e$AvgProbEst)
    est_fnames[[iter]] <- e %>% 
      mutate(case = case,
             AvgTempMult2 = AvgTempMult * NumberAttempts,
             AvgMinJanTempMult2 = AvgMinJanTempMult * NumberAttempts,
             AvgSoilMoistureMult2 = AvgSoilMoistureMult * NumberAttempts,
             AvgProbEst2 = AvgProbEst * NumberAttempts) %>% 
      group_by(case, Time, SpeciesName) %>% 
      summarise(AvgTempMult = sum(AvgTempMult2)/12/12169,
                AvgMinJanTempMult = sum(AvgMinJanTempMult2)/12/12169,
                AvgSoilMoistureMult = sum(AvgSoilMoistureMult2)/12/12169,
                AvgProbEst = sum(AvgProbEst2)/12/12169)
      
    iter <- iter + 1
  }
}

est_df <- bind_rows(est_fnames)

# Make scenario index
scenario_index_est <- str_split(est_df$case, pattern = "__", simplify = TRUE)
scenario_index_est_df <- as.data.frame(scenario_index_est)
scenario_index2_est <- str_split(scenario_index_est_df$V1, pattern = "_", n = 2, simplify = TRUE)
scenario_index2_est_df <- as.data.frame(scenario_index2_est)
index_est_df <- bind_cols(scenario_index_est_df, scenario_index2_est_df) %>% 
  dplyr::select(V1...4, V2...5, V2...2, V3) %>% 
  rename(scenario_no = V1...4,
         Climate = V2...5,
         WindthrowRegime = V2...2,
         PostWindthrowManagement = V3)
  
est_df2 <- bind_cols(index_est_df, est_df) %>% 
  gather(key = category, value = Probability,
         'AvgProbEst', 'AvgTempMult', 'AvgMinJanTempMult', 'AvgSoilMoistureMult')

# 順番ラベル変更
est_df2$WindthrowRegime <- factor(est_df2$WindthrowRegime,
                                        levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'),
                                        labels = c('Historical', 'Frequent', 'Intense', 'Frequent & Intense'))
est_df2$PostWindthrowManagement <- factor(est_df2$PostWindthrowManagement,
                                                levels = c('WT', 'SL', 'SLSC'))
est_df2$SpeciesName <- factor(est_df2$SpeciesName,
                                    levels = c("Abiesach", "Picejezo", "Picegleh", "Betuerma", "Betumaxi", 
                                               "Fraxmand", "Kalopict", "Quermong", "Aceritaya", "Tilijapo", 
                                               "Ulmulaci", "sasa_spp"),
                                    labels = c('Abies sachalinensis', 'Picea jezoensis', 'Picea glehnii',
                                               'Betula ermanii', 'Betula maximowicziana', 'Fraxinus mandshurica', 'Kalopanax septemlobus', 
                                               'Quercus crispula', 'Acer pictum', 'Tilia japonica', 
                                               'Ulmus laciniata', 'Sasa senanensis'))
est_df2$Climate <- factor(est_df2$Climate, 
                                levels = c("current", "CSIRO-Mk3-6-0_RCP2.6", "GFDL-CM3_RCP8.5"),
                                labels = c("Current", "RCP2.6 (CSIRO-Mk3-6-0)", "RCP8.5 (GFDL-CM3)"))


# Visualise
est_df2 %>% 
  filter(WindthrowRegime == 'Frequent & Intense',
         PostWindthrowManagement == 'SLSC') %>% 
  ggplot() +
  geom_line(aes(x = Time, y = Probability, color = SpeciesName)) +
  facet_grid(Climate ~ category)


est_df2 %>% 
  filter(WindthrowRegime == 'Frequent & Intense',
         PostWindthrowManagement == 'SLSC',
         category == 'AvgProbEst') %>% 
  ggplot() +
  geom_line(aes(x = Time, y = Probability, color = SpeciesName)) +
  facet_grid(Climate ~ SpeciesName)


























est_df <- map(est_fnames, ReadEstLog) %>% 
  reduce(bind_rows) %>% 
  mutate(name = fct_relevel(name, 'AvgProbEst', 'AvgTempMult', 'AvgMinJanTempMult', 'AvgSoilMoistureMult'))

# Ensemble mean
est_ens_df <- est_df %>% 
  group_by(climate, management, Time, Species, ClimateRegion, name) %>% 
  summarise(value_mean = mean(value, na.rm = TRUE),
            value_sd = sd(value, na.rm = TRUE),
            value_se = value_sd / sqrt(nreps),
            nsite_mean = mean(NumberSitesChecked, na.rm = TRUE),
            nsite_sd = sd(NumberSitesChecked, na.rm = TRUE),
            nsite_se = nsite_sd / sqrt(nreps)) %>% 
  ungroup()
skimr::skim(est_ens_df)


# climate condition and its prob are same among nreps
est_plt1 <- est_df %>% 
  group_by(Species, ClimateRegion, management, climate, name) %>% 
  mutate(value_rollmean = rollmean(value, 5, na.pad = T)) %>% 
  ungroup() %>% 
  filter(ClimateRegion == 'eco001', management == 'CL') %>% 
  ggplot(aes(x = Time + 2014 - 1, color = Species)) +
  geom_line(aes(y = value), size = .5, alpha = .7) +
  geom_line(aes(y = value_rollmean), size = 1.5) +
  scale_color_brewer(palette = 'Paired', drop = F) +
  facet_grid(name~climate) +
  labs(title = 'Establishment probs in Eco 001',
       x = 'Year', y = 'Probability (-)', color = 'Species') +
  theme_clean(base_size = 20) +
  theme(panel.background = element_rect(color = 'black', fill = NA),
        legend.background = element_rect(color = NA, fill = NA),
        legend.text = element_text(size = 13), 
        legend.title = element_text(size = 13),
        legend.position = 'bottom')
ggsave(file.path(result_dir, 'FigureS9-1_est_eco001_cl.pdf'), est_plt1, width = 16, height = 12)