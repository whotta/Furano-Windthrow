# LCA of salvaged wood
salvage_df <- bind_rows(spp_dfs) 
# Make scenario index
scenario_index_cf <- str_split(salvage_df$scenario, pattern = "__", simplify = TRUE)
scenario_index_cf_df <- as.data.frame(scenario_index_cf)
scenario_index2_cf <- str_split(scenario_index_cf_df$V1, pattern = "_", n = 2, simplify = TRUE)
scenario_index2_cf_df <- as.data.frame(scenario_index2_cf)
index_cf_df <- bind_cols(scenario_index_cf_df, scenario_index2_cf_df) %>% 
  dplyr::select(V1...4, V2...5, V2...2, V3) %>% 
  rename(scenario_no = V1...4,
         Climate = V2...5,
         WindthrowRegime = V2...2,
         PostWindthrowManagement = V3)

salvage_df4 <- bind_cols(salvage_df, index_cf_df) %>% 
  filter(Climate == 'CSIRO-Mk3-6-0_RCP2.6') %>% 
  gather(key = sppname, val = agb, starts_with('AboveGroundBiomass_')) %>% 
  mutate(sppname = str_replace(sppname, pattern = 'AboveGroundBiomass_', replacement = ''),
         agbtotal = agb*NumActiveSites) %>% 
  group_by(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name, Time, sppname) %>% 
  filter(!is.nan(agb)) %>% 
  summarise(AGB = sum(agbtotal)/12169) %>% 
  ungroup() %>% 
  filter(sppname != 'sasa_spp') %>% 
  group_by(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name, Time) %>% 
  summarise(TotalAGB = sum(AGB),
            TotalLiveC = TotalAGB/2) %>% 
  mutate(TotalLiveC_lag = lag(TotalLiveC, default = 0, order_by = Time),
         salvagedC = case_when((PostWindthrowManagement != 'WT' & Time %in% c(15, 40, 65, 90) & TotalLiveC - TotalLiveC_lag <= 0) ~
                                 -(TotalLiveC - TotalLiveC_lag),
                               TRUE ~ 0),
         harvest_emissions = case_when(Time == 15 ~ salvagedC * 0.00986278 * (43 / 53), # 宇城ら 2012より算出
                                       Time == 40 ~ salvagedC * 0.00986278 * (18 / 53), # 宇城ら 2012より算出
                                       TRUE ~ 0),
         scarif_emissions = case_when((PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'historicWindthrow' & Time == 15) ~
                                        0.05 * (43 / 53) * 0.2 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'historicWindthrow' & Time == 65) ~
                                        0,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'frequent' & Time == 15) ~ 
                                        0.05 * (43 / 53) * 0.2 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'frequent' & Time == 40) ~ 
                                        0.05 * (18 / 53) * 0.2 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'frequent' & Time %in% c(65, 90)) ~ 
                                        0,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'intense' & Time == 15) ~ 
                                        0.05 * (43 / 53) * 0.4 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'intense' & Time == 65) ~ 
                                        0,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'frequentintense' & Time == 15) ~ 
                                        0.05 * (43 / 53) * 0.4 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'frequentintense' & Time == 40) ~ 
                                        0.05 * (18 / 53) * 0.4 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'frequentintense' & Time %in% c(65, 90)) ~ 
                                        0,
                                      TRUE ~ 0), # Owari et al. (2011) 0.05 = carbon emissions by scarification (tC/ha), 0.2or0.4 = Windthrow area ratio
         manuf_emissions_wood = case_when(Time == 15 ~ salvagedC * 0.7 * 0.65 * 0.09262436 * (43 / 53), # 宇城ら 2012; Suzuki et al. 2019, App.F
                                          Time == 40 ~ salvagedC * 0.7 * 0.65 * 0.09262436 * (18 / 53),
                                          TRUE ~ 0),
         manuf_emissions_paper = case_when(Time == 15 ~ salvagedC * 0.3 * 0.15 * (43 / 53), # Hudiburg et al. 2019, Table S5; Suzuki et al. 2019, App.F
                                           Time == 40 ~ salvagedC * 0.3 * 0.15 * (18 / 53),
                                           TRUE ~ 0),
         transport_emissions = case_when(Time == 15 ~ salvagedC * 0.000155 * (43 / 53), # Hudiburg et al. 2019, Table S5
                                         Time == 40 ~ salvagedC * 0.000155 * (18 / 53),
                                         TRUE ~ 0),
         manuf_loss_emissions = case_when(Time == 15 ~ salvagedC * 0.7 * (1 - 0.65) * (43 / 53), # Suzuki et al. 2019, App.F # lossがすべて廃棄処分される場合
                                          Time == 40 ~ salvagedC * 0.7 * (1 - 0.65) * (18 / 53),
                                          TRUE ~ 0),
         loss_manuf_emissions_board = case_when(Time == 15 ~ salvagedC * 0.7 * (1 - 0.65) * 0.1444 * (43 / 53), # 南部ら 2012 # lossがすべてボード生産に回った場合
                                                Time == 40 ~ salvagedC * 0.7 * (1 - 0.65) * 0.1444 * (18 / 53),
                                                TRUE ~ 0),
         loss_manuf_emissions_paper = case_when(Time == 15 ~ salvagedC * 0.7 * (1 - 0.65) * 0.15 * (43 / 53), # Hudiburg et al. 2019, Table S5; Suzuki et al. 2019, App.F # lossがすべて紙製品に回った場合
                                                Time == 40 ~ salvagedC * 0.7 * (1 - 0.65) * 0.15 * (18 / 53),
                                                TRUE ~ 0),
         # year 15 -------------------
         salvagedC_15only = case_when(Time == 15 ~ salvagedC,
                                      TRUE ~ 0),
         salvagedC_15 = cumsum(salvagedC_15only),
         salvagedC_track_15 = salvagedC_15 * (0.7*0.65*exp(-0.0198*(Time-15)) + 0.3*1.0*exp(-0.347*(Time-15))), # Suzuki et al. 2019, App.F
         salvagedC_track_15_lossboard = salvagedC_15 * (0.7*0.65*exp(-0.0198*(Time-15)) + 0.7*(1-0.65)*exp(-0.02773*(Time-15)) + 0.3*1.0*exp(-0.347*(Time-15))), # Suzuki et al. 2019, App.F
         salvagedC_track_15_losspaper = salvagedC_15 * (0.7*0.65*exp(-0.0198*(Time-15)) + (0.3+0.7*(1-0.65))*1.0*exp(-0.347*(Time-15))), # Suzuki et al. 2019, App.F
         
         salvagedC_emissions_15 = case_when(Time == 15 ~ 0,
                                            TRUE ~ lag(salvagedC_track_15, default = 0, order_by = Time) - salvagedC_track_15),
         salvagedC_emissions_15_lossboard = case_when(Time == 15 ~ 0,
                                                      TRUE ~ lag(salvagedC_track_15_lossboard, default = 0, order_by = Time) - salvagedC_track_15_lossboard),
         salvagedC_emissions_15_losspaper = case_when(Time == 15 ~ 0,
                                                      TRUE ~ lag(salvagedC_track_15_losspaper, default = 0, order_by = Time) - salvagedC_track_15_losspaper),
         # year 40 -------------------
         salvagedC_40only = case_when(Time == 40 ~ salvagedC,
                                      TRUE ~ 0),
         salvagedC_40 = cumsum(salvagedC_40only),
         salvagedC_track_40 = salvagedC_40 * (0.7*0.65*exp(-0.0198*(Time-40)) + 0.3*1.0*exp(-0.347*(Time-40))), # Suzuki et al. 2019, App.F
         salvagedC_track_40_lossboard = salvagedC_40 * (0.7*0.65*exp(-0.0198*(Time-40)) + 0.7*(1-0.65)*exp(-0.02773*(Time-40)) + 0.3*1.0*exp(-0.347*(Time-40))), # Suzuki et al. 2019, App.F
         salvagedC_track_40_losspaper = salvagedC_40 * (0.7*0.65*exp(-0.0198*(Time-40)) + (0.3+0.7*(1-0.65))*1.0*exp(-0.347*(Time-40))), # Suzuki et al. 2019, App.F
         
         salvagedC_emissions_40 = case_when(Time == 40 ~ 0,
                                            TRUE ~ lag(salvagedC_track_40, default = 0, order_by = Time) - salvagedC_track_40),
         salvagedC_emissions_40_lossboard = case_when(Time == 40 ~ 0,
                                                      TRUE ~ lag(salvagedC_track_40_lossboard, default = 0, order_by = Time) - salvagedC_track_40_lossboard),
         salvagedC_emissions_40_losspaper = case_when(Time == 40 ~ 0,
                                                      TRUE ~ lag(salvagedC_track_40_losspaper, default = 0, order_by = Time) - salvagedC_track_40_losspaper),
         # year 65 ------------------
         salvagedC_65only = case_when(Time == 65 ~ salvagedC,
                                      TRUE ~ 0),
         salvagedC_65 = cumsum(salvagedC_65only),
         salvagedC_track_65 = salvagedC_65 * (0.7*0.65*exp(-0.0198*(Time-65)) + 0.3*1.0*exp(-0.347*(Time-65))), # Suzuki et al. 2019, App.F
         salvagedC_track_65_lossboard = salvagedC_65 * (0.7*0.65*exp(-0.0198*(Time-65)) + 0.7*(1-0.65)*exp(-0.02773*(Time-65)) + 0.3*1.0*exp(-0.347*(Time-65))), # Suzuki et al. 2019, App.F
         salvagedC_track_65_losspaper = salvagedC_65 * (0.7*0.65*exp(-0.0198*(Time-65)) + (0.3+0.7*(1-0.65))*1.0*exp(-0.347*(Time-65))), # Suzuki et al. 2019, App.F
         
         salvagedC_emissions_65 = case_when(Time == 65 ~ 0,
                                            TRUE ~ lag(salvagedC_track_65, default = 0, order_by = Time) - salvagedC_track_65),
         salvagedC_emissions_65_lossboard = case_when(Time == 65 ~ 0,
                                                      TRUE ~ lag(salvagedC_track_65_lossboard, default = 0, order_by = Time) - salvagedC_track_65_lossboard),
         salvagedC_emissions_65_losspaper = case_when(Time == 65 ~ 0,
                                                      TRUE ~ lag(salvagedC_track_65_losspaper, default = 0, order_by = Time) - salvagedC_track_65_losspaper),
         # year 90 ------------------
         salvagedC_90only = case_when(Time >= 90 ~ salvagedC,
                                      TRUE ~ 0),
         salvagedC_90 = cumsum(salvagedC_90only),
         salvagedC_track_90 = salvagedC_90 * (0.7*0.65*exp(-0.0198*(Time-90)) + 0.3*1.0*exp(-0.347*(Time-90))), # Suzuki et al. 2019, App.F
         salvagedC_track_90_lossboard = salvagedC_90 * (0.7*0.65*exp(-0.0198*(Time-90)) + 0.7*(1-0.65)*exp(-0.02773*(Time-90)) + 0.3*1.0*exp(-0.347*(Time-90))), # Suzuki et al. 2019, App.F
         salvagedC_track_90_losspaper = salvagedC_90 * (0.7*0.65*exp(-0.0198*(Time-90)) + (0.3+0.7*(1-0.65))*1.0*exp(-0.347*(Time-90))), # Suzuki et al. 2019, App.F
         
         salvagedC_emissions_90 = case_when(Time == 90 ~ 0,
                                            TRUE ~ lag(salvagedC_track_90, default = 0, order_by = Time) - salvagedC_track_90),
         salvagedC_emissions_90_lossboard = case_when(Time == 90 ~ 0,
                                                      TRUE ~ lag(salvagedC_track_90_lossboard, default = 0, order_by = Time) - salvagedC_track_90_lossboard),
         salvagedC_emissions_90_losspaper = case_when(Time == 90 ~ 0,
                                                      TRUE ~ lag(salvagedC_track_90_losspaper, default = 0, order_by = Time) - salvagedC_track_90_losspaper),
         # Total --------------------
         salvagedC_emissions = salvagedC_emissions_15 + salvagedC_emissions_40 + salvagedC_emissions_65 + salvagedC_emissions_90,
         salvagedC_emissions_lossboard = salvagedC_emissions_15_lossboard + salvagedC_emissions_40_lossboard + salvagedC_emissions_65_lossboard + salvagedC_emissions_90_lossboard,
         salvagedC_emissions_losspaper = salvagedC_emissions_15_losspaper + salvagedC_emissions_40_losspaper + salvagedC_emissions_65_losspaper + salvagedC_emissions_90_losspaper,
         salvage_related_emissions = harvest_emissions + scarif_emissions + manuf_emissions_wood + manuf_emissions_paper + 
           transport_emissions + manuf_loss_emissions + salvagedC_emissions,
         salvage_related_emissions_lossboard = harvest_emissions + scarif_emissions  + manuf_emissions_wood + manuf_emissions_paper + 
           transport_emissions + loss_manuf_emissions_board + salvagedC_emissions_lossboard,
         salvage_related_emissions_losspaper = harvest_emissions + scarif_emissions + manuf_emissions_wood + manuf_emissions_paper + 
           transport_emissions + loss_manuf_emissions_paper + salvagedC_emissions_losspaper
  ) %>% 
  ungroup()

write_csv(salvage_df4, 'salvage_df4.csv')

# RCP2.6 with energy transformation
necn_df3 <- bind_cols(necn_df, index_cf_df)
necn_df4 <- necn_df3 %>% filter(Climate == 'CSIRO-Mk3-6-0_RCP2.6')
nfcb_df3 <- bind_cols(salvage_df4, necn_df4$NEP) %>% 
  rename(NEP = ...57) %>% 
  mutate(NFCB = NEP - salvage_related_emissions,
         NFCB_lossboard = NEP - salvage_related_emissions_lossboard,
         NFCB_losspaper = NEP - salvage_related_emissions_losspaper) %>% 
  group_by(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name) %>% 
  mutate(cumulativeNFCB = cumsum(NFCB),
         cumulativeNFCB_lossboard = cumsum(NFCB_lossboard),
         cumulativeNFCB_losspaper = cumsum(NFCB_losspaper)) %>% 
  ungroup() %>% 
  dplyr::select(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name, Time, 
                salvagedC, harvest_emissions, scarif_emissions, manuf_emissions_wood, manuf_emissions_paper, 
                transport_emissions, manuf_loss_emissions, loss_manuf_emissions_board, loss_manuf_emissions_paper, 
                starts_with('salvagedC_emissions'), starts_with('salvage_related_emissions'),
                NEP, NFCB, NFCB_lossboard, NFCB_losspaper, cumulativeNFCB, cumulativeNFCB_lossboard, cumulativeNFCB_losspaper) %>% 
  gather(key = flux_type, value = flux,
         salvagedC, harvest_emissions, scarif_emissions, manuf_emissions_wood, manuf_emissions_paper, 
         transport_emissions, manuf_loss_emissions, loss_manuf_emissions_board, loss_manuf_emissions_paper, 
         starts_with('salvagedC_emissions'), starts_with('salvage_related_emissions'),
         NEP, NFCB, NFCB_lossboard, NFCB_losspaper, cumulativeNFCB, cumulativeNFCB_lossboard, cumulativeNFCB_losspaper) %>% 
  group_by(Climate, WindthrowRegime, PostWindthrowManagement, flux_type, Time) %>% 
  summarise(mean = mean(flux),
            max = max(flux),
            min = min(flux),
            sd = sd(flux)) %>% 
  mutate(Climate = 'CSIRO-Mk3-6-0_RCP2.6_energy')

# Current, rcp2.6, rcp8.5 normal ver
nfcb_df_clim <- bind_cols(salvage_df3, necn_df3$NEP) %>% 
  rename(NEP = ...57) %>% 
  mutate(NFCB = NEP - salvage_related_emissions,
         NFCB_lossboard = NEP - salvage_related_emissions_lossboard,
         NFCB_losspaper = NEP - salvage_related_emissions_losspaper) %>% 
  group_by(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name) %>% 
  mutate(cumulativeNFCB = cumsum(NFCB),
         cumulativeNFCB_lossboard = cumsum(NFCB_lossboard),
         cumulativeNFCB_losspaper = cumsum(NFCB_losspaper)) %>% 
  ungroup() %>% 
  dplyr::select(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name, Time, 
                salvagedC, harvest_emissions, scarif_emissions, manuf_emissions_wood, manuf_emissions_paper, 
                transport_emissions, manuf_loss_emissions, loss_manuf_emissions_board, loss_manuf_emissions_paper, 
                starts_with('salvagedC_emissions'), starts_with('salvage_related_emissions'),
                NEP, NFCB, NFCB_lossboard, NFCB_losspaper, cumulativeNFCB, cumulativeNFCB_lossboard, cumulativeNFCB_losspaper) %>% 
  gather(key = flux_type, value = flux,
         salvagedC, harvest_emissions, scarif_emissions, manuf_emissions_wood, manuf_emissions_paper, 
         transport_emissions, manuf_loss_emissions, loss_manuf_emissions_board, loss_manuf_emissions_paper, 
         starts_with('salvagedC_emissions'), starts_with('salvage_related_emissions'),
         NEP, NFCB, NFCB_lossboard, NFCB_losspaper, cumulativeNFCB, cumulativeNFCB_lossboard, cumulativeNFCB_losspaper) %>% 
  group_by(Climate, WindthrowRegime, PostWindthrowManagement, flux_type, Time) %>% 
  summarise(mean = mean(flux),
            max = max(flux),
            min = min(flux),
            sd = sd(flux))


## For publication 修論2 sensitivity analysis
nfcb_df_clim_ener <- bind_rows(nfcb_df_clim, nfcb_df3) %>% 
  filter(flux_type %in% c('NFCB_lossboard', 'cumulativeNFCB_lossboard',
                          'harvest_emissions', 'scarif_emissions', 'manuf_emissions_wood', 'manuf_emissions_paper', 
                          'loss_manuf_emissions_board', 'salvagedC_emissions_lossboard', 'salvage_related_emissions_lossboard'),
         Time != 0)
nfcb_df_clim_ener$flux_type <- factor(nfcb_df_clim_ener$flux_type,
                                levels = c('NFCB_lossboard', 'cumulativeNFCB_lossboard',
                                           'harvest_emissions', 'scarif_emissions', 'manuf_emissions_wood', 'manuf_emissions_paper', 
                                           'loss_manuf_emissions_board', 'salvagedC_emissions_lossboard', 'salvage_related_emissions_lossboard'),
                                labels = c('NFCB', 'Cumulative NFCB',
                                           'harvest_emissions', 'scarif_emissions', 'manuf_emissions_wood', 'manuf_emissions_paper', 
                                           'loss_manuf_emissions_board', 'salvagedC_emissions_lossboard', 'salvage_related_emissions_lossboard'))
nfcb_df_clim_ener$Climate <- factor(nfcb_df_clim_ener$Climate, 
                                     levels = c("current", "CSIRO-Mk3-6-0_RCP2.6", "CSIRO-Mk3-6-0_RCP2.6_energy", "GFDL-CM3_RCP8.5"),
                                     labels = c("Current", "RCP2.6 (CSIRO-Mk3-6-0)", "RCP2.6 (CSIRO-Mk3-6-0) decarbonization", "RCP8.5 (GFDL-CM3)"))
nfcb_df_clim_ener$WindthrowRegime <- factor(nfcb_df_clim_ener$WindthrowRegime,
                                   levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'),
                                   labels = c('Historical', 'Frequent', 'Intense', 'Frequent & Intense'))
nfcb_df_clim_ener$PostWindthrowManagement <- factor(nfcb_df_clim_ener$PostWindthrowManagement,
                                           levels = c('WT', 'SL', 'SLSC'))


nfcb_df_clim_ener2 <- nfcb_df_clim_ener %>% 
  select(-max, -min, -sd) %>% 
  spread(key = flux_type, value = mean) %>% 
  mutate(total_pwm_emissions = harvest_emissions + scarif_emissions + manuf_emissions_wood + manuf_emissions_paper + 
           loss_manuf_emissions_board + salvagedC_emissions_lossboard) %>% 
  select(-NFCB, -`Cumulative NFCB`) %>%
  gather(key = flux_type, value = mean,
         harvest_emissions, scarif_emissions, manuf_emissions_wood, manuf_emissions_paper,
         loss_manuf_emissions_board, salvagedC_emissions_lossboard, total_pwm_emissions)
nfcb_df_clim_ener2$flux_type <- factor(nfcb_df_clim_ener2$flux_type,
                                 levels = c('harvest_emissions', 'scarif_emissions', 'manuf_emissions_wood', 'manuf_emissions_paper', 
                                            'loss_manuf_emissions_board', 'salvagedC_emissions_lossboard', 'total_pwm_emissions'),
                                 labels = c('harvest_emissions', 'scarif_emissions', 'manuf_emissions_wood', 'manuf_emissions_paper', 
                                            'loss_manuf_emissions_board', 'salvagedC_emissions_lossboard', 'total_pwm_emissions'))
nfcb_df_clim_ener2$Climate <- factor(nfcb_df_clim_ener2$Climate, 
                            levels = c("current", "CSIRO-Mk3-6-0_RCP2.6", "CSIRO-Mk3-6-0_RCP2.6_energy", "GFDL-CM3_RCP8.5"),
                            labels = c("Current", "RCP2.6 (CSIRO-Mk3-6-0)", "RCP2.6 (CSIRO-Mk3-6-0) decarbonization", "RCP8.5 (GFDL-CM3)"))
nfcb_df_clim_ener2$WindthrowRegime <- factor(nfcb_df_clim_ener2$WindthrowRegime,
                                            levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'),
                                            labels = c('Historical', 'Frequent', 'Intense', 'Frequent & Intense'))
nfcb_df_clim_ener2$PostWindthrowManagement <- factor(nfcb_df_clim_ener2$PostWindthrowManagement,
                                                    levels = c('WT', 'SL', 'SLSC'))


#Figures
lca_a_clim_nfcb <- nfcb_df_clim_ener %>% 
  filter(flux_type %in% c('NFCB')) %>% 
  ggplot() +
  geom_line(aes(x = Time+2015, y = mean, color = PostWindthrowManagement), size = 1) +
  #geom_ribbon(aes(x = Time+2015, ymin = min, ymax = max, fill = PostWindthrowManagement), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_blank()) +
  ggtitle('NFCB') +
  labs(x = 'Simulation year', 
       y = expression(paste("Carbon flux (gC ", {m^-2}, {year^-1}, ")", sep=""))) +
  scale_colour_hc() +
  scale_fill_hc() +
  scale_x_continuous(breaks=seq(2030, 2130,length=5), limits=c(2015,2130))

lca_a_clim_cumnfcb <- nfcb_df_clim_ener %>% 
  filter(flux_type %in% c('Cumulative NFCB')) %>% 
  ggplot() +
  geom_line(aes(x = Time+2015, y = mean, color = PostWindthrowManagement), size = 1) +
  #geom_ribbon(aes(x = Time+2015, ymin = min, ymax = max, fill = PostWindthrowManagement), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(Climate ~ WindthrowRegime, scales = 'free_y') +
  theme_bw(base_size = 15) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_blank()) +
  ggtitle('Cumulative NFCB') +
  labs(x = 'Simulation year', 
       y = expression(paste("Carbon flux (gC ", {m^-2}, {year^-1}, ")", sep=""))) +
  scale_colour_hc() +
  scale_fill_hc() +
  scale_x_continuous(breaks=seq(2030, 2130,length=5), limits=c(2015,2130))

lca_b_clim_sl <- nfcb_df_clim_ener %>% 
  filter(!(flux_type %in% c('NFCB', 'Cumulative NFCB', 'salvage_related_emissions_lossboard')),
         PostWindthrowManagement == 'SL') %>% 
  ggplot() +
  geom_bar(aes(x = Time+2015, y = -mean, fill = flux_type), 
           stat = 'identity', position = 'stack', width = 1, color = NA) +
  #geom_line(aes(x = Time+2015, y = -mean,)) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        legend.text.align = 0,
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_blank()
  ) +
  ggtitle('SL') +
  labs(x = 'Simulation year', 
       y = expression(paste("Carbon flux (gC ", {m^-2}, " ", {Year^-1}, ')', sep=""))) +
  scale_fill_colorblind(#palette = 'Spectral' ,
    labels = 
      c(harvest_emissions = expression(paste('CE'[italic('Salvage')])), 
        scarif_emissions = expression(paste({'CE'[italic('Scarification')]})),
        manuf_emissions_wood = expression(paste({'CE'[italic('ManufactureLong-lived')]})),
        manuf_emissions_paper = expression(paste({'CE'[italic('ManufactureShort-lived')]})),
        loss_manuf_emissions_board = expression(paste({'CE'[italic('ManufactureWoodenBoard')]})),
        salvagedC_emissions_lossboard = expression(paste({'CE'[italic('WoodProductsDecomposed')]})))) +
  scale_x_continuous(breaks=seq(2030, 2130,length=5), limits=c(2015,2130))

lca_b_clim_slsc <- nfcb_df_clim_ener %>% 
  filter(!(flux_type %in% c('NFCB', 'Cumulative NFCB', 'salvage_related_emissions_lossboard')),
         PostWindthrowManagement == 'SLSC') %>% 
  ggplot() +
  geom_bar(aes(x = Time+2015, y = -mean, fill = flux_type), 
           stat = 'identity', position = 'stack', width = 1, color = NA) +
  #geom_line(aes(x = Time+2015, y = -mean,)) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        legend.text.align = 0,
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_blank()
  ) +
  ggtitle('SLSC') +
  labs(x = 'Simulation year', 
       y = expression(paste("Carbon flux (gC ", {m^-2}, " ", {Year^-1}, ')', sep=""))) +
  scale_fill_colorblind(#palette = 'Spectral' ,
    labels = 
      c(harvest_emissions = expression(paste('CE'[italic('Salvage')])), 
        scarif_emissions = expression(paste({'CE'[italic('Scarification')]})),
        manuf_emissions_wood = expression(paste({'CE'[italic('ManufactureLong-lived')]})),
        manuf_emissions_paper = expression(paste({'CE'[italic('ManufactureShort-lived')]})),
        loss_manuf_emissions_board = expression(paste({'CE'[italic('ManufactureWoodenBoard')]})),
        salvagedC_emissions_lossboard = expression(paste({'CE'[italic('WoodProductsDecomposed')]})))) +
  scale_x_continuous(breaks=seq(2030, 2130,length=5), limits=c(2015,2130))

lca_c_clim_sl <- ggplot() +
  geom_bar(data = nfcb_df_clim_ener %>% 
             filter(!(flux_type %in% c('NFCB', 'Cumulative NFCB', 'salvage_related_emissions_lossboard')),
                    PostWindthrowManagement == 'SL',
                    Time %in% c(15, 40, 65, 90)), 
           aes(x = Time+2015, y = -mean, fill = flux_type), 
           stat = 'identity', position = 'stack', width = 10) +
  geom_line(data = nfcb_df_clim_ener %>% 
              filter(flux_type == 'salvage_related_emissions_lossboard',
                     PostWindthrowManagement == 'SL',
                     Time >= 15),
            aes(x = Time+2015, y = -mean), size = 0.7, color = 'gray30') +
  # geom_ribbon(data = nfcb_df_pub %>% 
  #               filter(flux_type == 'salvage_related_emissions_lossboard',
  #                    PostWindthrowManagement != 'WT',
  #                    Time >= 15),
  #             aes(x = Time+2015, ymin = -max, ymax = -min), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        legend.text.align = 0,
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_blank()
  ) +
  ggtitle('SL') +
  labs(x = 'Simulation year', 
       y = expression(paste("Carbon flux (gC ", {m^-2}, " ", {year^-1}, ')', sep=""))) +
  scale_fill_brewer(palette = 'Spectral' ,
                    labels = 
                      c(harvest_emissions = expression(paste('CE'[italic('Salvage')])), 
                        scarif_emissions = expression(paste({'CE'[italic('Scarification')]})),
                        manuf_emissions_wood = expression(paste({'CE'[italic('ManufactureLong-lived')]})),
                        manuf_emissions_paper = expression(paste({'CE'[italic('ManufactureShort-lived')]})),
                        loss_manuf_emissions_board = expression(paste({'CE'[italic('ManufactureWoodenBoard')]})),
                        salvagedC_emissions_lossboard = expression(paste({'CE'[italic('WoodProductsDecomposed')]})))) +
  scale_x_continuous(breaks=seq(2030, 2130,length=5), limits=c(2015,2130))


lca_c_clim_slsc <- ggplot() +
  geom_bar(data = nfcb_df_clim_ener %>% 
             filter(!(flux_type %in% c('NFCB', 'Cumulative NFCB', 'salvage_related_emissions_lossboard')),
                    PostWindthrowManagement == 'SLSC',
                    Time %in% c(15, 40, 65, 90)), 
           aes(x = Time+2015, y = -mean, fill = flux_type), 
           stat = 'identity', position = 'stack', width = 10) +
  geom_line(data = nfcb_df_clim_ener %>% 
              filter(flux_type == 'salvage_related_emissions_lossboard',
                     PostWindthrowManagement == 'SLSC',
                     Time >= 15),
            aes(x = Time+2015, y = -mean), size = 0.7, color = 'gray30') +
  # geom_ribbon(data = nfcb_df_pub %>% 
  #               filter(flux_type == 'salvage_related_emissions_lossboard',
  #                    PostWindthrowManagement != 'WT',
  #                    Time >= 15),
  #             aes(x = Time+2015, ymin = -max, ymax = -min), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(Climate ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        legend.text.align = 0,
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_blank()
  ) +
  ggtitle('SLSC') +
  labs(x = 'Simulation year', 
       y = expression(paste("Carbon flux (gC ", {m^-2}, " ", {year^-1}, ')', sep=""))) +
  scale_fill_brewer(palette = 'Spectral' ,
                    labels = 
                      c(harvest_emissions = expression(paste('CE'[italic('Salvage')])), 
                        scarif_emissions = expression(paste({'CE'[italic('Scarification')]})),
                        manuf_emissions_wood = expression(paste({'CE'[italic('ManufactureLong-lived')]})),
                        manuf_emissions_paper = expression(paste({'CE'[italic('ManufactureShort-lived')]})),
                        loss_manuf_emissions_board = expression(paste({'CE'[italic('ManufactureWoodenBoard')]})),
                        salvagedC_emissions_lossboard = expression(paste({'CE'[italic('WoodProductsDecomposed')]})))) +
  scale_x_continuous(breaks=seq(2030, 2130,length=5), limits=c(2015,2130))


plca_clim1 <- lca_c_clim_sl + lca_c_clim_slsc + 
  plot_layout(ncol = 1)

plca_clim2 <- lca_b_clim_sl + lca_b_clim_slsc + 
  plot_layout(ncol = 1)

plca_clim3 <- lca_a_clim_nfcb + lca_a_clim_cumnfcb + 
  plot_layout(ncol = 1)

ggsave('lca1_sa.tiff', plca_clim3, dpi = 600, width = 15, height = 8)
ggsave('lca2_sa.tiff', plca_clim2, dpi = 600, width = 15, height = 8)
ggsave('lca3_sa.tiff', plca_clim1, dpi = 600, width = 15, height = 8)


