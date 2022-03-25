# ----------------------------------------------------------------------
# C flux & Life Cycle Analysis at landscape-scale
# 2021.03.15
# Reference --------------------------------------
# Law et al. (2018) PNAS
# Hudiburg et al. (2019) Environ. Res. Lett.
# Suzuki et al. (2019) For. Ecol. Manag.
# ----------------------------------------------------------------------


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

salvage_df3 <- bind_cols(salvage_df, index_cf_df) %>% 
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
         harvest_emissions = salvagedC * 0.00986278, # 宇城ら 2012より算出
         scarif_emissions = case_when((PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'historicWindthrow' & Time %in% c(15, 65)) ~
                                        0.05 * 0.2 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'frequent' & Time %in% c(15, 40, 65, 90)) ~ 
                                        0.05 * 0.2 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'intense' & Time %in% c(15, 65)) ~ 
                                        0.05 * 0.4 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'frequentintense' & Time %in% c(15, 40, 65, 90)) ~ 
                                        0.05 * 0.4 * 10^6 / 10^4,
                                      TRUE ~ 0), # Owari et al. (2011) 0.05 = carbon emissions by scarification (tC/ha), 0.2or0.4 = Windthrow area ratio
         manuf_emissions_wood = salvagedC * 0.7 * 0.65 * 0.09262436, # 宇城ら 2012; Suzuki et al. 2019, App.F
         manuf_emissions_paper = salvagedC * 0.3 * 0.15, # Hudiburg et al. 2019, Table S5; Suzuki et al. 2019, App.F
         transport_emissions = salvagedC *0.000155, # Hudiburg et al. 2019, Table S5
         manuf_loss_emissions = salvagedC * 0.7 * (1 - 0.65), # Suzuki et al. 2019, App.F # lossがすべて廃棄処分される場合
         loss_manuf_emissions_board = salvagedC * 0.7 * (1 - 0.65) * 0.1444, # 南部ら 2012 # lossがすべてボード生産に回った場合
         loss_manuf_emissions_paper = salvagedC * 0.7 * (1 - 0.65) * 0.15, # Hudiburg et al. 2019, Table S5; Suzuki et al. 2019, App.F # lossがすべて紙製品に回った場合
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

salvage_summary_df2 <- salvage_df3 %>% 
  dplyr::select(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name, Time, 
                salvagedC, harvest_emissions, scarif_emissions, manuf_emissions_wood, manuf_emissions_paper, 
                transport_emissions, manuf_loss_emissions, loss_manuf_emissions_board, loss_manuf_emissions_paper, 
                starts_with('salvagedC_emissions'), starts_with('salvage_related_emissions')) %>% 
  gather(key = emission_type, value = emissions,
         salvagedC, harvest_emissions, scarif_emissions, manuf_emissions_wood, manuf_emissions_paper, 
         transport_emissions, manuf_loss_emissions, loss_manuf_emissions_board, loss_manuf_emissions_paper, 
         starts_with('salvagedC_emissions'), starts_with('salvage_related_emissions')) %>% 
  group_by(scenario, Climate, WindthrowRegime, PostWindthrowManagement, emission_type, Time) %>% 
  summarise(mean = mean(emissions),
            max = max(emissions),
            min = min(emissions),
            sd = sd(emissions))


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
necn_df2 %>% 
  filter(Time != 0) %>%
  ggplot() +geom_line(aes(x = Time, y = meanflux, color = PostWindthrowManagement), size = 1) +
  geom_ribbon(aes(x = Time, ymin = minflux, ymax = maxflux, fill = PostWindthrowManagement), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(type ~ WindthrowRegime, scales = 'free_y') +
  theme_bw(base_size = 20) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_line(size = 0.3)) +
  ggtitle('Changes in carbon flux in landscape') +
  labs(x = 'Time', 
       y = expression(paste("Carbon flux (g/ ", {m^2}, "/year)", sep="")),
       caption = 'Shaded areas represent the ranges of maximum and minimum values among all climate scenarios.') +
  scale_colour_hc() +
  scale_fill_hc()
# scale_colour_viridis(discrete = T) +
# scale_fill_viridis(discrete = T)
# scale_color_manual(values = c('NEP' = 'grey70', 'NPP' = 'palegreen3', 'Rh' = 'lightcoral'),
#                    labels = c('NEP', 'NPP', 'Rh')) +


# Net Forest sector Carbon Balance ------------------------------------------------------------------------
necn_df3 <- bind_cols(necn_df, index_cf_df)
nfcb_df2 <- bind_cols(salvage_df3, necn_df3$NEP) %>% 
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
  group_by(WindthrowRegime, PostWindthrowManagement, flux_type, Time) %>% 
  summarise(mean = mean(flux),
            max = max(flux),
            min = min(flux),
            sd = sd(flux))

nfcb_df2$WindthrowRegime <- factor(nfcb_df2$WindthrowRegime,
                                   levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'),
                                   labels = c('Historical', 'Frequent', 'Intense', 'Frequent & Intense'))
nfcb_df2$PostWindthrowManagement <- factor(nfcb_df2$PostWindthrowManagement,
                                           levels = c('WT', 'SL', 'SLSC'))

nfcb_df2 %>% 
  filter(flux_type %in% c('NEP', 'NFCB', 'NFCB_lossboard', 'NFCB_losspaper', 'cumulativeNFCB', 'cumulativeNFCB_lossboard', 'cumulativeNFCB_losspaper'),
         Time != 0) %>% 
  ggplot() +
  geom_line(aes(x = Time, y = mean, color = PostWindthrowManagement), size = 1) +
  geom_ribbon(aes(x = Time, ymin = min, ymax = max, fill = PostWindthrowManagement), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(flux_type ~ WindthrowRegime, scales = 'free_y') +
  theme_bw(base_size = 20) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_line(size = 0.3)) +
  ggtitle('Changes in carbon flux in landscape') +
  labs(x = 'Time', 
       y = expression(paste("Carbon flux (g/ ", {m^2}, "/year)", sep="")),
       caption = 'Shaded areas represent the ranges of maximum and minimum values among all climate scenarios.') +
  scale_colour_hc() +
  scale_fill_hc()

# nfcb_df2 %>% 
#   filter(flux_type %in% c('NEP', 'NFCB', 'NFCB_lossboard', 'NFCB_losspaper', 'cumulativeNFCB', 'cumulativeNFCB_lossboard', 'cumulativeNFCB_losspaper'),
#          Time != 0) %>% 
ggplot() +
  # geom_line(data = nfcb_df2 %>% filter(flux_type == "cumulativeNFCB", Time != 0),
  #           aes(x = Time, y = mean, color = PostWindthrowManagement), size = 1) +
  geom_line(data = nfcb_df2 %>% filter(flux_type == "cumulativeNFCB_lossboard", Time != 0),
            aes(x = Time+2015, y = mean, color = PostWindthrowManagement), size = 1) +
  geom_line(data = nfcb_df2 %>% filter(flux_type == "cumulativeNFCB_losspaper", Time != 0),
            aes(x = Time+2015, y = mean, color = PostWindthrowManagement), size = 1, lty = "dotted") +
  # geom_ribbon(data = nfcb_df2 %>% filter(flux_type == "cumulativeNFCB", Time != 0),
  #             aes(x = Time, ymin = min, ymax = max, fill = PostWindthrowManagement), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid( ~ WindthrowRegime, scales = 'free_y') +
  theme_bw(base_size = 15) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_line(size = 0.3),
        plot.margin = unit(c(1,1,1,1), 'lines')) +
  #ggtitle('Changes in carbon flux in landscape') +
  labs(x = 'Simulation year', 
       y = expression(paste("Carbon flux (gC ", {m^-2}, {year^-1}, ")", sep=""))) +
  scale_colour_hc() +
  scale_fill_hc() +
  scale_x_continuous(breaks=seq(2030, 2130,length=5), limits=c(2015,2130))

## For publication 修論2
nfcb_df_pub <- nfcb_df2 %>% 
  filter(flux_type %in% c('NFCB_lossboard', 'cumulativeNFCB_lossboard',
                          'harvest_emissions', 'scarif_emissions', 'manuf_emissions_wood', 'manuf_emissions_paper', 
                          'loss_manuf_emissions_board', 'salvagedC_emissions_lossboard', 'salvage_related_emissions_lossboard'),
         Time != 0)
nfcb_df_pub$flux_type <- factor(nfcb_df_pub$flux_type,
                                   levels = c('NFCB_lossboard', 'cumulativeNFCB_lossboard',
                                              'harvest_emissions', 'scarif_emissions', 'manuf_emissions_wood', 'manuf_emissions_paper', 
                                              'loss_manuf_emissions_board', 'salvagedC_emissions_lossboard', 'salvage_related_emissions_lossboard'),
                                   labels = c('NFCB', 'Cumulative NFCB',
                                              'harvest_emissions', 'scarif_emissions', 'manuf_emissions_wood', 'manuf_emissions_paper', 
                                              'loss_manuf_emissions_board', 'salvagedC_emissions_lossboard', 'salvage_related_emissions_lossboard'))
nfcb_df_pub2 <- nfcb_df_pub %>% 
  select(-max, -min, -sd) %>% 
  spread(key = flux_type, value = mean) %>% 
  mutate(total_pwm_emissions = harvest_emissions + scarif_emissions + manuf_emissions_wood + manuf_emissions_paper + 
                                loss_manuf_emissions_board + salvagedC_emissions_lossboard) %>% 
  select(-NFCB, -`Cumulative NFCB`) %>%
  gather(key = flux_type, value = mean,
         harvest_emissions, scarif_emissions, manuf_emissions_wood, manuf_emissions_paper,
         loss_manuf_emissions_board, salvagedC_emissions_lossboard, total_pwm_emissions)
nfcb_df_pub2$flux_type <- factor(nfcb_df_pub2$flux_type,
                                levels = c('harvest_emissions', 'scarif_emissions', 'manuf_emissions_wood', 'manuf_emissions_paper', 
                                           'loss_manuf_emissions_board', 'salvagedC_emissions_lossboard', 'total_pwm_emissions'),
                                labels = c('harvest_emissions', 'scarif_emissions', 'manuf_emissions_wood', 'manuf_emissions_paper', 
                                           'loss_manuf_emissions_board', 'salvagedC_emissions_lossboard', 'total_pwm_emissions'))

lca_a <- nfcb_df_pub %>% 
  filter(flux_type %in% c('NFCB', 'Cumulative NFCB')) %>% 
  ggplot() +
  geom_line(aes(x = Time+2015, y = mean, color = PostWindthrowManagement), size = 1) +
  geom_ribbon(aes(x = Time+2015, ymin = min, ymax = max, fill = PostWindthrowManagement), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(flux_type ~ WindthrowRegime, scales = 'free_y') +
  theme_bw(base_size = 15) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_blank()) +
  #ggtitle('Changes in carbon flux in landscape') +
  labs(x = 'Simulation year', 
       y = expression(paste("Carbon flux (gC ", {m^-2}, {year^-1}, ")", sep="")),
       caption = 'Shaded areas represent the ranges of maximum and minimum values among all climate scenarios.') +
  scale_colour_hc() +
  scale_fill_hc() +
  scale_x_continuous(breaks=seq(2030, 2130,length=5), limits=c(2015,2130))

lca_b <- nfcb_df_pub %>% 
  filter(!(flux_type %in% c('NFCB', 'Cumulative NFCB')),
         PostWindthrowManagement != 'WT') %>% 
  ggplot() +
  geom_bar(aes(x = Time+2015, y = -mean, fill = flux_type), 
           stat = 'identity', position = 'stack', width = 1) +
  geom_line(aes(x = Time+2015, y = -mean,))
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(PostWindthrowManagement ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        legend.text.align = 0,
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_blank()
  ) +
  #ggtitle('Carbon emissions related to salvage logging') +
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
  
  


  
lca_c <- ggplot() +
  geom_bar(data = nfcb_df_pub %>% 
                    filter(!(flux_type %in% c('NFCB', 'Cumulative NFCB', 'salvage_related_emissions_lossboard')),
                           PostWindthrowManagement != 'WT',
                           Time %in% c(15, 40, 65, 90)), 
           aes(x = Time+2015, y = -mean, fill = flux_type), 
           stat = 'identity', position = 'stack', width = 10) +
  geom_line(data = nfcb_df_pub %>% 
              filter(flux_type == 'salvage_related_emissions_lossboard',
                     PostWindthrowManagement != 'WT',
                     Time >= 15),
            aes(x = Time+2015, y = -mean), size = 0.7, color = 'gray30') +
  # geom_ribbon(data = nfcb_df_pub %>% 
  #               filter(flux_type == 'salvage_related_emissions_lossboard',
  #                    PostWindthrowManagement != 'WT',
  #                    Time >= 15),
  #             aes(x = Time+2015, ymin = -max, ymax = -min), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(PostWindthrowManagement ~ WindthrowRegime) +
  theme_bw(base_size = 15) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        legend.text.align = 0,
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_blank()
  ) +
  #ggtitle('Carbon emissions related to salvage logging') +
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
plca <- cf + lca_c + lca_a + 
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "a")

ggsave('lca3.tiff', plca, dpi = 600, width = 14, height = 15)

# Cumulative NFCB ----------------------------
cum_nfcb_df2 <- bind_cols(salvage_df3, necn_df3$NEP) %>% 
  rename(NEP = ...35) %>% 
  mutate(NFCB = NEP - salvage_related_emissions) %>% 
  filter(Time >= 15) %>% 
  group_by(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name) %>% 
  mutate(cumulativeNFCB = cumsum(NFCB)) %>% 
  ungroup() %>% 
  dplyr::select(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name, Time, 
                NEP, NFCB, cumulativeNFCB) %>% 
  gather(key = flux_type, value = flux,
         NEP, NFCB, cumulativeNFCB) %>% 
  group_by(WindthrowRegime, PostWindthrowManagement, flux_type, Time) %>% 
  summarise(mean = mean(flux),
            max = max(flux),
            min = min(flux),
            sd = sd(flux))

cum_nfcb_df2$WindthrowRegime <- factor(cum_nfcb_df2$WindthrowRegime,
                                       levels = c('historicWindthrow', 'frequent', 'intense', 'frequentintense'),
                                       labels = c('Historical', 'Frequent', 'Intense', 'Frequent & Intense'))
cum_nfcb_df2$PostWindthrowManagement <- factor(cum_nfcb_df2$PostWindthrowManagement,
                                               levels = c('WT', 'SL', 'SLSC'))
cum_nfcb_df2$flux_type <- factor(cum_nfcb_df2$flux_type,
                                 levels = c('cumulativeNFCB', 'NFCB', 'NEP'),
                                 labels = c('Cumulative NFCB', 'NFCB', 'NEP'))

cum_nfcb_df2 %>% 
  filter(flux_type %in% c('NFCB', 'Cumulative NFCB'),
         Time != 0) %>% 
  mutate(flux_type = (str_extract_all(flux_type, '.{1,10}') %>% map_chr(., ~ paste(., collapse = "\n")))) %>% 
  ggplot() +
  geom_line(aes(x = Time, y = mean, color = PostWindthrowManagement), size = 1) +
  geom_ribbon(aes(x = Time, ymin = min, ymax = max, fill = PostWindthrowManagement), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(flux_type ~ WindthrowRegime, scales = 'free_y'
             #labeller = label_wrap_gen(10)
  ) +
  theme_bw(base_size = 25) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_line(size = 0.7),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15)) +
  #ggtitle('Changes in Net Forest sector Carbon Balance in landscape') +
  labs(x = 'Time', 
       y = expression(paste("Carbon flux (g/ ", {m^2}, "/year)", sep=""))
       #caption = 'Shaded areas represent the ranges of maximum and minimum values among all climate scenarios.'
  ) +
  scale_colour_hc() +
  scale_fill_hc()
?facet_grid
ggsave(file = "esj_nfcb_nolegend.png", dpi = 300, width = 13.47, height = 7.96)
