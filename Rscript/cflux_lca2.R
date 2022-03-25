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
         harvest_emissions = salvagedC * 0.0040576, # 宇城ら 2012より算出
         scarif_emissions = case_when((PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'historicWindthrow' & Time %in% c(15, 65)) ~
                                        0.05 * 0.2 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'frequent' & Time %in% c(15, 40, 65, 90)) ~ 
                                        0.05 * 0.2 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'intense' & Time %in% c(15, 65)) ~ 
                                        0.05 * 0.4 * 10^6 / 10^4,
                                      (PostWindthrowManagement == 'SLSC' & WindthrowRegime == 'frequentintense' & Time %in% c(15, 40, 65, 90)) ~ 
                                        0.05 * 0.4 * 10^6 / 10^4,
                                      TRUE ~ 0), # Owari et al. (2011) 0.05 = carbon emissions by scarification (tC/ha), 0.2or0.4 = Windthrow area ratio
         manuf_emissions_wood = salvagedC * 0.7 * 0.65 * 0.0902439, # 宇城ら 2012; Suzuki et al. 2019, App.F
         manuf_emissions_paper = salvagedC * 0.3 * 0.15, # Hudiburg et al. 2019, Table S5; Suzuki et al. 2019, App.F
         transport_emissions = salvagedC *0.000155, # Hudiburg et al. 2019, Table S5
         manuf_loss_emissions = salvagedC * 0.7 * (1 - 0.65), # Suzuki et al. 2019, App.F
         # year 15 -------------------
         salvagedC_15only = case_when(Time == 15 ~ salvagedC,
                                  TRUE ~ 0),
         salvagedC_15 = cumsum(salvagedC_15only),
         salvagedC_track_15 = salvagedC_15 * (0.7*0.65*exp(-0.0198*(Time-15)) + 0.3*1.0*exp(-0.347*(Time-15))), # Suzuki et al. 2019, App.F
         salvagedC_emissions_15 = case_when(Time == 15 ~ 0,
                                            TRUE ~ lag(salvagedC_track_15, default = 0, order_by = Time) - salvagedC_track_15),
         # year 40 -------------------
         salvagedC_40only = case_when(Time == 40 ~ salvagedC,
                                  TRUE ~ 0),
         salvagedC_40 = cumsum(salvagedC_40only),
         salvagedC_track_40 = salvagedC_40 * (0.7*0.65*exp(-0.0198*(Time-40)) + 0.3*1.0*exp(-0.347*(Time-40))), # Suzuki et al. 2019, App.F
         salvagedC_emissions_40 = case_when(Time == 40 ~ 0,
                                            TRUE ~ lag(salvagedC_track_40, default = 0, order_by = Time) - salvagedC_track_40),
         # year 65 ------------------
         salvagedC_65only = case_when(Time == 65 ~ salvagedC,
                                  TRUE ~ 0),
         salvagedC_65 = cumsum(salvagedC_65only),
         salvagedC_track_65 = salvagedC_65 * (0.7*0.65*exp(-0.0198*(Time-65)) + 0.3*1.0*exp(-0.347*(Time-65))), # Suzuki et al. 2019, App.F
         salvagedC_emissions_65 = case_when(Time == 65 ~ 0,
                                            TRUE ~ lag(salvagedC_track_65, default = 0, order_by = Time) - salvagedC_track_65),
         # year 90 ------------------
         salvagedC_90only = case_when(Time >= 90 ~ salvagedC,
                                  TRUE ~ 0),
         salvagedC_90 = cumsum(salvagedC_90only),
         salvagedC_track_90 = salvagedC_90 * (0.7*0.65*exp(-0.0198*(Time-90)) + 0.3*1.0*exp(-0.347*(Time-90))), # Suzuki et al. 2019, App.F
         salvagedC_emissions_90 = case_when(Time == 90 ~ 0,
                                            TRUE ~ lag(salvagedC_track_90, default = 0, order_by = Time) - salvagedC_track_90),
         # Total --------------------
         salvagedC_emissions = salvagedC_emissions_15 + salvagedC_emissions_40 + salvagedC_emissions_65 + salvagedC_emissions_90,
         salvage_related_emissions = harvest_emissions + manuf_emissions_wood + manuf_emissions_paper + 
           transport_emissions + manuf_loss_emissions + salvagedC_emissions
         ) %>% 
  ungroup()

salvage_summary_df2 <- salvage_df3 %>% 
  dplyr::select(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name, Time, 
         salvagedC, harvest_emissions, manuf_emissions_wood, manuf_emissions_paper, 
         transport_emissions, manuf_loss_emissions, starts_with('salvagedC_emissions'), salvage_related_emissions) %>% 
  gather(key = emission_type, value = emissions,
         salvagedC, harvest_emissions, manuf_emissions_wood, manuf_emissions_paper, 
         transport_emissions, manuf_loss_emissions, starts_with('salvagedC_emissions'), salvage_related_emissions) %>% 
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
  rename(NEP = ...35) %>% 
  mutate(NFCB = NEP - salvage_related_emissions) %>% 
  group_by(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name) %>% 
  mutate(cumulativeNFCB = cumsum(NFCB)) %>% 
  ungroup() %>% 
  dplyr::select(scenario, Climate, WindthrowRegime, PostWindthrowManagement, dir_name, Time, 
                salvagedC, harvest_emissions, scarif_emissions, manuf_emissions_wood, manuf_emissions_paper, 
                transport_emissions, manuf_loss_emissions, starts_with('salvagedC_emissions'), salvage_related_emissions,
                NEP, NFCB, cumulativeNFCB) %>% 
  gather(key = flux_type, value = flux,
         salvagedC, harvest_emissions, scarif_emissions, manuf_emissions_wood, manuf_emissions_paper, 
         transport_emissions, manuf_loss_emissions, starts_with('salvagedC_emissions'), salvage_related_emissions,
         NEP, NFCB, cumulativeNFCB) %>% 
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
  filter(flux_type %in% c('NEP', 'NFCB', 'cumulativeNFCB'),
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

nfcb_df2 %>% 
  filter(flux_type %in% c('harvest_emissions', 'scarif_emissions', 'manuf_emissions_wood', 'manuf_emissions_paper', 
                          'transport_emissions', 'manuf_loss_emissions', 'salvagedC_emissions'),
         #Time %in% c(15, 40, 65, 90),
         PostWindthrowManagement != 'WT') %>% 
  ggplot() +
  geom_bar(aes(x = Time, y = mean, fill = flux_type), stat = 'identity', position = 'stack') +
  facet_grid(PostWindthrowManagement ~ WindthrowRegime) +
  theme_bw(base_size = 20) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = 'white'),
        panel.grid = element_line(size = 0.3)
        ) +
  ggtitle('Carbon emissions related to salvage logging') +
  labs(x = 'Time', 
       y = expression(paste("Carbon flux (g/ ", {m^2}, "/year)", sep=""))) +
  scale_colour_hc() +
  scale_fill_hc()

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
