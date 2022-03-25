# load packages
library(raster)
library(rgdal)
library(tidyverse)
library(ggthemes)

root_path <- 'G:/Project_Furano_Windthrow_201201'
cases <- c('111_current__historicWindthrow__WT', '112_current__historicWindthrow__SL', '113_current__historicWindthrow__SLSC'
           #'121_current__frequent__WT', '122_current__frequent__SL', '123_current__frequent__SLSC',
           # '131_current__intense__WT', '132_current__intense__SL', '133_current__intense__SLSC',
           #'141_current__frequentintense__WT', '142_current__frequentintense__SL', '143_current__frequentintense__SLSC',
           # '211_CSIRO-Mk3-6-0_RCP2.6__historicWindthrow__WT', '212_CSIRO-Mk3-6-0_RCP2.6__historicWindthrow__SL', '213_CSIRO-Mk3-6-0_RCP2.6__historicWindthrow__SLSC',
           #'221_CSIRO-Mk3-6-0_RCP2.6__frequent__WT', '222_CSIRO-Mk3-6-0_RCP2.6__frequent__SL', '223_CSIRO-Mk3-6-0_RCP2.6__frequent__SLSC',
           #'231_CSIRO-Mk3-6-0_RCP2.6__intense__WT', '232_CSIRO-Mk3-6-0_RCP2.6__intense__SL', '233_CSIRO-Mk3-6-0_RCP2.6__intense__SLSC',
           #'241_CSIRO-Mk3-6-0_RCP2.6__frequentintense__WT', '242_CSIRO-Mk3-6-0_RCP2.6__frequentintense__SL', '243_CSIRO-Mk3-6-0_RCP2.6__frequentintense__SLSC',
           #'311_GFDL-CM3_RCP8.5__historicWindthrow__WT', '312_GFDL-CM3_RCP8.5__historicWindthrow__SL','313_GFDL-CM3_RCP8.5__historicWindthrow__SLSC',
           #'321_GFDL-CM3_RCP8.5__frequent__WT', '322_GFDL-CM3_RCP8.5__frequent__SL', '323_GFDL-CM3_RCP8.5__frequent__SLSC',
           #'331_GFDL-CM3_RCP8.5__intense__WT', '332_GFDL-CM3_RCP8.5__intense__SL', '333_GFDL-CM3_RCP8.5__intense__SLSC'
           #'341_GFDL-CM3_RCP8.5__frequentintense__WT', '342_GFDL-CM3_RCP8.5__frequentintense__SL', '343_GFDL-CM3_RCP8.5__frequentintense__SLSC'
           )
total_biom_init_ras <- raster(paste0(root_path,'/111_current__historicWindthrow__WT/1/OutputMaps/biomass/biomass-TotalBiomass-0.img'))

# 15年目に風倒した場所の動態---------------------------------------------------------------------------------------
# Abiesach-----------------------------------------------------------------------------------------------
agb_Abiesach_rasters <- list()
agb_Abiesach_dfs <- list()
agb_Abiesach_df_all <- list()
agb_Abiesach_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_Abiesach_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-Abiesach-', iter, '.img'))
      agb_Abiesach_dfs[[iter]] <- as.data.frame(agb_Abiesach_rasters[[iter]])
    }
    agb_Abiesach_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_Abiesach_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "Abiesach") 
    colnames(agb_Abiesach_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                          '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                          "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                          "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                          "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                          "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                          "51", "52", "53", "54", "55", 
                                          "56", "57", "58", "59", "60",
                                          "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                          "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                          "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                          "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                          "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                          "111", "112", "113", "114", "115",
                                          "case", "dir_name", "spp_name")
    agb_Abiesach_df_summary[[dir_name]] <- agb_Abiesach_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary Abiesach_AGB in each year by case
biom_Abiesach_df <- bind_rows(agb_Abiesach_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# Picejezo-----------------------------------------------------------------------------------------------
agb_Picejezo_rasters <- list()
agb_Picejezo_dfs <- list()
agb_Picejezo_df_all <- list()
agb_Picejezo_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_Picejezo_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-Picejezo-', iter, '.img'))
      agb_Picejezo_dfs[[iter]] <- as.data.frame(agb_Picejezo_rasters[[iter]])
    }
    agb_Picejezo_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_Picejezo_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "Picejezo") 
    colnames(agb_Picejezo_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                                   '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                                   "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                                   "51", "52", "53", "54", "55", 
                                                   "56", "57", "58", "59", "60",
                                                   "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                                   "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                                   "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                                   "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                                   "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                                   "111", "112", "113", "114", "115",
                                                   "case", "dir_name", "spp_name")
    agb_Picejezo_df_summary[[dir_name]] <- agb_Picejezo_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary Picejezo_AGB in each year by case
biom_Picejezo_df <- bind_rows(agb_Picejezo_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# Picegleh-----------------------------------------------------------------------------------------------
agb_Picegleh_rasters <- list()
agb_Picegleh_dfs <- list()
agb_Picegleh_df_all <- list()
agb_Picegleh_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_Picegleh_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-Picegleh-', iter, '.img'))
      agb_Picegleh_dfs[[iter]] <- as.data.frame(agb_Picegleh_rasters[[iter]])
    }
    agb_Picegleh_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_Picegleh_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "Picegleh") 
    colnames(agb_Picegleh_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                                   '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                                   "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                                   "51", "52", "53", "54", "55", 
                                                   "56", "57", "58", "59", "60",
                                                   "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                                   "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                                   "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                                   "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                                   "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                                   "111", "112", "113", "114", "115",
                                                   "case", "dir_name", "spp_name")
    agb_Picegleh_df_summary[[dir_name]] <- agb_Picegleh_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary Picegleh_AGB in each year by case
biom_Picegleh_df <- bind_rows(agb_Picegleh_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# Tilijapo-----------------------------------------------------------------------------------------------
agb_Tilijapo_rasters <- list()
agb_Tilijapo_dfs <- list()
agb_Tilijapo_df_all <- list()
agb_Tilijapo_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_Tilijapo_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-Tilijapo-', iter, '.img'))
      agb_Tilijapo_dfs[[iter]] <- as.data.frame(agb_Tilijapo_rasters[[iter]])
    }
    agb_Tilijapo_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_Tilijapo_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "Tilijapo") 
    colnames(agb_Tilijapo_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                                   '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                                   "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                                   "51", "52", "53", "54", "55", 
                                                   "56", "57", "58", "59", "60",
                                                   "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                                   "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                                   "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                                   "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                                   "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                                   "111", "112", "113", "114", "115",
                                                   "case", "dir_name", "spp_name")
    agb_Tilijapo_df_summary[[dir_name]] <- agb_Tilijapo_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary Tilijapo_AGB in each year by case
biom_Tilijapo_df <- bind_rows(agb_Tilijapo_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# Aceritaya-----------------------------------------------------------------------------------------------
agb_Aceritaya_rasters <- list()
agb_Aceritaya_dfs <- list()
agb_Aceritaya_df_all <- list()
agb_Aceritaya_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_Aceritaya_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-Aceritaya-', iter, '.img'))
      agb_Aceritaya_dfs[[iter]] <- as.data.frame(agb_Aceritaya_rasters[[iter]])
    }
    agb_Aceritaya_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_Aceritaya_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "Aceritaya") 
    colnames(agb_Aceritaya_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                                   '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                                   "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                                   "51", "52", "53", "54", "55", 
                                                   "56", "57", "58", "59", "60",
                                                   "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                                   "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                                   "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                                   "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                                   "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                                   "111", "112", "113", "114", "115",
                                                   "case", "dir_name", "spp_name")
    agb_Aceritaya_df_summary[[dir_name]] <- agb_Aceritaya_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary Aceritaya_AGB in each year by case
biom_Aceritaya_df <- bind_rows(agb_Aceritaya_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# Ulmulaci-----------------------------------------------------------------------------------------------
agb_Ulmulaci_rasters <- list()
agb_Ulmulaci_dfs <- list()
agb_Ulmulaci_df_all <- list()
agb_Ulmulaci_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_Ulmulaci_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-Ulmulaci-', iter, '.img'))
      agb_Ulmulaci_dfs[[iter]] <- as.data.frame(agb_Ulmulaci_rasters[[iter]])
    }
    agb_Ulmulaci_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_Ulmulaci_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "Ulmulaci") 
    colnames(agb_Ulmulaci_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                                   '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                                   "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                                   "51", "52", "53", "54", "55", 
                                                   "56", "57", "58", "59", "60",
                                                   "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                                   "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                                   "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                                   "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                                   "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                                   "111", "112", "113", "114", "115",
                                                   "case", "dir_name", "spp_name")
    agb_Ulmulaci_df_summary[[dir_name]] <- agb_Ulmulaci_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary Ulmulaci_AGB in each year by case
biom_Ulmulaci_df <- bind_rows(agb_Ulmulaci_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# Kalopict-----------------------------------------------------------------------------------------------
agb_Kalopict_rasters <- list()
agb_Kalopict_dfs <- list()
agb_Kalopict_df_all <- list()
agb_Kalopict_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_Kalopict_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-Kalopict-', iter, '.img'))
      agb_Kalopict_dfs[[iter]] <- as.data.frame(agb_Kalopict_rasters[[iter]])
    }
    agb_Kalopict_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_Kalopict_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "Kalopict") 
    colnames(agb_Kalopict_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                                   '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                                   "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                                   "51", "52", "53", "54", "55", 
                                                   "56", "57", "58", "59", "60",
                                                   "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                                   "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                                   "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                                   "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                                   "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                                   "111", "112", "113", "114", "115",
                                                   "case", "dir_name", "spp_name")
    agb_Kalopict_df_summary[[dir_name]] <- agb_Kalopict_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary Kalopict_AGB in each year by case
biom_Kalopict_df <- bind_rows(agb_Kalopict_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# Quermong-----------------------------------------------------------------------------------------------
agb_Quermong_rasters <- list()
agb_Quermong_dfs <- list()
agb_Quermong_df_all <- list()
agb_Quermong_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_Quermong_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-Quermong-', iter, '.img'))
      agb_Quermong_dfs[[iter]] <- as.data.frame(agb_Quermong_rasters[[iter]])
    }
    agb_Quermong_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_Quermong_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "Quermong") 
    colnames(agb_Quermong_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                                   '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                                   "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                                   "51", "52", "53", "54", "55", 
                                                   "56", "57", "58", "59", "60",
                                                   "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                                   "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                                   "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                                   "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                                   "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                                   "111", "112", "113", "114", "115",
                                                   "case", "dir_name", "spp_name")
    agb_Quermong_df_summary[[dir_name]] <- agb_Quermong_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary Quermong_AGB in each year by case
biom_Quermong_df <- bind_rows(agb_Quermong_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# Betuerma-----------------------------------------------------------------------------------------------
agb_Betuerma_rasters <- list()
agb_Betuerma_dfs <- list()
agb_Betuerma_df_all <- list()
agb_Betuerma_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_Betuerma_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-Betuerma-', iter, '.img'))
      agb_Betuerma_dfs[[iter]] <- as.data.frame(agb_Betuerma_rasters[[iter]])
    }
    agb_Betuerma_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_Betuerma_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "Betuerma") 
    colnames(agb_Betuerma_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                                   '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                                   "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                                   "51", "52", "53", "54", "55", 
                                                   "56", "57", "58", "59", "60",
                                                   "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                                   "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                                   "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                                   "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                                   "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                                   "111", "112", "113", "114", "115",
                                                   "case", "dir_name", "spp_name")
    agb_Betuerma_df_summary[[dir_name]] <- agb_Betuerma_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary Betuerma_AGB in each year by case
biom_Betuerma_df <- bind_rows(agb_Betuerma_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# Betumaxi-----------------------------------------------------------------------------------------------
agb_Betumaxi_rasters <- list()
agb_Betumaxi_dfs <- list()
agb_Betumaxi_df_all <- list()
agb_Betumaxi_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_Betumaxi_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-Betumaxi-', iter, '.img'))
      agb_Betumaxi_dfs[[iter]] <- as.data.frame(agb_Betumaxi_rasters[[iter]])
    }
    agb_Betumaxi_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_Betumaxi_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "Betumaxi") 
    colnames(agb_Betumaxi_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                                   '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                                   "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                                   "51", "52", "53", "54", "55", 
                                                   "56", "57", "58", "59", "60",
                                                   "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                                   "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                                   "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                                   "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                                   "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                                   "111", "112", "113", "114", "115",
                                                   "case", "dir_name", "spp_name")
    agb_Betumaxi_df_summary[[dir_name]] <- agb_Betumaxi_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary Betumaxi_AGB in each year by case
biom_Betumaxi_df <- bind_rows(agb_Betumaxi_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# Fraxmand-----------------------------------------------------------------------------------------------
agb_Fraxmand_rasters <- list()
agb_Fraxmand_dfs <- list()
agb_Fraxmand_df_all <- list()
agb_Fraxmand_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_Fraxmand_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-Fraxmand-', iter, '.img'))
      agb_Fraxmand_dfs[[iter]] <- as.data.frame(agb_Fraxmand_rasters[[iter]])
    }
    agb_Fraxmand_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_Fraxmand_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "Fraxmand") 
    colnames(agb_Fraxmand_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                                   '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                                   "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                                   "51", "52", "53", "54", "55", 
                                                   "56", "57", "58", "59", "60",
                                                   "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                                   "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                                   "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                                   "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                                   "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                                   "111", "112", "113", "114", "115",
                                                   "case", "dir_name", "spp_name")
    agb_Fraxmand_df_summary[[dir_name]] <- agb_Fraxmand_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary Fraxmand_AGB in each year by case
biom_Fraxmand_df <- bind_rows(agb_Fraxmand_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# sasa_spp-----------------------------------------------------------------------------------------------
agb_sasa_spp_rasters <- list()
agb_sasa_spp_dfs <- list()
agb_sasa_spp_df_all <- list()
agb_sasa_spp_df_summary <- list()
for (case in cases) {
  dir_names <- list.dirs(path = file.path(root_path, case), recursive = F)
  for (dir_name in dir_names) {
    id <- data.frame(id = 1:ncell(total_biom_init_ras))
    wt_ras <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-15.img'))
    wt_df <- as.data.frame(wt_ras)
    wt_ras2 <- raster(paste0(dir_name,'/OutputMaps/harvest/prescripts-65.img'))
    wt_df2 <- as.data.frame(wt_ras2)
    for (iter in 1:115) {
      agb_sasa_spp_rasters[[iter]] <- raster(paste0(dir_name, '/OutputMaps/biomass/biomass-sasa_spp-', iter, '.img'))
      agb_sasa_spp_dfs[[iter]] <- as.data.frame(agb_sasa_spp_rasters[[iter]])
    }
    agb_sasa_spp_df_all[[dir_name]] <- bind_cols(id, wt_df, wt_df2, agb_sasa_spp_dfs) %>%  
      mutate(case = case, 
             dir_name = dir_name,
             spp_name = "sasa_spp") 
    colnames(agb_sasa_spp_df_all[[dir_name]]) <- c("gridID", 'WindEvent1', 'WindEvent2', 
                                                   '1', "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                   "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                   "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
                                                   "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
                                                   "51", "52", "53", "54", "55", 
                                                   "56", "57", "58", "59", "60",
                                                   "61", "62", "63", "64", "65", "66", "67", "68", "69", "70",
                                                   "71", "72", "73", "74", "75", "76", "77", "78", "79", "80",
                                                   "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                                   "91", "92", "93", "94", "95", "96", "97", "98", "99", "100",
                                                   "101", "102", "103", "104", "105", "106", "107", "108", "109", "110",
                                                   "111", "112", "113", "114", "115",
                                                   "case", "dir_name", "spp_name")
    agb_sasa_spp_df_summary[[dir_name]] <- agb_sasa_spp_df_all[[dir_name]] %>% 
      filter(WindEvent1 == 2, WindEvent2 == 1, 1 > 0) %>% 
      gather(key = year, value = TotalBiomass, "1":"115") %>% 
      mutate(year = as.integer(year), TotalCarbon = TotalBiomass / 2 / 100) %>% 
      group_by(case, dir_name, spp_name, year) %>%
      summarise(meanTotalAGB = mean(TotalBiomass),
                minTotalAGB = min(TotalBiomass),
                maxTotalAGB = max(TotalBiomass),
                meanTotalC = mean(TotalCarbon),
                minTotalC = min(TotalCarbon),
                maxTotalC = max(TotalCarbon)
      ) %>% 
      arrange(year) %>% 
      mutate(C_Increment = meanTotalC - lag(meanTotalC))
  }
}

## Summary sasa_spp_AGB in each year by case
biom_sasa_spp_df <- bind_rows(agb_sasa_spp_df_summary) %>%
  group_by(case, spp_name, year) %>%
  summarise(MEANTotalAGB = mean(meanTotalAGB),
            SDTotalAGB = sd(meanTotalAGB),
            MINTotalAGB = min(minTotalAGB),
            MAXTotalAGB = max(maxTotalAGB),
            MEANTotalC = mean(meanTotalC),
            SDTotalC = sd(meanTotalC),
            MINTotalC = sd(minTotalC),
            MAXTotalC = max(maxTotalC),
            MEANCinc = mean(C_Increment),
            SDCinc = sd(C_Increment),
  )

# bind all species in one dataframe
biom_df <- bind_rows(biom_Abiesach_df,
                     biom_Picejezo_df,
                     biom_Picegleh_df,
                     biom_Tilijapo_df,
                     biom_Aceritaya_df,
                     biom_Ulmulaci_df,
                     biom_Kalopict_df,
                     biom_Quermong_df,
                     biom_Betuerma_df,
                     biom_Betumaxi_df,
                     biom_Fraxmand_df,
                     biom_sasa_spp_df)
# Make scenario index
case_id <- str_split(biom_df$case, pattern = "__", simplify = TRUE)
case_id_df <- as.data.frame(case_id)
case_id2 <- str_split(case_id_df$V1, pattern = "_", simplify = TRUE, n = 2)
case_id2_df <- as.data.frame(case_id2)
id_df <- bind_cols(case_id_df, case_id2_df) %>% 
  dplyr::select(V1...4, V2...5, V2...2, V3) %>% 
  rename(scenario_no = V1...4,
         Climate = V2...5,
         WindthrowRegime = V2...2,
         PostWindthrowManagement = V3)

biom_df2 <- bind_cols(biom_df, id_df)

# For ESA - Species composition ====================================
# Cummulative line of total AGB by species
biom_df2$Species <- factor(biom_df2$spp_name, 
                            levels = c("Abiesach", "Picejezo", "Picegleh", "Betuerma", "Betumaxi",
                                       "Fraxmand", "Kalopict", "Quermong", "Aceritaya", "Tilijapo", 
                                       "Ulmulaci", "sasa_spp"))
biom_df2$case <- factor(biom_df2$case,
                       levels = c('111_current__historicWindthrow__WT', '112_current__historicWindthrow__SL', '113_current__historicWindthrow__SLSC'
                                  #'121_current__frequent__WT', '122_current__frequent__SL', '123_current__frequent__SLSC',
                                  #'131_current__intense__WT', '132_current__intense__SL', '133_current__intense__SLSC',
                                  #'141_current__frequentintense__WT', '142_current__frequentintense__SL', '143_current__frequentintense__SLSC',
                                  #'211_CSIRO-Mk3-6-0_RCP2.6__historicWindthrow__WT', '212_CSIRO-Mk3-6-0_RCP2.6__historicWindthrow__SL', '213_CSIRO-Mk3-6-0_RCP2.6__historicWindthrow__SLSC',
                                  #'221_CSIRO-Mk3-6-0_RCP2.6__frequent__WT', '222_CSIRO-Mk3-6-0_RCP2.6__frequent__SL', '223_CSIRO-Mk3-6-0_RCP2.6__frequent__SLSC',
                                  #'231_CSIRO-Mk3-6-0_RCP2.6__intense__WT', '232_CSIRO-Mk3-6-0_RCP2.6__intense__SL', '233_CSIRO-Mk3-6-0_RCP2.6__intense__SLSC',
                                  #'241_CSIRO-Mk3-6-0_RCP2.6__frequentintense__WT', '242_CSIRO-Mk3-6-0_RCP2.6__frequentintense__SL', '243_CSIRO-Mk3-6-0_RCP2.6__frequentintense__SLSC',
                                  #'311_GFDL-CM3_RCP8.5__historicWindthrow__WT', '312_GFDL-CM3_RCP8.5__historicWindthrow__SL','313_GFDL-CM3_RCP8.5__historicWindthrow__SLSC',
                                  #'321_GFDL-CM3_RCP8.5__frequent__WT', '322_GFDL-CM3_RCP8.5__frequent__SL', '323_GFDL-CM3_RCP8.5__frequent__SLSC',
                                  #'331_GFDL-CM3_RCP8.5__intense__WT', '332_GFDL-CM3_RCP8.5__intense__SL', '333_GFDL-CM3_RCP8.5__intense__SLSC'
                                  #'341_GFDL-CM3_RCP8.5__frequentintense__WT', '342_GFDL-CM3_RCP8.5__frequentintense__SL', '343_GFDL-CM3_RCP8.5__frequentintense__SLSC'
                       ))
biom_df2$WindthrowRegime <- factor(biom_df2$WindthrowRegime,
                                  levels = c('historicWindthrow'),
                                  labels = c('Historical'))
biom_df2$PostWindthrowManagement <- factor(biom_df2$PostWindthrowManagement,
                                          levels = c('WT', 'SL', 'SLSC'))
biom_df2$Climate <- factor(biom_df2$Climate, 
                          levels = c("current"),
                          labels = c("Current"))
biom_df2$MEANTotalAGB <- biom_df2$MEANTotalAGB/ 100
# Trees ----------------------------------------------------------
## 15年目のみ風倒した箇所
recov_yr15_df <- biom_df2
#recov_yr15_65_df <- biom_df2
#recov_yr15_65_df$Species <- as.character(recov_yr15_65_df$Species)
# recov_yr15_65_df <- recov_yr15_65_df %>% 
#   mutate(Species2 = case_when(Species %in% c('Picejezo', 'Picegleh') ~ 'Picea spp.',
#                               Species %in% c('Betuerma', 'Betumaxi') ~ 'Betula spp.',
#                               TRUE ~ Species))
p <- recov_yr15_df %>% 
  filter(WindthrowRegime == 'Historical') %>% 
  ggplot() +
  #geom_line(aes(x = year, y = MEANTotalAGB, color = Species2), size = 1.3, position = "stack") +
  # geom_vline(xintercept = 64,size=1,linetype = 2,colour="red") +
  # geom_vline(xintercept = 32,size=1,linetype = 2,colour="blue") +
  geom_area(aes(x = year, y = MEANTotalAGB, group = Species, fill = Species), color = "black", alpha = 0.7) +
  #geom_hline(yintercept = 26) +
  #geom_hline(yintercept = 50) +           
  #ylim(0, 350) +
  facet_grid(. ~ PostWindthrowManagement) +
  theme_bw(base_size = 20) +
  theme(#plot.title = element_text(size = 30),
        legend.title = element_blank(),
        legend.text = element_text(size = 20, face = "italic"),
        legend.background = element_blank(),
        legend.position = "bottom",
        #axis.title.x = element_text(size = 30),
        #axis.title.y = element_text(size = 30),
        #axis.text = element_text(size = 25),
        #strip.text.x = element_text(size = 15),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "transparent", colour = "black", size = 0.7),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = "black", size = 0.7),
        #panel.grid = element_blank(),
        #panel.grid.major.y = element_line(colour = "grey50")
        ) +
  labs(title = 'Changes in species composition at windthrow area',
       subtitle = 'Windthrow occured in year 15.',
       x = "Year") +
  ylab(expression(paste('Biomass (Mg' ~ ha^-1 *')'))) +
  scale_color_brewer(palette = "Set3",
                     labels = c('Abies sachalinensis', 'Picea jezoensis', 'Picea glehnii',
                                'Betula ermanii', 'Betula maximowicziana', 'Fraxinus mandshurica',
                                'Kalopanax septemlobus', 'Quercus crispula', 'Acer pictum',
                                'Tilia japonica', 'Ulmus laciniata', 'Sasa senanensis')
                     ) +
  scale_fill_brewer(palette = "Set3",
                    labels = c('Abies sachalinensis', 'Picea jezoensis', 'Picea glehnii',
                               'Betula ermanii', 'Betula maximowicziana', 'Fraxinus mandshurica',
                               'Kalopanax septemlobus', 'Quercus crispula', 'Acer pictum',
                               'Tilia japonica', 'Ulmus laciniata', 'Sasa senanensis')
                    )
ggsave(p, filename = "SpeciesComposition15.tiff",  dpi = 1000, width = 14.09, height = 7.96)

