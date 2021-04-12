############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")

# from Github
devtools::install_github("expersso/OECD")

library(OECD)
library(skimr)
library(countrycode)
library(viridis)
library(GGally)

############################################################################
# Download Data
############################################################################

dataset_list <- get_datasets()
search_dataset("science|technology|innovation", data = dataset_list) %>% kable()


dstruc <- get_data_structure("STIO_2016")
str(dstruc, max.level = 1)

browse_metadata("STIO_2016")

dstruc$VAR_DESC
dstruc$COUNTRY
dstruc$INDICATOR



vars <- c("GDP_PPPK", "GDP_HAB_PPPK", "GDP_GRO", "POP_NB", "G_XGDP", "BE_XGDP", "GV_XGDP", "RDPERS_EMP", "PTRIAD_GDP", "UNI500_HAB")



data <- get_dataset(dataset = "STIO_2016", filter = list (dstruc$COUNTRY$id, vars))
data 



ctry <- c("DNK", "DEU", "CHN", "JPN", "KOR", "GBR", "USA")

############################################################################
# Plotting static
############################################################################

data.wide.agg <- data.wide %>%
  select(-ctry.name, -year, -period) %>%
  filter( !(ctry.iso3 %in% c("OECD", "EU28"))) %>%
  group_by(ctry.iso3) %>% 
  summarize_all(mean, na.rm = TRUE) %>% 
  ungroup()  


data.wide.agg %>%
  arrange(desc(GDP_GRO)) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(ctry.iso3, GDP_GRO), y = GDP_GRO)) +
  geom_bar(stat="identity") +
  coord_flip()

data.wide.agg %>%
  arrange(desc(BE_XGDP)) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(ctry.iso3, BE_XGDP), y = BE_XGDP)) +
  geom_bar(stat="identity") +
  coord_flip()

data.wide.agg %>%
  arrange(desc(PTRIAD_GDP)) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(ctry.iso3, PTRIAD_GDP), y = PTRIAD_GDP)) +
  geom_bar(stat="identity") +
  coord_flip()

############################################################################
# Plotting Comparison
############################################################################

data.wide.agg %>%
  ggplot(aes(x =  BE_XGDP, y = PTRIAD_GDP, size = GDP_PPPK, colour = GDP_GRO)) +
  geom_point(alpha = 0.9) +
  geom_text(aes(label = ctry.iso3), hjust = 0, vjust = 0, size = 4, color = "black") + 
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", alpha = 0.75) +
  theme(legend.position = "bottom") +
  labs(x = "BERT as % of GDP", 
       y = "Triadic Patents per mil. GDP")


data.wide.agg %>%
  ggplot(aes(x =  BE_XGDP, y = GDP_GRO, size = GDP_PPPK)) +
  geom_point(alpha = 0.9) +
  geom_text(aes(label = ctry.iso3), hjust = 0, vjust = 0, size = 4, color = "black") + 
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", alpha = 0.75) +
  theme(legend.position = "bottom") +
  labs(x = "BERT as % of GDP", 
       y = "Triadic Patents per mil. GDP")


############################################################################
# Plotting dynamic
############################################################################

data.wide %>%
  select(ctry.iso3, year, BE_XGDP) %>%
  filter(year <= 2014 & ctry.iso3 %in% ctry) %>%
  ggplot(aes(x = year, y = BE_XGDP, color = ctry.iso3)) +
  geom_line(size = 1)


data %>%
  filter(year <= 2014 & ctry.iso3 %in% ctry) %>%
  ggplot(aes(x = year, y = value, color = ctry.iso3)) +
  geom_line(size = 1) + 
  facet_wrap(~ indicator, scales = "free") 



############################################################################
# Plotting development
############################################################################

data.wide.period <- data.wide %>%
  select(-ctry.name, -year) %>%
  filter( !(ctry.iso3 %in% c("OECD", "EU28"))) %>%
  group_by(ctry.iso3, period) %>% 
  summarize_all(mean, na.rm = TRUE) %>% 
  ungroup() %>%
  arrange(ctry.iso3, period)

data.wide.period %<>%
  group_by(ctry.iso3) %>%
  mutate(GDP_HAB_PPPK_delta =  (GDP_HAB_PPPK - lag(GDP_HAB_PPPK, n = 2)) / lag(GDP_HAB_PPPK, n = 2),
         BE_XGDP_delta =  (BE_XGDP - lag(BE_XGDP, n = 2)) / lag(BE_XGDP, n = 2),
         PTRIAD_GDP_delta = (PTRIAD_GDP - lag(PTRIAD_GDP, n = 2)) / lag(PTRIAD_GDP, n = 2) ) %>%
  ungroup() %>%
  filter(period == 3 & !(ctry.iso3 %in% c("PRT", "ITA", "GRC", "LUX"))) 


data.wide.period %>%
  ggplot(aes(x =  GDP_HAB_PPPK, y = GDP_HAB_PPPK_delta, size = GDP_PPPK, color = BE_XGDP_delta)) +
  geom_point(alpha = 0.9) +
  geom_text(aes(label = ctry.iso3), hjust = 0, vjust = 0, size = 4, color = "black") + 
  # Plus some nice goodies
  geom_hline(yintercept = mean(data.wide.period$GDP_HAB_PPPK_delta, na.rm = TRUE), linetype = "dashed") +
  geom_vline(xintercept = mean(data.wide.period$GDP_HAB_PPPK, na.rm = TRUE), linetype = "dashed") +
  annotate("text", x = 15000, y = 0.05, colour = "red", size = 5, label = "Falling behind") + 
  annotate("text", x = 15000, y = 1.0, colour = "red", size = 5, label = "Catching up") + 
  annotate("text", x = 45000, y = 0.05, colour = "red", size = 5, label = "Loosing momentum") + 
  annotate("text", x = 45000, y = 1.0, colour = "red", size = 5, label = "Moving Ahead") +
  scale_x_log10() + 
  scale_y_log10() + 
  theme(legend.position = "bottom") +
  labs(x = "log. Initial GDP \n av. 2000-2005 compared with 2010-2015, constant USD ppp", 
       y = "log. GDP Growth \n av. 2000-2005 compared with 2010-2015, constant USD ppp") 


############################################################################
# Plotting on Maps
############################################################################

data.map <- data.wide %>% 
  select(-ctry.iso3, -year) %>%
  group_by(ctry.name) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  ungroup() 

map.world <- map_data("world") %>% 
  filter(region != "Antarctica") %>%
  mutate(region = region %>% recode(
    USA = "United States",
    UK = "United Kingdom"))

map.world %<>% 
  left_join(data.map, by = c("region" = "ctry.name" )  )

map.world %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = UNI500_HAB)) + 
  scale_fill_viridis(option = 'plasma') + 
  theme_bw()


############################################################################
# Other approach
############################################################################

rm(list = ls())

data <- read_csv(url("https://www.dropbox.com/s/sg1j2jshzej05rd/gi_index_2018.csv?dl=1"), na = "n/a")
data

data %<>%
  gather(key = country, value = value, -index, -indicator)
data

data %<>%
  mutate(indicator = indicator %>% str_to_lower() %>%  str_remove_all(",") %>% str_replace_all("[[:space:]\\/]", "\\.") %>% str_remove_all("\\(.*"),
         index_level = index %>% str_remove_all("\\.") %>% nchar()) %>%
  select(index_level, index, country, indicator, value) %>%
  arrange(country, index)


data.wide <- data %>%
  mutate(indicator = paste("L",index_level, "_", index, "_", indicator, sep = "")) %>%
  select(-index_level, -index) %>%
  spread(key = indicator, value = value) %>%
  select(country, L1_0_global.innovation.index, everything())




### Viz

ggcorr(data.wide %>% select(starts_with("L1")), label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)


ggpairs(data.wide %>% select(starts_with("L1")), 
        aes(alpha = 0.3), 
        ggtheme = theme_gray())  



library(FactoMineR)
library(factoextra)

data.pca <- data.wide %>% select(country, starts_with("L1"), -L1_0_global.innovation.index)
data.pca<- as.data.frame(data.pca)
rownames(data.pca) <- data.pca$country
data.pca <- data.pca[,-1]


res.pca <- PCA(data.pca, scale.unit = TRUE, graph = FALSE, ncp = 5)
str(res.pca, max.level = 2)

fviz_screeplot(res.pca, 
               addlabels = TRUE, 
               ncp = 10, 
               ggtheme = theme_gray())

as_tibble(res.pca$eig)



fviz_pca_var(res.pca, 
             alpha.var = "cos2",
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE,
             ggtheme = theme_gray())



get_pca_var(res.pca)
as_tibble(res.pca$var$coord) %>%
  mutate(variable = colnames(data.pca))


as_tibble(res.pca$ind$coord) %>%
  mutate(vcountry = rownames(data.pca)) %>%
  arrange(Dim.1) %>%
  head(10)

as_tibble(res.pca$ind$coord) %>%
  mutate(vcountry = rownames(data.pca)) %>%
  arrange(desc(Dim.2)) %>%
  head(10)



fviz_pca_ind(res.pca, 
             alpha.ind = "cos2",
             col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             ggtheme = theme_gray()) 



hcpc <- HCPC(res.pca, 
             nb.clust = -1,
             graph = FALSE) 

fviz_dend(hcpc, 
          rect = TRUE, 
          cex = 0.5)

fviz_cluster(hcpc, data = data.pca,
             ggtheme = theme_gray()) 

plot(hcpc, choice = "3D.map")



data.pca.res <- data.wide %>%
  mutate(cluster = hcpc$data.clust$clust %>% as.character(),
         pca.1 = res.pca$ind$coord %>% as_tibble() %>% pull(Dim.1),
         pca.2 = res.pca$ind$coord %>% as_tibble() %>% pull(Dim.2)) %>%
  select(country, cluster, pca.1, pca.2, everything())
  
data.pca.res %>%
  select(cluster, starts_with("L1")) %>%
  group_by(cluster) %>%
  mutate(n = n()) %>%
  select(cluster, n, everything()) %>%
  summarise_all(mean)



x <- psych::principal(data.pca %>% scale(), rotate = "varimax", nfactors = 5, scores = TRUE)

res.pca.rot <- x$scores %>% as.data.frame()

hc <- hcut(res.pca.rot, hc_func = "hclust", k = 4, stand = TRUE)

fviz_cluster(hc, data = res.pca.rot,
             ggtheme = theme_gray())  


data.pca %>%
  mutate(cluster = hc$cluster) %>%
  select(cluster, starts_with("L1")) %>%
  group_by(cluster) %>%
  mutate(n = n()) %>%
  select(cluster, n, everything()) %>%
  summarise_all(mean)


