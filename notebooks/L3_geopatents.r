
############################################################################
# Preamble
############################################################################
source("C:/Users/Admin/R_functions/preamble.R")




############################################################################
# CHINA PATENT GEOCODING
############################################################################

rm(list=ls())
#devtools::install_github("PABalland/EconGeo", force = T)
library(EconGeo)

## Helper function
create_sparse_matrix <- function(i.input, j.input){
  require(Matrix)
  mat <- spMatrix(nrow = length(unique(i.input)),
                  ncol = length(unique(j.input)),
                  i = as.numeric(factor(i.input)),
                  j = as.numeric(factor(j.input)),
                  x = rep(1, length(as.numeric(i.input))) ) 
  row.names(mat) <- levels(factor(i.input))
  colnames(mat) <- levels(factor(j.input))
  return(mat)
}

### Load data
patents <- readRDS("../input/CN_patent.rds")
inventors <- readRDS("../input/CN_inventor.rds") %>% filter(str_detect(city, "china")) %>% mutate(city = city %>% str_remove("\\,.*")) 
el.pat_inv <- readRDS("../input/CN_el_inventor_patent.rds") 
el.pat_tech <- readRDS("../input/CN_el_patent_field.rds")

field_names <- read_csv("../input/ipc_technology.csv") %>%
  select(field_nr, sector, field_name) %>%
  distinct(field_nr, .keep_all = TRUE) %>%
  mutate(field_nr = field_nr %>% as.character()) %>%
  arrange(field_nr)

cities.geo <- inventors %>%
  distinct(city, .keep_all = TRUE) %>%
  select(city, lon, lat)



mat.tech <- create_sparse_matrix(i = el.pat_tech %>% pull(appln_id) %>% as.numeric(),
                                     j = el.pat_tech %>% pull(techn_field_nr) %>% as.numeric())

mat.rel.tech <- tcrossprod(t(mat.pat_tech)) %>% 
  as.matrix() %>% 
  relatedness(method = "cosine")

### Regional diversity
reg_tech <- el.pat_inv %>%
  arrange(appln_id, invt_seq_nr) %>%
  distinct(appln_id, .keep_all = TRUE) %>%
  left_join(inventors %>% select(person_id, city), by = "person_id") %>%
  select(-invt_seq_nr, person_id)

  


reg_tech %<>%
  left_join(el.pat_tech %>% select(appln_id, techn_field_nr), by = "appln_id") %>%
  group_by(appln_id) %>%
  mutate(field_weight = 1 / n()) %>%
  ungroup()

reg_tech %<>%
  group_by(city, techn_field_nr) %>%
  summarise(n_tech_reg = sum(field_weight)) %>%
  ungroup() %>%
  filter(n_tech_reg >= 2.5) %>%
  drop_na() %>%
  left_join(cities.geo, by = "city")

mat.reg_tech <- reg_tech %<>%
  arrange(techn_field_nr, city) %>%
  spread(key = techn_field_nr, value = n_tech_reg, fill = 0) 

rownames(mat.reg_tech) <- mat.reg_tech %>% pull(city)

mat.reg_tech %<>% select(-city) %>%
  as.matrix() %>%
  round()

reg.RCA <- mat.reg_tech %>% location.quotient(binary = TRUE)
RCA <- reg.RCA %>% as.data.frame() %>% rownames_to_column("city") %>% as_tibble() %>% gather(key = "tech_field", value = "RCA", -city)


mat.reg_tech %>% Herfindahl()
mat.reg_tech %>% MORt(RCA = TRUE, steps = 2)


############################################################################
# Plot on map
############################################################################


library(mapdata) # with china map
map <- borders(database = "world", regions = "China") 

ggplot() + 
  map +
  geom_point(data = reg_tech %>% group_by(city) %>% summarise(n = sum), 
             aes(x = lon, y = lat, size = n, alpha = 1)) +
  stat_density2d(data = reg, 
                 aes(x = lon, y = lat, fill = ..level.. , alpha = 0.2, col = ..level..), size = 0.5, bins = 10, geom = "polygon") +
  scale_fill_gradient(low = "skyblue", high = "red") +
  scale_color_gradient(low = "skyblue", high = "red")
                      


### create technology network
library(tidygraph)
library(ggraph)

g.tech <- mat.rel.tech %>% as_tbl_graph(directed = FALSE) %N>%
  left_join(field_names, by = c("name" = "field_nr")) %>%
  mutate(dgr = centrality_eigen(weights = weight)) %E>%
  filter(weight >= mean(weight)) %>%

g.tech %E>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = sector, size = dgr)) + 
  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  theme_graph()







g.tech.city <- g.tech %N>%
  left_join(RCA %>% filter(city == "beijing") %>% select(-city), by = c("name" = "tech_field"))

g.tech %N>%
  left_join(RCA %>% filter(city == "shenzhen") %>% select(-city), by = c("name" = "tech_field")) %E>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(width = weight), alpha = 0.2, colour = "grey") + 
  geom_node_point(aes(colour = RCA, size = dgr)) + 
  geom_node_text(aes(label = field_name), size = 5, repel = TRUE) +
  scale_color_gradient(low = "skyblue", high = "red") +
  theme_graph()


###########################################################################
# GEOCODING exercise
############################################################################

library(maps)
library(ggmap)

library(tmaptools)
places <- c("tsinghua university", "University of Chinese Academy of Sciences")
geocode <- geocode_OSM(places, details = TRUE, as.data.frame = TRUE)

library(geosphere)
distm(geocode[1,c("lon","lat")], geocode[2,c("lon","lat")], fun = distHaversine) / 1000
