# aplly SCAN network to Amazonian Birds #

library(ggplot2)
library(dplyr)
library(sf)
library(s2)


#### PRODUCTS OF THIS SCRIPT ####

# general map
'dsn ="D:/SIG2018/", layer ="spp50_AEs_ecoreg"'
# general Cs table
"Cs_spp50_AEs_ecor.csv"

overlap_df




# read maps #
# SA = st_read("C:/SIG2018/CO_OCCUR_PATT/south_america_clean_shape/south_america_clean_shape.shp", crs = st_crs(map_SA))
# the SA map is out of scale :-(
# SA = st_read( "D:/SIG2018/South_America/South_America.shp")
# SA %>% st_crs
# merge countries and simplify geometry
# sa = SA %>% st_union %>% st_simplify %>% st_as_sf
# st_write(sa, "South_America_contour.shp", driver = "ESRI shapefile")

# South America contour and ecoregions
eco = st_read(dsn = "D:/SIG2018/ECOREGIOES", layer = "ecoregions_amazon_1")
eco = eco %>% transmute(SCINAME = eco_code)
sa = st_read("D:/Users/arila/hubiC/DOC-MAIN/SCAN_to_GRAPH/South_America_contour.shp")
riv = st_read("D:/SIG2018/South_America/South_America_Hydrography.shp")
# map all 1095 species with >50% distribution in Amazon
map_SA = st_read(dsn = "D:/SIG2018/test/map_species_lowland_50", layer = "map_species_lowland_50")
# Endemism map 1 AE
AE = st_read(dsn = "D:/SIG2018/Shapes Amazon/AED", layer = "AE_endemic_areas")
# remove duplicated rows
AE = AE %>% group_by(ENDEMISMO, SQ_KM) %>% summarize(geometry = st_union(geometry)) #km2 =sum(SQ_KM)
AE = AE %>% mutate(SCINAME = ENDEMISMO)
AE = AE %>% select(SCINAME)
AE$SCINAME = c("Belem", "Guiana", "Imeri","Inambari","Marajo","Napo","Rondonia","Tapajos","Xingu")
# ALTERNATIVE AE shapes
# combine multiple maps into a unique sf object 
# baseDir <- 'D:/SIG2018/AEs/'
# filenames <- c("Cracraft1985_Fig05_Area23_Belem.shp", "AE_Xingu.shp", "Cracraft1985_Fig05_Area22_Tapajos.shp",
#                "Cracraft1985_Fig05_Area21_Rondonia.shp", "Cracraft1985_Fig05_Area20_Inambari.shp",
#                "Cracraft1985_Fig05_Area19_Napo.shp", "Cracraft1985_Fig03_Area17_Guyanan.shp",
#                "Borges_and_Silva_2012_Fig02_Jau_cortada.shp")
# filepaths <- paste(baseDir, filenames, sep='')
# Areas of Endemism 2 AEs
# xingu = st_read('D:/SIG2018/AEs/AE_Xingu.shp')
# belem = st_read(filepaths[1])
# tapajos = st_read(filepaths[3])
# rondonia = st_read(filepaths[4])
# inambari  =st_read(filepaths[5])
# napo = st_read(filepaths[6])
# guiana = st_read(filepaths[7])
# jau = st_read(file.choose())
# jau$Nome = c("Jau")
# xingu$Nome = "Xingu"
# guiana$Nome = "Guiana"
# 
# 
# AEs = rbind(belem,xingu,tapajos,rondonia,inambari,napo,jau,guiana)
# AEs = AEs %>% mutate(SCINAME = Nome)
# AEs = AEs %>% select(SCINAME) %>% st_as_sf()
# old_wd = getwd()
# setwd("C:/SIG2018/Shapes Amazon/")
# st_write(AEs, "AEs.shp", driver = "ESRI shapefile")
# setwd(old_wd)

# read AEs alternative
AEs = st_read("D:/SIG2018/Shapes Amazon/AEs.shp")
# bbox to polygon
boxAE = st_bbox(AE) %>% st_as_sfc %>% st_as_sf() %>% st_buffer(2)
# recover Imeri without Jau
# Imeri_small  = AE %>% filter(SCINAME == "Imeri") %>% 
#         st_difference(AEs %>% filter(SCINAME == "Jau") %>% 
#                                st_buffer(5000))
# setwd("C:/SIG2018/Shapes Amazon/")
# st_write(Imeri_small,"Imeri2.shp", driver = "ESRI shapefile")
# setwd(old_wd)

# EDited in QGis
Imeri2 = st_read("D:/SIG2018/Shapes Amazon/Imeri2.shp") %>% select(SCINAME)
# rm(Imeri_small)
# Make a composite AE shapefile
selected = AEs %>% filter(SCINAME %in% c("Guiana", "Jau", "Napo"))
endem = rbind(AE,selected, Imeri2)
endem$SCINAME[10] = "Napo2";endem$SCINAME[11] = "Jau";endem$SCINAME[12] = "Guiana2";endem$SCINAME[13] = "Imeri2"
endem %>% st_drop_geometry() %>% select(SCINAME)
# join bird species with endemic areas

# map  = rbind(endem,eco1,map_SA)  # with ecoregions
map = rbind(endem, map_SA)
# map %>% st_drop_geometry() %>% head
# map = map %>% st_buffer(0.05) %>% st_buffer(-0.05) 

# simplify geometries
map = map %>% group_by(SCINAME) %>% summarise(geometry = st_union(geometry))

# https://github.com/r-spatial/sf/issues/1649  # did not work
# st_geometry(map[817,]) = s2_geog_from_wkb(st_as_binary(st_geometry(map[817,])), check = FALSE) %>% s2_rebuild() %>% st_geometry()


# transform to another projection to validate some shapes which are invalid at WGS84 epsg 4326
map = map %>% st_transform(crs = 5875)

eco = eco %>% st_transform(crs = 5875)

eco = eco %>% group_by(SCINAME) %>% summarise(geometry = st_union(geometry))

ggplot(data = eco) + geom_sf(aes(fill = SCINAME), show.legend = F) + them_bw()

#### sAVE GENERAL MAP ####
rbind(eco,map) %>% st_transform(crs = 4326) %>% st_write(., dsn ="D:/SIG2018", layer ="spp50_AEs_ecoreg", driver = "ESRI shapefile")



invalid = !(st_is_valid(map))
#NT0125,Chaetura meridionalis,Herpsilochmus stotzi,Ictinia plumbea,Phalacrocorax brasilianus,Sporophila caerulescens,Sporophila nigrorufa,Tyrannus albogularis

map[invalid,] = map[invalid,] %>% st_make_valid
map[which(!st_is_valid(map)),] %>% st_drop_geometry()
eco[which(!st_is_valid(eco)),] = eco[which(!st_is_valid(eco)),] %>% st_make_valid

dev.new();ggplot() + geom_sf(data = map[1:10,]) + theme_bw()
ggplot() + geom_sf(data = map[1:10,]) + geom_sf(data = eco, alpha = 0.3)+ theme_bw()
sa = sa %>% st_transform(crs = 5875)
riv = riv %>% st_transform(crs = 5875)
#

##### Cs dist ######
### Measure overlap between areas###

overlap_row1 = st_intersection(map[1:50,],map)
overlap_row2 = st_intersection(map[51:100,],map)
overlap_row3 = st_intersection(map[101:150,],map)
area1 = overlap_row1 %>% st_area  # 19:04 - 19:09 - OK
area2 = overlap_row2 %>% st_area 
area3 = overlap_row3 %>% st_area 
overlap_row4 = st_intersection(map[151:200,],map)
area4= st_area(overlap_row4)
overlap_row4 = overlap_row4 %>% st_drop_geometry() %>% mutate(overlap = area4)        
overlap_row5 = st_intersection(map[201:250,],map)
area5 = overlap_row5 %>% st_area()
overlap_row6 = st_intersection(map[251:300,],map)
area6 = st_area(overlap_row6)
overlap_row6 = overlap_row6 %>% st_drop_geometry() %>% mutate(overlap = area6)  
overlap_row7 = st_intersection(map[301:350,],map)
area7 = st_area(overlap_row7)
overlap_row7 = overlap_row7 %>% st_drop_geometry() %>% mutate(overlap = area7)
overlap_row8 = st_intersection(map[351:400,],map)
area8 = st_area(overlap_row8)
overlap_row8 = overlap_row8 %>% st_drop_geometry() %>% mutate(overlap = area8)
overlap_row9 = st_intersection(map[401:450,],map)
area9 = st_area(overlap_row9)
overlap_row9 = overlap_row9 %>% st_drop_geometry() %>% mutate(overlap = area9)
overlap_row10 = st_intersection(map[451:500,],map)
area10 = st_area(overlap_row10)
overlap_row10 = overlap_row10 %>% st_drop_geometry() %>% mutate(overlap = area10)
overlap_row11 = st_intersection(map[501:550,],map)
area11 = st_area(overlap_row11)
overlap_row11 = overlap_row11 %>% st_drop_geometry() %>% mutate(overlap = area11)
overlap_row12 = st_intersection(map[551:600,],map)
area12 = st_area(overlap_row12)
overlap_row12 = overlap_row12 %>% st_drop_geometry() %>% mutate(overlap = area12)
overlap_row13 = st_intersection(map[601:750,],map)
area13 = st_area(overlap_row13)
overlap_row13 = overlap_row13 %>% st_drop_geometry() %>% mutate(overlap = area13)

# I have data saved only to here (bkup.RData)  to get area intersections a re-run is needed
overlap_row14 = st_intersection(map[751:800,],map)
area14 = st_area(overlap_row14)
overlap_row14 = overlap_row14 %>% st_drop_geometry() %>% mutate(overlap = area14)

y = Sys.time()
overlap_row15 = st_intersection(map[801:850,],map)
area15 = st_area(overlap_row15)
overlap_row15 = overlap_row15 %>% st_drop_geometry() %>% mutate(overlap = area15)
overlap_row16 = st_intersection(map[851:900,],map)
area16 = st_area(overlap_row16)
overlap_row16 = overlap_row16 %>% st_drop_geometry() %>% mutate(overlap = area16)
overlap_row17 = st_intersection(map[901:950,],map)
area17 = st_area(overlap_row17)
overlap_row17 = overlap_row17 %>% st_drop_geometry() %>% mutate(overlap = area17)
overlap_row18 = st_intersection(map[951:1000,],map)
area18 = st_area(overlap_row18)
overlap_row18 = overlap_row18 %>% st_drop_geometry() %>% mutate(overlap = area18)

overlap_row19 = st_intersection(map[1001:1050,],map)
area19 = st_area(overlap_row19)
overlap_row19 = overlap_row19 %>% st_drop_geometry() %>% mutate(overlap = area19)

overlap_row20 = st_intersection(map[1051:1100,],map)
area20 = st_area(overlap_row20)
overlap_row20 = overlap_row20 %>% st_drop_geometry() %>% mutate(overlap = area20)

overlap_row21 = st_intersection(map[1101:1150,],map)
area21 = st_area(overlap_row21)
overlap_row21 = overlap_row21 %>% st_drop_geometry() %>% mutate(overlap = area21)

overlap_row22 = st_intersection(map[1151:nrow(map),],map)
area22 = st_area(overlap_row22)
overlap_row22 = overlap_row22 %>% st_drop_geometry() %>% mutate(overlap = area22)

# overlap with ECOREGIONS
overlap_eco = st_intersection (eco, map)
area_eco = st_area(overlap_eco)
overlap_eco = overlap_eco %>% st_drop_geometry() %>% mutate(overlap = area_eco)

# combine areas of overlaps for all species
overlap_area = tibble()
for(x in 1:22){ vec = c(paste0("overlap_row",x))
        overlap_area = rbind(overlap_area, get(vec)) }
# exclude self-overlaps
same = which(overlap_area$SCINAME == overlap_area$SCINAME.1)
overlap_area = overlap_area[-same,]

# species areas        
area_map = map %>% st_area
area_map = map %>% st_drop_geometry() %>% mutate(area_sp = area_map)

area_eco = eco %>% st_area
area_eco = eco %>% st_drop_geometry() %>% mutate(area_sp = area_eco)

        # 
        overlap = overlap_area %>% left_join(area_map) 
        overlap = overlap %>% select(SCINAME, SCINAME.1, overlap, area_sp1 = area_sp)
        overlap = overlap %>% left_join(area_map, by = c("SCINAME.1" = "SCINAME")) %>% 
                select(1,2,3,4, area_sp2 = area_sp)

overlap_eco = overlap_eco %>% left_join(area_eco) %>% mutate(area_sp1 = area_sp)
overlap_eco = overlap_eco %>% select(-area_sp)
overlap_eco = overlap_eco %>% left_join(area_map, by = c("SCINAME.1" = "SCINAME")) %>% mutate(area_sp2 = area_sp)
overlap_eco = overlap_eco %>% select(-area_sp)

write.csv(overlap_eco,"overlap_ecoregions_species_AE.csv")
# if using bird maps and eco together rbind both overlap datasets
# overlap = rbind(overlap, overlap_eco)

# Cs index
Cs = overlap %>% mutate(Cs = as.numeric((overlap/area_sp1)*(overlap/area_sp2)))
Cs = Cs %>% select(sp1 = SCINAME, sp2 = SCINAME.1, Cs)
#last filter
Cs = Cs %>% filter(sp1 != sp2) %>% filter(Cs >= 0.01)       


Cs_eco = overlap_eco %>% mutate(Cs = as.numeric((overlap/area_sp1)*(overlap/area_sp2))) %>% 
        select(sp1 = SCINAME, sp2 = SCINAME.1, Cs)

# write.csv(Cs,"Cs_birdsAE.csv")
# Cs = read.csv("Cs_birdsAE.csv")
# with ecoregions
Cs_old = Cs  # this object has only bird species and areas of endemism

# this new Cs has species, AE, and  ecoregions
Cs = rbind(Cs_eco,Cs_old[,2:4])
Cs = Cs %>% mutate(Cs = round(Cs,3))
Cs = Cs %>% filter(Cs > 0)


write.csv(Cs, "Cs_spp50_AEs_ecor.csv")


#### SCAN GRAPH ####
source("D:/Users/arila/hubiC/DOC-MAIN/SCAN_to_GRAPH/SOURCE_SCAN_network_8.R")

C_old = C

Cs = read.csv( "Cs_spp50_AEs_ecor.csv")
Cs = Cs %>% select(sp1,sp2,Cs)

#### CREATE GRAPH OBJECT
C  = Cs %>% mutate(Cs = round(Cs,2)) %>%
        select(from = sp1, to = sp2, Cs) %>%
        as_tbl_graph(directed=FALSE)
        
        ### Simplify in IGRAPH and convert back to tbl_graph
        # avoid double edges connections but see the attributes functions at https://igraph.org/r/doc/igraph-attribute-combination.html
        C = C %>% igraph::simplify(remove.multiple = TRUE, remove.loops = FALSE, edge.attr.comb="first")
        C = C %>% as_tbl_graph(directed = FALSE)
        C = C %>% activate(edges) %>% select(from,to, Cs)
        C = C %>% activate(nodes) %>% mutate(.tidygraph_index = seq_len(n()))

dev.new();plot(C)

#### SCAN Network function ####
"see SOURCE_SCAN_network.R"

#### Exectue and Summarize results ####

# testing SCAN

bird_scan = SCAN (graph = C,
                max_depth = 7, 
                max_diameter = 10, 
                # max_Ct = 0.9,
                max_Ct = max(C %>% activate(edges) %>% as_tibble %>% .$Cs),
                min_Ct = 0.88,
                Ct_resolution = -0.02,
                overlap = TRUE,
                filter_depth = TRUE)




chorotypes$graph_chorotypes %>% group_by(component_spp) %>% summarize(max(Ct), max_depth = max(maximum_depth),max_diam = max(diameter), max_order = max(order), max_centr = max(centrality))
# at 0.5 there are wrong estimates of depth and diameter, possibly caused by the filtering at nodes


# bird_scan exploration

bs = bird_scan

bs %>% names()
bs$parameters
bs[["max_depth_species"]]

bs1 = SCAN (graph = C,
                max_depth = 7, 
                max_diameter = 10, 
                # max_Ct = 0.88,
                max_Ct = max(C %>% activate(edges) %>% as_tibble %>% .$Cs),
                min_Ct = 0.94,
                Ct_resolution = -0.02,
                overlap = TRUE,
                filter_depth = TRUE)

#  bs %>% names()
' "chorotypes"        "raw_chorotypes"    "bfs_all_pairs"     "graph"             "max_depth_species" "parameters"       '




# miscellaneuos

# 
# 
# # Miscellaneous tests
# graph = C; max_depth = 7; 
# max_diameter = 15; 
# max_Ct = 0.7; min_Ct = 0.68;
# Ct_resolution = -0.02; overlap = TRUE
# 
# 
# # subgraph based on edges
# test = subgraph.edges(g,eids = which(E(g)$Cs>0.96), delete.vertices = TRUE) %>% as_tbl_graph()
# 
# which(V(C)$name == "Gymnopithys rufigula")
# # 445
# # NOT WORKING
# test = subgraph.edges(C, eids = which(E(C)$Cs>0.92 & E(C)$from == 445), delete.vertices = TRUE) %>% as_tbl_graph()
# plot(test)
# #
# 
# 
# max_depth_spp = c("Amazilia fimbriata", "Accipiter bicolor", "Amazona farinosa")
# 
# test = g %>% activate(nodes) %>% filter(!(name %in% max_depth_spp))
# 
# plot(test)
# 
# # # check and Fix invalid shapes
# # any(!st_is_valid(map))
# # issues = map[which(!st_is_valid(map)),]$SCINAME
# # issues
# # map %>% st_drop_geometry() %>% .[which(!st_is_valid(map)),]
# # map %>%  .[which(!st_is_valid(map)),] %>% plot()
# # 
# # for (iss in issues){
# #         map[map$SCINAME==iss,] = 
# #                 map[map$SCINAME==iss,] %>% st_make_valid() %>% st_buffer(0) %>% 
# #                 st_simplify(preserveTopology = TRUE,) %>% 
# #                 st_is_valid()
# # }
# # 
# # 
# # map = map %>% group_by(SCINAME)%>%summarize()
# # # checking validity of sf maps inside
# # valid = st_is_valid(st_geometry(map))
# # 
# # map[!valid,]  = map[!valid,] %>% st_make_valid()
# # valid2 = st_is_valid(st_geometry(map))
# # map[!valid2,]  = map[!valid2,] %>% st_make_valid()
# # # correcting through buffer (st_make_valid not working)
# # map[!valid2,] = st_simplify(map[!valid2,], tolerance_meters = 100000)
# #  # using s2 package
# # # map[!valid2,] = s2::s2_rebuild(map[!valid2,],options = s2_options(duplicate_edges = FALSE, validate = TRUE,
# # #                            simplify_edge_chains = TRUE, split_crossing_edges = TRUE ))
# # 
# # # reproject to fix!!! Finally!
# # test = map[!valid2,] %>% st_transform(crs = 5875) # utm18S SAD69
# # 
# # plot(test[1,] %>% st_buffer(dist = 10000), add = T)
# # 
# # test = st_buffer(test,10)
# # test = st_buffer(test, -10)
# # test = st_make_valid(test)
# # test = test %>% st_transform(crs = 4326)
# # st_is_valid(test)  # OK!
# # test = test %>% st_transform(crs = 5875)
# # st_is_valid(test)  # 
# # 
# # valid_test = st_is_valid(st_geometry(test))
# # test[!valid_test,] = test[!valid_test,]
# # 
# # test[!valid_test,] = test[!valid_test,] %>% st_transform(crs = 5875)
# # test[!valid2,] = test[!valid_test,] %>% st_make_valid()
# # 
# # 
# # # check validity
# # if (all(st_is_valid(st_geometry(map)))) {rm(valid)}
# # issues
# # 
# # 
# # test = st_read("C:/SIG2018/test/mapSA_shp/mapSA.shp")
# # 
# # test[test$SCINAME %in% issues,] %>% 
# #         st_make_valid() %>% st_is_valid()
# # 
# 
# 
# # APPENDIX ECOREGIONS MAPS
# "D:\Users\arila\hubiC\DOC-MAIN\SCAN_to_GRAPH"
# south_america = st_read("D:/Users/arila/hubiC/DOC-MAIN/SCAN_to_GRAPH/South_America_contour.shp")
# sa = south_america
# plot(south_america)
# # eco = st_read(dsn = "D:/SIG2018/ECOREGIOES/official_WWF2019", layer = "wwf_terr_ecos")
# # eco = eco %>% filter(REALM == "NT") %>% select(eco_code,BIOME)
# 
# # st_write(eco,dsn = "D:/SIG2018/ECOREGIOES", layer = "ecoregions_Neotropics.shp", driver = "ESRI shapefile")
# 
# eco = st_read(dsn = "D:/SIG2018/ECOREGIOES", layer = "ecoregions_amazon_1")
# plot(eco[,1])
# eco %>% names

