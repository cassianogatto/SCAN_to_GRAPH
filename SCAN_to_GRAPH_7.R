SCAN = function(graph = C,
                max_depth = 7,
                max_diameter = 10,
                min_Ct = 0.3,
                max_Ct = max(C %>% activate(edges) %>% as_tibble %>% .$Cs),
                Ct_resolution = -0.01,
                overlap = TRUE,
                filter_depth = TRUE,		# the diameter and depth filters are controlled by this parameter in this version - a split is required
				max_depth_spp = c() ){		# if given, these species are excluded from analysis if filter_depth TRUE  	
        
        # Setup objects                
        graph_chorotypes = tibble()
        chorotypes = list()
        bfs_all_pairs = tibble()
       
        # setup graph column to filter by depth
        graph = graph %>% activate(nodes) %>% mutate(depth_filter = NA)
		
		# mark species already given as reaching max depth or diameter to be filtered later
		if(length(max_depth_spp) > 1) {
                        graph = graph %>% morph(to_subgraph, subset_by = "nodes", name %in% max_depth_spp,
                                                remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                mutate(depth_filter = 1) %>% 
                                unmorph()}
		
        #### loop across Thresholds - Ct ####
        for(threshold in seq(max_Ct,min_Ct,Ct_resolution)){
                
                # morph to sub-graph based on C threshold (see function partial_components above)
                # filter_depth used inside morph state to calculate components
                graph = partial_components(graph = graph, threshold = threshold, filter_depth = filter_depth)
                
                #### COMMUNITY-BASED GRAPH METRICS ####
                # calculate diameter, order, and centrality for each component separately
                # order (richness) and diameter (depth) # g is a sub-graph filtered by threshold (filter is needed to calculate diameter - there are 'ghost' vertices and edges without filtering)} # (split into component subgraphs (connected islands) and calculate max _depth)
                g = map_by_component(graph = graph, threshold = threshold, filter_depth = filter_depth)
                
                g_spp = g %>% activate(nodes) %>% as_tibble %>% group_by(name, get(paste0("components",threshold)), get(paste0("diameter",threshold)),
                                                                         get(paste0("order",threshold)), get(paste0("centrality",threshold))) %>% 
                        summarize(Ct = threshold) %>% 
                        select(1,6, components = 2, diameter = 3, order = 4, centrality = 5)
                g_spp = g_spp %>% arrange(name)
                
                g_summary = {g %>% activate(nodes) %>% as_tibble %>% group_by(get(paste0("components",threshold)),
                                                                              get(paste0("order",threshold))) %>% 
                                summarize(Ct = threshold, chorotype_spp = paste(name, collapse = ", "), richness_spp = n(),
                                          diameter = max(get(paste0("diameter",threshold))), 
                                          max_centrality = max(get(paste0("centrality",threshold)))) %>% 
                                select(component = 1, Ct, chorotype_spp, richness_spp, diameter, max_centrality)}
                
                #### UPDATE GRAPH - left join graph with order and diameter  ####
                graph = graph %>% activate(nodes) %>% 
                        left_join(select(activate(g,nodes),name,
                                         grep(names(as_tibble(activate(g,nodes))),pattern="diameter"),
                                         grep(names(as_tibble(activate(g,nodes))),pattern="order"),
                                         grep(names(as_tibble(activate(g,nodes))),pattern="centrality")),
                                  copy = TRUE) 
                
                #### check for spatial overlap #### between components # using igraph::are.connected in the original graph #
                # results in object connected_nodes_in_components
                if(isTRUE(overlap)){
                        overlap_df = g %>% activate(nodes) %>% as_tibble %>% 
                                group_by(components = get(paste0("components", threshold))) %>% 
                                summarize(species = paste(name, collapse = ", ")) %>% 
                                filter(!is.na(components))
                        
                        # Cs_sim = Cs %>% select(species1, species2)      
                        are_connected = data.frame()
                        for(component in overlap_df$components){
                                spp = overlap_df %>% as.data.frame %>% .[component,"species"] %>% 
                                        strsplit(.,", ") %>% .[[1]]
                                for (sp1 in spp){
                                        for(sp2 in spp[which(spp != sp1)]){
                                                conn = data.frame("species1" = sp1, "species2" = sp2, 
                                                                  connected = igraph::are.connected(graph, sp1,sp2))
                                                are_connected = rbind(are_connected, conn)
                                        }
                                        
                                }
                        }
                        # if all species in a component which sp1 belongs are connected -> TRUE
                        connected_nodes_in_components =  are_connected %>% 
                                group_by(species1) %>%
                                summarize(all_connected = ifelse(all(connected), TRUE, FALSE)) %>% 
                                arrange(species1)
                }
                
                # remove communities in which not all components are connected (overlapped)
                if(isTRUE(overlap)){
                        
                }
                
                #### BFS SETUP # Depth/ Diameter_max ####
                # filter those components (communities) which diameter (depth) is not larger than max_diameter
                
                if(filter_depth){
                        nodes = g %>% activate(nodes) %>% as.data.frame %>% 
                                filter(get(paste0("diameter",threshold)) <= max_diameter) %>% 
                                filter(is.na(depth_filter)) %>% .$name
                } else {
                        nodes = g %>% activate(nodes) %>% as.data.frame %>% 
                                filter(get(paste0("diameter",threshold)) <= max_diameter) %>% .$name
                }
                
                # HERE THERE IS A POSSIBILITY TO INTRODUCE AN 'area_max' FILTER OVER nodes...
                # a function to calculate areas and join to graph...
                
                #### Break if vector is empty ####
                if(length(nodes) == 0){ print(paste0("At Ct = ",threshold, "all chorotypes have diameter larger than ", max_diameter))
                        break}
                # filter those species giving rise to components with no overlap (connection at any threshold)
                if(isTRUE(overlap)){
                        connected_nodes_true = 
                                connected_nodes_in_components[connected_nodes_in_components$all_connected,]$species1
                        # apply filter to nodes (whole connected components only)
                        nodes = nodes[which(nodes %in% connected_nodes_true)]
                }
                nodes
                
                #### BFS to distance #### see bfs_depth function
                g_bfs = bfs_depth(graph,nodes,threshold)
                
                g_bfs_summ = g_bfs %>% group_by(root, Ct) %>% 
                        summarize(component_spp = paste(neighbors, collapse = ", "),
                                  maximum_depth = max(depth)) %>% arrange(root)
                
                #### join tables to get the final results ####
                bfs_all_pairs = rbind(bfs_all_pairs, g_bfs)
                graph_summary = g_bfs_summ %>% left_join(g_spp, by = c("root" = "name", "Ct" = "Ct"))
                graph_chorotypes = rbind(graph_chorotypes, graph_summary) %>% select(1,2,5,3,4,6,7,8) %>% 
                        arrange(desc(Ct),components,component_spp)
                
                # AQUI INSERIR A LISTA DE ESPÉCIES 
                # 
                
                max_depth_spp_partial  = graph_chorotypes %>% filter(maximum_depth > max_depth) %>% 
                        select(component_spp) %>% as.data.frame %>% .$component_spp %>% str_split(pattern = ", ") %>% unlist()
                
                # list with all species of any component with any species reaching max_depth
                # max_depth_spp_partial = g_bfs_summ %>% filter(maximum_depth > max_depth) %>% 
                #         select(name = root) %>% as.data.frame %>% .$name
                # max_depth_spp = unique(c(max_depth_spp, max_depth_spp_partial))
                
                max_depth_spp = unique(c(max_depth_spp, max_depth_spp_partial))
                
                # copy max_depth species to graph depth_filter
                
                if(isTRUE(filter_depth)){
                        graph = graph %>% morph(to_subgraph, subset_by = "nodes", name %in% max_depth_spp_partial,
                                                remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                mutate(depth_filter = threshold) %>% 
                                unmorph()}
                
                # list with results
                chorotypes[[1]] = graph_chorotypes %>%
                        group_by(component_spp, diameter, order) %>% 
                        summarize(Ct = max(Ct), Ct_min = min(Ct), maximum_depth = max(maximum_depth), max_centrality = max(centrality)) %>% 
                        arrange(desc(Ct) , component_spp)
                chorotypes[[2]] = graph_chorotypes
                chorotypes[[3]] = bfs_all_pairs
                chorotypes[[4]] = graph
                chorotypes[[5]] = max_depth_spp
                chorotypes[[6]] = tibble(max_depth = max_depth, max_diameter = max_diameter,
                                         max_Ct = max_Ct, min_Ct = min_Ct, Ct_resolution = Ct_resolution,
                                         overlap = overlap, filter_depth = filter_depth)
                names(chorotypes) = c("chorotypes", "raw_chorotypes", "bfs_all_pairs",
                                      "graph", "max_depth_species", "parameters")
                
        }
        return(chorotypes)
}