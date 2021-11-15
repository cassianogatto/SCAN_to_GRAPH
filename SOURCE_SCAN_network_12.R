# version 7 - givin' stick
# version 6 updated depth_filter to remove all species in a component in which any species reached max depth

# there is still a possibility of error ... 
# does any removed species has the possibility to form a new closed group with any other species at lower thresholds?
# this is impossible through the individual assessment of species - does the network approach makes any difference?


# room for improvement:
# 1- add an option to give a list of undesired species (i.e. those already blowing up the depth or diameter limits. 
# This allows an easier way to start an assessment in a middle range of congruence thresholds


#### SOURCE to SCAN network ####
" Tidygraph has a problem: it does not recognizes the option delete.vertices, as in igraph original function
to_subgraph, here, addresses and fixes this problem"

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(readr)
library(sf)
library(dplyr,warn.conflicts = FALSE)
# to silence dplyr group_by + summarize  .groups options ("drop_last", "drop", "keep", "rowwise")  # see: https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override/62140681
options(dplyr.summarise.inform=F)
library(ggplot2)
# to avoid graphical warning about fonts
library(extrafont)
loadfonts(device = "win")
# extrafont::loadfonts(device="win")


# description: 
partial_components = function (graph = graph, threshold = 0.5, filter_depth = FALSE, overlap = FALSE,...){
        
        if(isTRUE(overlap) & isTRUE(filter_depth)) {
                graph %>% 
                        morph(to_subgraph, subset_by = "edges", (Cs >= threshold & is.na(.N()$no_overlap[from]) & is.na(.N()$depth_filter[from])), # check the node respective to the 'from' edge table
                              remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                        # write to both edges and nodes tibbles
                        activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
                        activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
                        # identify separate components
                        mutate("components{threshold}" := group_components("weak")) %>% # identify connected elements in COMPONENTS (simpler commuity definition)
                        unmorph()
        } else {
                if(isTRUE(overlap)) {
                        graph %>% 
                                morph(to_subgraph, subset_by = "edges", (Cs >= threshold & is.na(.N()$no_overlap[from])), remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
                                activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
                                mutate("components{threshold}" := group_components("weak")) %>% 
                                unmorph()
                } else {
                        
                        if(isTRUE(filter_depth)) {
                                graph %>% 
                                        morph(to_subgraph, subset_by = "edges", (Cs >= threshold & is.na(.N()$depth_filter[from])), 
                                              remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                        activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
                                        activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
                                        mutate("components{threshold}" := group_components("weak")) %>% # identify connected elements
                                        unmorph()
                        } else{
                                graph %>% 
                                        morph(to_subgraph, subset_by = "edges", Cs >= threshold, remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                        activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
                                        activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
                                        mutate("components{threshold}" := group_components("weak")) %>% 
                                        unmorph()
}       }       }       }


# calculate diameter, order, and centrality for each component separately
map_by_component = function(graph = graph, threshold = threshold, filter_depth = filter_depth){
        
        if(filter_depth) {graph = graph %>% activate(nodes) %>% filter(is.na(depth_filter))}
        
        graph %>% activate(edges) %>% filter(!is.na(get(paste0("Ct",threshold)))) %>% 
                activate(nodes) %>% filter(!is.na(get(paste0("Ct",threshold)))) %>% 
        # split by components
                morph(to_split, group_by = get(paste0("components",threshold)), subset_by = "nodes") %>% 
        # diameter = depth in SCAN
                mutate("diameter{threshold}" := graph_diameter(unconnected = TRUE),
        # order = richness of species
                       "order{threshold}" := graph_order(),
        # centrality by component
                "centrality{threshold}" := centrality_degree()) %>% 
        # # betweenness of each node - (cannot calculate betweenness like the above parameters... don't know why...)
        # mutate("betweenness" = betweenness()) %>% 
                
        unmorph()
}

bfs_depth = function(graph = graph, nodes = nodes, threshold = threshold){
        # max_depth_spp = c()
        bfs_reference_df = tibble()
        for (var_node in nodes){
                # check if  
                # if(var_node %in% max_depth_sp) next
                bfs_reference = graph %>% 
                        # filter threshold
                        activate(edges) %>% filter(get(paste0("Ct", threshold)) >= threshold) %>%
                        activate(nodes) %>% filter(get(paste0("Ct", threshold)) >= threshold) %>%
                        # # BFS rank (Check if update to dplyr::pull is OK)
                        bfs(root = var_node, order = FALSE, dist = TRUE, rank = FALSE, unreachable = FALSE) %>% .$dist
                # filter NA out
                bfs_reference = bfs_reference[!is.na(bfs_reference)]
                bfs_names = bfs_reference %>% names
                # include any sp > max_depth to exclusion list to nodes
                # if (any(bfs_reference > max_depth)){max_depth_spp = c(max_depth_spp, var_node)}
                bfs_reference = tibble(root = rep(var_node,length(bfs_reference)),
                                       Ct = threshold, neighbors = bfs_names, depth = bfs_reference)
                bfs_reference
                #check if depth_max was already reached -> break loop and set max_depth = TRUE
                # if(any(bfs_reference$depth > max_depth)) {max_depth = TRUE; break}
                # check for spatial overlap
                bfs_reference_df = rbind(bfs_reference_df, bfs_reference)
        }
        return(bfs_reference_df)
}

# overwrite Tidygraph to_subgraph with the option delete.vertices, as in igraph original function
to_subgraph <- function(graph, ..., subset_by = NULL, delete.vertices = TRUE) {
        if (is.null(subset_by)) {
                subset_by <- active(graph)
                message('Subsetting by ', subset_by)
        }
        ind <- as_tibble(graph, active = subset_by)
        ind <- mutate(ind, .tidygraph_index = seq_len(n()))
        ind <- filter(ind, ...)
        ind <- ind$.tidygraph_index
        subset <- switch(
                subset_by,
                nodes = induced_subgraph(graph, ind),
                edges = subgraph.edges(graph, ind, delete.vertices = delete.vertices)
        )
        list(subgraph = as_tbl_graph(subset))
}


# no_overlap over restrictive
# depth filter too permissive :-(


SCAN = function(graph = C,
                max_depth = 7,
                max_diameter = 7,
                min_Ct = 0.3,
                max_Ct = max(C %>% activate(edges) %>% as_tibble %>% .$Cs),
                Ct_resolution = -0.01,
                overlap = TRUE,
                filter_depth = TRUE,		# the diameter and depth filters are controlled by this parameter in this version - a split is required
                filter_diameter = TRUE,
                max_depth_spp = c()){		# if given, these species are excluded from analysis if filter_depth TRUE  
        
        # there is a difference between filter_diameter, which is measured directly over the whole component and excludes
        # the whole component reaching max_diameter,  before BFS analysis,
        # and filter_depth, which depends on the depth calculated by the BFS algorithm and assesses species individually;
        # then, there is the option to exclude only those species reaching max_depth (delete_all_component_spp_max_depth = FALSE),
        # or all species (TRUE); These BFS species reaching max_depth are written to 
        
        # Setup objects 
        {
        graph_chorotypes = tibble()
        chorotypes = list()
        bfs_all_pairs = tibble()
        
        # setup graph column to filter by depth
        if(filter_depth | filter_diameter){graph = graph %>% activate(nodes) %>% mutate(diameter_filter = NA, depth_filter = NA)}
        # setup graph column to filter by overlap
        if(overlap){graph = graph %>% activate(nodes) %>% mutate(no_overlap = NA)}
        
        # mark species already given as reaching max depth or diameter to be filtered later
        if(length(max_depth_spp) >= 1) {
                graph = graph %>% morph(to_subgraph, subset_by = "nodes",  name %in% max_depth_spp,
                                        remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                        mutate(depth_filter = 1) %>% 
                        unmorph()}
        }
        
# graph + diameter_filter + depth_filter + no_overlap
        
        #### MAIN LOOP ####
        for(threshold in seq(max_Ct,min_Ct,Ct_resolution)){
                
                # morph to sub-graph based on C threshold (see function partial_components above) it allows overlap and/or depth_filter
                # filter_depth used inside morph state to calculate components
                graph = partial_components(graph = graph, threshold = threshold, filter_depth = filter_depth, overlap = overlap)
                
# graph + Ct{threshold} + components{threshold}
                
        #### COMMUNITY-BASED GRAPH METRICS ####
                # calculate diameter, order, and centrality for each component separately (allows only depth_filter)
                # order (richness) and diameter (depth) # g is a sub-graph filtered by threshold (filter is needed to calculate diameter - there are 'ghost' vertices and edges without filtering)} # (split into component subgraphs (connected islands) and calculate max _depth)
                g = map_by_component(graph = graph, threshold = threshold, filter_depth = filter_depth)
                
                # write diameter filter blown-ups
                g = g %>% activate(nodes) %>% mutate("diameter_filter" = ifelse(get(paste0("diameter",threshold)) > max_depth, threshold, NA))
                
                # betweenness for each node
                g = g %>% activate(nodes) %>% mutate("betweenness{threshold}" := round(betweenness(g),1))
                
                # summarize
                g_spp = g %>% activate(nodes) %>% as_tibble %>% 
                        group_by(name, get(paste0("components",threshold)), get(paste0("diameter",threshold)),
                                 get(paste0("order",threshold)), get(paste0("centrality",threshold))) %>% 
                        summarize(Ct = threshold, betweenness = get(paste0("betweenness",threshold))) %>% 
                        select(1,Ct, components = 2, diameter = 3, order = 4, centrality = 5, betweenness) %>% 
                        arrange(name)
                
                g_summary = {g %>% activate(nodes) %>% as_tibble %>% group_by(get(paste0("components",threshold)),
                                                                              get(paste0("order",threshold))) %>% 
                                summarize(Ct = threshold, chorotype_spp = paste(name, collapse = ", "), richness_spp = n(),
                                          diameter = max(get(paste0("diameter",threshold))), 
                                          max_centrality = max(get(paste0("centrality",threshold))),
                                          max_betweenness = max(get(paste0("betweenness", threshold)))) %>% 
                                select(component = 1, Ct, chorotype_spp, richness_spp, diameter, max_centrality, max_betweenness)}
                
        #### UPDATE GRAPH with community parameters ####
                # <- left join graph with order and diameter 
                #graph_bkp
                graph = graph %>% activate(nodes) %>%
                # join with g
                        left_join(activate(g,nodes), by = c("name", ".tidygraph_index", "depth_filter", "no_overlap", 
                                                            paste0("Ct",threshold), paste0("components",threshold)), copy = TRUE)
                # graph_bkp1
                graph = graph %>% mutate(diameter_filter = ifelse(is.na(diameter_filter.y), diameter_filter.x, diameter_filter.y)) %>% 
                        select(name,.tidygraph_index,diameter_filter,depth_filter,no_overlap, everything()) %>% 
                        select(-contains(".y"), - contains(".x"), everything())
                
                
        #### SPATIAL OVERLAP FILTER #### 
                # between components # using igraph::are.connected in the original graph #
                # results in object connected_nodes_in_components
                if(isTRUE(overlap)){
                        # Cs_sim = Cs %>% select(species1, species2)      
                        are_connected = data.frame()
                        for(comp in g_summary$component){
                                spp = g_summary %>% filter(component == comp) %>% pull(.,"chorotype_spp") %>% strsplit(.,", ") %>% .[[1]]
                                
                                for (sp1 in spp){
                                        for(sp2 in spp[which(spp != sp1)]){
                                                conn = tibble(species1 = sp1, species2 = sp2, 
                                                              connected = igraph::are.connected(graph, sp1,sp2))
                                                are_connected = rbind(are_connected, conn)
                                        }
                                        
                                }
                        }
                        
                        # if all species in a component which sp1 belongs are connected -> TRUE
                        connected_nodes_in_components =  are_connected %>% 
                                group_by(species1) %>% summarize(all_connected = ifelse(all(connected), TRUE, FALSE)) %>% 
                                left_join(g_spp, by = c("species1" = "name")) %>% select(component = 4, name = 1,2) %>% 
                                arrange(component, name)
                        
                        # identify and remove communities in which not all components are connected (overlapped)
                        all_connected_components = connected_nodes_in_components %>% group_by(component) %>%
                                summarize(all_connected = ifelse(all(all_connected), TRUE, FALSE))
                        
                        not_connected_components = all_connected_components %>% filter(all_connected == FALSE) %>% pull(.,"component")
                        spp_in_not_connected_components = g_spp %>% filter(components %in% not_connected_components) %>% 
                                pull(.,name)
                        
                        # write not all-overlapped components to non-overlap column
                        if(length(not_connected_components) > 0) {graph = graph %>% morph(to_subgraph, subset_by = "nodes", 
                                          # criteria to write non-overlap for the first and only time in graph
                                          is.na(no_overlap) & name %in% spp_in_not_connected_components,
                                          remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                mutate(no_overlap = threshold) %>% 
                                unmorph()
                        }
                }  # not_connected_components and mark graph "no_overlap" = threshold
                
        #### BFS ####
                # Filter out diameter_max of bfs analyzes
                if(filter_diameter){ nodes = graph %>% activate(nodes) %>% filter(!is.na(get(paste0("Ct", threshold)))) %>% 
                        filter(is.na(diameter_filter)) %>% pull(name)
                
                } else { nodes = graph %>% activate(nodes) %>% filter(!is.na(get(paste0("Ct", threshold)))) %>% 
                                pull(name) }
                
                # Break if vector is empty #
                if(length(nodes) == 0){ print(paste0("At Ct = ",threshold, "all chorotypes have diameter larger than ", max_diameter))
                        break}
                
                # obs. the 'nodes' vector was already filtered for max_depth - only communities < max_diameter will be analyzed for bfs
                # see bfs_depth function (Breadth-First Survey)
                g_bfs = bfs_depth(graph,nodes,threshold)
                
                g_bfs_summ = g_bfs %>% group_by(root, Ct) %>% 
                        summarize(component_spp = paste(neighbors, collapse = ", "),
                                  maximum_depth = max(depth)) %>% arrange(root)
                
                bfs_all_pairs = rbind(bfs_all_pairs, g_bfs)
                
                graph_summary = g_bfs_summ %>% left_join(g_spp, by = c("root" = "name", "Ct" = "Ct"))
                
                graph_chorotypes = rbind(graph_chorotypes, graph_summary) %>% select(1,2,5,3,4,6,7,8,9) %>% 
                        arrange(desc(Ct),components,component_spp)
                
                # unique species reaching max_depth (not the whole component) (but only those already filtered by nodes, i.e. from communities not reaching max diameter!!)
                max_depth_spp_partial  = graph_summary %>% filter(maximum_depth > max_depth) %>% pull(root)
                
                max_depth_spp = unique(c(max_depth_spp, max_depth_spp_partial))
                
                # mark only species reaching max_depth as roots in bfs to graph's "depth_filter" column
                if(isTRUE(filter_depth)){
                        graph = graph %>% morph(to_subgraph, subset_by = "nodes", {name %in% max_depth_spp_partial},
                                                remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                mutate(depth_filter = ifelse(is.na(depth_filter), threshold, paste(depth_filter, threshold, collapse = ", ") )) %>% unmorph()
                }
                # # mark ALL species in a component in which ANY species is reaching max_depth (but this was already filtered by 'nodes' before bfs...)
                # removed (see v11); better use max_diameter instead
                
                # update depth to graph
                
                graph = graph %>% activate(nodes) %>%
                        # join with g
                        left_join(activate(g,nodes), by = c("name", ".tidygraph_index", "depth_filter", "no_overlap", 
                                                            paste0("Ct",threshold), paste0("components",threshold)), copy = TRUE)
                
                graph = graph %>% mutate(diameter_filter = ifelse(is.na(diameter_filter.y), diameter_filter.x, diameter_filter.y)) %>% 
                        select(name,.tidygraph_index,diameter_filter,depth_filter,no_overlap, everything()) %>% 
                        select(-contains(".y"), - contains(".x"))
                
                
        #### Return Results ####
                {
                chorotypes[["Chorotypes"]] = graph_chorotypes %>%
                        group_by(component_spp, diameter, order) %>% 
                        summarize(Ct = max(Ct), Ct_min = min(Ct), maximum_depth = max(maximum_depth), max_centrality = max(centrality)) %>% 
                        arrange(component_spp, order, desc(Ct))
                chorotypes[["raw_chorotypes"]] = graph_chorotypes
                chorotypes[["bfs_all_pairs"]] = bfs_all_pairs
                chorotypes[["graph"]] = graph
                chorotypes[["max_depth_species"]] = ifelse(is.null(max_depth_spp), "no species in any chorotype reached max_depth",
                                                           max_depth_spp)
                chorotypes[["parameters"]] = tibble(max_depth = max_depth, max_diameter = max_diameter,
                                         max_Ct = max_Ct, min_Ct = min_Ct, Ct_resolution = Ct_resolution,
                                         overlap = overlap, filter_depth = filter_depth)
                }
        }
        return(chorotypes)
}



# minor functions
nodes_names = function(x){x %>% activate(nodes) %>% as_tibble %>% names}
edges_names = function(x){x %>% activate(edges) %>% as_tibble %>% names}
plot_graph = function(graph = graph, threshold = threshold, layout = 'fr', sizefont = 2.3, comment = c()){ # layouts: fr, dh, kk, mds, gem, graphopt etc
        graph %>% 
                activate(edges) %>% filter(get(paste0("Ct",threshold))) %>%
                activate(nodes) %>% filter(get(paste0("Ct",threshold))) %>%
                
                ggraph(layout = layout) + 
                geom_edge_link(aes(alpha = (Cs*1.5) ), show.legend = FALSE) + 
                
                geom_node_point(aes( colour = as.factor(get(paste0("components",threshold)))),
                                size = 3.5,   show.legend = FALSE) + 
                
                geom_node_text(aes(label = name), size = sizefont, repel = TRUE, 
                               show.legend = FALSE) +
                
                labs(subtitle = paste0("Ct = ", threshold, " ", comment)) +
                theme(text=element_text(family="mono")) +
                theme_graph()
}

plot_SCAN = function(graph = graph, threshold = threshold, layout = 'fr',
                     sizefont = 2.3, comment = c("max_depth is ... ")){ # layouts: fr, dh, kk, mds, gem, graphopt etc
        graph %>% 
                
                activate(edges) %>% filter(get(paste0("Ct",threshold))) %>%
                activate(nodes) %>% filter(get(paste0("Ct",threshold))) %>%
                
                filter(is.na(diameter_filter))  %>%
                filter(is.na(depth_filter)) %>%
                filter(is.na(no_overlap))  %>%
                
                ggraph(layout = layout) + 
                geom_edge_link(aes(alpha = (Cs*1.5) ), show.legend = FALSE) + 
                
                geom_node_point(aes( colour = as.factor(get(paste0("components",threshold)))),
                                size = 3.5,   show.legend = FALSE) + 
                
                geom_node_text(aes(label = name), size = sizefont, repel = TRUE, 
                               show.legend = FALSE) +
                
                labs(subtitle = paste0("Ct = ", threshold, " ", comment)) +
                theme(text=element_text(family="mono")) +
                theme_graph()
}


# improved list of objects from internet
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
        napply <- function(names, fn) sapply(names, function(x)
                fn(get(x, pos = pos)))
        names <- ls(pos = pos, pattern = pattern)
        obj.class <- napply(names, function(x) as.character(class(x))[1])
        obj.mode <- napply(names, mode)
        obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
        obj.size <- napply(names, object.size)
        obj.dim <- t(napply(names, function(x)
                as.numeric(dim(x))[1:2]))
        vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
        obj.dim[vec, 1] <- napply(names, length)[vec]
        out <- data.frame(obj.type, obj.size, obj.dim)
        names(out) <- c("Type", "Size", "Rows", "Columns")
        if (!missing(order.by))
                out <- out[order(out[[order.by]], decreasing=decreasing), ]
        if (head)
                out <- head(out, n)
        out
}
# shorthand
lsos <- function(..., n=10) {
        .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
