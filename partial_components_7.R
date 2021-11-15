partial_components = function (graph = graph, threshold = threshold,filter_depth = TRUE, overlap = TRUE,...){
	if(isTRUE(overlap) & isTRUE(filter_depth) {
			graph %>% 
			morph(to_subgraph, subset_by = "edges", (Cs >= threshold & is.na(.N()$no_overlap[from]) & is.na(.N()$depth_filter[from])), # check the node respective to the 'from' edge table
			      remove_multiples = TRUE, delete.vertices= TRUE) %>% 
			# write to both edges and nodes tibbles
			activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
			activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
			# identify separate components
			mutate("components{threshold}" := group_components("weak")) %>% # identify connected elements
			unmorph()
		} else {
			if(isTRUE(overlap) & !(isTRUE(filter_depth))) {
				graph %>% 
				morph(to_subgraph, subset_by = "edges", (Cs >= threshold & is.na(.N()$no_overlap[from])), remove_multiples = TRUE, delete.vertices= TRUE) %>% 
				activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
				activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
				mutate("components{threshold}" := group_components("weak")) %>% 
				unmorph()
				} else {
					if(isTRUE(filter_depth) & !(isTRUE(overlap))) {
						graph %>% 
						morph(to_subgraph, subset_by = "edges", (Cs >= threshold & is.na(.N()$depth_filter[from])), 
						      remove_multiples = TRUE, delete.vertices= TRUE) %>% 
						activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
						activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
						mutate("components{threshold}" := group_components("weak")) %>% # identify connected elements
						unmorph()
						} else{
							graph %>% 
							morph(to_subgraph, subset_by = "edges", Cs >= threshold, 
								remove_multiples = TRUE, delete.vertices= TRUE) %>% 
							activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
							activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
							mutate("components{threshold}" := group_components("weak")) %>% 
							unmorph()
							}
				       }
			}
                }
