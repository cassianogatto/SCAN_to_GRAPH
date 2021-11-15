
# install.packages("extrafont")
library(extrafont)
loadfonts(device = "win")
# extrafont::loadfonts(device="win")
library(ggplot2)
dev.new()
# threshold = 0.9

# for(threshold in c(0.94, 0.9, 0.85, 0.8)){
  dev.new()

plot_graph = function(graph = graph, threshold = threshold){
    
    graph %>% 
          # for use with the large graph
            activate(edges) %>% filter(get(paste0("Ct",threshold))) %>%
            activate(nodes) %>% filter(get(paste0("Ct",threshold))) %>%
            # mutate(components = as.factor(group_components())) %>% 
            ggraph(layout = 'kk') + 
            geom_edge_link(aes(alpha = 1 ), show.legend = FALSE) + 
            # geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE) + 
            geom_node_point(aes(#size = get(paste0("centrality",threshold)), 
            colour = as.factor(get(paste0("components",threshold)))),
              # colour = 10+  get(paste0("components",threshold))), 
                            show.legend = FALSE, size = 4) + 
            geom_node_text(aes(label = name 
                               #, colour = as.factor(get(paste0("components",threshold)))
                               ), 
                           label.size = 0.05,repel = TRUE, show.legend = FALSE)+
            labs(subtitle = paste0("Ct = ", threshold)) +
            theme_graph()
    }


dev.new()
threshold = 0.71
graph %>% 
  # for use with the large graph
  activate(edges) %>% filter(get(paste0("Ct",threshold))) %>%
  activate(nodes) %>% filter(get(paste0("Ct",threshold))) %>%
  # mutate(components = as.factor(group_components())) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(colour = rgb(0,0,0,Cs)), show.legend = FALSE)+
  # geom_edge_density(aes(fill = Cs*10)) + 
  # geom_edge_link(aes(label = Cs), show.legend = FALSE,
  #                angle_calc = 'along',
  #                label_dodge = unit(2.5, 'mm'), 
  #                label_size = 2.5 ) + 
  # geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE) + 
  geom_node_point(aes(#size = get(paste0("centrality",threshold)), 
    colour = get(paste0("components",threshold))
    ), 
  show.legend = FALSE, size = 4) + 
  geom_node_text(aes(label = name, size = 3
                     #, colour = as.factor(get(paste0("components",threshold)))
  ), 
 repel = TRUE, show.legend = FALSE)+
  labs(subtitle = paste0("Ct = ", threshold)) +
  theme_graph()



threshold = 0.9

C %>% activate(edges) %>% filter(Cs > threshold) %>%  
  # for use with the large graph
#  activate(edges) %>% filter(get(paste0("Ct",threshold))) %>%
#  activate(nodes) %>% filter(get(paste0("Ct",threshold))) %>%
  # mutate(components = as.factor(group_components())) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(alpha = 1 ), show.legend = FALSE) + 
  # geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE) + 
  geom_node_point() +
    #colour = get(paste0("components",threshold))), 
    #show.legend = FALSE, size = 4) + 
  geom_node_text(aes(label = name 
                     #, colour = as.factor(get(paste0("components",threshold)))
  ), 
  label.size = 0.05,repel = TRUE, show.legend = FALSE)+
  labs(subtitle = paste0("Ct = ", threshold)) +
  theme_graph()
