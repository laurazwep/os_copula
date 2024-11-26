#plot functions
library(tidyverse)
library(ggrepel)
library(patchwork)


plot_loadings <- function(output_nom) {
  loading_plot <- output_nom$os_loadings %>% 
    rownames_to_column("variable") %>% 
    ggplot(aes(x = V1, y = V2)) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_segment(aes(xend = 0, yend = 0), arrow = arrow(length = unit(0.2, "cm"), ends = "first")) +
    geom_text_repel(aes(label = variable), bg.colour='white', color = "black", segment.color = NA) +
    labs(x = "dimension 1", y = "dimension 2") +
    scale_x_continuous(limits = range(c(output_nom$os_loadings$V2, output_nom$os_loadings$V1))*1.1, breaks = seq(-1, 1, by = 0.25)) +
    scale_y_continuous(limits = range(c(output_nom$os_loadings$V2, output_nom$os_loadings$V1))*1.1, breaks = seq(-1, 1, by = 0.25)) +
    coord_equal() +
    theme_base(color = "#1B1B1B")
  return(loading_plot)
}

plot_transformations <- function(output_nom, of_interest, order_facets) {
  if (missing(of_interest)) {
    of_interest <- names(output_nom$data)
  }
  
  if (missing(order_facets)) {
    order_facets <- names(output_nom$data)
  }
  suppressMessages(
  ord_data_long <- output_nom$os_data %>% 
    mutate(ID = 1:n()) %>% 
    pivot_longer(-ID, names_to = "variable", values_to = "OS") %>% 
    mutate(variable = factor(variable, levels = order_facets)) %>%
    left_join(output_nom$data %>%
                mutate(across(everything(), ~ factor(.x, ordered = F))) %>%
                mutate(ID = 1:n()) %>%
                pivot_longer(all_of(order_facets), names_to = "variable", values_to = "alphabetic") %>% 
                mutate(variable = factor(variable, levels = order_facets))) %>%
    select(-ID) %>% 
    distinct() %>% 
    
    filter(variable %in% of_interest)
  )
  
  plot_transformations <- ord_data_long %>%
    ggplot(aes(x = alphabetic, y = OS)) +
    geom_hline(yintercept = 0, color = "grey65") +
    geom_point() +
    geom_step(aes(group = 1)) +
    facet_wrap(~ variable, scales = "free_x", nrow = 2) +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    labs(y = "Optimal scaling quantification", x = "Original values") +
    theme_base()
  
  return(plot_transformations)
}
