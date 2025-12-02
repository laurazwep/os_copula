#plot functions
library(tidyverse)
library(ggrepel)
library(patchwork)

theme_base <- function(color = "black") {
  theme_bw() +
    theme(text = element_text(size = 12, color = color), 
          panel.grid = element_blank(), panel.border = element_rect(color = color),
          axis.title.y = element_text(size = 12, color = color, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.title.x = element_text(size = 12, color = color, margin = margin(t = 12, r = 0, b = 0, l = 0)),
          strip.background = element_blank(), 
          strip.text = element_text(size = 12, color = color, face = "bold", margin = margin(t = 0, r = 0, b = 5, l = 0)),
          axis.text = element_text(size = 12, color = color),
          plot.title = element_text(face = "bold", hjust = 0.5, color = color, margin = margin(t = 0, r = 0, b = 14, l = 0, unit = "pt")))
}

plot_loadings <- function(output_nom, flip_x = FALSE, flip_y = FALSE) {
  plot_data <- output_nom$os_loadings %>% 
    rownames_to_column("variable")
  if (flip_x) {
    plot_data <- plot_data %>% 
      mutate(V1 = -V1)
  }
  if (flip_y) {
    plot_data <- plot_data %>% 
      mutate(V2 = -V2)
  }
  loading_plot <- plot_data %>% 
    ggplot(aes(x = V1, y = V2)) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_segment(aes(xend = 0, yend = 0), arrow = arrow(length = unit(0.2, "cm"), ends = "first")) +
    geom_text_repel(aes(label = variable), bg.colour='white', color = "black", segment.color = NA) +
    labs(x = "dimension 1", y = "dimension 2") +
    scale_x_continuous(limits = range(c(plot_data$V2, plot_data$V1))*1.1, breaks = seq(-1, 1, by = 0.25)) +
    scale_y_continuous(limits = range(c(plot_data$V2, plot_data$V1))*1.1, breaks = seq(-1, 1, by = 0.25)) +
    coord_fixed() +
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


barboxplot_comparison <- function(variables, data_os, data_orig, data_naive = NULL) {
  my_label <- function(x) {
    names(x)[[1]] <- stringr::str_wrap(gsub("_", " ", variables[[2]]), 10)
    x[[1]] <- stringr::str_wrap(gsub("_", " ", x[[1]]), 10)
    label_both(x, sep = "\n")
  }
  
  set_ordering_equal <- function(data, data_orig, variables) {
    for (j in 1:length(variables)) {
      data[, j] <- factor(as.character(data[[j]]), levels = levels(data_orig[, variables[j]]))
    }
    return(data)
  }
  
  all_combs <- crossing(unique(data_os[[variables[1]]]), unique(data_os[[variables[2]]]))
  colnames(all_combs) <- variables
  all_combs <- all_combs %>% set_ordering_equal(data_orig, variables)
  n_lines <- length(levels(data_os[[variables[1]]])) + 1
  
  suppressMessages(
    frequencies_os <- data_os %>% 
      select(all_of(variables), sim_nr) %>% 
      group_by(across(all_of(variables)), sim_nr) %>% 
      summarize(count = n()) %>% ungroup() %>% 
      set_ordering_equal(data_orig, variables) %>% 
      group_by(sim_nr) %>% full_join(all_combs) %>% 
      mutate(count = ifelse(is.na(count), 0, count)) %>% 
      mutate(freq = count/sum(count)) %>% ungroup() %>% 
      set_ordering_equal(data_orig, variables)
  )
  suppressMessages(
    frequencies_orig <- data_orig %>% 
      select(all_of(variables)) %>% 
      group_by(across(all_of(variables))) %>% 
      summarize(count = n()) %>% ungroup() %>% 
      mutate(freq = count/sum(count)) %>% ungroup() %>% 
      set_ordering_equal(data_orig, variables)
  )
  
  if (is.null(data_naive)) {
    bar_box_plots <- frequencies_orig %>% 
      ggplot(aes(x = .data[[variables[1]]], y = freq)) +
      geom_bar(stat = "identity", position  = "dodge", aes(fill = "#969696")) +
      geom_vline(xintercept = seq(0.5, n_lines, by = 1), color = "gray93", linewidth = 0.5) +
      geom_boxplot(data = frequencies_os, alpha = 0.7, aes(fill = "#3ABAC1")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      scale_fill_identity(name = "data", guide = 'legend', labels = c("simulated", "observed")) +
      facet_grid(~ .data[[variables[2]]], labeller = my_label) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_bw() + 
      theme(panel.grid.major.x = element_blank(), strip.background = element_rect(fill = "white"))
  } else {
    suppressMessages(
      frequencies_naive <- data_naive %>% 
        select(all_of(variables), sim_nr) %>% 
        group_by(across(all_of(variables)), sim_nr) %>% 
        summarize(count = n()) %>% ungroup() %>% 
        set_ordering_equal(data_orig, variables) %>% 
        group_by(sim_nr) %>% full_join(all_combs) %>% 
        mutate(count = ifelse(is.na(count), 0, count)) %>% 
        mutate(freq = count/sum(count)) %>% ungroup() %>% 
        mutate(data = "simulated original")
    )
    
    suppressMessages(
      frequencies_both <- frequencies_os %>% 
        mutate(data = "simulated os") %>% 
        bind_rows(frequencies_naive) %>% 
        mutate(fill_col = ifelse(data == "simulated os", "#3ABAC1", "#E7C734"))
    )
    bar_box_plots <- frequencies_orig %>% 
      ggplot(aes(x = .data[[variables[1]]], y = freq)) +
      geom_bar(stat = "identity", position  = "dodge", aes(fill = "#969696")) +
      geom_vline(xintercept = seq(0.5, n_lines, by = 1), color = "gray93", linewidth = 0.5) +
      geom_boxplot(data = frequencies_both, alpha = 0.7, aes(fill = fill_col)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      scale_fill_identity(name = "data", guide = 'legend', labels = c("os simulated", "observed", "original simulated")) +
      facet_grid(~ .data[[variables[2]]], labeller = my_label) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_bw() + 
      theme(panel.grid.major.x = element_blank(), strip.background = element_rect(fill = "white"))
  }
  
  return(bar_box_plots)
}