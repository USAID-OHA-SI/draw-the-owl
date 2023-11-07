# PROJECT: draw-the-owl data analysis
# PURPOSE: Munge and Analysis of Malawi Strategic Information Needs
# AUTHOR:  Tim Esssam | SI
# REF ID:  d4781b35
# LICENSE: MIT
# DATE:   2023-11-06
# NOTES:  Assessment conducted in November 2023 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)
    library(googlesheets4)
    
    
  # SI specific paths/functions  
    load_secrets()

  # link to different tools
    mwi_logframe_path <- "1EA6bhDaPU_03aH6G0lK7m_IDpsRIvLNvrR41NEoQkKw"
    task_path <- "1d4uwOGyjPQ-gis_xD7XjriGSb0sKoXtiiBhdIMXxUwI"
      
  
  # REF ID for plots
    ref_id <- "d4781b35"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_tasks <- read_sheet(ss = task_path)

# MUNGE ============================================================================
  
  df_tasks %>% 
      select(-Notes) %>% 
      pivot_longer(cols = Angella:Malik,
                   names_to = "staff", 
                   values_to = "coverage") %>% 
      mutate(facet_order = fct_relevel(focus, c("Partner Monitoring & Support", "MER Reporting", 
                                                "Portfolio Management", "Strategic Planning & Stakeholder Coordination", 
                                                "Other")),
             staff_order = fct_relevel(staff, c("Angella", "Collins", "Andrew", "Victor", "Malik"))) %>% 
      ggplot(aes(y = task, x = staff_order)) +
      geom_tile(aes(fill = coverage), color = "white") +
          facet_grid(facet_order ~ ., scales = "free_y", space = "free", switch = "y") +
      scale_fill_manual(values = c("Primary" = scooter_med, "Secondary" = "#5bd5b8"), 
                        na.value = grey10k) +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      si_style_nolines(facet_space = 0.5) +
      theme(axis.text.y = element_text(size = 8)) +
      labs(x = NULL, y = NULL, 
           title = "SI Team Portfolio FY23",
           caption = "Source: USAID Malawi SI Team") 
      si_save("Graphics/MWI_portfolio_si_lanes.svg", height = 11, width = 8.5)
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

