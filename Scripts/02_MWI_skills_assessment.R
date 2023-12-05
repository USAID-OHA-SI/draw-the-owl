# PROJECT: MWI Skills Analysis
# PURPOSE: Munge and Analysis of skills data
# AUTHOR:  Tim Esssam | SI
# REF ID:  57cb3566
# LICENSE: MIT
# DATE:   2023-12-05
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)
    library(googlesheets4)
    
  # SI specific paths/functions  
    load_secrets()
    gd_id <- "1d4uwOGyjPQ-gis_xD7XjriGSb0sKoXtiiBhdIMXxUwI"
  
  # REF ID for plots
    ref_id <- "57cb3566"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_skills <- read_sheet(ss = gd_id, sheet = "SI Skills Matrix") %>% 
      janitor::clean_names()

# MUNGE ============================================================================
  
  # What is the regular expression pattern?
  # Skill_1; Interest_1, Time_1
   names <-  df_skills %>% select(-c(1:3)) %>%  
      names()
    
  time_order <- c("never", "seldom", "some", "most")
    
    
    # REGEX
    # (\\w+_\\w+) - Matches the first two terms separated by an underscore and captures them as a single entity.
    # ([^_]+) - Matches one or more characters that are not underscores
    # _(\\d+)$ - Matches the underscore followed by one or more digits at the end of the column names
    
    df_skills_long <- 
      df_skills %>% 
      select(-x19) %>% 
      pivot_longer(cols = -c(1:3),
                   names_pattern = "(\\w+_\\w+)_([^_]+)_(\\d+)$",
                   names_to = c("position", "category", "pd_no")) %>% 
      unite("position", c(position, pd_no), sep = "_") %>% 
      pivot_wider(names_from = "category", values_from = value) %>% 
      mutate(time = fct_relevel(time, time_order))
    
    df_skills_long %>% 
      count(topic, technical_skill, interest, skill) %>% 
      filter(!is.na(skill)) %>% 
      complete(technical_skill, skill, interest) %>% 
      group_by(technical_skill) %>% 
      fill(topic, .direction = "updown") %>% 
      ggplot(aes(y = technical_skill, x = skill, fill = factor(n))) +
      geom_tile(color = "white") +
      scale_fill_si(palette = "genoas", discrete = T, na.value = grey10k) +
      facet_grid(topic ~ interest, scales = "free_y", space = "free") +
      si_style(facet_space = 0.25)
    

    

    
    
    
  
    
    
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

