

# Geofacet plot -----------------------------------------------------------


geofacet_state_plot <-
  function(
    data,
    measure_name, 
    cause_name = "Bladder cancer",
    metric_name = "Rate", 
    age_id = 27
    ) {
    
    file_name <- 
      paste0(c({{measure_name}}, {{cause_name}}, {{metric_name}}), collapse = "_") %>% 
      paste0("fig_A_", ., ".svg")
    
    file_path <- 
      c("output/figures/Geofacet plot/", file_name)
    
    if (file.exists(paste0(file_path, collapse = ""))) {return(NA)}
    
    library(tidyverse)
    library(geofacet)
    library(ggnewscale)
    
    if (!(27 %in% data$age_id)) {
      age_id <- 22
    }
    
    
    bg_data <-
      data %>%
      filter(
        location_id == 102 &
          age_id == {{age_id}}
      ) %>%
      mutate(
        stateabb = NULL,
        sex_name = NULL
      )
    
    if ({{cause_name}} %in% c("Bladder cancer", "Kidney cancer")) {
      bg_data <-
        bg_data %>% 
        filter(sex_id == 3)
    } else {
      bg_data <- 
        bg_data %>% 
        filter(sex_id == 1)
    }
    
    plotdata <- 
      data %>% 
      filter(!is.na(stateabb)) %>% 
      filter(age_id == {{age_id}})
    
    maxval <-
      data %>% 
      filter(
        age_id == {{age_id}}
      ) %>% 
      .$upper %>% 
      max
    
    minval <-
      data %>% 
      filter(
        age_id == {{age_id}}
      ) %>% 
      .$lower %>% 
      min()
    
    
    maxyear <- 
      plotdata %>% 
      .$year %>% 
      max
    
    minyear <- 
      plotdata %>% 
      .$year %>% 
      min
    
    
    abbplacey <- mean(c(maxval, minval))
    abbplacex <- mean(c(maxyear, minyear))
    
    labely <-
      plotdata %>%
      distinct(stateabb) %>%
      mutate(year = abbplacex, val = abbplacey)
    
    sex_colors <- 
      c(
        "Both" = "#FF9F1C",
        "Male" = "#6184D8",
        "Female" = "#09814A"
      )
    
    man_colors <- 
      c(
        "Both" = "#FF9F1C",
        "Male" = "#6184D8",
        "Female" = "#09814A",
        "USA" = "gray15"
      )
    
    
    if ((
      27 %in% data$age_id
    )) {
      
      yaxtitle <- 
        paste0(
          {{ measure_name }},
          " of ",
          {{ cause_name }},
          " (", 
          {{ metric_name }},
          ") | Age-standardized"
        )
      
    } else {
      
      yaxtitle <- 
        paste0(
          {{ measure_name }},
          " of ", 
          {{ cause_name }},
          " (",
          {{ metric_name }},
          ") | All ages"
        )
      
    }
    
    
    plot <-
      plotdata %>%
      ggplot(aes(x = year, y = val)) +
      
      geom_text(
        data = labely,
        aes(x = year, y = val, label = stateabb),
        size = 40, fontface = "bold", alpha = .25,
        colour = "#2D4739"
      )
    
    if ({{metric_name}} != "Number") {
      
      plot <- 
        plot +
        
        geom_ribbon(
          data = {{bg_data}},
          aes(ymin = lower, ymax = upper, fill = "USA"),
          alpha = .25, show.legend = TRUE
        ) +
        
        geom_line(
          data = {{bg_data}},
          aes(y = val, color = "USA"),
          alpha = .7, linewidth = .9, show.legend = TRUE
        )
      
    }
    
    plot <- 
      plot + 
      
      scale_fill_manual(
        name = "",
        values = c("USA" = "gray15")
      ) +
      
      scale_color_manual(
        name = "",
        values = c("USA" = "gray15")
      ) +
      
      # Reset legend scales for new legend box
      new_scale("fill") +
      new_scale("color") +
      
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex_name), alpha = .25) +
      geom_line(aes(color = sex_name), linewidth = 1.25) +
      
      xlab(label = "Time (Year)") +
      ylab(label = yaxtitle) +
      
      scale_color_manual(
        name = "Sex",
        values = {{sex_colors}}
      ) +
      
      scale_fill_manual(
        name = "Sex",
        values = {{sex_colors}}
      ) +
      
      facet_geo(~stateabb) +
      
      theme_minimal() +
      theme(
        axis.text = element_text(face = "bold", size = 18),
        panel.grid.minor = element_blank(), strip.text.x = element_blank(),
        strip.background = element_blank(), panel.spacing = unit(2, "lines"),
        axis.title = element_text(face = "bold", size = 30),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 30)), 
        legend.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        legend.position = "inside",
        legend.position.inside = c(.9, .2),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "white", colour = "white")
      )
    
    
    
    
    ggsave(
      plot = plot,
      filename = file_name,
      path = file_path[1],
      device = "svg",
      width = 90,
      height = 47,
      units = "cm",
      dpi = 600
    )
    
  }


# US perfoemance plot -----------------------------------------------------



us_performance_plt <- 
  function(data, cause_name = "Bladder cancer", metric_name = "Rate") {
    
    file_name <- 
      paste0(c({{cause_name}}, {{metric_name}}), collapse = "_") %>% 
      paste0("fig_B_", ., ".svg")
    
    file_path <- 
      c("output/figures/US performance plot/", file_name)
    
    if (file.exists(paste0(file_path, collapse = ""))) {return(NA)}
    
    library(ggplot2)
    library(dplyr)
    
    color_mapping <- c(
      "USA" = "#FF9F1C",
      "European Union" = "gray15",
      "High-income" = "gray15",
      "High SDI" = "gray15"
    )
    
    linetype_mapping <- c(
      "USA" = "solid",
      "European Union" = "solid",
      "High-income" = "dashed",
      "High SDI" = "dotted"
    )
    
    plotdata <- data %>%
      mutate(
        country_group = case_when(
          location_id == 102 ~ "USA",
          location_name == "European Union" ~ "European Union",
          location_name == "High-income" ~ "High-income",
          location_name == "High SDI" ~ "High SDI",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(country_group))
    
    ytitle <- paste0(cause_name, " (", metric_name, ") | ", attr(data, which = "ageattr"))
    
    usa_data <- filter(plotdata, country_group == "USA")
    others_data <- filter(plotdata, country_group != "USA")
    
    plot <- 
      
      ggplot() +
      # All ribbons
      geom_ribbon(
        data = plotdata,
        aes(
          x = year, 
          ymin = lower,
          ymax = upper,
          fill = country_group,
          group = location_name
        ),
        alpha = 0.1, color = NA
      ) +
      
      # Lines for others
      geom_line(
        data = others_data,
        aes(
          x = year, 
          y = val, 
          color = country_group,
          linetype = country_group,
          group = location_name
        ),
        linewidth = 0.7,
        alpha = 0.2
      ) +
      
      # Line for USA
      geom_line(
        data = usa_data,
        aes(
          x = year,
          y = val, 
          color = country_group, 
          linetype = country_group,
          group = location_name
        ),
        linewidth = 1.25,
        alpha = 1
      ) +
      
      scale_color_manual(name = NULL, values = color_mapping) +
      scale_fill_manual(name = NULL, values = color_mapping) +
      scale_linetype_manual(name = NULL, values = linetype_mapping) +
      
      guides(
        fill = guide_legend(override.aes = list(alpha = 0)),
        color = guide_legend(override.aes = list(alpha = .4, linewidth = 1.2)),
        linetype = guide_legend()
      ) +
      
      facet_wrap(~measure_name, scales = "free") +
      theme_minimal() +
      labs(x = "Year", y = ytitle) +
      theme(
        legend.position = "top",
        legend.key.width = unit(1.5, 'cm'), 
        legend.key.height = unit(.5, "cm"), 
        legend.key.spacing.x = unit(1, "cm"),
        legend.box.spacing = unit(.5, "cm"),
        axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 15),
        panel.grid = element_blank(), 
        # panel.spacing = unit(.5, 'cm'),
        axis.title.x = element_blank(), 
        # axis.text.y = element_text(margin = margin(l = 5)), 
        plot.background = element_rect(fill = "white", color = "white")
      )
    
    
    
    ggsave(
      plot = plot,
      filename = file_name, 
      path = file_path[1],
      device = "svg", 
      width = 25,
      height = 16, 
      units = "cm", 
      dpi = 600
    )
    
    
  }



# Bump plot ---------------------------------------------------------------


bumpplot <- 
  function(data, cause_name = "Bladder cancer", metric_name = "Rate") {
    
    file_name <- 
      paste0(c({{cause_name}}, {{metric_name}}), collapse = "_") %>% 
      paste0("fig_F_", ., ".svg")
    
    file_path <- 
      c("output/figures/Bump plot/", file_name)
    
    if (file.exists(paste0(file_path, collapse = ""))) {return(NA)}
    
    library(ggbump)
    library(ggtext)
    library(paletteer)
    
    highlight_locs <-
      data %>% 
      filter(color == "yes") %>% 
      group_by(measure_name) %>% 
      distinct(location_id) %>% 
      split(.$measure_name) %>% 
      map(\(x) x$location_id) %>% 
      enframe(
        name = "measure_name", 
        value = "location_id"
      ) %>% 
      unnest(location_id) %>% 
      mutate(color = "yes")
    
    plotdata <- 
      data %>% 
      mutate(color = NULL) %>% 
      left_join(highlight_locs, join_by(measure_name, location_id)) %>% 
      mutate(
        color = replace_na(color, "no")
      ) %>% 
      split(.$color)
    
    txtdata <- 
      bind_rows(plotdata) %>% 
      filter(year %in% c(min(year), max(year))) %>% 
      mutate(
        first_or_last = if_else(year == max(year), "last", "first")
      ) %>% 
      ungroup() %>% 
      split(.$first_or_last)
    
    pointdata <- 
      plotdata %>% 
      bind_rows() %>% 
      group_by(measure_name, location_name) %>%
      filter(year %in% c(min(year), max(year))) %>% 
      ungroup() %>% 
      split(.$color)
    
    
    ytitle <- paste0(
      cause_name,
      " (", metric_name, ") | ",
      attr(data, which = "ageattr")
    )
    
    txtpadding <- 7
    ordpadding <- 4
    txtsize <- 6
    ordersize <- 6
    linewidth <- 1.5
    pointsize <- 5.5
    unhighlightcolor <- "gray80"
    
    
    plot <- 
      
      ggplot(
        mapping = aes(x = year, y = rank, group = location_name)
      ) + 
      
      
      geom_bump(
        data = plotdata$no, 
        color = unhighlightcolor,
        linewidth = linewidth,
        alpha = .3
      ) +
      geom_bump(
        data = plotdata$yes, 
        mapping = aes(color = stateabb), 
        linewidth = linewidth + 1
      ) +
      geom_bump(
        data = plotdata$yes %>% filter(stateabb == "USA"), 
        linewidth = linewidth + 1,
        color = "black"
      ) +
      
      geom_text(
        data = txtdata$first,
        aes(x = year - txtpadding, label = stateabb), 
        size = txtsize, 
        hjust = 1
      ) +
      geom_text(
        data = txtdata$last,
        aes(x = year + txtpadding, label = stateabb), 
        size = txtsize,
        hjust = 0
      ) +
      geom_text(
        data = txtdata$first %>% filter(stateabb == "USA"),
        aes(x = year - txtpadding, label = stateabb), 
        fontface = "bold",
        size = txtsize, 
        hjust = 1
      ) +
      geom_text(
        data = txtdata$last %>% filter(stateabb == "USA"),
        aes(x = year + txtpadding, label = stateabb), 
        fontface = "bold",
        size = txtsize,
        hjust = 0
      ) +
      
      geom_text(
        data = txtdata$last,
        aes(x = year + ordpadding, label = rank), 
        size = ordersize,
        fontface = "bold"
      ) +
      geom_text(
        data = txtdata$first,
        aes(x = year - ordpadding, label = rank), 
        size = ordersize,
        fontface = "bold"
      ) +
      
      geom_point(
        data = pointdata$no,
        size = pointsize - 1,
        color = unhighlightcolor,
        alpha = .8
      ) + 
      geom_point(
        data = pointdata$yes,
        aes(color = stateabb),
        size = pointsize
      ) +
      geom_point(
        data = pointdata$yes %>% filter(stateabb == "USA"),
        color = "black",
        size = pointsize
      ) +
      
      
      theme_minimal() + 
      theme(
        panel.grid = element_blank(),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 25),
        axis.title.x = element_blank(),
        axis.text = element_text(face = "bold", size = 20),
        # axis.title.y = element_blank(),
        axis.title.y = element_text(size = 30, face = "bold"),
        axis.text.y = element_blank(), 
        # panel.spacing.y = unit(1, "cm"), 
        axis.text.x = element_text(
          margin = margin(-30, 0, 5, 0),
          angle = 90,
          vjust = .5,
          # hjust = -100
        ),
        plot.background = element_rect(fill = "white", colour = "white"), 
        plot.margin = unit(c(0,0,0,0), "cm")
      ) +
      ylab(ytitle) +
      
      scale_x_continuous(limits = c(1965,2045), breaks = c(1990, 2000, 2010, 2021)) +
      # scale_color_paletteer_d("palettetown::ariados") +
      
      facet_wrap(~measure_name, nrow = 1, scales = "free") + 
      scale_y_reverse(
        breaks = 1:55, 
        # sec.axis = dup_axis()
      ) 
    
  
    
    
    ggsave(
      plot = plot,
      filename = file_name, 
      path = file_path[1],
      device = "svg", 
      width = 90,
      height = 40, 
      units = "cm", 
      dpi = 600
    )
    
    
  }



# Pyramid plot ------------------------------------------------------------


pyramid_plot <- 
  function(
    data,
    year = 2021,
    cause_name = "Bladder cancer",
    measure_name,
    metric_name = "Rate"
  ) {
    
    maxbarheight_fm <- 
      data %>% 
      filter(year %in% {{year}}) %>% 
      group_by(sex_id, sex_name, age_id, year) %>% 
      summarise(sumval = sum(val)) %>% 
      group_by(sex_id, sex_name) %>% 
      summarise(maxval = max(sumval)) %>% 
      ungroup %>% 
      mutate(
        hjust = if_else(sex_id == 1, 1, 0),
        position = if_else(sex_id == 1, max(maxval), -max(maxval))
      ) %>% 
      suppressMessages()
    
    limitmax <- max(maxbarheight_fm$maxval)
    limitmin <- -max(maxbarheight_fm$maxval)
    if (cause_name %in% c("Testicular cancer", "Prostate cancer")) {
      limitmin <- 0
      maxbarheight_fm <- 
        maxbarheight_fm %>% 
        bind_rows(
          tibble(
            sex_id = 2,
            sex_name = "Female",
            maxval = NA,
            hjust = 0,
            position = NA
          )
        )
    }
    
    
    agelabels <- 
      data %>% 
      distinct(age_id, age_name)
    
    anotpoisition <- 
      agelabels %>% 
      filter(age_id == min(age_id)) %>% 
      pull
    
    
    dataplot <- 
      data %>% 
      filter(year %in% {{year}}) %>% 
      mutate(
        val = if_else(sex_id == 2, -val, val)
      )
    
    colors <- 
      c(
        "Male" = "#2A9D8F",
        "Female" = "#264653"
      )
    
    plot <-
      ggplot(
        data = dataplot,
        aes(x = val, y = age_name, fill = sex_name)
      ) + 
      
      geom_col(alpha = .8) + 
      
      geom_label(
        data = maxbarheight_fm, 
        mapping = aes(
          x = position,
          hjust = hjust,
          label = sex_name, 
          color = sex_name
        ),
        fill = "white",
        y = anotpoisition,
        size = 8, 
        linewidth = 1,
        fontface = "bold"
      ) +
      
      scale_x_continuous(
        limits = 1.15 * c(limitmin, limitmax),
        labels = \(x) scales::label_number_auto()(abs(x)),
        breaks = \(x) scales::breaks_extended()(x)
      ) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      
      theme_minimal() + 
      
      xlab(paste0(measure_name, " (", {{metric_name}}, ")")) + 
      
      theme(
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title.x = element_text(face = "bold", size = 17.5),
        axis.title.y = element_blank(),
        axis.text = element_text(face = "bold", size = 15), 
        strip.text = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = "bold"),
        plot.background = element_rect(fill = "white", colour = "white")
      ) 
    
    return(plot)
  }



# APC plot ----------------------------------------------------------------


apc_plot1 <- 
  function(data, measure_name, location_name) {
    
    file_name <- 
      paste0(
        c(
          "fig_G",
          location_name, 
          measure_name, 
          "Bladder Cancer",
          "Rate"
        ),
        collapse = "_"
      ) %>%
      paste0(".svg")
    
    file_path <- 
      c("output/figures/APC plot 1/", file_name)
    
    # if (file.exists(paste0(file_path, collapse = ""))) {return(NA)}
    if (!str_detect(location_name, "United")) {return(NA)}
    
    colpal <- 
      c(
        "1990" = "#F4E2B9",
        "2000" = "#FACD38",
        "2010" = "#8E5F01",
        "2021" = "black"
      )
    
    
    plot <-
      data %>% 
      ggplot(aes(
        x = age_name,
        y = val, 
        color = factor(year),
        group = factor(year)
      )) + 
      geom_line(linewidth = 2, alpha = .8) + 
      facet_wrap(~ sex_name) +
      theme_minimal(base_size = 20) + 
      ylab(paste(measure_name, "(Rate)")) +
      scale_color_manual(values = colpal, name = "Year") + 
      theme(
        axis.text.x = element_text(
          angle = 90,
          hjust = 1, 
          size = 20
        ),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 30), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30, face = 'bold'), 
        legend.position = "top",
        legend.key.size = unit(2, "cm"), 
        legend.margin = margin(0, 0, -1, 0, "cm"),
        # legend.key.spacing.y = unit(0, "cm"), 
        # legend.position.inside = c(.5, .9),
        axis.text.y = element_text(size = 20), 
        panel.spacing.x = unit(0, "cm"), 
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "gray98", colour = "white"), 
        plot.margin = margin(0, 0, 0, 0, "cm"),
        legend.text = element_text(size = 25)
      ) + 
      guides(
        color = guide_legend(
          override.aes = list(linewidth = 4)
      ))
    
    
    
    ggsave(
      plot = plot,
      filename = file_name, 
      device = "svg",
      path = file_path[1],
      width = 50,
      height = 25, 
      units = "cm", 
      dpi = 600
    )
    
    if (str_detect(location_name, "United")) {
      patch_plots[[measure_name]] <<- plot
    }
    
    
  }


layout_maker <- 
  function(n) {
    
    layout <- vector("list", 3)
    
    for (i in 1:6) {
      
      item <- ceiling(i / 2)
      subitem <- (i %% 2) + 1
      
      layout[[item]][subitem] <-
        rep(LETTERS[i], n) %>% 
        paste0(collapse = "")
      
    }
    
    
    layout <- 
      layout %>% 
      map(\(x) paste0(x, collapse = "#")) %>% 
      paste0(collapse = "\n")
    
    return(layout)
    
  }


apc_plot2 <- 
  function(data, measure_name, location_name) {
    
    file_name <- 
      paste0(
        c(
          "fig_G",
          location_name, 
          measure_name, 
          "Bladder Cancer",
          "Rate"
        ),
        collapse = "_"
      ) %>%
      paste0(".svg")
    
    file_path <- 
      c("output/figures/APC plot 2/", file_name)
    
    if (file.exists(paste0(file_path, collapse = ""))) {return(NA)}
    
    data <- 
      data %>% 
      split(data$year == 2021)
    
    
    
    colpal <- 
      c(
        "1990" = "#F4E2B9",
        "2000" = "#FACD38",
        "2010" = "#8E5F01",
        "2021" = "black"
      )
    
    
    plot <-
      data$`FALSE` %>% 
      ggplot(aes(
        x = age_name,
        y = val, 
        # color = (year),
        group = factor(year)
      )) + 
      geom_line(
        aes(
          color = (year),
          alpha = year
        ), 
        # alpha = .1,
        linewidth = 1, 
        # color = "#F4E2B9"
      ) + 
      geom_line(
        data = data$`TRUE`, 
        color = "black",
        linewidth = 1.5, 
        lineend = "round"
      ) +
      facet_wrap(~ sex_name) +
      theme_minimal(base_size = 20) + 
      ylab("Rate") +
      scale_color_gradient(low = "#FFFACD", high = "#FF8C00", name = "Year")  +
      # scale_color_manual(values = colpal, name = "Year") + 
      scale_alpha_continuous(range = c(0.1, 0.3)) +
      theme(
        axis.text.x = element_text(
          angle = 90,
          hjust = 1, 
          size = 20
        ),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 30), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30, face = 'bold'), 
        legend.position = "none",
        legend.key.size = unit(2, "cm"), 
        legend.margin = margin(0, 0, -1, 0, "cm"),
        # legend.key.spacing.y = unit(0, "cm"), 
        # legend.position.inside = c(.5, .9),
        axis.text.y = element_text(size = 20), 
        panel.spacing.x = unit(0, "cm"), 
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"), 
        plot.margin = margin(0, 0, 0, 0, "cm")
      )
    
    
    
    ggsave(
      plot = plot,
      filename = file_name, 
      device = "svg",
      path = file_path[1],
      width = 50,
      height = 25, 
      units = "cm", 
      dpi = 600
    )
    
  }



# Table -------------------------------------------------------------------



ggmaker <- 
  function(diffval, diffupper, difflower, upperlim, lowerlim, color) {
    
    
    colorpal <- 
      c(
        neutral = "#FFD166",
        negative = "#91F5AD",
        positive = "#EF476F"
      )
    
    data <- 
      tibble(
        diffval = diffval,
        diffupper = diffupper,
        difflower = difflower,
        upperlim = upperlim,
        lowerlim = lowerlim,
        color = color
      ) %>% 
      mutate(
        upperlim = if_else(
          (sign(upperlim) == sign(lowerlim)) & sign(upperlim) == -1,
          .1,
          upperlim
        ),
        lowerlim = if_else(
          (sign(upperlim) == sign(lowerlim)) & sign(upperlim) == +1,
          -.1,
          lowerlim
        )
      )
    
    labels <- 
      paste0(
        diffval %>% scales::percent(accuracy = 1),
        " [", 
        difflower %>% scales::percent(accuracy = 1, suffix = ""), 
        ", ",
        diffupper %>% scales::percent(accuracy = 1, suffix = ""), 
        "]"
      )
    
    plot <-
      data %>% 
      ggplot() + 
      
      geom_linerange(
        x = 0,
        ymin = -.25, ymax = .25,
        color = "gray70", 
        # linetype = 2, 
        linewidth = .1
      ) + 
      
      geom_linerange(
        aes(
          xmin = 0,
          xmax = diffval,
          y = 0, 
          color = color
        ), 
        linewidth = 3
      ) + 
      
      geom_errorbarh(
        aes(
          xmin = difflower,
          xmax = diffupper,
          y = 0
        ),
        color = "gray30",
        alpha = 1,
        linewidth = .5, 
        height = 0
      ) +
      
      geom_text(
        x = mean(c(data$upperlim, data$lowerlim)),
        label = labels,
        hjust = .5,
        y = .5, 
        # fontface = "bold", 
        size = 2
      ) + 
      
      
      scale_color_manual(values = colorpal) +
      scale_x_continuous(
        limits = c(data$lowerlim, data$upperlim),
        expand = c(0, 0)
      ) + 
      scale_y_continuous(
        limits = c(-.3, .75),
        expand = c(0, 0)
      ) +
      
      theme_void() +
      # theme_minimal() +
      
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "white", colour = "white")
        # panel.background = element_rect(fill = "lightblue")
      )
    
    return(plot)
    
  }



fpmaker <- 
  function(data, cause) {
    
    file_name <- 
      paste0(
        "output/tables/Percent change since 1980 in ",
        cause,
        " measures.docx"
      )
    
    if (file.exists(file_name)) {return(NA)}
    
    library(officer)
    library(flextable)
    
    all_colnames <- 
      data %>% 
      colnames()
    
    key_cols <- 
      tibble(cols = all_colnames[-1]) %>%
      separate_wider_delim(
        cols = cols,
        delim = "_", 
        names = c("measure", "sex"),
        cols_remove = FALSE
      ) %>% 
      group_by(measure) %>%
      mutate(n = row_number()) %>%
      ungroup %>% 
      mutate(
        emptycol = if_else(
          n == 2,
          paste0("empty", cumsum(n == 2)), 
          NA_character_
        )
      ) %>%
      pivot_longer(
        c(cols, emptycol),
        values_to = "emptycol2"
      ) %>%
      filter(!is.na(emptycol2)) %>%
      pull(emptycol2) %>% 
      .[1:length(.)-1] %>% 
      c("location", "empty0", .)
    
    
    empty_cols <- 
      key_cols %>% 
      .[str_detect(., "empty")]
    
    nonempty_cols <- 
      key_cols %>% 
      .[!str_detect(., "empty")]
    
    init_flextable_defaults()
    set_flextable_defaults(
      font.size = 5,
      font.family = "Calibri",
      theme_fun = "theme_box",
      big.mark = ",",
      font.color = "black",
      border.color = "gray80",
      padding.top = 0, padding.bottom = 0,
      padding.left = 0, padding.right = 0,
      border.width = .5,
      digits = 2,
      decimal.mark = ".",
      text.align = "center",
      na_str = "", 
      table.layout = "fixed",
      table_align = "table_align", 
      fonts_ignore = TRUE, 
      background.color = "white"
    )
    
    empty_borders <-
      fp_border(color = NA, style = "none", width = 0)
    separtor_border <- 
      fp_border(color = "black", style = "solid", width = 1)
    
    ftable <-
      data %>% 
      mutate(location = case_when(
        location == "United States of America" ~ "USA",
        location == "European Union" ~ "EU",
        .default = location
      )) %>% 
      flextable(col_keys = key_cols) %>% 
      separate_header() %>%
      mk_par(
        j = ~. - location, use_dot = TRUE,
        value = as_paragraph(
          gg_chunk(
            value = .,
            width = 1.46,
            height = 1,
            unit = "cm",
            res = 600
          )
        )
      ) %>%
      # align(part = "all", align = "left") %>% 
      align(part = "all", align = "center") %>%
      valign(part = "all", valign = "center") %>% 
      # valign(j = 1, valign = "center", part = "body") %>%
      fontsize(part = "header", size = 7) %>% 
      fontsize(part = "body", size = 7) %>% 
      border(j = empty_cols, border = empty_borders, part = "all") %>% 
      padding(j = 1, part = "body", padding.left = 1) %>% 
      void(j = empty_cols, part = "all") %>% 
      height(height = 1.5, unit = "cm", part = "body") %>%
      hline(
        part = "body", 
        i = ~ location == "USA", 
        j = nonempty_cols,
        border = separtor_border
      ) %>% 
      hline_bottom(
        part = "body",
        border = separtor_border,
        j = nonempty_cols
      ) %>% 
      # add_footer(values = cause) %>% 
      # add_footer_lines(values = cause) %>% 
      width(width = 1.49, unit = "cm") %>% 
      width(j = empty_cols, width = .15, unit = "cm") %>% 
      # hline_bottom(part = "footer", border = separtor_border) %>% 
      labelizor(
        j = 1, 
        labels = stringr::str_to_title,
        part = "header"
      )
    
    pageprop <- 
      prop_section(
        page_size = page_size(
          width = 21.59, 
          height = 27.94, 
          unit = "cm", 
          orient = "portrait"
        ), 
        page_margins = page_mar(
          right = .1, 
          left = .1
        )
      )
    
    
    save_as_docx(
      ftable, 
      path = file_name,
      pr_section = pageprop
    )
    
    save_as_image(
      ftable, 
      path = str_replace(file_name, "docx", "svg")
      )
    
    # save_as_html(
    #   ftable, 
    #   path = paste0("output/tables/", cause, ".html"),
    #   pr_section = pageprop
    #   )
    
  }
