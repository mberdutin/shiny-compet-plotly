
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RPostgreSQL)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(lubridate)
library(stringi)
library(magick)
library(jsonlite)
library(ini)



log_file <- file("calc.log", open = "at")

log <- function(text, conn = log_file) {
  if (stri_detect_fixed(text, 'start')) cat(paste0('\n', Sys.time(), " ",text), file = log_file, append = T, sep = "\n") 
  else cat(paste(Sys.time(), text), file = log_file, append = T, sep = "\n")
}

param <- read.ini('setup.ini')
param$plot_num    <- lapply(param$plot_num, as.numeric)
param$format_a <- lapply(param$format, function(x) {as.numeric(strsplit(x, '-')[[1]][1])})
param$format_b <- lapply(param$format, function(x) {as.numeric(strsplit(x, '-')[[1]][2])})


get_data      <- function(brand, date) {
  log(paste('start', brand, date[1], date[2]))
  mydb <- dbConnect(pg, dbname="max", user="max", password="docker", host="10.12.0.104", port=5432)
  on.exit({ dbDisconnect(mydb) })
  brand <- tolower(brand)
  if (stri_detect_regex(brand, ' -')) {
    brand_incl <- substr(brand, 1, stri_locate_first_fixed(brand, ' -') - 1)
    brand_excl <- substr(brand, stri_locate_first_fixed(brand, ' -') + 2, nchar(brand))
    query <- paste0("
                    with tmp as (
                    select subbrands_list, banner_network, site, site_category, type, date, ", '"adId_list"', ", ad_format_adj,
                    row_number() over (partition by subbrands_list, banner_network, site, site_category, type, date order by ad_format_adj) rn,
                    count(ad_format_adj) over (partition by subbrands_list, banner_network, site, site_category, type, date) n_formats
                    from bannerdays
                    where date between date'", date[1], "' and date'", date[2], "'", "
                    and lower(subbrands_list) not similar to '%(", paste(strsplit(brand_excl, ' -')[[1]], collapse = '|'), ")%' 
                    and lower(subbrands_list) like '%", brand_incl, "%'
                    )
                    select * from tmp where rn = 1
                    ")
  } else {
    query <- paste0("
                    with tmp as (
                    select subbrands_list, banner_network, site, site_category, type, date, ", '"adId_list"', ", ad_format_adj,
                    row_number() over (partition by subbrands_list, banner_network, site, site_category, type, date order by ad_format_adj) rn,
                    count(ad_format_adj) over (partition by subbrands_list, banner_network, site, site_category, type, date) n_formats
                    from bannerdays
                    where date between date'", date[1], "' and date'", date[2], "' and lower(subbrands_list) like '%", brand, "%'
                    )
                    select * from tmp where rn = 1
                    ")
  }
  
  data <- dbGetQuery(mydb, query)
  Encoding(data$subbrands_list) <- 'UTF-8'
  log('get')
  return(data)
  }
filter_data   <- function(data, top_net, top_sub, top_creative, category, clean, network_first, type) {
  placement <- data %>%
    mutate(subbrands_list = substr(subbrands_list, 1, 40)) %>%
    mutate(banner_network = gsub('N/A', '_______', banner_network)) %>%
    mutate(banner_network = ifelse(nchar(banner_network) <= 8, banner_network, substr(banner_network, 1, 8))) %>%
    mutate(site = gsub('.ru|.com', '', site)) %>%
    group_by(subbrands_list) %>%
    mutate(strength_sub = n()) %>%
    ungroup() %>%
    mutate(rank_sub = dense_rank(desc(strength_sub))) %>%
    filter(rank_sub <= top_sub) %>%
    group_by(banner_network) %>%
    mutate(strength_net = n()) %>%
    group_by(adId_list) %>%
    mutate(strength_ad = n()) %>%
    rowwise() %>% 
    mutate(site_net = ifelse(network_first, paste(banner_network, site, sep = ', '), paste(site, banner_network, sep = ', '))) %>%
    mutate(sub_net = ifelse(network_first, paste(banner_network, subbrands_list, sep = ', '), paste(subbrands_list, banner_network, sep = ', '))) %>%
    mutate(format_site = paste(ad_format_adj, site, sep = ', ')) %>%
    ungroup() %>%
    mutate(week = week(date)) %>%
    mutate(rank_net = dense_rank(desc(strength_net))) %>%
    filter(rank_net <= top_net) %>%
    mutate(rank_ad = dense_rank(desc(strength_ad))) %>%
    mutate(adId_list = ifelse(rank_ad <= top_creative, adId_list, 'other'))
  
  if (tolower(category) != 'all') placement <- filter(placement, stri_detect_regex(site_category, gsub(', ', '|', tolower(category))))
  if (clean > 0) placement <- placement %>% group_by(subbrands_list, banner_network, site) %>% filter(length(unique(date)) > clean) %>% ungroup()
  if (type > 1) placement <- placement %>% filter(type == 'network')
  log(paste('filter', top_net, top_sub, category, clean))
  placement
}
plot_brand    <- function(placement, plot_type, fill_radio) {
  
  lev_banner <- placement %>% distinct(banner_network, rank_net) %>% arrange(rank_net)
  lev_site_net   <- placement %>% distinct(site_net) %>% arrange(desc(site_net))
  lev_sub_net   <- placement %>% distinct(sub_net) %>% arrange(desc(sub_net))
  lev_category   <- placement %>% group_by(site_category) %>% mutate(m = mean(n_formats)) %>% distinct(site, m) %>% arrange(m, site_category)
  lev_sub <- placement %>% distinct(subbrands_list, rank_sub) %>% arrange(rank_sub)
  lev_format_site   <- placement %>% distinct(format_site) %>% arrange(desc(format_site))
  date_range <- seq(min(placement$date), max(placement$date), 'days')
  
  plot_sub <- function() {
    one_sub <- function(subbrand) {
      placement <- placement %>% filter(subbrands_list == subbrand)
      exp_grid <- expand.grid(site_net = unique(placement$site_net), 
                              date = date_range, N = 0, stringsAsFactors = F) 
      
      placement_expand <- placement %>%
        full_join(exp_grid) %>%
        mutate(n_formats = ifelse(is.na(n_formats), N, n_formats)) %>%
        mutate(site_f = factor(site_net, levels = lev_site_net$site_net)) %>%
        mutate(type_fl = type == 'network') %>%
        filter(!is.na(site_f)) %>%
        mutate(shade = ifelse(dense_rank(site_f) %% 10 == 0, 1, 0)) 
      
      gg <- ggplot(placement_expand, aes(x = date, y = site_f)) +
        coord_equal() +
        labs(x = NULL, y = NULL, title = paste0(subbrand, ', ', nrow(placement), " formatdays")) +
        geom_point(aes(size = ifelse(type_fl, 'network', 'other'))) +
        scale_size_manual(values=c(network = param$plot_num$dot_size, other = NA), guide="none") +
        scale_x_date(date_breaks = param$plot_str$date_breaks, expand=c(0,0)) +
        theme_tufte() +
        theme(title = element_text(size = param$plot_num$text_size), plot.title = element_text(hjust = 0)) +
        theme(axis.ticks = element_blank()) +
        theme(panel.border = element_blank())

      gg <- gg + geom_tile(colour = 'black', size = param$plot_num$tile_size, aes(fill = adId_list)) + 
        theme(legend.position = "none") +
        scale_fill_discrete(na.value = "white") 
      gg
      
    }
    return(one_sub(lev_sub$subbrands_list[1]))
  }
  
  result <- plot_sub()
  log(paste('plot', plot_type))
  return(result)
}

get_image     <- function(filtered) {
  mydb <- dbConnect(pg, dbname="max", user="max", password="docker", host="10.12.0.104", port=5432)
  on.exit({ dbDisconnect(mydb) })
  
  adIds <- filtered %>% distinct(adId_list) %>% filter(adId_list != 'other')
  out <- dbGetQuery(mydb, paste('select "adId", img from raw_creative where "adId" in (', paste(adIds$adId_list, collapse = ', '), ')'))
  log(paste(adIds$adId_list, collapse = ', '))
  return(out)
}
check_image   <- function(out) {
  img <- image_read('http://jeroenooms.github.io/images/tiger.svg')
  for (i in 1:nrow(out)) { 
    read <- tryCatch({
      image_read(unserializeJSON(out$img[i]))[1]
    },
    error = function(cond) {
      message(paste("Cant read image", out$adId[i]))
      cond
    }
    )
    
    if (inherits(read, "error")) next
    info <- image_info(read)
    annotation <- paste(as.character(out$adId[i]), info$format, paste(info$width, info$height, sep = 'x'))
    if (info$width >= 600 && info$height > 100) read <- image_scale(read, 'x100')
    if (info$width >= 600 && info$height < 100) read <- image_scale(read, '600x')
    if (abs(info$width - info$height) < 100) read <- image_scale(read, '300x')
    img <- append(img, image_annotate(read, annotation, color = "white", size = 20, boxcolor = "black"))
  }
  if (length(img) > 1) img <- img[2:length(img)]
  return(img)
}


pg <- dbDriver("PostgreSQL")

shinyServer(function(input, output) {
  v <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go, {v$doPlot <- input$go})
  
  dataInput <- reactive({ get_data(input$text, input$dates) })
  filteredInput <- reactive({ filter_data(dataInput(), input$top_net, input$top_sub, input$top_creative, 
                                          input$category, input$clean, input$network_first, input$type) })
  creativeInput <- reactive({ get_image(filteredInput()) })
  
  output$map <- renderPlotly({
    if (v$doPlot == FALSE) return()
    isolate({ ggplotly(plot_brand(filteredInput(), plot_type = input$radio, input$fill)) })
  })
  
})

