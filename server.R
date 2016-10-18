
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
                    and lower(subbrands_list) = '", brand_incl, "'
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
                    where date between date'", date[1], "' and date'", date[2], "' and lower(subbrands_list) = '", brand, "'
                    )
                    select * from tmp where rn = 1
                    ")
  }
  
  data <- dbGetQuery(mydb, query)
  Encoding(data$subbrands_list) <- 'UTF-8'
  log('get')
  return(data)
  }
filter_data   <- function(data, top_net, top_creative, category, network_first, type, clean) {
  placement <- data %>%
    mutate(subbrands_list = substr(subbrands_list, 1, 40)) %>%
    mutate(banner_network = gsub('N/A', '_______', banner_network)) %>%
    mutate(banner_network = ifelse(nchar(banner_network) <= 8, banner_network, substr(banner_network, 1, 8))) %>%
    mutate(site = gsub('.ru|.com', '', site)) %>%
    group_by(subbrands_list) %>%
    mutate(strength_sub = n()) %>%
    ungroup() %>%
    mutate(rank_sub = dense_rank(desc(strength_sub))) %>%
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
    mutate(adId_list = ifelse(rank_ad <= top_creative, adId_list, 'other')) %>%
    filter(rank_sub == 1)
  
  if (tolower(category) != 'all') placement <- filter(placement, stri_detect_regex(site_category, gsub(', ', '|', tolower(category))))
  if (clean > 0) placement <- placement %>% group_by(subbrands_list, banner_network, site) %>% filter(length(unique(date)) > clean) %>% ungroup()
  if (type > 1) placement <- placement %>% filter(type == 'network')
  log(paste('filter', top_net, category))
  write.csv(placement, 'filter.txt', row.names = F)
  placement
}
plot_brand    <- function(placement) {
  
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
      
      placement_expand <- placement %>%
        mutate(site_f = as.character(site_net), date = as.character(date)) %>%
        mutate(type_fl = type == 'network') %>%
        filter(!is.na(site_f)) %>%
        mutate(shade = ifelse(dense_rank(site_f) %% 10 == 0, 1, 0)) %>%
        mutate(adId = ifelse(stri_detect_fixed(adId_list, ','), stri_extract_first_regex(adId_list, '[0-9]*'), adId_list)) %>%
        mutate(adId = as.integer(adId)) %>%
        filter(!is.na(adId)) %>%
        select(date, site_f, adId, adId_list)


      # write.csv(placement_expand, 'for_graph.txt', row.names = F)

      gg1 <- placement_expand %>% plot_ly(x =~ date, y =~ site_f, z =~ adId, text =~ adId_list) %>% layout(showlegend = F) %>% add_heatmap() %>% layout(showlegend = F)
      # gg2 <- ggplot(placement_expand, aes(x = date, y = site_f)) +
      #   coord_equal() +
      #   labs(x = NULL, y = NULL, title = paste0(subbrand, ', ', nrow(placement), " formatdays")) +
      #   # scale_x_date(date_breaks = param$plot_str$date_breaks, expand=c(0,0)) +
      #   theme_tufte() +
      #   theme(title = element_text(size = param$plot_num$text_size), plot.title = element_text(hjust = 0)) +
      #   theme(axis.ticks = element_blank()) +
      #   theme(panel.border = element_blank()) +
      #   geom_tile(colour = 'black', size = param$plot_num$tile_size, aes(fill = adId_list)) +
      #   theme(legend.position = "none") + scale_fill_discrete(na.value = "white")
      # subplot(gg1, ggplotly(gg2), nrows = 2, margin = 0.05)
      pb <- plotly_build(gg1)
      pb$x$layout$margin$b <- 100
      pb$x$layout$margin$l <- 150
      pb$x$data[[1]]$showscale <- FALSE
      pb
    }
    return(one_sub(lev_sub$subbrands_list[1]))
  }
  
  result <- plot_sub()
  log(paste('plot'))
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
  img <- image_read(param$default_img$path)

  for (i in 1:nrow(out)) { 
    read <- tryCatch({
      arr <- image_read(unserializeJSON(out$img[i]))
      horizontal_append <- image_info(arr[1])$width > image_info(arr[1])$height
      if (length(arr) == 1) {
        arr_r <- arr[1] 
      }
      if (length(arr) == 2) arr_r <- image_append(arr[1:2], stack = horizontal_append) 
      if (length(arr) > 2) arr_r <- image_append(arr[c(1, ceiling(length(arr) / 2), length(arr))], stack = horizontal_append)
      arr_r
    },
    error = function(cond) {
      message(paste("Cant read image", out$adId[i]))
      cond
    }
    )
    
    if (inherits(read, "error")) next
    info <- image_info(read)
    annotation <- paste(as.character(out$adId[i]), info$format, paste(info$width, info$height, sep = 'x'))
    img <- append(img, image_annotate(read, annotation, color = "white", size = 20, boxcolor = "black") %>% image_scale(paste0('x', param$format$height_1row))) 
  }
  if (length(img) > 1) img <- img[2:length(img)]
  return(img)
}
do_click      <- function(data) {
  d <- event_data("plotly_hover")
  if (is.null(d)) NULL 
  else {
    clicked <- data %>% 
      
      filter(as.character(date) == d$x & as.character(site_net) == d$y) %>%
      mutate(adId = ifelse(stri_detect_fixed(adId_list, ','), stri_extract_first_regex(adId_list, '[0-9]*'), adId_list))

    return(list(date = d$x, network_site = d$y, adId = unique(clicked$adId), subbrand = unique(clicked$subbrands_list)))
  }
}

pg <- dbDriver("PostgreSQL")

shinyServer(function(input, output) {
  v <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go, {v$doPlot <- input$go})
  
  dataInput <- reactive({ get_data(input$text, input$dates) })
  filteredInput <- reactive({ filter_data(dataInput(), input$top_net, input$top_creative, 
                                          input$category, input$network_first, input$type, input$clean) })
  creativeInput <- reactive({ get_image(filteredInput()) })
  creativeInputClick <- reactive({ do_click(filteredInput()) })
  
  output$map <- renderPlotly({
    if (v$doPlot == FALSE) return()
    isolate({ ggplotly(plot_brand(filteredInput())) %>% layout(dragmode = 'select') })
  })
  
  output$click <- renderPrint({ 
    d <- creativeInputClick() 
    if (is.null(d)) 'Press Enter and select point' else d
    })
  
  output$myImage <- renderImage({
    
    # A temp file to save the output.
    # This file will be removed later by renderImage
    d <- do_click(filteredInput())

    if (is.null(d)) return(list(src = tempfile(fileext='.png'), contentType = 'image/png', width = 1, height = 1, alt = ""))
    else {
      out <- creativeInput() %>% filter(as.character(adId) %in% d$adId) %>% filter(nchar(as.character(adId)) > 2)
      if (nrow(out) == 0) return(list(src = tempfile(fileext='.png'), contentType = 'image/png', width = 1, height = 1, alt = ""))
      img <- check_image(out) %>%
        image_convert("png", 8)
      
      filename <- tempfile(fileext='.png')
      image_write(img, path = filename, format = "png")
      
      list(src = filename,
           contentType = 'image/png',
           width = image_info(img)['width'],
           height = image_info(img)['height'],
           alt = "")
    }
  }, deleteFile = TRUE)

  
})


