# European Diatom Inventory for Palaeoecology and Ecology (EDIPE)
# Shiny app to query the inventory via filtering and visualizing where the diatom collection units are and their temporal age span, as well as associated metadata
# PalaeOpen EU COST Action (https://www.cost.eu/actions/CA23116/)
# Xavier Benito (xavier.benito@irta.cat)
# October 2026

# Load libraries needed
library(openxlsx)
library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
library(egg)  
library(shinyWidgets) 

# Read the EDIPE table
inventory <- read.csv("EDIPE.csv") %>%
  dplyr::filter(is.na(verified_duplicate) | verified_duplicate != 1) #filter out verified collection units
  
# function to prepare the time bins
clean_age <- function(age_vec) {
  sapply(as.character(age_vec), function(val) {
    if (is.na(val)) return(NA_real_)
    val_clean <- tolower(trimws(val))
    if (val_clean %in% c("present", "recent", "modern", "0")) return(0)
    if (val_clean %in% c("pre-industrial", "preindustrial")) return(150)
    num <- str_extract(val_clean, "^-?\\d+\\.?\\d*")
    if (!is.na(num)) as.numeric(num) else NA_real_
  }, USE.NAMES = FALSE)
}

# function to pre-process the inventory dataframe
process_data <- function(input_df = NULL) {
  if (is.null(input_df)) {
    stop("Please pass your data frame to process_data(). Example: process_data(df)")
  }
  # Standardize column names
  names(input_df) <- gsub(" ", "_", names(input_df))
  
  # Identify dating methods column
  dm_col <- intersect(c("Dating_methods", "dating_method"), names(input_df))
  input_df$dating_method <- if (length(dm_col) > 0) as.character(input_df[[dm_col[1]]]) else "Unspecified"
  
  # Identify site description column
  sd_col <- intersect(c("Site_description", "site_description"), names(input_df))
  input_df$site_desc <- if (length(sd_col) > 0) as.character(input_df[[sd_col[1]]]) else "Unspecified"
  
  # Identify country & site name columns
  c_col <- intersect(c("Country", "country"), names(input_df))
  input_df$country <- if (length(c_col) > 0) as.character(input_df[[c_col[1]]]) else "Unspecified"
  
  s_col <- intersect(c("Site_name", "site_name"), names(input_df))
  input_df$site_name <- if (length(s_col) > 0) as.character(input_df[[s_col[1]]]) else "Unspecified"
  
  # Identify article title
  article_col <- intersect(c("Article_Title", "article_title"), names(input_df))
  input_df$article_title <- if (length(article_col) > 0) as.character(input_df[[article_col[1]]]) else "Unspecified"
  
  time_breaks <- c(0, -50, 25, 100, 200, 500, 1000, 5000, 10000, 15000, Inf)
  bin_labels  <- c("Modern [-29 — -50]", "-50-25", "25-100", 
                   "100-200", "200-500", "500-1000", "1000-5000",
                   "5000-10000", "10000-15000", ">15000")
  input_df %>%
    mutate(
      data_type = case_when(
        tolower(trimws(is_core)) %in% c("yes", "true", "1")       ~ "Core",
        tolower(trimws(is_top_bottom)) %in% c("yes", "true", "1") ~ "Top-Bottom",
        tolower(trimws(is_top)) %in% c("yes", "true", "1")        ~ "Sediment surface",
        TRUE                                                     ~ "Unspecified"
      ),
      min_age_clean = clean_age(Min_age),
      max_age_clean = clean_age(Max_age),
      temporal_coverage = abs(max_age_clean - min_age_clean),
      Latitude_num  = suppressWarnings(as.numeric(Latitude)),
      Longitude_num = suppressWarnings(as.numeric(Longitude)),
      time_bin = cut(
        temporal_coverage, 
        breaks = time_breaks, 
        labels = bin_labels, 
        right = TRUE, 
        include.lowest = TRUE
      )
    ) %>%
    mutate(data_type = fct_relevel(
      data_type,
      "Sediment surface",
      "Top-Bottom",
      "Core"),
      data_type = if_else(coll_unit_type == "section", "Section", as.character(data_type)),
      data_type = fct_relevel(data_type,"Sediment Surface", "Top-Bottom","Core","Section")) 
}

# apply the function to the inventory table
dataset <- process_data(inventory)

# Bin sequence labels for the range slider
bin_levels <- levels(dataset$time_bin)

# ==============================================================================
# 2. Shiny User Interface (UI)
# ==============================================================================
ui <- page_sidebar(
  title = "European Diatom Inventory for Palaeoecology and Ecology (EDIPE)",
  theme = bs_theme(bootswatch = "flatly", primary = "#2c3e50"),
  
  sidebar = sidebar(
    width = 320,
    
    # Filter 1: Dataset
    selectInput("dataset_filter", "Dataset:", 
                choices = c("All", sort(unique(dataset$Dataset))), 
                selected = "All"),
    
    # Filter for collection unit type
    # selectInput("coll_unit_type_filter", "Collection unit type:", 
    #             choices = c("All", sort(unique(dataset$coll_unit_type))), 
    #             selected = "All"),
    # 
    # Filter 2: Data Type
    checkboxGroupInput("datatype_filter", "Data Type:", 
                       choices = sort(unique(dataset$data_type)),
                       selected = sort(unique(dataset$data_type))),
    
    # Filter 3: data available
    checkboxGroupInput("data_available_filter", "Data available:", 
                       choices = sort(unique(dataset$data_available)),
                       selected = NULL),
    
    # Filter 4: Temporal Bin Range Slider Bar (NEW BAR OPTION)
    sliderTextInput(
      inputId = "timebin_filter",
      label = "Temporal Bin Range:",
      choices = bin_levels,
      selected = c(bin_levels[1], bin_levels[length(bin_levels)]),
      grid = TRUE
    ),
    
    # Filter 5: Country
    selectInput("country_filter", "Country:", 
                choices = c("All", sort(unique(dataset$country))),
                selected = "All"),
    
    # Filter 6: Site Name (Searchable Multi-select)
    selectizeInput("site_filter", "Site Name:", 
                   choices = NULL, 
                   multiple = TRUE, 
                   options = list(placeholder = 'Search site name...')),
    
    # Filter 7: Site Description (Searchable Multi-select)
    selectizeInput("site_desc_filter", "Site Description:", 
                   choices = NULL, 
                   multiple = TRUE, 
                   options = list(placeholder = 'Search site type/desc...')),
    
    # Filter 8: Dating Methods (Searchable Multi-select)
    selectizeInput("dating_filter", "Dating Method:", 
                   choices = NULL, 
                   multiple = TRUE, 
                   options = list(placeholder = 'Search dating method...')),
    
    # Filter 9: Article Title (Searchable Multi-select)
    selectizeInput("article_filter", "Article Title:", 
                   choices = NULL, 
                   multiple = TRUE, 
                   options = list(placeholder = 'Search article title...')),
    
    hr(),
    downloadButton("download_data", "Export Filtered CSV", class = "btn-sm btn-primary w-100")
  ),
  
  navset_card_tab(
    nav_panel(
      "Temporal Coverage",
      layout_column_wrap(
        width = 1,
        card(card_header("Temporal distribution"), plotlyOutput("time_bin_plot", height = "380px")),
        card(card_header("Temporal distribution table"), DTOutput("summary_matrix_table"))
      )
    ),
    nav_panel(
      "Map Overview",
      card(card_header("Geographic distribution of collection units"), leafletOutput("map_plot", height = "600px"))
    ),
    nav_panel(
      "Raw Data Explorer",
      card(card_header("Filter Inventory collection units"), DTOutput("raw_data_table"))
    )
  )
)

# ==============================================================================
# 3. Shiny Server Logic
# ==============================================================================
server <- function(input, output, session) {
  
  # Dynamic options loading
  updateSelectizeInput(session, "site_filter", 
                       choices = sort(unique(dataset$site_name)), server = TRUE)
  
  updateSelectizeInput(session, "site_desc_filter", 
                       choices = sort(unique(dataset$site_desc)), server = TRUE)
  
  updateSelectizeInput(session, "dating_filter", 
                       choices = sort(unique(dataset$dating_method)), server = TRUE)
  
  updateSelectizeInput(session, "article_filter", 
                       choices = sort(unique(dataset$article_title)), server = TRUE)
  
  # Reactive Filtered Dataset
  filtered_df <- reactive({
    res <- dataset
    
    if (input$dataset_filter != "All") {
      res <- res %>% filter(Dataset == input$dataset_filter)
    }
    
    # if (input$coll_unit_type_filter != "All") {
    #   res <- res %>% filter(coll_unit_type == input$coll_unit_type_filter)
    # }
    
    if (!is.null(input$datatype_filter)) {
      res <- res %>% filter(data_type %in% input$datatype_filter)
    }
    
    if (!is.null(input$data_available_filter)) {
      res <- res %>% filter(data_available %in% input$data_available_filter)
    }
    
    # Temporal Bin Slider Filtering Logic 
    # (Keeps selected bins AND retains NA time_bins so valid coordinates aren't dropped)    # if (!is.null(input$timebin_filter)) {

    if (!is.null(input$timebin_filter)) {
      min_bin <- input$timebin_filter[1]
      max_bin <- input$timebin_filter[2]
      
      start_idx <- match(min_bin, bin_levels)
      end_idx   <- match(max_bin, bin_levels)
      
      selected_bins <- bin_levels[start_idx:end_idx]
      res <- res %>% filter(time_bin %in% selected_bins | is.na(time_bin))
    }
    
    
    if (input$country_filter != "All") {
      res <- res %>% filter(country == input$country_filter)
    }
    
    if (!is.null(input$site_filter) && length(input$site_filter) > 0) {
      res <- res %>% filter(site_name %in% input$site_filter)
    }
    if (!is.null(input$site_desc_filter) && length(input$site_desc_filter) > 0) {
      res <- res %>% filter(site_desc %in% input$site_desc_filter)
    }
    if (!is.null(input$dating_filter) && length(input$dating_filter) > 0) {
      res <- res %>% filter(dating_method %in% input$dating_filter)
    }
    if (!is.null(input$article_filter) && length(input$article_filter) > 0) {
      res <- res %>% filter(article_title %in% input$article_filter)
    }
    
    res
  })
  
  # Render Interactive Bar Chart
  output$time_bin_plot <- renderPlotly({
    df_plot <- filtered_df() %>%
      filter(!is.na(time_bin)) %>%
      count(Dataset, data_type, time_bin)
    
    validate(
      need(nrow(df_plot) > 0, "No data available for the current filter selection.")
    )
    
    p <- ggplot(df_plot, aes(x = time_bin, y = n, fill = data_type)) +
      geom_col(position = "stack", width = 0.7) +
      facet_wrap(~ Dataset, scales = "free_y") +
      scale_fill_manual(values = c("Core" = "#7570b3", "Top-Bottom" = "#d95f02", "Sediment surface" = "#1b9e77", "Section" = "grey")) +
      labs(x = "Temporal bins (calibrated years BP)", y = "Number of collection units") +
      theme_article() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
      guides(fill = guide_legend(title = "Data type"))
    
    ggplotly(p)
  })
  
  # Render Summary Matrix
  output$summary_matrix_table <- renderDT({
    req(filtered_df())
    
    filtered_df() %>%
      filter(!is.na(time_bin)) %>%
      group_by(Dataset, data_type, time_bin) %>%
      summarise(count = n(), .groups = "drop") %>%
      pivot_wider(names_from = time_bin, values_from = count, values_fill = 0) %>%
      datatable(options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })
  
  # Render Leaflet Map
  output$map_plot <- renderLeaflet({
    map_df <- filtered_df() %>% filter(!is.na(Latitude_num) & !is.na(Longitude_num))
    
    validate(
      need(nrow(map_df) > 0, "No valid geographic coordinates found for the selected filters.")
    )
    
    pal <- colorFactor(palette = c("Core" = "#7570b3", "Top-Bottom" = "#d95f02", "Sediment surface" = "#1b9e77", "Section" = "grey"), domain = map_df$data_type)
    
    
   # pre process doi article
    map_df <- map_df %>%
      mutate(
        paper_html = case_when(
          is.na(doi_article) | doi_article == "" ~ "None",
          # If it doesn't start with http:// or https://, prepend https:// automatically
          !grepl("^http://|^https://", doi_article) ~ paste0("<a href='https://", doi_article, "' target='_blank'>", doi_article, "</a>"),
          # Otherwise, use it as is
          TRUE ~ paste0("<a href='", doi_article, "' target='_blank'>", doi_article, "</a>")
        )
      )
    
    leaflet(map_df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude_num, 
        lat = ~Latitude_num,
        color = ~pal(data_type),
        radius = 5,
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>Site: </b>", site_name, "<br>",
          "<b>Dataset: </b>", Dataset, "<br>",
          "<b>Type: </b>", data_type, "<br>",
          "<b>Dating method: </b>", dating_method, "<br>",
          "<b>Temporal coverage: </b>", temporal_coverage, " years (<I>", time_bin, "</I>)", "<br>",
          "<b>DOI article: </b>", paper_html, "<br>",
          #"<b>DOI article: </b><a href='", doi_article, "' target='_blank'>", doi_article, "</a><br>",
          #"<b>Data repository </b><a href='", data_repository, "' target='_blank'>", data_repository, "</a><br>")
          "<b>Data repository: </b>", data_repository)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~data_type, title = "Data Type")
  })
  
  # Data Table View
  output$raw_data_table <- renderDT({
    filtered_df() %>%
      select(Dataset, Id_dataset, article_title, doi_article, data_available, data_repository, country, site_name, site_desc, 
             dating_method, data_type, Min_age, Max_age, temporal_coverage, time_bin) %>%
      # rename(country_clean=Country,
      #        site_name_clean=Site_name,
      #        site_desc_clean=Site_description,
      #        dating_method_clean=Dating_method,
      #        data_type=Data_type) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # Download Handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("diatom_palaeopen_filtered_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_df(), file, row.names = FALSE)
    }
  )
}

# Run Application
shinyApp(ui = ui, server = server)
