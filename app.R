# Illinois Community Health Dashboard ----

# Community Environmental Health Data Access and Research (CEHDAR) Working Group 

# License: AGPL-3.0

# Packages ----
library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)
library(stringr)
library(bslib)
library(shinythemes)
library(leaflet.extras)
library(viridisLite)
library(shinyjs)
library(shinycssloaders)

# Spinner color setting
spinner_color <- "#2FA4E7"

# Labels ----
pop_labels <- c(
  "E_AFAM" = "Percent Black",
  "E_HISP" = "Percent Hispanic",
  "oth.nonhis.wh" = "Percent of Other Non-White Groups",
  "E_DISABL" = "Percent with Disability",
  "E_AGE17" = "Percent Under 18",
  "E_AGE65" = "Percent Over 65",
  "E_UNEMP" = "Percent Unemployed",
  "E_RENTER" = "Percent Renters"
)

climate_labels <- c(
  "E_NEHD" = "Extreme Heat Days",
  "E_SMOKE" = "Wildfire Smoke Days"
)

health_labels <- c(
  "E_ASTHMA" = "Asthma",
  "E_CANCER" = "Cancer",
  "E_CHD" = "Coronary Heart Disease",
  "E_DIABETES" = "Diabetes",
  "E_MHLTH" = "Poor Mental Health"
)

env_labels <- c(
  "E_HOUAGE" = "Potential Lead Exposure",
  "exceed" = "High Air Pollution Days"
)

health_system_labels <- c(
  "D_MEDICALPRO" = "Practicing Medical Professionals (per 10,000)",
  "PR_INSURANCE" = "Private Insurance Coverage",
  "PU_INSURANCE" = "Public Insurance Coverage"
)

# Load data ----
illinois_data <- st_read("Illinois_dashboard_data.geojson", quiet = TRUE)
illinois_data <- st_make_valid(illinois_data)
map_data <- st_transform(illinois_data)

# Mask areas outside Illinois
il_boundary <- st_union(map_data)
bbox <- st_bbox(c(xmin = -100, ymin = 30, xmax = -80, ymax = 50), crs = st_crs(map_data))
mask <- st_difference(st_as_sfc(bbox), il_boundary)

# County dropdown choices
county_choices <- unique(str_sort(map_data$NAMELSADCO))

# Tab UI Definitions ----
## Tab 1: Population Groups ----
tab1_ui <- tabPanel("Population Groups",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        h5(icon("map-location-dot"), "Tract Details"),
                        wellPanel(
                          style = "background-color: #ffffff; border: 1px solid #e0e0e0; padding: 12px;",
                          uiOutput("hover_info")
                        )
                      ),
                    
                       mainPanel(
                         width = 9,
                        p("Select different variables from the dropdown menus to explore the data."),
                        
                        selectInput("color", "Historically Marginalized & Socially Categorized Populations:",
                                    width = "450px",
                                    choices = c("Percent Black" = "E_AFAM",
                                                "Percent Hispanic" = "E_HISP",
                                                "Percent of Other Non-White Groups" = "oth.nonhis.wh",
                                                "Percent with a disability" = "E_DISABL",
                                                "Percent under 18" = "E_AGE17",
                                                "Percent over 65" = "E_AGE65",
                                                "Percent unemployed" = "E_UNEMP",
                                                "Percent of renters" = "E_RENTER"),
                                    selected = "E_AGE17"),
                      br(),
                      fluidRow(withSpinner(leafletOutput("map"), type = 1, color = spinner_color)),
                      div(style = "margin-top: 8px;"),                         
                      selectInput("neighborhood", "Select County:",
                                  width = "450px",
                                  choices = county_choices,
                                  selected = 'Champaign County'),
                      br(),
                      fluidRow(withSpinner(plotlyOutput("populationChart"), type = 1, color = spinner_color))
                      )
                    ))

## Tab 2: Environment & Population ----
tab2_ui <- tabPanel("Environmental Exposures",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        h5(icon("map-location-dot"), "Tract Details"),
                        wellPanel(
                          style = "background-color: #ffffff; border: 1px solid #e0e0e0; padding: 12px;",
                          uiOutput("hover_info_2")
                        )
                      ),
                      mainPanel(
                        width = 9,
                        p("Select different variables from the dropdown menus to explore the data."),

                        selectInput("color_env", "Environmental Exposures:",
                                    width = "450px",
                                    choices = c("Percentage of Homes with Potential Lead Exposure" = "E_HOUAGE",
                                                "Number of High Air Pollution Days" = "exceed"),
                                    selected = "E_HOUAGE"),
                        br(),
                        h5(strong("Environmental Exposures", style = "color:666666;")),
                        fluidRow(withSpinner(leafletOutput("map_environment"), type = 1, color = spinner_color)),
                        br(),
                      selectInput("color_1", "Historically Marginalized & Socially Categorized Populations:",
                                  width = "450px",
                                  choices = c("Percent Black" = "E_AFAM",
                                              "Percent Hispanic" = "E_HISP",
                                              "Percent of Other Non-White Groups" = "oth.nonhis.wh",
                                              "Percent with a disability" = "E_DISABL",
                                              "Percent under 18" = "E_AGE17",
                                              "Percent over 65" = "E_AGE65",
                                              "Percent unemployed" = "E_UNEMP",
                                              "Percent of renters" = "E_RENTER"),
                                  selected = "E_AGE17"),
                      br(),
                      h5(strong("Population Group", style = "color:999999;")),
                      fluidRow(withSpinner(leafletOutput("map_1"), type = 1, color = spinner_color))
                    )))

## Tab 3: Climate ----
tab3_ui <- tabPanel("Climate Events",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        h5(icon("map-location-dot"), "Tract Details"),
                        wellPanel(
                          style = "background-color: #ffffff; border: 1px solid #e0e0e0; padding: 12px;",
                          uiOutput("hover_info_3")
                        )
                      ),
                    mainPanel(
                      width = 9,
                      p("Select different variables from the dropdown menus to explore the data."),
                        selectInput("color_climate", "Climate Events:",
                                    width = "450px",
                                    choices = c("Number of Extreme Heat Days" = "E_NEHD",
                                                "Number of Wildfire Smoke Days" = "E_SMOKE"),
                                    selected = "E_NEHD"),
                      br(),
                      fluidRow(withSpinner(leafletOutput("map_climate"), type = 1, color = spinner_color)),
                      div(style = "margin-top: 8px;"),
                      selectInput("neighborhood_climate", "Select County:",
                                  width = "450px",
                                  choices = county_choices,
                                  selected = 'Champaign County'),
                      br(),
                      fluidRow(
                        div(style = "display: flex; justify-content: center;",
                            withSpinner(plotlyOutput("climateChart", width = "80%", height = "400px"), type = 1, color = spinner_color)
                        )
                      )
                    )))

## Tab 4: Health ----
tab4_ui <- tabPanel("Health Conditions",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        h5(icon("map-location-dot"), "Tract Details"),
                        wellPanel(
                          style = "background-color: #ffffff; border: 1px solid #e0e0e0; padding: 12px;",
                          uiOutput("hover_info_4")
                        )
                      ),
                    mainPanel(
                       width = 9,
                        p("Select different variables from the dropdown menus to explore the data."),
                        selectInput("color_health", "Health Conditions:",
                                    width = "450px",
                                    choices = c("Prevalence of Asthma" = "E_ASTHMA",
                                                "Prevalence of Cancer" = "E_CANCER",
                                                "Prevalence of Coronary Heart Disease" = "E_CHD",
                                                "Prevalence of Diabetes" = "E_DIABETES",
                                                "Prevalence of Poor Mental Health" = "E_MHLTH"),
                                    selected = "E_CANCER"),
                      br(),
                      fluidRow(withSpinner(leafletOutput("map_health"), type = 1, color = spinner_color)),
                      div(style = "margin-top: 8px;"),         
                      selectInput("neighborhood_health", "Select County:",
                                  width = "450px",
                                  choices = county_choices,
                                  selected = 'Champaign County'),
                      br(),
                      fluidRow(withSpinner(plotlyOutput("healthChart"), type = 1, color = spinner_color))
                      )))

## Tab 6: Healthcare Access ----
tab6_ui <- tabPanel("Healthcare Access",
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        h5(icon("map-location-dot"), "Tract Details"),
                        wellPanel(
                          style = "background-color: #ffffff; border: 1px solid #e0e0e0; padding: 12px;",
                          uiOutput("hover_info_6")
                        )
                      ),
                      mainPanel(
                        width = 9,
                        p("Select different variables from the dropdown menus to explore the data."),
                        selectInput("color_health_system", "Healthcare Access:",
                                    width = "450px",
                                    choices = c(
                                                "Practicing Medical Professionals" = "D_MEDICALPRO",
                                                "Private Insurance Coverage" = "PR_INSURANCE",
                                                "Public Insurance Coverage" = "PU_INSURANCE"),
                                    selected = "PR_INSURANCE"),
                        br(),
                        fluidRow(withSpinner(leafletOutput("map_health_system"), type = 1, color = spinner_color)),
                        div(style = "margin-top: 8px;"),
                        selectInput("neighborhood_health_system", "Select County:",
                                    width = "450px",
                                    choices = county_choices,
                                    selected = 'Champaign County'),
                        br(),
                        fluidRow(
                          column(4, withSpinner(plotlyOutput("healthDensityChart", height = "350px"), type = 1, color = spinner_color)),
                          column(7, withSpinner(plotlyOutput("healthInsuranceChart", height = "350px"), type = 1, color = spinner_color))
                        )
                      )))

## Tab 5a: How to Use ----
tab5a_ui <- tabPanel("How to Use",
                     fluidRow( 
                       column(
                         width = 10,
                         offset = 1,
                         br(),
                         
                         h4(icon("book-open"), "How to Use This Dashboard?"),
                         p(
                           strong("Get started by navigating the dashboard by clicking on the different tabs in blue text above!"),
                           tags$br(),
                           "Within each tab you will see", tags$span(style = "color: #0077b6; font-weight: 600;", "maps"), "that you can:", 
                           tags$ul(
                             tags$li(tags$span(style = "color: #0077b6; font-weight: 600;", "search by entering a location"), "(e.g., address, ZIP Code, etc.)"), 
                             tags$li("zoom in/out by using your mouse"),
                             tags$li(tags$span(style = "color: #0077b6; font-weight: 600;", "click on a location on the map"), "to see data for the local area in the side panel, and"), 
                             tags$li("use drop-down menus on the top of the map to select different information to display for local areas and across the state of Illinois")), 
                           tags$p("On the bottom half of each tab, there is either a drop-down menu to select different",
                                  tags$span(style = "color: #0077b6; font-weight: 600;", "counties"),
                                  "in Illinois to view graphs with information, or another map with a different drop-down menu to visualize and compare with the top map."),
                           tags$p("When you move your mouse over graphs, a small",
                                  tags$span(style = "color: #0077b6; font-weight: 600;", "toolbar"),
                                  "appears in the upper-right corner where you can download the graph as an image, zoom in/out, rescale, or reset the graph."),
                           
                           hr(),
                           
                           h4(icon("info-circle"), "What is a Dashboard?"),
                           p(
                             "A dashboard is web-based application that presents customized data visualizations and geographic maps conveying", tags$span(style = "color: #0077b6; font-weight: 600;", "place-based"), "information. Dashboards allow for interactivity with its users and have the flexibility to present multiple pieces of information within and across multiple tabs in a single dashboard."),
                           tags$p("This dashboard contains the following tabs, each presenting specific information for users to explore:"),
                           tags$ul(
                             tags$li(strong("Population Groups"), " (racial/ethnic groups, persons with disability, age groups, renters, unemployed)"),
                             tags$li(strong("Health Conditions"), " (asthma, coronary heart disease, cancer, diabetes, poor mental health)"),
                             tags$li(strong("Healthcare Access"), " (medical professional density, private and public insurance coverage)"),
                             tags$li(strong("Climate Events"), " (extreme heat, wildfire smoke)"),
                             tags$li(strong("Environmental Exposures"), " (potential lead exposure, air pollution)")
                           ),
                           br() 
                         ))))

## Tab 5b: About the Data ----
tab5b_ui <- tabPanel("About the Data",
                     fluidRow( 
                       column(
                         width = 10,
                         offset = 1,
                         br(),                
                         h4(icon("map-marked-alt"), "What Information is Included in this Dashboard?"),
                         
                         tags$p(strong("Administrative Areas")), 
                         tags$ul(
                           tags$li("This dashboard presents community health information about Illinois census tract areas and counties.")), 
                         tags$p(strong("Population Groups")), 
                         tags$ul(
                           tags$li("For census tract areas and counties, the dashboard includes demographic population information of historically marginalized and socially categorized population groups."), 
                           tags$li("The population groups included are percent of the population that are: renters, unemployed, under 18, or over 65. Other groups included are percent of the population with a disability, and percent of the population that identified as different minoritized racial or ethnic groups: Black, Hispanic, and other non-Hispanic white ethnic/racial groups."),
                           tags$li("These population groups have been disproportionately experiencing health challenges from built environments, exposure to environmental chemicals and pollutants, and extreme weather or climate events.")), 
                         tags$p(strong("Health Conditions")),
                         tags$ul(
                           tags$li("For census tract areas and counties, the dashboard includes prevalence of asthma, cancer, coronary heart disease, diabetes, and poor mental health.")
                         ),
                         tags$p(strong("Healthcare Access")),
                         tags$ul(
                           tags$li("For census tract areas and counties, the dashboard includes practicing medical professionals, expressed as the density of medical providers per 10,000 people."),
                           tags$li("For census tract areas and counties, the dashboard includes information of different insurance coverage: percent of the population with private insurance, and percent of the population with public insurance.")
                         ),
                         
                         tags$p(strong("Climate Events")),
                         tags$ul(
                           tags$li(HTML("For census tract areas and counties, the dashboard includes the annualized frequency of the number of extreme heat days, which represent days above the 95<sup>th</sup>&nbsp;percentile of temperatures for the census tract area.")),
                           tags$li("The dashboard also includes for census tract areas and counties, the annualized frequency of the number of wildfire smoke days, which represent days with medium and heavy density of smoke cover.")
                         ), 
                         
                         
                         tags$p(strong("Environmental Exposures")), 
                         tags$ul(
                           tags$li("For census tract areas, the dashboard includes: percentage of homes with potential lead exposure, based on share of homes built before 1980 when lead paint was more commonly used in homes."),
                           tags$li("The dashboard also includes for census tract areas, the annual number of days with air pollution levels above the WHO daily limit.")), 
                         
                         hr(), 
                         
                         h4(icon("database"), "Where Does the Data Come from?"),
                         tags$p("Data presented in the dashboard come from publicly aggregated datasets and from multiple original public secondary data sources. Population group data was obtained from CDC’s Environmental Justice Index (EJI) dataset from 2024, which originated from the American Community Survey (ACS) administered by the U.S. Census Bureau and represent five-year estimates from 2018 – 2022."), 
                         tags$p("Health condition data were obtained from the EJI, which originated from the CDC from 2024. Medical professional provider density was obtained from the FEMA Resilience Analysis and Planning Tool (RAPT) dataset, which originates from the American community survey and represent five-year estimates from 2019-2023. Public/private insurance coverage was obtained directly from the U.S. Census Bureau and represent five-year ACS estimates from 2018 – 2022."),
                         tags$p("Data on climate events were obtained from the EJI, which originated from multiple sources: the CDC for extreme heat days representing data from 2018 – 2022, and from NOAA for wildfire smoky days representing data from 2013 – 2022. Potential lead exposure data was obtained from the EJI and air pollution data was obtained directly from the CDC’s database and represents daily air pollution levels from 2020."),
                         tags$span(class = "text-muted", "Notes:"),
                         tags$br(),
                         tags$span(class = "text-muted", "Data were not available for a few census tract locations and these locations appear in light grey on the maps in the dashboard."),
                         tags$br(),
                         tags$span(class = "text-muted", "These unavailable locations are the following tracts: 3817, 9800, 9801 (Cook); 8630.05, 8630.06 (Lake); 9800 (Will); and 9800 (Winnebago)."),
                         tags$br(),
                         tags$span(class = "text-muted", "Abbreviations, CDC – Center for Disease Control, FEMA – Federal Emergency Management Agency, NOAA – National Oceanic and Atmospheric Administration.")
                       )))

## Tab 5c: App Info ----
tab5c_ui <- tabPanel("App Information",
                     fluidRow( 
                       column(
                         width = 10,
                         offset = 1,
                         br(),
                         
                         h4(icon("lightbulb"), "What Is the Motivation Behind this Dashboard?"),
                         p("The motivation for developing an", tags$span(style = "color: #0077b6; font-weight: 600;", "Illinois Community Health Dashboard"), "was inspired by engaging with different communities in Illinois and learning how", tags$span(style = "color: #0077b6; font-weight: 600;", "environmental burdens"), "have adversely impacted the", tags$span(style = "color: #0077b6; font-weight: 600;", "health and well-being"), "of their neighborhoods and residents. These communities face intersecting challenges related to health conditions, environmental disparities, and socioeconomic hardships."), 
                         p("Communities have expressed concerns with exposure to lead and pollution in disinvested communities, increasing climate burdens of extreme heat and wildfire smoke events, lack of health access, and populations managing physical and mental health conditions. The dashboard helps to highlight relevant", tags$span(style = "color: #0077b6; font-weight: 600;", "place-based"), "data, which can be valuable for communities to understand their own neighborhood conditions, communicate information to various stakeholders in local planning and development initiatives, and support efforts for new environmental health programs and funding opportunities."),
                         p("We used human-centered design principles to construct the web app and then ran focus groups with community partners to incorporate their ideas and feedback for additional features and interactivity to maximize user experience."),
                         
                         hr(),
                         
                         h4(icon("info"), "Web App and Team Information"),
                         
                         p(strong("Current Release Version:"), "1.1 Acorn (March 2026)"),
                         div("Incubated as a SDOH & Place project"),
                         div("Developed as an Engagement Resource for the University YMCA UIUC community"),
                         div("Piloted for Illinois communities by the Community Environmental Health Data Access and Research (CEHDAR) Working Group"),
                         p(style = "margin-top: 15px; text-align: right;", 
                           "For questions or feedback, contact us at ", 
                           tags$a(href = "mailto:cehdar@proton.me", "cehdar@proton.me")),
                         
                         hr(style = "border-color: black;"),
                         
                         p(strong("Citation Information:"), br(), "This Illinois Community Health dashboard can be cited by using the following information and DOI, which represents all stable versions released and the link resolving to the latest version (DOI to be updated; web app publication in preparation. Please contact us for use and citation):"),
                         p(style = "text-align: center;", "Gounder, B. & Cho, S. (2026).", tags$em("CEHDAR and the Illinois Community Health Dashboard: Communicating Environmental and Public Health Data with Interactive Geospatial Maps for Community Access."), "Zenodo. DOI:"),
                         hr(),
                         
                         p(
                           style = "text-align: center; color: #666;",
                           "© Copyright 2026 Community Environmental Health Data Access and Research Working Group.",
                           br(),
                           "Data Licensed ",
                           icon("creative-commons"),
                           icon("creative-commons-by"),
                           icon("creative-commons-nc"),
                           "Creative Commons Attribution-Non Commercial")
                         
                        
                         )))

# Tab Server Definitions ----

tab1_server <- function(input, output, session) {
  
  state_avg_afam <- mean(map_data$E_AFAM, na.rm = TRUE)
  state_avg_hisp <- mean(map_data$E_HISP, na.rm = TRUE)
  state_avg_oth <- mean(map_data$oth.nonhis.wh, na.rm = TRUE)
  state_avg_disabl <- mean(map_data$E_DISABL, na.rm = TRUE)
  state_avg_age17 <- mean(map_data$E_AGE17, na.rm = TRUE)
  state_avg_age65 <- mean(map_data$E_AGE65, na.rm = TRUE)
  state_avg_unemp <- mean(map_data$E_UNEMP, na.rm = TRUE)
  state_avg_renter <- mean(map_data$E_RENTER, na.rm = TRUE)
  
  output$hover_info <- renderUI({
    p(style = "color: #999; font-style: italic; font-size: 13px;", 
      "Click on a tract to view details")
  })
  
  observeEvent(input$map_shape_click, {
    event <- input$map_shape_click
    if (!is.null(event$id)) {
      tract <- map_data[map_data$GEOID == event$id, ]
      if (nrow(tract) > 0) {
        
        leafletProxy("map") %>%
          clearGroup("highlight") %>%
          addPolygons(data = tract, group = "highlight",
                      fillColor = NA, fillOpacity = 0,
                      weight = 3, color = "#1a5276", opacity = 1)
        
        updateSelectInput(session, "neighborhood", selected = tract$NAMELSADCO[1])
        
        pop_vars <- c("E_AFAM", "E_HISP", "oth.nonhis.wh", "E_DISABL", "E_AGE17", "E_AGE65", "E_UNEMP", "E_RENTER")
        pop_rows <- lapply(pop_vars, function(v) {
          is_selected <- (v == input$color)
          
          state_avg <- switch(v,
                              "E_AFAM" = state_avg_afam,
                              "E_HISP" = state_avg_hisp,
                              "oth.nonhis.wh" = state_avg_oth,
                              "E_DISABL" = state_avg_disabl,
                              "E_AGE17" = state_avg_age17,
                              "E_AGE65" = state_avg_age65,
                              "E_UNEMP" = state_avg_unemp,
                              "E_RENTER" = state_avg_renter
          )
          
          tagList(
            tags$tr(
              tags$td(style = paste0(
                "padding: 4px 0; font-size: 12px;",
                if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
              ), pop_labels[v]),
              tags$td(style = paste0(
                "text-align: right; padding: 4px 0; font-size: 12px;",
                if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
              ), paste0(round(tract[[v]][1], 1), "%"))
            ),
            tags$tr(
              tags$td(style = "color: #999; padding: 2px 0 8px 0; font-size: 11px;", "State Average"),
              tags$td(style = "text-align: right; color: #999; padding: 2px 0 8px 0; font-size: 11px;", 
                      paste0(round(state_avg, 1), "%"))
            )
          )
        })
        
        health_rows <- lapply(c("E_ASTHMA", "E_CANCER", "E_CHD", "E_DIABETES", "E_MHLTH"), function(v) {
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", health_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), "%"))
          )
        })
        
        climate_rows <- lapply(c("E_NEHD", "E_SMOKE"), function(v) {
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", climate_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), " days"))
          )
        })
        
        env_rows <- tagList(
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", env_labels["E_HOUAGE"]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract$E_HOUAGE[1], 1), "%"))
          ),
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", env_labels["exceed"]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract$exceed[1], 1), " days"))
          )
        )
        
        health_system_rows <- lapply(c("D_MEDICALPRO", "PR_INSURANCE", "PU_INSURANCE"), function(v) {
          unit <- if(v %in% c("PR_INSURANCE", "PU_INSURANCE")) "%" else ""
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", health_system_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), unit))
          )
        })
        
        output$hover_info <- renderUI({
          tagList(
            tags$div(
              style = "margin-bottom: 8px;",
              tags$div(style = "font-size: 14px; font-weight: 600; color: #1a5276;",
                       paste("Tract", tract$NTAName[1])),
              tags$div(style = "font-size: 14px; color: #1a5276;",
                       tract$NAMELSADCO[1]),
              tags$a(href = "#",
                     onclick = "Shiny.setInputValue('reset_hover_1', Math.random())",
                     style = "font-size: 11px; color: #999; margin-top: 5px; display: inline-block;",
                     "Clear selection")
            ),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #1a5276; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Population"),
            tags$table(style = "width: 100%;", pop_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Health Conditions"),
            tags$table(style = "width: 100%;", health_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Healthcare Access"),
            tags$table(style = "width: 100%;", health_system_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Climate"),
            tags$table(style = "width: 100%;", climate_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Environmental Exposures"),
            tags$table(style = "width: 100%;", env_rows)
          )
        })
      }
    }
  })
         
  
  observeEvent(input$reset_hover_1, {
    leafletProxy("map") %>% clearGroup("highlight")
    output$hover_info <- renderUI({
      p(style = "color: #999; font-style: italic; font-size: 13px;", 
        "Click on a tract to view details")
    })
  })
  
  output$map <- renderLeaflet({
    
    valid_data <-map_data[!is.na(map_data[[input$color]]), ]
    pal <- colorBin(
      viridisLite::mako(256), 
      domain = valid_data[[input$color]], 
      bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
      reverse = T)
    
    leaflet(valid_data, options = leafletOptions(minZoom = 6, maxZoom = 18, zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({position: 'bottomleft'}).addTo(this);
      }") %>%
      addPolygons(data = mask, fillColor = "white", fillOpacity = 1,
                  stroke = FALSE, group = "mask") %>%
      setMaxBounds(lng1 = -91.6, lat1 = 36.9, lng2 = -87.0, lat2 = 42.6) %>%
      addSearchOSM(options = searchOptions(
        autoCollapse = FALSE,
        collapsed = F,
        position = "topright",
        textPlaceholder = "Search address or ZIP...",
        zoom = 10,
        marker = list(
          circle = list(
            radius = 20,
            weight = 4,
            color = "#2FA4E7",
            stroke = TRUE,
            fill = FALSE
          )
        )
      )) %>%
      addPolygons(
        fillColor = ~pal(valid_data[[input$color]]),
        fillOpacity = 0.7, weight = 1, color = "#d0d0d0",
        layerId = ~GEOID,
        label = ~paste0(
          NTAName, " - ",
          pop_labels[input$color], ": ", round(valid_data[[input$color]], 2), "%"
        )
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = valid_data[[input$color]],
        title = "% of population"
      )  %>%
      
      setView(lng = -89.395242, lat = 40.630610, zoom = 7)
  })
  
  output$populationChart <- renderPlotly({
    chart_data <- map_data[map_data$NAMELSADCO == input$neighborhood, ]
    
    population_data <- st_drop_geometry(chart_data)
    population_data <-population_data[, c("E_AFAM", "E_HISP", "oth.nonhis.wh", "E_DISABL", "E_AGE17", "E_AGE65", "E_UNEMP", "E_RENTER")]

    county_avgs <- map_data %>%
      st_drop_geometry() %>%
      group_by(NAMELSADCO) %>%
      summarise(across(c(E_AFAM, E_HISP, oth.nonhis.wh, E_DISABL, E_AGE17, E_AGE65, E_UNEMP, E_RENTER), mean, na.rm = TRUE))
    
    state_avg <- colMeans(county_avgs[, -1], na.rm = TRUE)
    
    population_data <- colMeans(population_data)
    population_data <- as.data.frame(population_data)
    population_data <- cbind(Population = rownames(population_data), Percentage = population_data[, 1])
    rownames(population_data) <- NULL
    
    population_data <- as.data.frame(population_data)
    population_data$Population <- pop_labels[population_data$Population]
    population_data$CountyAvg <- as.numeric(state_avg)
    
    plot_ly(data = as.data.frame(population_data), x = ~Population, y = ~as.numeric(Percentage), type = 'bar', color = ~Population, colors = "Dark2",
            text = ~paste0(Population, ": ", round(as.numeric(Percentage), 1), "%"),
            hoverinfo = "text") %>%
      
      add_markers(x = ~Population, y = ~CountyAvg, name = "Avg Across Counties",
                  marker = list(symbol = "diamond", size = 10, color = "#1a5276",
                                line = list(width = 1, color = "white")),
                  text = ~paste0("Avg Across Counties: ", round(CountyAvg, 1), "%"),
                  hoverinfo = "text",
                  showlegend = FALSE) %>%
      
      layout(title = paste("Population Demographics -", input$neighborhood),
             xaxis = list(title = "Population Group", showticklabels = FALSE),
             yaxis = list(title = "Percentage", range = c(0, 100), tickvals = seq(0, 100, 10)),
             annotations = list(
               list(x = 1, y = 1, xref = "paper", yref = "paper",
                    xanchor = "right", yanchor = "top",
                    text = "◆ = Avg Across Counties", showarrow = FALSE,
                    font = list(size = 12, color = "#1a5276"))
             )) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
                                   "hoverClosestCartesian", "hoverCompareCartesian")
      )
  })
}


tab2_server <- function(input, output, session) {
  
  state_avg_lead <- mean(map_data$E_HOUAGE, na.rm = TRUE)
  state_avg_pm25 <- mean(map_data$exceed, na.rm = TRUE)
  
  output$hover_info_2 <- renderUI({
    p(style = "color: #999; font-style: italic; font-size: 13px;", 
      "Click on a tract to view details")
  })
  
  observeEvent(input$map_1_shape_click, {
    event <- input$map_1_shape_click
    if (!is.null(event$id)) {
      tract <- map_data[map_data$GEOID == event$id, ]
      if (nrow(tract) > 0) {
        
        leafletProxy("map_1") %>%
          clearGroup("highlight") %>%
          addPolygons(data = tract, group = "highlight",
                      fillColor = NA, fillOpacity = 0,
                      weight = 3, color = "#1a5276", opacity = 1)
        
        vars <- c("E_AFAM", "E_HISP", "oth.nonhis.wh", "E_DISABL", "E_AGE17", "E_AGE65", "E_UNEMP", "E_RENTER")
        rows <- lapply(vars, function(v) {
          is_selected <- (v == input$color_1)
          tags$tr(
            tags$td(style = paste0(
              "padding: 4px 0; font-size: 12px;",
              if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
            ), pop_labels[v]),
            tags$td(style = paste0(
              "text-align: right; padding: 4px 0; font-size: 12px;",
              if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
            ), paste0(round(tract[[v]][1], 1), "%"))
          )
        })
        output$hover_info_2 <- renderUI({
          tagList(
            tags$div(
              style = "margin-bottom: 8px;",
              tags$div(style = "font-size: 14px; font-weight: 600; color: #1a5276;",
                       paste("Tract", tract$NTAName[1])),
              tags$div(style = "font-size: 14px; color: #1a5276;",
                       tract$NAMELSADCO[1]),
              tags$a(href = "#",
                     onclick = "Shiny.setInputValue('reset_hover_2', Math.random())",
                     style = "font-size: 11px; color: #999; margin-top: 5px; display: inline-block;",
                     "Clear selection")
            ),
            tags$table(style = "width: 100%;", rows)
          )
        })
      }
    }
  })
  
  observeEvent(input$map_environment_shape_click, {
    event <- input$map_environment_shape_click
    if (!is.null(event$id)) {
      tract <- map_data[map_data$GEOID == event$id, ]
      if (nrow(tract) > 0) {
        
        leafletProxy("map_environment") %>%
          clearGroup("highlight") %>%
          addPolygons(data = tract, group = "highlight",
                      fillColor = NA, fillOpacity = 0,
                      weight = 3, color = "#1a5276", opacity = 1)
        
        pop_vars <- c("E_AFAM", "E_HISP", "oth.nonhis.wh", "E_DISABL", "E_AGE17", "E_AGE65", "E_UNEMP", "E_RENTER")
        pop_rows <- lapply(pop_vars, function(v) {
          is_selected <- (v == input$color_1)
          tags$tr(
            tags$td(style = paste0(
              "padding: 4px 0; font-size: 12px;",
              if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
            ), pop_labels[v]),
            tags$td(style = paste0(
              "text-align: right; padding: 4px 0; font-size: 12px;",
              if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
            ), paste0(round(tract[[v]][1], 1), "%"))
          )
        })
        
        env_vars <- c("E_HOUAGE", "exceed")
        env_rows <- lapply(env_vars, function(v) {
          is_selected <- (v == input$color_env)
          state_avg <- switch(v,
                              "E_HOUAGE" = state_avg_lead,
                              "exceed" = state_avg_pm25
          )
          unit <- if(v == "exceed") " days" else "%"
          tagList(
            tags$tr(
              tags$td(style = paste0(
                "padding: 4px 0; font-size: 12px;",
                if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
              ), env_labels[v]),
              tags$td(style = paste0(
                "text-align: right; padding: 4px 0; font-size: 12px;",
                if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
              ), paste0(round(tract[[v]][1], 1), unit))
            ),
            tags$tr(
              tags$td(style = "color: #999; padding: 2px 0 8px 0; font-size: 11px;", "State Average"),
              tags$td(style = "text-align: right; color: #999; padding: 2px 0 8px 0; font-size: 11px;", 
                      paste0(round(state_avg, 1), unit))
            )
          )
        })
        
        health_rows <- lapply(c("E_ASTHMA", "E_CANCER", "E_CHD", "E_DIABETES", "E_MHLTH"), function(v) {
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", health_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), "%"))
          )
        })
        
        climate_rows <- lapply(c("E_NEHD", "E_SMOKE"), function(v) {
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", climate_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), " days"))
          )
        })
        
        health_system_rows <- lapply(c("D_MEDICALPRO", "PR_INSURANCE", "PU_INSURANCE"), function(v) {
          unit <- if(v %in% c("PR_INSURANCE", "PU_INSURANCE")) "%" else ""
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", health_system_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), unit))
          )
        })
        
        output$hover_info_2 <- renderUI({
          tagList(
            tags$div(
              style = "margin-bottom: 8px;",
              tags$div(style = "font-size: 14px; font-weight: 600; color: #1a5276;",
                       paste("Tract", tract$NTAName[1])),
              tags$div(style = "font-size: 14px; color: #1a5276;",
                       tract$NAMELSADCO[1]),
              tags$a(href = "#",
                     onclick = "Shiny.setInputValue('reset_hover_2', Math.random())",
                     style = "font-size: 11px; color: #999; margin-top: 5px; display: inline-block;",
                     "Clear selection")
            ),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #1a5276; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Environmental Exposures"),
            tags$table(style = "width: 100%;", env_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Health Conditions"),
            tags$table(style = "width: 100%;", health_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Healthcare Access"),
            tags$table(style = "width: 100%;", health_system_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Climate"),
            tags$table(style = "width: 100%;", climate_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #1a5276; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Population"),
            tags$table(style = "width: 100%;", pop_rows)
          )
        })
      }
    }
  })
  
  observeEvent(input$reset_hover_2, {
    leafletProxy("map_1") %>% clearGroup("highlight")
    leafletProxy("map_environment") %>% clearGroup("highlight")
    output$hover_info_2 <- renderUI({
      p(style = "color: #999; font-style: italic; font-size: 13px;", 
        "Click on a tract to view details")
    })
  })
  
  output$map_1 <- renderLeaflet({
    
    valid_data <- map_data[!is.na(map_data[[input$color_1]]), ]
    pal <- colorQuantile(
      viridisLite::rocket(256)[60:256], 
      domain = valid_data[[input$color_1]], 
      n = 10, 
      reverse = T)
    
    leaflet(valid_data, options = leafletOptions(minZoom = 6, maxZoom = 18, zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({position: 'bottomleft'}).addTo(this);
      }") %>%
      addPolygons(data = mask, fillColor = "white", fillOpacity = 1,
                  stroke = FALSE, group = "mask") %>%
      setMaxBounds(lng1 = -91.6, lat1 = 36.9, lng2 = -87.0, lat2 = 42.6) %>%
      addSearchOSM(options = searchOptions(
        autoCollapse = F,
        collapsed = F,
        position = "topright",
        textPlaceholder = "Search address or ZIP...",
        zoom = 10,
        marker = list(
          circle = list(
            radius = 20,
            weight = 4,
            color = "#2FA4E7",
            stroke = TRUE,
            fill = FALSE
          )
        )
      )) %>%
      addPolygons(
        fillColor = ~pal(valid_data[[input$color_1]]),
        fillOpacity = 0.7, weight = 1, color = "#d0d0d0",
        layerId = ~GEOID,
        label = ~paste0(
          NTAName, " - ",
          pop_labels[input$color_1], ": ", round(valid_data[[input$color_1]], 2), "%"
        )
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = valid_data[[input$color_1]],
        title = "Decile group <br> (greater than X% of <br> other census tracts)"
      )  %>%
      
      setView(lng = -89.395242, lat = 40.630610, zoom = 7)
  })
  
  output$map_environment <- renderLeaflet({
    
    valid_data <- map_data[!is.na(map_data[[input$color_env]]), ]
      
    if (input$color_env == "E_HOUAGE") {
      pal <- colorQuantile("BuPu", valid_data[[input$color_env]], n = 10)
      legend_title <- "Decile group<br>(greater than X% of<br>other census tracts)"
      legend_suffix <- ""
    } else {
      pal <- colorBin("BuPu", domain = range(valid_data[["exceed"]], na.rm = TRUE), 
                      bins = seq(0, 50, 10))
      legend_title <- "Number of days (annual)"
      legend_suffix <- ""
    }
    
    leaflet(valid_data, options = leafletOptions(minZoom = 6, maxZoom = 18, zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({position: 'bottomleft'}).addTo(this);
      }") %>%
      addPolygons(data = mask, fillColor = "white", fillOpacity = 1,
                  stroke = FALSE, group = "mask") %>%
      setMaxBounds(lng1 = -91.6, lat1 = 36.9, lng2 = -87.0, lat2 = 42.6) %>%
        addSearchOSM(options = searchOptions(
          autoCollapse = F,
          collapsed = F,
          position = "topright",
          textPlaceholder = "Search address or ZIP...",
          zoom = 10,
          marker = list(
            circle = list(
              radius = 20,
              weight = 4,
              color = "#2FA4E7",
              stroke = TRUE,
              fill = FALSE
            )
          )
        )) %>%
        addPolygons(
          fillColor = ~pal(valid_data[[input$color_env]]),
          fillOpacity = 0.7, weight = 1, color = "#d0d0d0",
          layerId = ~GEOID,
          label = ~paste0(
            NTAName, " - ",
            env_labels[input$color_env], ": ", round(valid_data[[input$color_env]], 1),
            if(input$color_env == "exceed") " days" else "%"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = valid_data[[input$color_env]],
          title = legend_title,
          labFormat = labelFormat(suffix = legend_suffix)
        ) %>%
        setView(lng = -89.395242, lat = 40.630610, zoom = 7)
  })
}


tab3_server <- function(input, output, session) {
  
  state_avg_heat <- mean(map_data$E_NEHD, na.rm = TRUE)
  state_avg_smoke <- mean(map_data$E_SMOKE, na.rm = TRUE)
  
  output$hover_info_3 <- renderUI({
    p(style = "color: #999; font-style: italic; font-size: 13px;", 
      "Click on a tract to view details")
  })
  
  observeEvent(input$map_climate_shape_click, {
    event <- input$map_climate_shape_click
    if (!is.null(event$id)) {
      tract <- map_data[map_data$GEOID == event$id, ]
      if (nrow(tract) > 0) {
        
        leafletProxy("map_climate") %>%
          clearGroup("highlight") %>%
          addPolygons(data = tract, group = "highlight",
                      fillColor = NA, fillOpacity = 0,
                      weight = 3, color = "#0e7c7b", opacity = 1)
        
        updateSelectInput(session, "neighborhood_climate", selected = tract$NAMELSADCO[1])
        
        climate_vars <- c("E_NEHD", "E_SMOKE")
        climate_rows <- lapply(climate_vars, function(v) {
          is_selected <- (v == input$color_climate)
          state_avg <- if(v == "E_NEHD") state_avg_heat else state_avg_smoke
          tagList(
            tags$tr(
              tags$td(style = paste0(
                "padding: 4px 0; font-size: 12px;",
                if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
              ), climate_labels[v]),
              tags$td(style = paste0(
                "text-align: right; padding: 4px 0; font-size: 12px;",
                if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
              ), paste0(round(tract[[v]][1], 1), " days"))
            ),
            tags$tr(
              tags$td(style = "color: #999; padding: 2px 0 8px 0; font-size: 11px;", "State Average"),
              tags$td(style = "text-align: right; color: #999; padding: 2px 0 8px 0; font-size: 11px;", 
                      paste0(round(state_avg, 1), " days"))
            )
          )
        })
        
        health_rows <- lapply(c("E_ASTHMA", "E_CANCER", "E_CHD", "E_DIABETES", "E_MHLTH"), function(v) {
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", health_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), "%"))
          )
        })
        
        env_rows <- tagList(
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", env_labels["E_HOUAGE"]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract$E_HOUAGE[1], 1), "%"))
          ),
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", env_labels["exceed"]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract$exceed[1], 1), " days"))
          )
        )
        
        pop_rows <- lapply(c("E_AFAM", "E_HISP", "oth.nonhis.wh", "E_DISABL", "E_AGE17", "E_AGE65", "E_UNEMP", "E_RENTER"), function(v) {
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", pop_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), "%"))
          )
        })
        
        health_system_rows <- lapply(c("D_MEDICALPRO", "PR_INSURANCE", "PU_INSURANCE"), function(v) {
          unit <- if(v %in% c("PR_INSURANCE", "PU_INSURANCE")) "%" else ""
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", health_system_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), unit))
          )
        })
        
        output$hover_info_3 <- renderUI({
          tagList(
            tags$div(
              style = "margin-bottom: 8px;",
              tags$div(style = "font-size: 14px; font-weight: 600; color: #1a5276;",
                       paste("Tract", tract$NTAName[1])),
              tags$div(style = "font-size: 14px; color: #1a5276;",
                       tract$NAMELSADCO[1]),
              tags$a(href = "#",
                     onclick = "Shiny.setInputValue('reset_hover_3', Math.random())",
                     style = "font-size: 11px; color: #999; margin-top: 5px; display: inline-block;",
                     "Clear selection")
            ),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #1a5276; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Climate"),
            tags$table(style = "width: 100%;", climate_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Health Conditions"),
            tags$table(style = "width: 100%;", health_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Healthcare Access"),
            tags$table(style = "width: 100%;", health_system_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Environmental Exposures"),
            tags$table(style = "width: 100%;", env_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Population"),
            tags$table(style = "width: 100%;", pop_rows)
          )
        })
      }
    }
  })
  
  observeEvent(input$reset_hover_3, {
    leafletProxy("map_climate") %>% clearGroup("highlight")
    output$hover_info_3 <- renderUI({
      p(style = "color: #999; font-style: italic; font-size: 13px;", 
        "Click on a tract to view details")
    })
  })
  
  output$map_climate <- renderLeaflet({
    
    valid_data <- map_data[!is.na(map_data[[input$color_climate]]), ]
    pal <- colorNumeric("OrRd", valid_data[[input$color_climate]], n = 5)
    
    leaflet(valid_data, options = leafletOptions(minZoom = 6, maxZoom = 18, zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({position: 'bottomleft'}).addTo(this);
      }") %>%
      addPolygons(data = mask, fillColor = "white", fillOpacity = 1,
                  stroke = FALSE, group = "mask") %>%
      setMaxBounds(lng1 = -91.6, lat1 = 36.9, lng2 = -87.0, lat2 = 42.6) %>%
      addSearchOSM(options = searchOptions(
        autoCollapse = F,
        collapsed = F,
        position = "topright",
        textPlaceholder = "Search address or ZIP...",
        zoom = 10,
        marker = list(
          circle = list(
            radius = 20,
            weight = 4,
            color = "#0e7c7b",
            stroke = TRUE,
            fill = FALSE
          )
        )
      )) %>%
      addPolygons(
        fillColor = ~pal(valid_data[[input$color_climate]]),
        fillOpacity = 0.7, weight = 1, color = "#d0d0d0",
        layerId = ~GEOID,
        label = ~paste0(
          NTAName, " - ",
          climate_labels[input$color_climate], ": ", round(valid_data[[input$color_climate]], 1), " days"
        )
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = valid_data[[input$color_climate]],
        title = "Number of days (annual average)"
      )  %>%
      
      setView(lng = -89.395242, lat = 40.630610, zoom = 7)
  })
  
  output$climateChart <- renderPlotly({
    chart_data <- map_data[map_data$NAMELSADCO == input$neighborhood_climate, ]
    
    climate_data <- st_drop_geometry(chart_data)
    climate_data <- climate_data[, c("E_NEHD","E_SMOKE")]
    
    county_avgs <- map_data %>%
      st_drop_geometry() %>%
      group_by(NAMELSADCO) %>%
      summarise(across(c(E_NEHD, E_SMOKE), mean, na.rm = TRUE))
    
    state_avg <- colMeans(county_avgs[, -1], na.rm = TRUE)
    
    climate_data <- colMeans(climate_data)
    climate_data <- as.data.frame(climate_data)
    climate_data <- cbind(Category = rownames(climate_data), Percentage = climate_data[, 1])
    rownames(climate_data) <- NULL
    
    climate_data <- as.data.frame(climate_data)
    climate_data$Category <- climate_labels[climate_data$Category] 
    climate_data$CountyAvg <- as.numeric(state_avg)
    
    plot_ly(data = as.data.frame(climate_data), x = ~Category, y = ~as.numeric(Percentage), type = 'bar', color = ~Category, colors = "Dark2",
            text = ~paste0(Category, ": ", round(as.numeric(Percentage), 1), " days"),
            hoverinfo = "text") %>%
      
      add_markers(x = ~Category, y = ~CountyAvg, name = "Avg Across Counties",
                  marker = list(symbol = "diamond", size = 10, color = "#1a5276",
                                line = list(width = 1, color = "white")),
                  text = ~paste0("Avg Across Counties: ", round(CountyAvg, 1), " days"),
                  hoverinfo = "text",
                  showlegend = FALSE) %>%
      
      layout(title = paste("Climate Events -", input$neighborhood_climate),
             xaxis = list(title = "Climate Events", showticklabels = FALSE),
             yaxis = list(title = " Annual number of days", range = c(0, 30), tickvals = seq(0, 30, 3)),
             bargap = 0.5,
             annotations = list(
               list(x = 1, y = 1, xref = "paper", yref = "paper",
                    xanchor = "right", yanchor = "top",
                    text = "◆ = Avg Across Counties", showarrow = FALSE,
                    font = list(size = 12, color = "#1a5276"))
             )) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
                                   "hoverClosestCartesian", "hoverCompareCartesian")
      )
  })
}


tab4_server <- function(input, output, session) {
  
  state_avg_asthma <- mean(map_data$E_ASTHMA, na.rm = TRUE)
  state_avg_cancer <- mean(map_data$E_CANCER, na.rm = TRUE)
  state_avg_chd <- mean(map_data$E_CHD, na.rm = TRUE)
  state_avg_diabetes <- mean(map_data$E_DIABETES, na.rm = TRUE)
  state_avg_mhlth <- mean(map_data$E_MHLTH, na.rm = TRUE)
  
  output$hover_info_4 <- renderUI({
    p(style = "color: #999; font-style: italic; font-size: 13px;", 
      "Click on a tract to view details")
  })
  
  observeEvent(input$map_health_shape_click, {
    event <- input$map_health_shape_click
    if (!is.null(event$id)) {
      tract <- map_data[map_data$GEOID == event$id, ]
      if (nrow(tract) > 0) {
        
        leafletProxy("map_health") %>%
          clearGroup("highlight") %>%
          addPolygons(data = tract, group = "highlight",
                      fillColor = NA, fillOpacity = 0,
                      weight = 3, color = "#1a5276", opacity = 1)
        
        updateSelectInput(session, "neighborhood_health", selected = tract$NAMELSADCO[1])
        
        health_vars <- c("E_ASTHMA", "E_CANCER", "E_CHD", "E_DIABETES", "E_MHLTH")
        health_rows <- lapply(health_vars, function(v) {
          is_selected <- (v == input$color_health)
          state_avg <- switch(v,
                              "E_ASTHMA" = state_avg_asthma,
                              "E_CANCER" = state_avg_cancer,
                              "E_CHD" = state_avg_chd,
                              "E_DIABETES" = state_avg_diabetes,
                              "E_MHLTH" = state_avg_mhlth
          )
          tagList(
            tags$tr(
              tags$td(style = paste0(
                "padding: 4px 0; font-size: 12px;",
                if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
              ), health_labels[v]),
              tags$td(style = paste0(
                "text-align: right; padding: 4px 0; font-size: 12px;",
                if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
              ), paste0(round(tract[[v]][1], 1), "%"))
            ),
            tags$tr(
              tags$td(style = "color: #999; padding: 2px 0 8px 0; font-size: 11px;", "State Average"),
              tags$td(style = "text-align: right; color: #999; padding: 2px 0 8px 0; font-size: 11px;", 
                      paste0(round(state_avg, 1), "%"))
            )
          )
        })
        
        climate_rows <- lapply(c("E_NEHD", "E_SMOKE"), function(v) {
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", climate_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), " days"))
          )
        })
        
        env_rows <- tagList(
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", env_labels["E_HOUAGE"]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract$E_HOUAGE[1], 1), "%"))
          ),
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", env_labels["exceed"]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract$exceed[1], 1), " days"))
          )
        )
        
        pop_rows <- lapply(c("E_AFAM", "E_HISP", "oth.nonhis.wh", "E_DISABL", "E_AGE17", "E_AGE65", "E_UNEMP", "E_RENTER"), function(v) {
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", pop_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), "%"))
          )
        })
        
        health_system_rows <- lapply(c("D_MEDICALPRO", "PR_INSURANCE", "PU_INSURANCE"), function(v) {
          unit <- if(v %in% c("PR_INSURANCE", "PU_INSURANCE")) "%" else ""
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", health_system_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), unit))
          )
        })
        
        output$hover_info_4 <- renderUI({
          tagList(
            tags$div(
              style = "margin-bottom: 8px;",
              tags$div(style = "font-size: 14px; font-weight: 600; color: #1a5276;",
                       paste("Tract", tract$NTAName[1])),
              tags$div(style = "font-size: 14px; color: #1a5276;",
                       tract$NAMELSADCO[1]),
              tags$a(href = "#",
                     onclick = "Shiny.setInputValue('reset_hover_4', Math.random())",
                     style = "font-size: 11px; color: #999; margin-top: 5px; display: inline-block;",
                     "Clear selection")
            ),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #1a5276; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Health Conditions"),
            tags$table(style = "width: 100%;", health_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Healthcare Access"),
            tags$table(style = "width: 100%;", health_system_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Climate"),
            tags$table(style = "width: 100%;", climate_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Environmental Exposures"),
            tags$table(style = "width: 100%;", env_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Population"),
            tags$table(style = "width: 100%;", pop_rows)
          )
        })
      }
    }
  })
  
  observeEvent(input$reset_hover_4, {
    leafletProxy("map_health") %>% clearGroup("highlight")
    output$hover_info_4 <- renderUI({
      p(style = "color: #999; font-style: italic; font-size: 13px;", 
        "Click on a tract to view details")
    })
  })
  
  output$map_health <- renderLeaflet({
    
    valid_data <- map_data[!is.na(map_data[[input$color_health]]), ]
    pal <- colorQuantile(
      viridisLite::magma(256)[60:256],
      valid_data[[input$color_health]],
      n = 10,
      na.color = "#808080",
      alpha = FALSE,
      reverse = TRUE,
      right = FALSE
    )
    
    leaflet(valid_data, options = leafletOptions(minZoom = 6, maxZoom = 18, zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({position: 'bottomleft'}).addTo(this);
      }") %>%
      addPolygons(data = mask, fillColor = "white", fillOpacity = 1,
                  stroke = FALSE, group = "mask") %>%
      setMaxBounds(lng1 = -91.6, lat1 = 36.9, lng2 = -87.0, lat2 = 42.6) %>%
      addSearchOSM(options = searchOptions(
        autoCollapse = F,
        collapsed = F,
        position = "topright",
        textPlaceholder = "Search address or ZIP...",
        zoom = 10,
        marker = list(
          circle = list(
            radius = 20,
            weight = 4,
            color = "#2FA4E7",
            stroke = TRUE,
            fill = FALSE
          )
        )
      )) %>%
      addPolygons(
        fillColor = ~pal(valid_data[[input$color_health]]),
        fillOpacity = 0.5, weight = 1, color = "#d0d0d0",
        layerId = ~GEOID,
        label = ~paste0(
          NTAName, " - ",
          health_labels[input$color_health], ": ", round(valid_data[[input$color_health]], 1), "%"
        )
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = valid_data[[input$color_health]],
        title = "Decile group <br>
        (greater than X% of <br> other census tracts)"
      )  %>%
      
      setView(lng = -89.395242, lat = 40.630610, zoom = 7)
  })
  
  output$healthChart <- renderPlotly({
    chart_data <- map_data[map_data$NAMELSADCO == input$neighborhood_health, ]
    
    health_data <- st_drop_geometry(chart_data)
    
    health_data <- health_data[, c("E_ASTHMA","E_CANCER","E_CHD","E_DIABETES","E_MHLTH")]
    
    county_avgs <- map_data %>%
      st_drop_geometry() %>%
      group_by(NAMELSADCO) %>%
      summarise(across(c(E_ASTHMA, E_CANCER, E_CHD, E_DIABETES, E_MHLTH), mean, na.rm = TRUE))
    
    state_avg <- colMeans(county_avgs[, -1], na.rm = TRUE)
    
    health_data <- t(health_data)
    health_data <- as.data.frame(health_data)
    health_data <- cbind(Category = rownames(health_data), Percentage = health_data[, 1])
    rownames(health_data) <- NULL
    
    health_data <- as.data.frame(health_data)
    health_data$Category <- health_labels[health_data$Category]
    
    health_data$CountyAvg <- as.numeric(state_avg)
    
    plot_ly(data = as.data.frame(health_data), x = ~Category, y = ~as.numeric(Percentage), type = 'bar', color = ~Category, colors = "Dark2",
            text = ~paste0(Category, ": ", round(as.numeric(Percentage), 1), "%"),
            hoverinfo = "text") %>%
      
      add_markers(x = ~Category, y = ~CountyAvg, name = "Avg Across Counties",
                  marker = list(symbol = "diamond", size = 12, color = "#1a5276",
                                line = list(width = 1, color = "white")),
                  text = ~paste0("Avg Across Counties: ", round(CountyAvg, 1), "%"),
                  hoverinfo = "text",
                  showlegend = FALSE) %>%
      
      layout(title = paste("Health Conditions -", input$neighborhood_health),
             xaxis = list(title = "Health Conditions", showticklabels = FALSE),
             yaxis = list(title = "Percentage", range = c(0, 50), tickvals = seq(0, 50, 5)),
             annotations = list(
               list(x = 1, y = 1, xref = "paper", yref = "paper",
                    xanchor = "right", yanchor = "top",
                    text = "◆ = Avg Across Counties", showarrow = FALSE,
                    font = list(size = 12, color = "#1a5276"))
             )) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
                                   "hoverClosestCartesian", "hoverCompareCartesian")
      )
  })
}


tab6_server <- function(input, output, session) {
  
  state_avg_medical <- mean(map_data$D_MEDICALPRO, na.rm = TRUE)
  state_avg_private <- mean(map_data$PR_INSURANCE, na.rm = TRUE)
  state_avg_public <- mean(map_data$PU_INSURANCE, na.rm = TRUE)
  
  output$hover_info_6 <- renderUI({
    p(style = "color: #999; font-style: italic; font-size: 13px;", 
      "Click on a tract to view details")
  })
  
  observeEvent(input$map_health_system_shape_click, {
    event <- input$map_health_system_shape_click
    if (!is.null(event$id)) {
      tract <- map_data[map_data$GEOID == event$id, ]
      if (nrow(tract) > 0) {
        
        leafletProxy("map_health_system") %>%
          clearGroup("highlight") %>%
          addPolygons(data = tract, group = "highlight",
                      fillColor = NA, fillOpacity = 0,
                      weight = 3, color = "#1a5276", opacity = 1)
        
        updateSelectInput(session, "neighborhood_health_system", selected = tract$NAMELSADCO[1])
        
        health_system_vars <- c("D_MEDICALPRO", "PR_INSURANCE", "PU_INSURANCE")
        health_system_rows <- lapply(health_system_vars, function(v) {
          is_selected <- (v == input$color_health_system)
          state_avg <- switch(v,
                              "D_MEDICALPRO" = state_avg_medical,
                              "PR_INSURANCE" = state_avg_private,
                              "PU_INSURANCE" = state_avg_public
          )

          unit <- if(v %in% c("PR_INSURANCE", "PU_INSURANCE")) "%" else ""
          tagList(
            tags$tr(
              tags$td(style = paste0(
                "padding: 4px 0; font-size: 12px;",
                if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
              ), health_system_labels[v]),
              tags$td(style = paste0(
                "text-align: right; padding: 4px 0; font-size: 12px;",
                if(is_selected) " color: #1a5276; font-weight: bold;" else " color: #666;"
              ), paste0(round(tract[[v]][1], 1), unit))
            ),
            tags$tr(
              tags$td(style = "color: #999; padding: 2px 0 8px 0; font-size: 11px;", "State Average"),
              tags$td(style = "text-align: right; color: #999; padding: 2px 0 8px 0; font-size: 11px;", 
                      paste0(round(state_avg, 1), unit))
            )
          )
        })
        
        health_rows <- lapply(c("E_ASTHMA", "E_CANCER", "E_CHD", "E_DIABETES", "E_MHLTH"), function(v) {
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", health_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), "%"))
          )
        })
        
        climate_rows <- lapply(c("E_NEHD", "E_SMOKE"), function(v) {
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", climate_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), " days"))
          )
        })
        
        env_rows <- tagList(
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", env_labels["E_HOUAGE"]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract$E_HOUAGE[1], 1), "%"))
          ),
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", env_labels["exceed"]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract$exceed[1], 1), " days"))
          )
        )
        
        pop_rows <- lapply(c("E_AFAM", "E_HISP", "oth.nonhis.wh", "E_DISABL", "E_AGE17", "E_AGE65", "E_UNEMP", "E_RENTER"), function(v) {
          tags$tr(
            tags$td(style = "padding: 4px 0; font-size: 12px; color: #666;", pop_labels[v]),
            tags$td(style = "text-align: right; padding: 4px 0; font-size: 12px; color: #666;", 
                    paste0(round(tract[[v]][1], 1), "%"))
          )
        })
        
        output$hover_info_6 <- renderUI({
          tagList(
            tags$div(
              style = "margin-bottom: 8px;",
              tags$div(style = "font-size: 14px; font-weight: 600; color: #1a5276;",
                       paste("Tract", tract$NTAName[1])),
              tags$div(style = "font-size: 14px; color: #1a5276;",
                       tract$NAMELSADCO[1]),
              tags$a(href = "#",
                     onclick = "Shiny.setInputValue('reset_hover_6', Math.random())",
                     style = "font-size: 11px; color: #999; margin-top: 5px; display: inline-block;",
                     "Clear selection")
            ),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #1a5276; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Healthcare Access"),
            tags$table(style = "width: 100%;", health_system_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Health Conditions"),
            tags$table(style = "width: 100%;", health_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Climate"),
            tags$table(style = "width: 100%;", climate_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Environmental Exposures"),
            tags$table(style = "width: 100%;", env_rows),
            
            tags$div(style = "font-size: 12px; font-weight: 600; color: #888; margin-top: 10px; border-bottom: 1px solid #ddd; padding-bottom: 4px;", 
                     "Population"),
            tags$table(style = "width: 100%;", pop_rows)
          )
        })
      }
    }
  })
  
  observeEvent(input$reset_hover_6, {
    leafletProxy("map_health_system") %>% clearGroup("highlight")
    output$hover_info_6 <- renderUI({
      p(style = "color: #999; font-style: italic; font-size: 13px;", 
        "Click on a tract to view details")
    })
  })
  
  output$map_health_system <- renderLeaflet({
    
    valid_data <- map_data[!is.na(map_data[[input$color_health_system]]), ]
    
    if (input$color_health_system == "PR_INSURANCE") {
      
      pal <- colorNumeric("YlGnBu", domain = c(0, 100))
      legend_title <- health_system_labels[[input$color_health_system]]
      
      leaflet(valid_data, options = leafletOptions(minZoom = 6, maxZoom = 18, zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({position: 'bottomleft'}).addTo(this);
      }") %>%
        addPolygons(data = mask, fillColor = "white", fillOpacity = 1,
                    stroke = FALSE, group = "mask") %>%
        setMaxBounds(lng1 = -91.6, lat1 = 36.9, lng2 = -87.0, lat2 = 42.6) %>%
        addSearchOSM(options = searchOptions(
          autoCollapse = F,
          collapsed = F,
          position = "topright",
          textPlaceholder = "Search address or ZIP...",
          zoom = 10,
          marker = list(
            circle = list(
              radius = 20,
              weight = 4,
              color = "#2FA4E7",
              stroke = TRUE,
              fill = FALSE
            )
          )
        )) %>%
        addPolygons(
          fillColor = ~pal(valid_data[[input$color_health_system]]),
          fillOpacity = 0.7, weight = 1, color = "#d0d0d0",
          layerId = ~GEOID,
          label = ~paste0(
            NTAName, " - ",
            health_system_labels[input$color_health_system], ": ", round(valid_data[[input$color_health_system]], 1), "%"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = valid_data[[input$color_health_system]],
          title = legend_title,
          labFormat = labelFormat(suffix = "%")
        ) %>%
        setView(lng = -89.395242, lat = 40.630610, zoom = 7)
    
      
    } else if (input$color_health_system == "PU_INSURANCE") {
      
      pal <- colorNumeric("OrRd", domain = c(0, 100))
      legend_title <- health_system_labels[[input$color_health_system]]
      
      leaflet(valid_data, options = leafletOptions(minZoom = 6, maxZoom = 18, zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({position: 'bottomleft'}).addTo(this);
      }") %>%
        addPolygons(data = mask, fillColor = "white", fillOpacity = 1,
                    stroke = FALSE, group = "mask") %>%
        setMaxBounds(lng1 = -91.6, lat1 = 36.9, lng2 = -87.0, lat2 = 42.6) %>%
        addSearchOSM(options = searchOptions(
          autoCollapse = F,
          collapsed = F,
          position = "topright",
          textPlaceholder = "Search address or ZIP...",
          zoom = 10,
          marker = list(
            circle = list(
              radius = 20,
              weight = 4,
              color = "#2FA4E7",
              stroke = TRUE,
              fill = FALSE
            )
          )
        )) %>%
        addPolygons(
          fillColor = ~pal(valid_data[[input$color_health_system]]),
          fillOpacity = 0.7, weight = 1, color = "#d0d0d0",
          layerId = ~GEOID,
          label = ~paste0(
            NTAName, " - ",
            health_system_labels[input$color_health_system], ": ", round(valid_data[[input$color_health_system]], 1), "%"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = valid_data[[input$color_health_system]],
          title = legend_title,
          labFormat = labelFormat(suffix = "%")
        ) %>%
        setView(lng = -89.395242, lat = 40.630610, zoom = 7)
        
    } else {
      
      pal <- colorNumeric("YlGnBu", domain = c(0, max(valid_data[[input$color_health_system]], na.rm = TRUE)))
      legend_title <- health_system_labels[[input$color_health_system]]
      
      unit <- if(input$color_health_system %in% c("PR_INSURANCE", "PU_INSURANCE")) "%" else ""
      
      leaflet(valid_data, options = leafletOptions(minZoom = 6, maxZoom = 18, zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({position: 'bottomleft'}).addTo(this);
      }") %>%
        addPolygons(data = mask, fillColor = "white", fillOpacity = 1,
                    stroke = FALSE, group = "mask") %>%
        setMaxBounds(lng1 = -91.6, lat1 = 36.9, lng2 = -87.0, lat2 = 42.6) %>%
        addSearchOSM(options = searchOptions(
          autoCollapse = F,
          collapsed = F,
          position = "topright",
          textPlaceholder = "Search address or ZIP...",
          zoom = 10,
          marker = list(
            circle = list(
              radius = 20,
              weight = 4,
              color = "#2FA4E7",
              stroke = TRUE,
              fill = FALSE
            )
          )
        )) %>%
        addPolygons(
          fillColor = ~pal(valid_data[[input$color_health_system]]),
          fillOpacity = 0.7, weight = 1, color = "#d0d0d0",
          layerId = ~GEOID,
          label = ~paste0(
            NTAName, " - ",
            health_system_labels[input$color_health_system], ": ", round(valid_data[[input$color_health_system]], 1), unit
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = valid_data[[input$color_health_system]],
          title = "Practicing Medical<br>Professionals (per 10,000)"
        ) %>%
        setView(lng = -89.395242, lat = 40.630610, zoom = 7)
    }
  })
  
  output$healthDensityChart <- renderPlotly({
    chart_data <- map_data[map_data$NAMELSADCO == input$neighborhood_health_system, ]
    
    health_data <- st_drop_geometry(chart_data)
    health_data <- health_data[, "D_MEDICALPRO", drop = FALSE]
    
    county_avgs <- map_data %>%
      st_drop_geometry() %>%
      group_by(NAMELSADCO) %>%
      summarise(across(D_MEDICALPRO, mean, na.rm = TRUE))
    
    county_avg <- colMeans(county_avgs[, -1], na.rm = TRUE)
    
    health_data <- colMeans(health_data, na.rm = TRUE)
    health_data <- as.data.frame(health_data)
    health_data <- cbind(Category = rownames(health_data), Value = health_data[, 1])
    rownames(health_data) <- NULL
    
    health_data <- as.data.frame(health_data)
    health_data$Category <- "Practicing Medical Professionals"
    health_data$CountyAvg <- as.numeric(county_avg)
    
    plot_ly(data = health_data, x = ~Category, y = ~as.numeric(Value), type = 'bar', color = ~Category, colors = "Paired", 
            text = ~paste0(Category, ": ", round(as.numeric(Value), 1)),
            hoverinfo = "text") %>%
      add_markers(x = ~Category, y = ~CountyAvg, name = "Avg Across Counties",
                  marker = list(symbol = "diamond", size = 12, color = "#1a5276",
                                line = list(width = 1, color = "white")),
                  text = ~paste0("Avg Across Counties: ", round(CountyAvg, 1)),
                  hoverinfo = "text",
                  showlegend = FALSE) %>%
      layout(title = list(text = "Practicing Medical Professionals", font = list(size = 14)),
             xaxis = list(title = "Medical Professionals", showticklabels = FALSE),
             yaxis = list(title = "Per 10,000 people", range = c(0, 30), tickvals = seq(0, 30, 5)),
             bargap = 0.5,
             margin = list(b = 57),
             annotations = list(
               list(x = 1, y = 1, xref = "paper", yref = "paper",
                    xanchor = "right", yanchor = "top",
                    text = "◆ = Avg Across Counties", showarrow = FALSE,
                    font = list(size = 11, color = "#1a5276"))
             )) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
                                   "hoverClosestCartesian", "hoverCompareCartesian")
      )
  })
  
  output$healthInsuranceChart <- renderPlotly({
    chart_data <- map_data[map_data$NAMELSADCO == input$neighborhood_health_system, ]
    
    insurance_data <- st_drop_geometry(chart_data)
    insurance_data <- insurance_data[, c("PR_INSURANCE", "PU_INSURANCE")]
    
    county_avgs <- map_data %>%
      st_drop_geometry() %>%
      group_by(NAMELSADCO) %>%
      summarise(across(c(PR_INSURANCE, PU_INSURANCE), mean, na.rm = TRUE))
    
    county_avg <- colMeans(county_avgs[, -1], na.rm = TRUE)
    
    insurance_data <- colMeans(insurance_data, na.rm = TRUE)
    insurance_data <- as.data.frame(insurance_data)
    insurance_data <- cbind(Category = rownames(insurance_data), Value = insurance_data[, 1])
    rownames(insurance_data) <- NULL
    
    insurance_data <- as.data.frame(insurance_data)
    insurance_data$Category <- c("Private Insurance", "Public Insurance")
    insurance_data$CountyAvg <- as.numeric(county_avg)
    
    plot_ly(data = insurance_data, x = ~Category, y = ~as.numeric(Value), type = 'bar', color = ~Category, colors = "Dark2",
            text = ~paste0(Category, ": ", round(as.numeric(Value), 1), "%"),
            hoverinfo = "text") %>%
      add_markers(x = ~Category, y = ~CountyAvg, name = "Avg Across Counties",
                  marker = list(symbol = "diamond", size = 12, color = "#1a5276",
                                line = list(width = 1, color = "white")),
                  text = ~paste0("Avg Across Counties: ", round(CountyAvg, 1), "%"),
                  hoverinfo = "text",
                  showlegend = FALSE) %>%
      layout(title = list(text = "Health Insurance", font = list(size = 14)),
             xaxis = list(title = "", showticklabels = FALSE),
             yaxis = list(title = "Percentage", range = c(0, 100), tickvals = seq(0, 100, 10)),
             bargap = 0.5,
             legend = list(orientation = "h", x = 0, y = -0.05, xanchor = "left"),
             annotations = list(
               list(x = 1, y = 1, xref = "paper", yref = "paper",
                    xanchor = "right", yanchor = "top",
                    text = "◆ = Avg Across Counties", showarrow = FALSE,
                    font = list(size = 11, color = "#1a5276"))
             )) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
                                   "hoverClosestCartesian", "hoverCompareCartesian")
      )
  })
}

tab5a_server <- function(input, output, session) {
  
}

tab5b_server <- function(input, output, session) {
  
}

tab5c_server <- function(input, output, session) {
  
}
  
# Main UI & Server ----

## ui ----
ui <- fluidPage(
  
  shinyjs::useShinyjs(),   
  
  theme = bs_theme(
    version = 5,
    bootswatch = "cerulean"
  ),
  
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Nunito+Sans:wght@400;600;700&display=swap", rel = "stylesheet")
  ),
  
  tags$style(HTML("
    body, .well, .sidebar, p, h1, h2, h3, h4, h5, h6, table, .form-control, .btn, .nav-tabs, .nav-pills, label, select {
      font-family: 'Nunito Sans', sans-serif;
    }
    .well {
      min-height: 100vh;
      background-color: #ffffff;
      border-left: 3px solid #2FA4E7;
    }
    .nav-tabs > li > a {
      font-size: 17px;
      padding: 8px 14px;
      color: #1a5276;
      font-weight: 600;
    }
    .nav-tabs {
      margin-top: 10px;
      margin-bottom: 15px;
    }
    .leaflet-container {
      background: #ffffff !important;
    }
    .leaflet-control-search .search-input {
      height: 33px !important;
      width: 260px !important;
      font-size: 14px !important;
      border: 2px solid #1a5276 !important;
    }
     .leaflet-control-search .search-cancel {
      opacity: 0 !important;
      pointer-events: none !important;
      position: absolute !important;
      right: -9999px !important;
    }
     .leaflet-control-search .search-button {
      width: 30px !important;
      height: 25px !important;
    }
    .leaflet-control-search .search-input:focus {
      border-color: #2FA4E7 !important;
      outline: none !important;
      box-shadow: 0 0 3px #2FA4E7 !important;
    }
  ")),
  
  div(id = "landing_page",
      style = "min-height: 100vh; display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 40px; text-align: center;",
      
      img(src = "logo.svg", height = "80px", style = "margin-bottom: 20px;"),
      
      tags$h2(style = "color: #1a5276; font-weight: 700;",
              "Illinois Community Health Dashboard"),
      
      tags$p(style = "font-size: 16px; color: #555; max-width: 600px; margin-bottom: 25px;",
             "Explore health and environment data across the state of Illinois through interactive maps and charts."),
      
      div(style = "margin-bottom: 30px; border-radius: 8px; overflow: hidden; box-shadow: 0 2px 10px rgba(0,0,0,0.15); width: 100%; max-width: 500px;",
          leafletOutput("landing_map", height = "250px", width = "100%")
      ),
      
      tags$a("Enter Dashboard",
             href = "#",
             onclick = "Shiny.setInputValue('enter_app', Math.random())",
             style = "background-color: #0e7c7b; color: white; padding: 14px 45px; font-size: 18px; font-weight: 700; border-radius: 5px; text-decoration: none; box-shadow: 0 3px 8px rgba(0,0,0,0.25); display: inline-block;")
  ),
  
  shinyjs::hidden(
    div(id = "main_app",
        
        style = "margin-top: 18px; margin-left: 15px;",
        
        titlePanel(
          div(
            style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
            span("Illinois Community Health Dashboard"),
            img(src = "logo.svg", height = "40px")
          )
        ),
        
        tabsetPanel(
          id = "main_tabs",
          tab5a_ui,
          tab1_ui,
          tab4_ui,
          tab6_ui,
          tab3_ui,
          tab2_ui,
          navbarMenu("About", tab5b_ui,
          tab5c_ui)
        )
    )
  )
)


server <- function(input, output, session) {
  
  output$landing_map <- renderLeaflet({
    valid_data <- map_data[!is.na(map_data$E_HOUAGE), ]
    pal <- colorBin("GnBu", domain = c(0, 100),
                    bins = seq(0, 100, 10))
    
    leaflet(valid_data, options = leafletOptions(
      zoomControl = FALSE, dragging = FALSE,
      scrollWheelZoom = FALSE, doubleClickZoom = FALSE
    )) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = mask, fillColor = "white", fillOpacity = 1,
                  stroke = FALSE) %>%
      addPolygons(fillColor = ~pal(E_HOUAGE), fillOpacity = 0.7,
                  weight = 0.5, color = "white") %>%
      setView(lng = -89.395242, lat = 40.630610, zoom = 7)
  })
  
  
  observeEvent(input$enter_app, {
    shinyjs::hide("landing_page")
    shinyjs::show("main_app")
    shinyjs::runjs("$(window).trigger('resize');")
  })
  
  tab5a_server(input, output, session)
  tab1_server(input, output, session)
  tab2_server(input, output, session)
  tab3_server(input, output, session)
  tab4_server(input, output, session)
  tab6_server(input, output, session)
  tab5b_server(input, output, session)
}


# Run App ----
shinyApp(ui = ui, server = server)