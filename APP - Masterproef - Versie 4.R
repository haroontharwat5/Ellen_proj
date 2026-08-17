# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(readxl)
library(readr)
library(lubridate)
library(scales)
library(RColorBrewer)
library(shinyWidgets)
library(grid)
library(data.table)
library(ggnewscale)
library(DT)
library(magick)


tijd_van_dag <- function(datetime) {
  as.POSIXct(paste("2000-01-01", format(datetime, "%H:%M:%S")),
             format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}


MIN_INTERACTIE_DUUR_SEC <- 30  #ondergrens tijd samen om als interactie gezien te worden in seconden


#min marge voor coordinaten op de plattegrond
MARGE_X_PIXELS <- 30  # marge links en rechts
MARGE_Y_PIXELS <- 60  # marge boven en onder


#initiele voorbeeld 
Ward_Floor_Plan_Details_init <- read_excel(
  "Ward_Floor_Plan_Details.xlsx",
  sheet = "Ward Floor Plan", range = "A5:I39"
) %>%
  rename(
    location_id   = `Location ID`,
    room_name     = `Room Name`,
    room_code     = `Room Code`,
    location_type = `Location Type`,
    patient_id    = `Patient ID`
  )

data_init <- read_csv("rtls (S1_Mini_Seed_42 - Mini Dataset (Limited Nurses & 1 hour shift)).csv")


data_joined_init <- left_join(data_init, Ward_Floor_Plan_Details_init, by = "location_id")
data_joined_init$location_name <- NULL
data_joined_init$room_code     <- NULL
data_joined_init$location_code <- NULL
data_joined_init$room_name     <- NULL

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Exploratie verplaatsingsgegevens"),
                
                sidebarLayout( fluid = F,
                               sidebarPanel( width = 3, #add filters
                                             checkboxGroupInput( # asset type
                                               "asset_types", "Select asset type"),
                                             
                                             pickerInput( # specifieke asset
                                               "sel_asset", "Select asset",
                                               choices  = c("All" = "all", sort(unique(data_joined_init$asset_id))),
                                               selected = "all",
                                               multiple = TRUE),
                                             
                                             dateRangeInput( #datum
                                               "datum_filter",
                                               label      = "Datumbereik",
                                               start      = as.Date(min(data_joined_init$start_time)),
                                               end        = as.Date(max(data_joined_init$start_time)),
                                               min        = as.Date(min(data_joined_init$start_time)),
                                               max        = as.Date(max(data_joined_init$start_time)),
                                               language   = "nl",
                                               separator  = " tot "
                                             ),
                                             
                                             sliderInput( #tijdsslider
                                               "uur_filter",
                                               label = "Tijdstip van de dag",
                                               min   = tijd_van_dag(min(data_joined_init$start_time)),
                                               max   = tijd_van_dag(max(data_joined_init$start_time)),
                                               value = c(tijd_van_dag(min(data_joined_init$start_time)),
                                                         tijd_van_dag(max(data_joined_init$start_time))),
                                               timeFormat = "%H:%M:%S",
                                               step  = 60  # stappen van 1 minuut
                                             ),
                                             
                                             hr(),
                                            
                                             sliderInput( #minimale verblijfsduur
                                               "min_duur_filter",
                                               label = "Minimale verblijfsduur (sec)",
                                               min   = 0,
                                               max   = max(60, ceiling(max(data_joined_init$duration_sec, na.rm = TRUE))),
                                               value = 10,
                                               step  = 1
                                             ),
                                             
                                             hr(),
                                             
                                             radioButtons( #alle verplaatsingen vs enkel interacties
                                               "interactie_filter",
                                               label   = "Welke verplaatsingen tonen?",
                                               choices = c("Alle verplaatsingen"           = "alle",
                                                           "Interacties" = "samen_bewegen"),
                                               selected = "alle"
                                             ),
                                             
                                             hr(),
                                             
                                             checkboxInput( #beperk assets 
                                               "limiteer_assets", 
                                               "Beperk aantal getoonde assets", 
                                               value = FALSE
                                              ),
                                             
                                             conditionalPanel( #min of max aantal beperkte assets
                                               condition = "input.limiteer_assets == true",
                                               numericInput("max_aantal_assets", "Max. aantal assets", value = 10, min = 1, step = 1),
                                               radioButtons(
                                                 "asset_selectie_type",
                                                 label   = NULL,
                                                 choices = c("Meest bewegend" = "meest", "Minst bewegend" = "minst"),
                                                 selected = "meest"
                                               )
                                             )
                                             
                               ), #sidebarpanel
                               
                               mainPanel( #show visualisations
                                 tabsetPanel(
                                   tabPanel("Data input",
                                            column(9,
                                                   h4("Plattegrond (Excel)"),
                                                   fileInput("grondplan", "Choose Excel File", accept = ".xlsx"),
                                                   tableOutput("grondplan"),
                                                   hr(),
                                                   h4("Verplaatsingsgegevens (CSV)"),
                                                   fileInput("verplaatsing", "Choose CSV File", accept = ".csv"),
                                                   tableOutput("verplaatsing"),
                                                   hr(),
                                                   h4("Plattegrond afbeelding"),
                                                   fileInput("grondplan_afbeelding", "Choose Floor Plan Image",
                                                             accept = c("image/png", "image/jpeg", "image/jpg")),
                                                   imageOutput("grondplan_img_preview", height = "auto")
                                            )
                                   ),
                                   
                                   
                                   tabPanel( "Spaghetti chart",
                                             fluidRow(
                                               column(12, uiOutput("spaghetti_toggle_ui"))
                                             ),
                                             hr(),
                                             column(10,
                                                    uiOutput("spaghetti_hoofdweergave")
                                             ),
                                             column(2, 
                                                    uiOutput("spaghetti_zijpaneel")
                                             )
                                   ), #spaghetti chart toevoegen
                                   
                                   tabPanel("Heatmap",
                                            column( 10,
                                                    h4("Heatmap verblijfsduur per locatie"),
                                                    plotOutput('heatmap', height = "750px")
                                            ),
                                            column(2,
                                                   h4("Overzicht tijdsduur per locatie"),
                                                   DTOutput('Overzicht_assets_heat')
                                                   
                                            )
                                   ), #heatmap toevoegen
                                   
                                   
                                   tabPanel("Flowmap",
                                            column(10,
                                                   h4("Bewegingsstromen tussen kamers"),
                                                   plotOutput('flowmap_plot', height = "750px")
                                            ),
                                            column(2,
                                                   h4("Grootste stromen"),
                                                   DTOutput('flowmap_tabel')
                                            )
                                   ), #flowmap toevoegen
                                   
                                   tabPanel("Interacties", 
                                            column(8, 
                                                   h4("Interacties tussen assets"),
                                                   plotlyOutput('plot_interacties')
                                                   ),
                                            column(4, 
                                                   h4("Interactiedetails"),
                                                   tableOutput('tabel_interactie_detail')
                                                   )
                                            
                                   ) #interactiesnetwerk toevoegen
                                 )
                               ) #mainpanel
                               
                )#sidebarlayout
                
) # fluidPage



# ---------------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # -------------------------------------------------------------------------
  # Reactieve waarden: wanneer iets geupload wordt, worden deze automatisch bijgewerkt ne de visualisaties geupdate
  # -------------------------------------------------------------------------
  rv <- reactiveValues(
    floorplan_details = Ward_Floor_Plan_Details_init,
    data               = data_init
  )
  
  #-----------------------------------------------------------------------------
  # Upload: plattegrond (Excel)
  floorplan_raw_upload <- reactive({
    file <- input$grondplan
    req(file)
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    
    # Lees alle rijen zonder header om de headerrij te vinden
    ruw <- read_excel(file$datapath, col_names = FALSE)
    header_rij <- which(apply(ruw, 1, function(x) any(grepl("Location ID", x, ignore.case = TRUE))))
    
    # Lees opnieuw vanaf de headerrij
    read_excel(file$datapath, skip = header_rij - 1)
  })
  
  output$grondplan <- renderTable({
    head(floorplan_raw_upload(), 5)
  })
  
  observeEvent(floorplan_raw_upload(), {
    nieuw_floorplan <- floorplan_raw_upload() %>%
      rename(
        location_id   = `Location ID`,
        room_name     = `Room Name`,
        room_code     = `Room Code`,
        location_type = `Location Type`,
        patient_id    = `Patient ID`
      )
    rv$floorplan_details <- nieuw_floorplan
  })
  
  #--------------------------------------------------------------------------
  #Upload: verplaatsingsgegevens (CSV)
  verplaatsing_upload <- reactive({
    file <- input$verplaatsing
    req(file)
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read_csv(file$datapath)
  })
  
  output$verplaatsing <- renderTable({
    head(verplaatsing_upload(), 5)
  })
  
  observeEvent(verplaatsing_upload(), {
    rv$data <- verplaatsing_upload()
  })
  
  
  #------------------------------------------------------------------------------
  #Upload: plattegrond afbeelding (png/jpg)
  render_grondplan_afbeelding <- function() {
    renderImage({
      file <- input$grondplan_afbeelding
      req(file)
      list(
        src         = file$datapath,
        contentType = file$type,
        width       = "100%",
        alt         = "Plattegrond afbeelding"
      )
    }, deleteFile = FALSE)
  }
  
  output$grondplan_img_preview   <- render_grondplan_afbeelding()
  
  grondplan_image_raster <- reactive({
    file <- input$grondplan_afbeelding
    req(file)
    img <- magick::image_read(file$datapath)
    as.raster(img)
  })
  
  
  
  #-------------------------------------------------------------------------
  #Data samenvoegen en opschonen dat automatisch bijgewerkt wordt door reactive functies
  #-------------------------------------------------------------------------
  
  # gejoinde en opgeschoonde dataset
  data_joined <- reactive({
    df <- left_join(rv$data, rv$floorplan_details, by = "location_id")
    df$location_name <- NULL
    df$room_code     <- NULL
    df$location_code <- NULL
    df$room_name     <- NULL
    df
  })
  
  # enkel de plattegrondpunten
  floorplan_reactive <- reactive({
    rv$floorplan_details %>% select(location_id, x_pixel, y_pixel)
  })
  
  #herschalen van coordinaten naar afmetingen afbeelding
  herschaal_naar_afbeelding <- function(data, min_x, max_x, min_y, max_y, breedte, hoogte,
                                        marge_x = MARGE_X_PIXELS, marge_y = MARGE_Y_PIXELS) {
    data %>%
      mutate(
        x_pixel = marge_x + (x_pixel - min_x) / (max_x - min_x) * (breedte - 2 * marge_x),
        y_pixel = marge_y + (y_pixel - min_y) / (max_y - min_y) * (hoogte - 2 * marge_y)
      )
  }
  
  # kleurenpalet per asset_type, gebaseerd op de actieve dataset
  kleur_palet_reactive <- reactive({
    types   <- unique(data_joined()$asset_type)
    n_types <- length(types)
    setNames(brewer.pal(max(n_types, 3), "Set1")[seq_len(n_types)], types)
  })
  
  #Filters dynamisch invullen o.b.v. de actieve dataset
  observeEvent(data_joined(), {
    asset_types <- unique(data_joined()$asset_type)
    updateCheckboxGroupInput(session, "asset_types",
                             label = NULL,
                             choices = asset_types,
                             selected = asset_types)  # alle opties selecteren
  }, ignoreNULL = FALSE)
  
  observeEvent(list(input$asset_types, data_joined()), {
    df <- data_joined()
    # Filter asset_id op basis van geselecteerde asset_types
    if (!is.null(input$asset_types) && length(input$asset_types) > 0) {
      beschikbare_assets <- df %>%
        filter(asset_type %in% input$asset_types) %>%
        pull(asset_id) %>%
        unique() %>%
        sort()
    } else {
      beschikbare_assets <- sort(unique(df$asset_id))
    }
    
    updatePickerInput(session, "sel_asset",
                      choices  = c("All" = "all", beschikbare_assets),
                      selected = "all")
  }, ignoreNULL = FALSE)
  
  observe({ # als de gebruiker alle opties deselecteert, wordt all aangeduid
    huidige_selectie <- input$sel_asset
    
    if (length(huidige_selectie) == 0) {
      updatePickerInput(session, "sel_asset",
                        selected = "all")
    }
  })
  
  # Datum-, tijdstip- en verblijfsduur-filters bijwerken zodra er nieuwe data geladen wordy
  observeEvent(data_joined(), {
    df <- data_joined()
    
    updateDateRangeInput(session, "datum_filter",
                         start = as.Date(min(df$start_time, na.rm = TRUE)),
                         end   = as.Date(max(df$start_time, na.rm = TRUE)),
                         min   = as.Date(min(df$start_time, na.rm = TRUE)),
                         max   = as.Date(max(df$start_time, na.rm = TRUE)))
    
    updateSliderInput(session, "uur_filter",
                      min   = tijd_van_dag(min(df$start_time, na.rm = TRUE)),
                      max   = tijd_van_dag(max(df$start_time, na.rm = TRUE)),
                      value = c(tijd_van_dag(min(df$start_time, na.rm = TRUE)),
                                tijd_van_dag(max(df$start_time, na.rm = TRUE))))
    
    updateSliderInput(session, "min_duur_filter",
                      max   = max(60, ceiling(max(df$duration_sec, na.rm = TRUE))),
                      value = 10)
  }, ignoreNULL = FALSE)
  
  
  # Reactieve data filtering (basisfilters: type, asset, datum, tijdstip, duur)
  data_filtered <- reactive({
    df <- data_joined()
    
    if (!is.null(input$asset_types) && length(input$asset_types) > 0) {
      df <- df %>% filter(asset_type %in% input$asset_types)
    }
    
    geselecteerde_assets <- input$sel_asset[input$sel_asset != "all"]
    if (length(geselecteerde_assets) > 0) {
      df <- df %>% filter(asset_id %in% geselecteerde_assets)
    }
    
    req(input$datum_filter, input$uur_filter, input$min_duur_filter)
    
    df %>%
      filter(as.Date(start_time) >= input$datum_filter[1],
             as.Date(start_time) <= input$datum_filter[2]) %>%
      filter(tijd_van_dag(start_time) >= input$uur_filter[1],
             tijd_van_dag(start_time) <= input$uur_filter[2]) %>%
      filter(duration_sec >= input$min_duur_filter)
  })
  
  # -------------------------------------------------------------------------
  # Interacties zoeken
  # -------------------------------------------------------------------------
  interactie_data <- reactive({
    df <- data_filtered()
    
    df %>%
      select(asset_id_1 = asset_id, location_id, start_1 = start_time, end_1 = end_time) %>%
      inner_join(
        df %>% select(asset_id_2 = asset_id, location_id, start_2 = start_time, end_2 = end_time),
        by = "location_id",
        relationship = "many-to-many"
      ) %>%
      filter(asset_id_1 != asset_id_2) %>%
      filter(asset_id_1 < asset_id_2) %>%
      filter(start_1 <= end_2 & start_2 <= end_1) %>%
      mutate(
        overlap_start = pmax(start_1, start_2),
        overlap_einde = pmin(end_1, end_2),
        overlap_duur_sec = as.numeric(difftime(overlap_einde, overlap_start, units = "secs"))
      ) %>%
      filter(overlap_duur_sec > MIN_INTERACTIE_DUUR_SEC) #interactie
  })
  
  interactie_plot_data <- reactive({
    interactie_data() %>%
      group_by(asset_id_1, asset_id_2) %>%
      summarise(n_interacties = n(), .groups = "drop")
  })
  
  #zelfde locatie, zelfde personen, zelfdde tijdsstip, voor meerdere locaties achtereenvolgend
  samen_beweging_data <- reactive({
    transities <- data_filtered() %>%
      arrange(asset_id, start_time) %>%
      group_by(asset_id) %>%
      mutate(
        van_locatie   = location_id,
        naar_locatie  = lead(location_id),
        vertrek_tijd  = end_time,
        aankomst_tijd = lead(start_time)
      ) %>%
      ungroup() %>%
      filter(!is.na(naar_locatie), van_locatie != naar_locatie)
    
    transities %>%
      select(asset_id_1 = asset_id, van_locatie, naar_locatie,
             vertrek_1 = vertrek_tijd, aankomst_1 = aankomst_tijd) %>%
      inner_join(
        transities %>%
          select(asset_id_2 = asset_id, van_locatie, naar_locatie,
                 vertrek_2 = vertrek_tijd, aankomst_2 = aankomst_tijd),
        by = c("van_locatie", "naar_locatie"),
        relationship = "many-to-many"
      ) %>%
      filter(asset_id_1 != asset_id_2) %>%
      filter(asset_id_1 < asset_id_2) %>%
      # tijdvensters van de verplaatsing moeten overlappen
      filter(vertrek_1 <= aankomst_2 & vertrek_2 <= aankomst_1) %>%
      mutate(
        overlap_duur_sec = as.numeric(difftime(pmin(aankomst_1, aankomst_2),
                                               pmax(vertrek_1, vertrek_2),
                                               units = "secs"))
      )
  })
  
  # Alle asset_id's die minstens 1 keer samen met een andere asset bewogen
  samen_beweging_asset_ids_reactive <- reactive({
    sb <- samen_beweging_data()
    unique(c(sb$asset_id_1, sb$asset_id_2))
  })
  
  # Tussenstap: elke gedeelde transitie krijgt een keten_id (welke aaneengesloten route ze vormt). Dit blijft op transitie-niveau (niet samengevat), zodat we hieruit zowel de routes-tabel als de exacte punten voor de spaghetti chart kunnen afleiden.
  samen_beweging_ketens_ruw <- reactive({
    sb <- samen_beweging_data() %>%
      arrange(asset_id_1, asset_id_2, vertrek_1)
    
    validate(need(nrow(sb) > 0, "Geen gedeelde routes gevonden."))
    
    sb %>%
      group_by(asset_id_1, asset_id_2) %>%
      mutate(
        nieuwe_keten = row_number() == 1 | van_locatie != lag(naar_locatie),
        keten_id     = cumsum(nieuwe_keten)
      ) %>%
      ungroup()
  })
  
  #volledige route
  samen_beweging_routes <- reactive({
    samen_beweging_ketens_ruw() %>%
      group_by(asset_id_1, asset_id_2, keten_id) %>%
      summarise(
        route           = paste(c(first(van_locatie), naar_locatie), collapse = " \u2192 "),
        aantal_locaties = n() + 1,
        start_route     = min(vertrek_1, vertrek_2),
        einde_route     = max(aankomst_1, aankomst_2),
        .groups = "drop"
      ) %>%
      arrange(asset_id_1, asset_id_2, start_route)
  })
  
  # Hulpfunctie: zet een set gedeelde traject om in losse locatis met tijdsstip per asset voor enkel de locaties in het traject samen afgelegd
  interactie_punten_opbouwen <- function(ketens) {
    ketens <- ketens %>%
      mutate(interactie_id = paste(asset_id_1, asset_id_2, keten_id, sep = "_"))
    
    been_1 <- ketens %>%
      transmute(asset_id = asset_id_1, interactie_id, van_locatie, naar_locatie,
                vertrek = vertrek_1, aankomst = aankomst_1)
    been_2 <- ketens %>%
      transmute(asset_id = asset_id_2, interactie_id, van_locatie, naar_locatie,
                vertrek = vertrek_2, aankomst = aankomst_2)
    
    benen <- bind_rows(been_1, been_2)
    
    vertrek_punten <- benen %>%
      transmute(asset_id, interactie_id, location_id = van_locatie, start_time = vertrek)
    aankomst_punten <- benen %>%
      transmute(asset_id, interactie_id, location_id = naar_locatie, start_time = aankomst)
    
    bind_rows(vertrek_punten, aankomst_punten) %>%
      distinct() %>%
      arrange(interactie_id, asset_id, start_time)
  }
  
  # Lookup asset_id -> asset_type, nodig om de interactie-punten dezelfde kleurcodering te geven als de rest van de spaghetti chart
  asset_type_lookup_reactive <- reactive({
    data_filtered() %>% distinct(asset_id, asset_type)
  })
  
  # Alle interactie-punten (over alle assetparen heen), gebruikt voor de algemene "samen bewegen"-weergave zonder specifieke rij-selectie.
  alle_interactie_punten <- reactive({
    interactie_punten_opbouwen(samen_beweging_ketens_ruw()) %>%
      left_join(floorplan_reactive(), by = "location_id") %>%
      left_join(asset_type_lookup_reactive(), by = "asset_id") %>%
      mutate(pad_groep = paste(asset_id, interactie_id, sep = "_"))
  })
  
  # Stap 2 van de filterketen: indien gekozen, enkel verplaatsingen tonen van assets die minstens 1 keer samen bewogen
  data_na_interactiefilter <- reactive({
    df <- data_filtered()
    
    if (!is.null(input$interactie_filter) && input$interactie_filter == "samen_bewegen") {
      ids <- samen_beweging_asset_ids_reactive()
      df  <- df %>% filter(asset_id %in% ids)
    }
    
    df
  })
  
  # Stap 3 van de filterketen: optioneel het aantal getoonde assets beperken tot de x meest- of minst-bewegende assets
  data_eindfilter <- reactive({
    df <- data_na_interactiefilter()
    
    if (isTRUE(input$limiteer_assets) && !is.null(input$max_aantal_assets)) {
      scores <- df %>% count(asset_id, name = "aantal_registraties")
      n_max  <- min(input$max_aantal_assets, nrow(scores))
      
      geselecteerd <- if (identical(input$asset_selectie_type, "minst")) {
        scores %>% slice_min(aantal_registraties, n = n_max, with_ties = FALSE) %>% pull(asset_id)
      } else {
        scores %>% slice_max(aantal_registraties, n = n_max, with_ties = FALSE) %>% pull(asset_id)
      }
      
      df <- df %>% filter(asset_id %in% geselecteerd)
    }
    
    df
  })
  
  # ---------------------------------------------------------------------
  # Stapsgewijs
  # ---------------------------------------------------------------------
  
  # Alle momenten waarop minstens 1 asset van plaats verandert, gegroepeerd per minuut: alle locatiewijzigingen binnen dezelfde minuut worden als 1
  # stap/tijdstip beschouwd
  tijdlijn_momenten <- reactive({
    df <- data_eindfilter()
    validate(need(nrow(df) > 0, "Geen data beschikbaar voor de geselecteerde filters."))
    
    df %>%
      arrange(asset_id, start_time) %>%
      group_by(asset_id) %>%
      mutate(vorige_locatie = lag(location_id)) %>%
      ungroup() %>%
      filter(is.na(vorige_locatie) | location_id != vorige_locatie) %>%
      mutate(minuut_moment = lubridate::floor_date(start_time, unit = "minute")) %>%
      pull(minuut_moment) %>%
      unique() %>%
      sort()
  })
  
  # Huidige stap-index (1-gebaseerd). Wordt teruggezet naar 1 zodra de
  # tijdlijn zelf verandert (nieuwe filters/data), zodat de index nooit
  # buiten bereik valt.
  stap_index <- reactiveVal(1)
  
  # Play/pauze: houdt bij of de tijdlijn automatisch doorloopt.
  spelen <- reactiveVal(FALSE)
  
  observeEvent(tijdlijn_momenten(), {
    stap_index(1)
    spelen(FALSE)
  })
  
  observeEvent(input$stap_volgende, {
    spelen(FALSE)  # handmatige stap onderbreekt het afspelen
    max_stap <- length(tijdlijn_momenten())
    stap_index(min(stap_index() + 1, max_stap))
  })
  
  observeEvent(input$stap_vorige, {
    spelen(FALSE)
    stap_index(max(stap_index() - 1, 1))
  })
  
  # Play/pauze-knop: schakelt tussen afspelen en pauzeren. Als op play
  # gedrukt wordt terwijl je al op de laatste stap staat, begin dan
  # opnieuw vanaf stap 1.
  observeEvent(input$stap_play_toggle, {
    max_stap <- length(tijdlijn_momenten())
    
    if (!isTRUE(spelen()) && stap_index() >= max_stap) {
      stap_index(1)
    }
    spelen(!isTRUE(spelen()))
  })
  
  output$stap_play_ui <- renderUI({
    actionButton("stap_play_toggle",
                 if (isTRUE(spelen())) "\u23f8 Pauze" else "\u25b6 Play",
                 width = "100%")
  })
  
  # Zolang "spelen" aan staat, telt deze observer om de STAP_INTERVAL_MS
  # milliseconden automatisch 1 stap verder. Stopt automatisch bij de
  # laatste stap.
  STAP_INTERVAL_MS <- 1000  # tijd tussen 2 automatische stappen (in ms)
  
  observe({
    if (isTRUE(spelen())) {
      invalidateLater(STAP_INTERVAL_MS, session)
      
      isolate({
        max_stap <- length(tijdlijn_momenten())
        if (stap_index() < max_stap) {
          stap_index(stap_index() + 1)
        } else {
          spelen(FALSE)  # einde van de tijdlijn bereikt
        }
      })
    }
  })
  
  # Teller boven de knoppen: "Stap X van Y - DD/MM/YYYY HH:MM"
  output$stap_teller <- renderText({
    momenten  <- tijdlijn_momenten()
    idx       <- stap_index()
    huidig    <- momenten[idx]
    
    paste0("Stap ", idx, " van ", length(momenten),
           " \u2014 ", format(huidig, "%d/%m/%Y %H:%M"))
  })
  
  # Positie van elk asset op het huidige moment: de meest recente registratie
  # waarvan de minuut (afgerond naar onder) <= het huidige moment is. Zo
  # vallen alle wijzigingen binnen dezelfde minuut samen, ook als hun exacte
  # seconde net na de "vloer" van die minuut valt.
  posities_op_huidig_moment <- reactive({
    momenten <- tijdlijn_momenten()
    huidig   <- momenten[stap_index()]
    
    data_eindfilter() %>%
      filter(lubridate::floor_date(start_time, unit = "minute") <= huidig) %>%
      group_by(asset_id) %>%
      filter(start_time == max(start_time)) %>%
      slice(1) %>%  # bij exact gelijke start_time (zelden) toch 1 rij per asset
      ungroup()
  })
  
  # Positie van elk asset op de VORIGE stap (NULL bij de eerste stap, want
  # dan is er nog geen "vorige" om een pijl vanaf te tekenen).
  posities_op_vorig_moment <- reactive({
    idx <- stap_index()
    if (idx <= 1) return(NULL)
    
    momenten <- tijdlijn_momenten()
    vorig    <- momenten[idx - 1]
    
    data_eindfilter() %>%
      filter(lubridate::floor_date(start_time, unit = "minute") <= vorig) %>%
      group_by(asset_id) %>%
      filter(start_time == max(start_time)) %>%
      slice(1) %>%
      ungroup()
  })
  
  # Overzichtstabel per asset op het huidige stapmoment: huidige plaats,
  # vorige plaats (waar ze vandaan komen) en hoe lang ze al op de huidige
  # plaats zijn. "Vorige plaats" wordt opgezocht via de eigen historiek van
  # het asset (niet via de vorige stap, want als een asset op deze stap niet
  # bewoog, zou dat toch dezelfde plaats teruggeven).
  overzicht_stap_tabel <- reactive({
    huidig_posities <- posities_op_huidig_moment()
    momenten <- tijdlijn_momenten()
    huidig   <- momenten[stap_index()]
    
    vorige_locatie_lookup <- data_eindfilter() %>%
      arrange(asset_id, start_time) %>%
      group_by(asset_id) %>%
      mutate(vorige_locatie = lag(location_id)) %>%
      ungroup() %>%
      select(asset_id, location_id, start_time, vorige_locatie)
    
    huidig_posities %>%
      left_join(vorige_locatie_lookup, by = c("asset_id", "location_id", "start_time")) %>%
      mutate(
        # duur sinds aankomst, t.o.v. het huidige stapmoment; door de
        # minuut-afronding van de stappen kan dit voor de allereerste
        # minuut van een verblijf licht negatief uitvallen - dan tonen we 0.
        duur_min = pmax(0, round(as.numeric(difftime(huidig, start_time, units = "mins")), 1)),
        vorige_locatie = ifelse(is.na(vorige_locatie), "-", as.character(vorige_locatie))
      ) %>%
      select(
        "Asset ID"       = asset_id,
        "Asset type"     = asset_type,
        "Huidige plaats" = location_id,
        "Vorige plaats"  = vorige_locatie,
        "Duur (min)"     = duur_min
      ) %>%
      arrange(`Asset ID`)
  })
  
  output$stap_tabel <- renderDT({
    df <- overzicht_stap_tabel()
    
    validate(
      need(nrow(df) > 0, "Geen assets gevonden op dit moment.")
    )
    
    datatable(
      df,
      selection = "none",
      options = list(
        pageLength = 10,
        lengthChange = FALSE,
        order = list(0, "asc"),
        language = list(
          search = "Zoeken:",
          info = "Rijen _START_ tot _END_ van _TOTAL_",
          paginate = list(previous = "Vorige", `next` = "Volgende")
        )
      ),
      rownames = FALSE
    )
  })
  
  output$stapsgewijs_plot <- renderPlot({
    df <- posities_op_huidig_moment()
    
    validate(
      need(nrow(df) > 0, "Geen assets gevonden op dit moment voor de geselecteerde filters.")
    )
    
    # Voor elk asset dat sinds de vorige stap van locatie veranderd is: bouw
    # 1 rij met de vorige en de nieuwe positie, om als pijl te tonen. Enkel
    # geldig voor DEZE stap - bij de volgende stap wordt dit opnieuw
    # berekend en verdwijnt de vorige pijl.
    vorig_pos  <- posities_op_vorig_moment()
    pijl_data  <- NULL
    
    if (!is.null(vorig_pos)) {
      pijl_data <- df %>%
        select(asset_id, naar_x = x_pixel, naar_y = y_pixel, naar_locatie = location_id) %>%
        inner_join(
          vorig_pos %>% select(asset_id, van_x = x_pixel, van_y = y_pixel, van_locatie = location_id),
          by = "asset_id"
        ) %>%
        filter(naar_locatie != van_locatie)
      
      if (nrow(pijl_data) == 0) pijl_data <- NULL
    }
    
    kamer_punten <- rv$floorplan_details
    coord_laag   <- coord_fixed()
    p            <- ggplot()
    
    if (!is.null(input$grondplan_afbeelding)) {
      img     <- grondplan_image_raster()
      breedte <- ncol(img)
      hoogte  <- nrow(img)
      
      min_x <- min(rv$floorplan_details$x_pixel, na.rm = TRUE)
      max_x <- max(rv$floorplan_details$x_pixel, na.rm = TRUE)
      min_y <- min(rv$floorplan_details$y_pixel, na.rm = TRUE)
      max_y <- max(rv$floorplan_details$y_pixel, na.rm = TRUE)
      
      df           <- herschaal_naar_afbeelding(df, min_x, max_x, min_y, max_y, breedte, hoogte)
      kamer_punten <- herschaal_naar_afbeelding(kamer_punten, min_x, max_x, min_y, max_y, breedte, hoogte)
      
      if (!is.null(pijl_data)) {
        pijl_data <- pijl_data %>%
          mutate(
            van_x  = MARGE_X_PIXELS + (van_x  - min_x) / (max_x - min_x) * (breedte - 2 * MARGE_X_PIXELS),
            van_y  = MARGE_Y_PIXELS + (van_y  - min_y) / (max_y - min_y) * (hoogte - 2 * MARGE_Y_PIXELS),
            naar_x = MARGE_X_PIXELS + (naar_x - min_x) / (max_x - min_x) * (breedte - 2 * MARGE_X_PIXELS),
            naar_y = MARGE_Y_PIXELS + (naar_y - min_y) / (max_y - min_y) * (hoogte - 2 * MARGE_Y_PIXELS)
          )
      }
      
      p <- p + annotation_raster(img, xmin = 0, xmax = breedte, ymin = 0, ymax = hoogte)
      coord_laag <- coord_fixed(xlim = c(0, breedte), ylim = c(0, hoogte))
    }
    
    p <- p +
      # plattegrond-kamers als lichte achtergrondstippen
      geom_point(data = kamer_punten, aes(x = x_pixel, y = y_pixel),
                 colour = "grey50", size = 2, alpha = 0.4, shape = 16)
    
    if (!is.null(pijl_data)) {
      p <- p +
        geom_segment(
          data = pijl_data,
          aes(x = van_x, y = van_y, xend = naar_x, yend = naar_y),
          arrow     = arrow(length = unit(0.25, "cm"), type = "closed"),
          colour    = "black",
          linewidth = 0.8,
          alpha     = 0.8
        )
    }
    
    p +
      # assets op hun huidige positie
      geom_point(data = df, aes(x = x_pixel, y = y_pixel, colour = asset_type),
                 size = 4, alpha = 0.9) +
      geom_text(data = df, aes(x = x_pixel, y = y_pixel, label = asset_id, colour = asset_type),
                vjust = -1, size = 3.2, fontface = "bold", show.legend = FALSE) +
      labs(colour = "Asset type") +
      coord_laag +
      theme_minimal() +
      xlab("X coördinaat") +
      ylab("Y coördinaat")
  })
  
  # Wissel-knop in de Spaghetti chart-tab: schakelt de hoofdweergave in die
  # tab tussen de spaghetti chart en de stapsgewijze weergave.
  spaghetti_stap_modus <- reactiveVal(FALSE)  # FALSE = spaghetti chart, TRUE = stapsgewijs
  
  observeEvent(input$spaghetti_weergave_toggle, {
    spaghetti_stap_modus(!isTRUE(spaghetti_stap_modus()))
  })
  
  output$spaghetti_toggle_ui <- renderUI({
    actionButton("spaghetti_weergave_toggle",
                 if (isTRUE(spaghetti_stap_modus()))
                   "\u2190 Terug naar spaghetti chart"
                 else
                   "Bekijk stapsgewijs \u2192",
                 width = "260px")
  })
  
  output$spaghetti_hoofdweergave <- renderUI({
    if (isTRUE(spaghetti_stap_modus())) {
      tagList(
        fluidRow(
          column(2, actionButton("stap_vorige", "\u25c0 Vorige", width = "100%")),
          column(2, actionButton("stap_volgende", "Volgende \u25b6", width = "100%")),
          column(2, uiOutput("stap_play_ui")),
          column(6, div(style = "padding-top: 7px;", textOutput("stap_teller")))
        ),
        hr(),
        plotOutput('stapsgewijs_plot', height = "750px")
      )
    } else {
      tagList(
        h4("Bewegingstrajecten per asset"),
        plotOutput('spaghetti', height = "750px")
      )
    }
  })
  
  output$spaghetti_zijpaneel <- renderUI({
    if (isTRUE(spaghetti_stap_modus())) {
      tagList(
        h4("Overzicht per asset"),
        DTOutput('stap_tabel')
      )
    } else {
      tagList(
        h4("Overzicht tijdsduur per locatie"),
        DTOutput('Overzicht_assets_spagh')
      )
    }
  })
  
  # Vaste sortering van de routes, zodat de rij-index van de tabel altijd
  # overeenkomt met de rij-index in deze data (nodig om een klik correct
  # naar het juiste assetpaar/route te kunnen herleiden).
  samen_beweging_routes_weergave <- reactive({
    samen_beweging_routes() %>% arrange(desc(aantal_locaties))
  })
  
  # Houdt bij welke route (assetpaar + keten) geselecteerd is via een klik in
  # de tabel. NULL = geen selectie = normale weergave.
  geselecteerde_route <- reactiveVal(NULL)
  
  observeEvent(input$Overzicht_assets_spagh_rows_selected, {
    idx <- input$Overzicht_assets_spagh_rows_selected
    
    if (is.null(idx) || length(idx) == 0) {
      geselecteerde_route(NULL)
    } else {
      rij <- samen_beweging_routes_weergave()[idx, ]
      geselecteerde_route(list(
        asset_id_1 = rij$asset_id_1,
        asset_id_2 = rij$asset_id_2,
        keten_id   = rij$keten_id
      ))
    }
  }, ignoreNULL = FALSE)
  
  # Selectie leegmaken zodra van filter gewisseld wordt, zodat er geen
  # "vastgeklikte" route blijft hangen die niet meer zichtbaar is.
  observeEvent(input$interactie_filter, {
    geselecteerde_route(NULL)
  })
  
  # Enkel de punten (kamers) die deel zijn van de geselecteerde, gedeelde
  # route - niet de volledige verblijfsdata van beide assets.
  geselecteerde_route_punten <- reactive({
    sel <- geselecteerde_route()
    req(sel)
    
    ketens <- samen_beweging_ketens_ruw() %>%
      filter(asset_id_1 == sel$asset_id_1,
             asset_id_2 == sel$asset_id_2,
             keten_id   == sel$keten_id)
    
    interactie_punten_opbouwen(ketens) %>%
      left_join(floorplan_reactive(), by = "location_id") %>%
      left_join(asset_type_lookup_reactive(), by = "asset_id") %>%
      mutate(pad_groep = paste(asset_id, interactie_id, sep = "_"))
  })
  
  
  
  # reactieve data heatmap
  data_heat_reactive <- reactive({
    data_heat <- data_eindfilter() %>%
      group_by(location_id) %>%
      summarise(total_duration = sum(duration_sec, na.rm = TRUE), .groups = "drop")
    
    floorplan_reactive() %>%
      left_join(data_heat, by = "location_id") %>%
      mutate(total_duration = replace_na(total_duration, 0))
  })
  
  # Heatmap output: data en achtergrondafbeelding op dezelfde schaal.
  output$heatmap <- renderPlot({
    df <- data_heat_reactive()
    
    validate(
      need(nrow(df) > 0, "Geen data beschikbaar voor de geselecteerde filters.")
    )
    
    coord_laag <- coord_fixed()
    p <- ggplot()
    
    if (!is.null(input$grondplan_afbeelding)) {
      img     <- grondplan_image_raster()
      breedte <- ncol(img)
      hoogte  <- nrow(img)
      
      min_x <- min(rv$floorplan_details$x_pixel, na.rm = TRUE)
      max_x <- max(rv$floorplan_details$x_pixel, na.rm = TRUE)
      min_y <- min(rv$floorplan_details$y_pixel, na.rm = TRUE)
      max_y <- max(rv$floorplan_details$y_pixel, na.rm = TRUE)
      
      df <- herschaal_naar_afbeelding(df, min_x, max_x, min_y, max_y, breedte, hoogte)
      
      # achtergrondafbeelding eerst toevoegen, zodat de heatmap-punten erboven liggen
      p <- p + annotation_raster(
        img,
        xmin = 0, xmax = breedte,
        ymin = 0, ymax = hoogte
      )
      
      coord_laag <- coord_fixed(xlim = c(0, breedte), ylim = c(0, hoogte))
    }
    
    p +
      geom_point(
        aes(x = x_pixel, y = y_pixel, color = total_duration, size = total_duration),
        data = df
      ) +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal() +
      coord_laag +
      xlab("X coördinaat") +
      ylab("Y coördinaat") +
      labs(color = "Seconden", size = "Seconden")
  })
  
  
  
  # spaghetti chart output
  output$spaghetti <- renderPlot({
    sel <- geselecteerde_route()
    interactie_modus <- !is.null(input$interactie_filter) && input$interactie_filter == "samen_bewegen"
    
    if (interactie_modus) {
      df <- if (!is.null(sel)) geselecteerde_route_punten() else alle_interactie_punten()
    } else {
      df <- data_eindfilter() %>%
        group_by(asset_id) %>%
        arrange(start_time) %>%
        ungroup() %>%
        mutate(pad_groep = asset_id)
    }
    
    validate(
      need(nrow(df) > 0, "Geen data beschikbaar voor de geselecteerde filters.")
    )
    
    # Bepaal kleur op basis van aantal geselecteerde asset types
    een_type <- length(unique(df$asset_type)) == 1
    
    if (een_type) {
      # Kleur per asset_id
      lijn_plot <- geom_path(aes(colour = asset_id, group = pad_groep))
      lijn_label <- "Asset ID"
    } else {
      lijn_plot <- geom_path(aes(colour = asset_type, group = pad_groep))
      lijn_label <- "Asset type"
    }
    
    # De assen volgen de afmetingen van de afbeelding, en ook de DATA zelf wordt herschaald naar die pixelgrootte
    kamer_punten <- rv$floorplan_details
    coord_laag   <- coord_fixed()
    
    if (!is.null(input$grondplan_afbeelding)) {
      img     <- grondplan_image_raster()
      breedte <- ncol(img)
      hoogte  <- nrow(img)
      
      min_x <- min(rv$floorplan_details$x_pixel, na.rm = TRUE)
      max_x <- max(rv$floorplan_details$x_pixel, na.rm = TRUE)
      min_y <- min(rv$floorplan_details$y_pixel, na.rm = TRUE)
      max_y <- max(rv$floorplan_details$y_pixel, na.rm = TRUE)
      
      df           <- herschaal_naar_afbeelding(df, min_x, max_x, min_y, max_y, breedte, hoogte)
      kamer_punten <- herschaal_naar_afbeelding(kamer_punten, min_x, max_x, min_y, max_y, breedte, hoogte)
      
      coord_laag <- coord_fixed(xlim = c(0, breedte), ylim = c(0, hoogte))
    }
    
    p <- ggplot(aes(x = x_pixel, y = y_pixel), data = df)
    
    if (!is.null(input$grondplan_afbeelding)) {
      p <- p + annotation_raster(
        img,
        xmin = 0, xmax = breedte,
        ymin = 0, ymax = hoogte
      )
    }
    
    p +
      lijn_plot +
      labs(colour = if (een_type) "Asset ID" else "Asset type") +
      ggnewscale::new_scale_color() +
      geom_point(data = kamer_punten, aes(colour = location_type), size = 4) +
      scale_color_brewer(
        palette = "Set1",
        name = "Kamer type",
        labels = function(x) gsub("_", " ", x)
      ) +
      coord_laag +
      theme_minimal() +
      theme(
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.spacing.y = unit(0.1, "cm"),
        legend.box = "vertical"
      ) +
      xlab("X coördinaat") +
      ylab("Y coördinaat") +
      labs(colour = if (een_type) "Asset ID" else "Asset type")
  })
  
  
  
  
  
  # Flowmap
  # Alle afzonderlijke transities
  flow_transities <- reactive({
    data_eindfilter() %>%
      arrange(asset_id, start_time) %>%
      group_by(asset_id) %>%
      mutate(
        van_locatie  = location_id,
        naar_locatie = lead(location_id)
      ) %>%
      ungroup() %>%
      filter(!is.na(naar_locatie), van_locatie != naar_locatie)
  })
  
  # Geaggregeerd per richting (van -> naar)
  flow_geaggregeerd <- reactive({
    ft <- flow_transities()
    validate(need(nrow(ft) > 0, "Geen verplaatsingen gevonden voor de geselecteerde filters."))
    
    ft %>%
      count(van_locatie, naar_locatie, name = "aantal_verplaatsingen")
  })
  
  # Totaal verkeer (in + uit) per kamer
  flow_verkeer_per_kamer <- reactive({
    fg <- flow_geaggregeerd()
    
    uitgaand     <- fg %>% group_by(location_id = van_locatie)  %>% summarise(aantal = sum(aantal_verplaatsingen), .groups = "drop")
    binnenkomend <- fg %>% group_by(location_id = naar_locatie) %>% summarise(aantal = sum(aantal_verplaatsingen), .groups = "drop")
    
    bind_rows(uitgaand, binnenkomend) %>%
      group_by(location_id) %>%
      summarise(totaal_verkeer = sum(aantal), .groups = "drop")
  })
  
  
  flow_plot_data <- reactive({
    fp <- rv$floorplan_details %>% select(location_id, x_pixel, y_pixel)
    
    stromen <- flow_geaggregeerd() %>%
      left_join(fp, by = c("van_locatie" = "location_id")) %>%
      rename(x_van = x_pixel, y_van = y_pixel) %>%
      left_join(fp, by = c("naar_locatie" = "location_id")) %>%
      rename(x_naar = x_pixel, y_naar = y_pixel)
    
    kamers <- fp %>%
      left_join(flow_verkeer_per_kamer(), by = "location_id") %>%
      mutate(totaal_verkeer = replace_na(totaal_verkeer, 0))
    
    list(stromen = stromen, kamers = kamers)
  })
  
  output$flowmap_plot <- renderPlot({
    plot_data <- flow_plot_data()
    stromen   <- plot_data$stromen
    kamers    <- plot_data$kamers
    
    validate(
      need(nrow(stromen) > 0, "Geen bewegingsstromen gevonden voor de geselecteerde filters.")
    )
    
    coord_laag <- coord_fixed()
    p <- ggplot()
    
    if (!is.null(input$grondplan_afbeelding)) {
      img     <- grondplan_image_raster()
      breedte <- ncol(img)
      hoogte  <- nrow(img)
      
      min_x <- min(rv$floorplan_details$x_pixel, na.rm = TRUE)
      max_x <- max(rv$floorplan_details$x_pixel, na.rm = TRUE)
      min_y <- min(rv$floorplan_details$y_pixel, na.rm = TRUE)
      max_y <- max(rv$floorplan_details$y_pixel, na.rm = TRUE)
      
      herschaal_xy <- function(x, y) {
        list(
          x = MARGE_X_PIXELS + (x - min_x) / (max_x - min_x) * (breedte - 2 * MARGE_X_PIXELS),
          y = MARGE_Y_PIXELS + (y - min_y) / (max_y - min_y) * (hoogte - 2 * MARGE_Y_PIXELS)
        )
      }
      
      van  <- herschaal_xy(stromen$x_van,  stromen$y_van)
      naar <- herschaal_xy(stromen$x_naar, stromen$y_naar)
      stromen$x_van  <- van$x;  stromen$y_van  <- van$y
      stromen$x_naar <- naar$x; stromen$y_naar <- naar$y
      
      kamers <- herschaal_naar_afbeelding(kamers, min_x, max_x, min_y, max_y, breedte, hoogte)
      
      p <- p + annotation_raster(img, xmin = 0, xmax = breedte, ymin = 0, ymax = hoogte)
      coord_laag <- coord_fixed(xlim = c(0, breedte), ylim = c(0, hoogte))
    }
    
    p +
      # pijlen: dikte proportioneel aan aantal verplaatsingen in die richting
      geom_segment(
        data = stromen,
        aes(x = x_van, y = y_van, xend = x_naar, yend = y_naar,
            linewidth = aantal_verplaatsingen),
        arrow  = arrow(length = unit(0.2, "cm"), type = "closed"),
        colour = "steelblue",
        alpha  = 0.6,
        lineend = "round"
      ) +
      scale_linewidth(range = c(0.3, 4), name = "Aantal\nverplaatsingen") +
      # kamerpunten: grootte proportioneel aan totaal verkeer door die kamer
      geom_point(
        data = kamers,
        aes(x = x_pixel, y = y_pixel, size = totaal_verkeer),
        colour = "grey20", alpha = 0.7
      ) +
      scale_size(range = c(2, 10), name = "Totaal verkeer\nper kamer") +
      coord_laag +
      theme_minimal() +
      xlab("X coördinaat") +
      ylab("Y coördinaat")
  })
  
  output$flowmap_tabel <- renderDT({
    flow_geaggregeerd() %>%
      arrange(desc(aantal_verplaatsingen)) %>%
      select(
        "Van"                   = van_locatie,
        "Naar"                  = naar_locatie,
        "Aantal verplaatsingen" = aantal_verplaatsingen
      ) %>%
      datatable(
        selection = "none",
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          order = list(2, "desc"),
          language = list(
            search = "Zoeken:",
            info = "Rijen _START_ tot _END_ van _TOTAL_",
            paginate = list(previous = "Vorige", `next` = "Volgende")
          )
        ),
        rownames = FALSE
      )
  })
  
  # table overzicht assets
  output$Overzicht_assets_heat <- renderDT({
    data_eindfilter() %>%
      group_by(asset_id, asset_type, location_id) %>%
      summarise(
        totaal_duur_sec = sum(duration_sec, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        totaal_duur_min = round(totaal_duur_sec / 60, 2)
      ) %>%
      select(
        "Asset ID"    = asset_id,
        "Asset type"  = asset_type,
        "Locatie"     = location_id,
        "Duur (min)"  = totaal_duur_min
      ) %>%
      arrange(`Asset ID`, `Locatie`) %>%
      datatable(
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          order = list(0, "asc"),
          language = list(
            search = "Zoeken:",
            info = "Rijen _START_ tot _END_ van _TOTAL_",
            paginate = list(previous = "Vorige", `next` = "Volgende")
          )
        ),
        rownames = FALSE
      )
  })
  
  #tabel spaghetti chart
  # Bij "Alle verplaatsingen": normaal duur-overzicht per asset.
  # Bij "Interacties": tabel met de assetparen en de gedeelde route die ze samen aflegden.
  output$Overzicht_assets_spagh <- renderDT({
    if (!is.null(input$interactie_filter) && input$interactie_filter == "samen_bewegen") {
      
      routes <- samen_beweging_routes_weergave()
      
      validate(
        need(nrow(routes) > 0, "Geen assets gevonden die samen bewegen voor de geselecteerde filters.")
      )
      
      routes %>%
        transmute(
          "Asset 1"         = asset_id_1,
          "Asset 2"         = asset_id_2,
          "Route"           = route,
          "Aantal locaties" = aantal_locaties,
          "Start"           = format(start_route, "%H:%M:%S"),
          "Einde"           = format(einde_route, "%H:%M:%S"),
          "Duur (min)"      = round(as.numeric(difftime(einde_route, start_route, units = "mins")), 2)
        ) %>%
        datatable(
          selection = "single",
          options = list(
            pageLength = 10,
            lengthChange = FALSE,
            order = list(3, "desc"),
            language = list(
              search = "Zoeken:",
              info = "Rijen _START_ tot _END_ van _TOTAL_",
              paginate = list(previous = "Vorige", `next` = "Volgende")
            )
          ),
          rownames = FALSE
        )
      
    } else {
      
      data_eindfilter() %>%
        group_by(asset_id, asset_type, location_id) %>%
        summarise(
          totaal_duur_sec = sum(duration_sec, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          totaal_duur_min = round(totaal_duur_sec / 60, 2)
        ) %>%
        select(
          "Asset ID"    = asset_id,
          "Asset type"  = asset_type,
          "Locatie"     = location_id,
          "Duur (min)"  = totaal_duur_min
        ) %>%
        arrange(`Asset ID`, `Locatie`) %>%
        datatable(
          selection = "none",
          options = list(
            pageLength = 10,
            lengthChange = FALSE,
            order = list(0, "asc"),
            language = list(
              search = "Zoeken:",
              info = "Rijen _START_ tot _END_ van _TOTAL_",
              paginate = list(previous = "Vorige", `next` = "Volgende")
            )
          ),
          rownames = FALSE
        )
    }
  })
  
  # Reactieve waarde voor geselecteerde asset (netwerk-interacties)
  geselecteerde_asset <- reactiveVal(NULL)
  
  output$plot_interacties <- renderPlotly({
    interacties <- interactie_plot_data()
    
    validate(
      need(nrow(interacties) > 0, "Geen interacties gevonden voor de geselecteerde filters.")
    )
    
    assets <- unique(c(interacties$asset_id_1, interacties$asset_id_2))
    n <- length(assets)
    nodes <- data.frame(
      asset_id = assets,
      x = cos(2 * pi * seq_len(n) / n),
      y = sin(2 * pi * seq_len(n) / n)
    )
    
    geselecteerd <- geselecteerde_asset()
    
    edges <- interacties %>%
      left_join(nodes, by = c("asset_id_1" = "asset_id")) %>%
      rename(x1 = x, y1 = y) %>%
      left_join(nodes, by = c("asset_id_2" = "asset_id")) %>%
      rename(x2 = x, y2 = y) %>%
      mutate(
        highlight = if (is.null(geselecteerd)) TRUE else
          (asset_id_1 == geselecteerd | asset_id_2 == geselecteerd),
        lijn_kleur = ifelse(highlight, "steelblue", "lightgrey"),
        lijn_alpha = ifelse(highlight, 0.8, 0.2)
      )
    
    verbonden_assets <- if (!is.null(geselecteerd)) {
      c(
        interacties$asset_id_2[interacties$asset_id_1 == geselecteerd],
        interacties$asset_id_1[interacties$asset_id_2 == geselecteerd]
      )
    } else {
      character(0)
    }
    
    nodes <- nodes %>%
      mutate(
        highlight = if (is.null(geselecteerd)) TRUE else
          (asset_id == geselecteerd | asset_id %in% verbonden_assets),
        punt_kleur = ifelse(highlight, "steelblue", "lightgrey")
      )
    
    p <- plot_ly()
    
    for (i in seq_len(nrow(edges))) {
      p <- p %>%
        add_trace(
          x = c(edges$x1[i], edges$x2[i]),
          y = c(edges$y1[i], edges$y2[i]),
          type = "scatter",
          mode = "lines",
          line = list(
            color = edges$lijn_kleur[i],
            width = edges$n_interacties[i] * 2
          ),
          opacity = edges$lijn_alpha[i],
          hoverinfo = "text",
          text = paste0(edges$asset_id_1[i], " ↔ ", edges$asset_id_2[i],
                        "<br>Interacties: ", edges$n_interacties[i]),
          showlegend = FALSE
        )
    }
    
    p <- p %>%
      add_trace(
        data = nodes,
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "markers+text",
        marker = list(size = 20, color = ~punt_kleur),
        text = ~asset_id,
        textposition = "top center",
        hoverinfo = "text",
        customdata = ~asset_id,
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE, scaleanchor = "x")
      ) %>%
      event_register("plotly_click")
    
    p
  })
  
  observeEvent(event_data("plotly_click"), {
    klik <- event_data("plotly_click")
    
    if (!is.null(klik) && !is.null(klik$customdata)) {
      huidig <- geselecteerde_asset()
      if (!is.null(huidig) && huidig == klik$customdata) {
        geselecteerde_asset(NULL)
      } else {
        geselecteerde_asset(klik$customdata)
      }
    }
  })
  
  output$tabel_interactie_detail <- renderTable({
    geselecteerd <- geselecteerde_asset()
    
    validate(
      need(!is.null(geselecteerd), "Klik op een asset om de interacties te bekijken.")
    )
    
    interactie_data() %>%
      filter(asset_id_1 == geselecteerd | asset_id_2 == geselecteerd) %>%
      mutate(
        "Geselecteerde asset" = geselecteerd,
        "Interactie met"      = ifelse(asset_id_1 == geselecteerd, asset_id_2, asset_id_1),
        "Locatie"             = location_id,
        "Start"               = format(overlap_start, "%H:%M:%S"),
        "Einde"               = format(overlap_einde, "%H:%M:%S"),
        "Duur (min)"          = round(overlap_duur_sec / 60, 2)
      ) %>%
      select(
        "Geselecteerde asset",
        "Interactie met",
        "Locatie",
        "Start",
        "Einde",
        "Duur (min)"
      ) %>%
      arrange(`Start`)
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)