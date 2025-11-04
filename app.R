# ======================================================================
# Where on Earth? — Natural Earth (styled, robust, responsive)
#
# Minimalist world:
#   - Sea: light blue; Land: white; Coastline: dark grey; Red pin per round
#   - No basemap tiles; No DB; No user data stored
#
# Robustness & quiet console:
#   - Natural Earth downloads wrapped in try(..., silent=TRUE) + inherits("try-error")
#   - Uses NE "coastline" layer (avoids st_union planar warnings)
#   - Sea rectangle clamped to valid lon/lat (no bbox warning)
#   - Dropdown choices are UNNAMED (avoids jsonlite named-vector deprecation)
#   - isolate() for reactive reads in helper functions (prevents reactive context errors)
#
# Layout:
#   - Left column: title, Day 5 subheading, input, buttons, feedback, score, legal footer
#   - Right column: responsive-height map (~80% viewport desktop / ~60% mobile) + footer
#
# Folder layout expected:
#   country-quiz/
#     app.R
#     data/                  # auto-created cache for NE countries RDS
#     www/geodat-logo.png    # optional (branding in footer)
# ======================================================================

# ---------------------------- packages --------------------------------
# install.packages(c("shiny","leaflet","sf","rnaturalearth","rnaturalearthdata","bslib"))

library(shiny)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(bslib)

sf::sf_use_s2(FALSE)

# ----------------------------- colours --------------------------------
SEA  <- "#cfe8ff"
LAND <- "#ffffff"
OUTL <- "#404040"

# ------------------- ocean rectangle (valid bbox) ----------------------
sea_poly <- st_as_sf(
  st_sfc(st_polygon(list(rbind(
    c(-179.999, -89.999), c(-179.999,  89.999),
    c( 179.999,  89.999), c( 179.999, -89.999),
    c(-179.999, -89.999)
  ))), crs = 4326)
)

# ---------------------------- safe loaders -----------------------------
load_land_safe <- function() {
  showNotification("Downloading Natural Earth land…", type = "message", duration = 2)
  land <- try(
    ne_download(scale = "small", type = "land", category = "physical", returnclass = "sf"),
    silent = TRUE
  )
  if (inherits(land, "try-error")) return(NULL)
  land <- try(st_make_valid(land),  silent = TRUE); if (inherits(land, "try-error")) return(NULL)
  land <- try(st_transform(land, 4326), silent = TRUE); if (inherits(land, "try-error")) return(NULL)
  land
}

load_coastline_safe <- function() {
  coast <- try(
    ne_download(scale = "small", type = "coastline", category = "physical", returnclass = "sf"),
    silent = TRUE
  )
  if (inherits(coast, "try-error")) return(NULL)
  coast <- try(st_transform(coast, 4326), silent = TRUE)
  if (inherits(coast, "try-error")) return(NULL)
  coast
}

load_countries_safe <- function() {
  if (file.exists("data/ne_admin0_small.rds")) {
    readRDS("data/ne_admin0_small.rds")
  } else {
    dir.create("data", showWarnings = FALSE, recursive = TRUE)
    x <- try(ne_countries(scale = "small", returnclass = "sf"), silent = TRUE)
    if (inherits(x, "try-error")) return(NULL)
    x <- try(st_make_valid(x),  silent = TRUE); if (inherits(x, "try-error")) return(NULL)
    x <- try(st_transform(x, 4326), silent = TRUE); if (inherits(x, "try-error")) return(NULL)
    x <- x[, c("iso_a3","name_long","continent","geometry")]
    names(x)[names(x) == "name_long"] <- "name"
    saveRDS(x, "data/ne_admin0_small.rds"); x
  }
}

# -------------------------------- UI -----------------------------------
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  {
    css <- sprintf("
/* --- map container + frame --- */
.leaflet-container{
  background-color:%s;
  border:3px solid %s;
  border-radius:12px;
}

/* --- left column structure --- */
.left-header{ margin-top:24px; margin-bottom:24px; }
.map-title{
  margin:0;
  font-weight:800;
  font-size:2.1rem;   /* was 1.8rem — now slightly larger */
  color:#1a3c52;
  letter-spacing:0.5px; /* optional: gives a little breathing room */
}
.map-sub{    margin:0; font-weight:700; font-size:1.05rem; color:#4b6b82; }


/* small hint under the instruction */
.hint{ margin-top:6px; font-size:0.95rem; color:#6b7680; }

/* spacer between title header and control panel */
.title-spacer{ height:32px; }

/* --- control panel 'card' --- */
.control-panel{
  background:#f9fbfd;
  border:1px solid #e0e6ef;
  border-radius:10px;
  padding:18px 22px;
  box-shadow:0 2px 6px rgba(0,0,0,0.04);
}
label.control-label{ font-weight:600; color:#2c6e9f; }

/* buttons */
#submit, #reveal_btn, #next_btn{
  border-radius:8px !important;
  font-weight:600;
}

/* feedback + score */
.feedback{ min-height:1.6rem; }

/* subtle orange separation for metrics */
.score-panel{
  font-size:1.05rem; font-weight:600; color:#1a3c52;
  background:#fff8f0;            /* panel: very light orange */
  border:1px solid #ffe3c2;
  border-radius:12px; padding:10px 12px; margin-top:10px;
  display:flex; flex-wrap:wrap; gap:8px; align-items:center;
}
.score-panel .metric{
  padding:4px 10px;
  background:#fff2e0;            /* pills: subtle orange */
  border:1px solid #ffd7a8;
  border-radius:999px;
  line-height:1.2;
  white-space:nowrap;
}
.score-panel .metric b{ font-weight:700; margin-right:4px; }

/* spacer above legal footer */
.panel-spacer{ height:72px; }
@media (max-width:992px){ .panel-spacer{ height:48px; } }

/* legal footer */
.control-panel .legal{
  font-size:0.9rem;
  color:#536d85;
  margin-top:12px;
}
.control-panel .legal .copyright{
  font-weight:600;
  margin-bottom:4px;
}

/* responsive map height wrapper */
.map-wrapper{ height:80vh; min-height:420px; border-radius:12px; overflow:hidden; }
@media (max-width:992px){ .map-wrapper{ height:60vh; } }

/* footer */
.app-footer{
  display:flex; justify-content:space-between; align-items:center; gap:1rem;
  margin-top:12px; padding:12px 0; border-top:1px solid #ddd;
}
.challenge{ font-weight:700; font-size:1.2rem; }
.hashtag{   font-weight:800; font-size:1.25rem; }
.brand{ display:flex; align-items:center; gap:12px; }
.brand img{ height:64px; width:auto; display:block; }
.byline{ font-weight:800; font-size:1.1rem; }

@media (max-width:992px){
  .app-footer{ flex-direction:column; align-items:flex-start; }
}
", SEA, OUTL)
    
    tagList(
      tags$head(
        tags$title("Where on Earth? - Natural Earth"),
        tags$style(HTML(css))
      )
    )
  },

# Two-column layout
fluidRow(
  # LEFT COLUMN
  column(
    width = 4,
    div(class = "left-header",
        h2(class = "map-title", "Where on Earth?"),
        p(
          class = "map-sub",
          HTML('Day 5 of the 30 Day Map Challenge: <a href="https://30daymapchallenge.com/" target="_blank">#30DayMapChallenge</a>')
        )
    ),
    div(class = "title-spacer"),
    div(
      class = "control-panel",
      div(
        class = "instruction-block",
        p("A pin has been dropped somewhere on land. Type or pick the country:"),
        p(class = "hint", "(you can zoom and pan the map)")
      ),
      uiOutput("country_input"),
      fluidRow(
        column(4, actionButton("submit",     "Submit", class = "btn btn-primary w-100")),
        column(4, actionButton("reveal_btn", "Reveal", class = "btn btn-outline-secondary w-100")),
        column(4, actionButton("next_btn",   "Next",   class = "btn btn-outline-secondary w-100"))
      ),
      tags$hr(),
      div(class = "feedback", htmlOutput("feedback")),
      div(class = "score-panel", htmlOutput("score")),
      div(class = "panel-spacer"),
      div(
        class = "legal",
        div(
          class = "copyright",
          HTML('&copy; Robert Berry, GeoDat AI Ltd | <a href="mailto:rob@geodatai.co.uk">rob@geodatai.co.uk</a>')
        ),
        div(
          "Made with Natural Earth. Free vector and raster map data @ ",
          a(href = "https://www.naturalearthdata.com", target = "_blank", "naturalearthdata.com"), "."
        )
      )
    )
  ),
  
  # RIGHT COLUMN
  column(
    width = 8,
    div(class = "map-wrapper",
        leafletOutput("map", height = "100%")
    ),
    div(class = "app-footer",
        div(class = "challenge",
            "Day 5 of the 30 Day Map Challenge: ",
            a(href = "https://30daymapchallenge.com/", target = "_blank",
              span("#30DayMapChallenge", class = "hashtag"))
        ),
        div(class = "brand",
            a(href = "https://geodat.ai", target = "_blank",
              img(src = "geodat-logo.png", alt = "GeoDat AI Ltd logo")),
            a(href = "https://geodat.ai", target = "_blank",
              span("Powered by GeoDat AI Ltd", class = "byline"))
        )
    )
  )
)
)

# ------------------------------- server --------------------------------
server <- function(input, output, session) {
  
  ne_land       <- load_land_safe()
  coastline     <- load_coastline_safe()
  ne_countries  <- load_countries_safe()
  
  if (is.null(ne_land))       showNotification("Natural Earth land failed; showing ocean only.", type = "warning", duration = 5)
  if (is.null(coastline))     showNotification("Coastline failed to load; outline hidden.", type = "warning", duration = 5)
  if (is.null(ne_countries))  showNotification("Countries failed to load — dropdown will be empty.", type = "error", duration = 6)
  
  if (is.null(ne_countries)) {
    country_names <- character(0)
  } else {
    country_names <- sort(unique(ne_countries$name))
    names(country_names) <- NULL
  }
  
  output$country_input <- renderUI({
    selectizeInput("guess", "Your guess:",
                   choices = country_names, multiple = FALSE,
                   options = list(placeholder = "Start typing…"))
  })
  
  pin_icon  <- makeAwesomeIcon(icon = "map-marker", library = "fa",
                               markerColor = "red", iconColor = "white")
  PIN_GROUP <- "pin"
  
  sample_point_in_country <- function(geom) {
    pt <- try(suppressWarnings(st_sample(geom, size = 1, type = "random")), silent = TRUE)
    if (inherits(pt, "try-error") || length(pt) == 0) pt <- st_point_on_surface(geom)
    st_as_sf(pt)
  }
  
  state <- reactiveValues(
    target_name  = NULL,
    target_point = NULL,
    score        = 0L,
    streak       = 0L,
    best_streak  = 0L,
    rounds       = 0L
  )
  
  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 2))
    m <- setMaxBounds(m, lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85)
    m <- setView(m, lng = 0, lat = 20, zoom = 2)
    m
  })
  
  paint_map <- function() {
    p <- leafletProxy("map")
    p <- clearShapes(p); p <- clearMarkers(p); p <- clearGroup(p, PIN_GROUP)
    p <- addPolygons(p, data = sea_poly, fillColor = SEA, color = SEA, weight = 0, fillOpacity = 1)
    if (!is.null(ne_land))   p <- addPolygons(p, data = ne_land, fillColor = LAND, color = LAND, weight = 0, fillOpacity = 1)
    if (!is.null(coastline)) p <- addPolylines(p, data = coastline, color = OUTL, weight = 1.7, opacity = 1)
    tp <- isolate(state$target_point)
    if (!is.null(tp) && is.matrix(tp) && ncol(tp) == 2 && all(is.finite(tp[1, ]))) {
      lon <- tp[1, 1]; lat <- tp[1, 2]
      p <- addAwesomeMarkers(p, lng = lon, lat = lat, icon = pin_icon,
                             options = markerOptions(riseOnHover = TRUE),
                             group = PIN_GROUP)
      p <- setView(p, lng = lon, lat = lat, zoom = 4)
    } else {
      p <- fitBounds(p, lng1 = -180, lat1 = -60, lng2 = 180, lat2 = 80)
    }
  }
  
  new_round <- function() {
    state$rounds <- isolate(state$rounds) + 1L
    if (is.null(ne_countries) || nrow(ne_countries) == 0) {
      state$target_name  <- NULL
      state$target_point <- NULL
      output$feedback <- renderUI(HTML("Countries not available."))
      paint_map(); return(invisible(NULL))
    }
    row    <- ne_countries[sample(nrow(ne_countries), 1), ]
    pt     <- sample_point_in_country(row$geometry)
    coords <- st_coordinates(pt)
    state$target_name  <- row$name[1]
    state$target_point <- coords
    updateSelectizeInput(session, "guess", selected = "")
    output$feedback <- renderUI(HTML("&nbsp;"))
    paint_map()
  }
  
  new_round()
  
  observeEvent(input$submit, {
    g <- input$guess
    if (is.null(g) || !nzchar(g)) {
      output$feedback <- renderUI(HTML("Please enter or pick a country."))
      return(invisible(NULL))
    }
    if (!is.null(state$target_name) && identical(g, state$target_name)) {
      state$score  <- state$score + 1L
      state$streak <- state$streak + 1L
      if (state$streak > state$best_streak) state$best_streak <- state$streak
      output$feedback <- renderUI(HTML(
        paste0("Correct: <b>", htmltools::htmlEscape(state$target_name),
               "</b>. Click <b>Next</b> for another round.")
      ))
    } else {
      state$streak <- 0L
      output$feedback <- renderUI(HTML(
        "Not quite. Press <b>Reveal</b> to see the answer, or try again."
      ))
    }
  })
  
  observeEvent(input$reveal_btn, {
    if (!is.null(state$target_name)) {
      state$streak <- 0L
      output$feedback <- renderUI(HTML(
        paste0("Reveal: <b>", htmltools::htmlEscape(state$target_name),
               "</b>. Press <b>Next</b> to continue.")
      ))
    }
  })
  
  observeEvent(input$next_btn, { new_round() })
  
  output$score <- renderUI({
    HTML(paste0(
      '<span class="metric"><b>Score:</b> ', state$score, '</span>',
      '<span class="metric"><b>Streak:</b> ', state$streak, '</span>',
      '<span class="metric"><b>Best streak:</b> ', state$best_streak, '</span>',
      '<span class="metric"><b>Rounds:</b> ', state$rounds, '</span>'
    ))
  })
}

# -------------------------------- run ----------------------------------
shinyApp(ui, server)

