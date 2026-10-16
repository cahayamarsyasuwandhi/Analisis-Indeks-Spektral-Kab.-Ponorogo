# ============================================================
# WEBGIS ANALISIS INDEKS SPEKTRAL - KABUPATEN PONOROGO
# Jalankan file ini sebagai app.R di folder data proyek.
# ============================================================

# Instal paket yang belum tersedia:
# install.packages(c("shiny", "shinydashboard", "leaflet", "terra",
#                    "sf", "DT", "ggplot2", "dplyr", "scales"))

sf_path <- find.package("sf")

Sys.setenv(
  PROJ_DATA = file.path(sf_path, "proj"),
  PROJ_LIB  = file.path(sf_path, "proj"),
  GDAL_DATA = file.path(sf_path, "gdal")
)

library(shiny)
library(shinydashboard)
library(leaflet)
library(terra)
library(sf)
library(DT)
library(ggplot2)
library(dplyr)
library(scales)

# ---- KONFIGURASI FILE ---------------------------------------
# Simpan app.R di folder yang sama dengan data, atau ubah DATA_DIR.
DATA_DIR <- "C:/Users/Chaca/OneDrive/Documents/1matkul/Pemspa/#1UTS"

CSV_CANDIDATES <- c(
  "Statistik_Desa_Ponorogo(4).csv",
  "Statistik_Desa_Ponorogo(3).csv",
  "Statistik_Desa_Ponorogo.csv",
  "Statistik_Desa_Ponorogo_v2.csv",
  "Statistik_Desa_Ponorogo.csv"
)

RASTER_FILES <- list(
  NDVI = c("NDVI_Ponorogo.tif", "NDVI_Ponorogo.tiff"),
  NDWI = c("NDWI_Ponorogo.tif", "NDWI_Ponorogo.tiff"),
  NDBI = c("NDBI_Ponorogo.tif", "NDBI_Ponorogo.tiff"),
  NDCI = c("NDCI_Ponorogo.tif", "NDCI_Ponorogo.tiff"),
  "Klasifikasi Tutupan Lahan" = c(
    "Klasifikasi_Ponorogo_v2.tif",
    "Klasifikasi_Ponorogo.tif",
    "Klasifikasi_Ponorogo_v2.tiff"
  )
)

VECTOR_CANDIDATES <- c(
  "StatistikDesa_Ponorogo.gpkg",
  "Administrasi_Desa_Ponorogo.gpkg",
  "Administrasi_Desa_Ponorogo.shp",
  "StatistikDesa_Ponorogo.shp"
)

find_first <- function(candidates, dir = DATA_DIR) {
  p <- file.path(dir, candidates)
  p <- p[file.exists(p)]
  if (length(p)) p[1] else NA_character_
}

read_stats <- function() {
  f <- find_first(CSV_CANDIDATES)
  if (is.na(f)) return(NULL)
  tryCatch(
    read.csv(f, check.names = FALSE, stringsAsFactors = FALSE,
             fileEncoding = "UTF-8"),
    error = function(e) tryCatch(
      read.csv(f, check.names = FALSE, stringsAsFactors = FALSE),
      error = function(e2) NULL
    )
  )
}

stats_raw <- read_stats()
if (!is.null(stats_raw)) {
  names(stats_raw) <- trimws(names(stats_raw))
  # Normalisasi nama kolom administrasi yang umum ditemukan
  if (!"NAMOBJ" %in% names(stats_raw)) {
    nm <- names(stats_raw)[tolower(names(stats_raw)) %in%
                             c("namobj", "nama_desa", "desa", "namadesa")]
    if (length(nm)) names(stats_raw)[names(stats_raw) == nm[1]] <- "NAMOBJ"
  }
  if (!"WADMKC" %in% names(stats_raw)) {
    nm <- names(stats_raw)[tolower(names(stats_raw)) %in%
                             c("wadmkc", "kecamatan", "nama_kecamatan")]
    if (length(nm)) names(stats_raw)[names(stats_raw) == nm[1]] <- "WADMKC"
  }
  if (!"WADMKK" %in% names(stats_raw)) {
    nm <- names(stats_raw)[tolower(names(stats_raw)) %in%
                             c("wadmkk", "kabupaten", "nama_kabupaten")]
    if (length(nm)) names(stats_raw)[names(stats_raw) == nm[1]] <- "WADMKK"
  }
}

load_vector <- function() {
  f <- find_first(VECTOR_CANDIDATES)
  if (is.na(f)) return(NULL)
  tryCatch({
    x <- sf::st_read(f, quiet = TRUE)
    if ("WADMKK" %in% names(x)) {
      x <- x[toupper(trimws(as.character(x$WADMKK))) == "PONOROGO", ]
    }
    if (nrow(x) > 0) x else NULL
  }, error = function(e) NULL)
}
village_sf <- load_vector()

load_raster <- function(key) {
  if (is.null(key) || length(key) != 1L || is.na(key) ||
      !key %in% names(RASTER_FILES)) {
    return(NULL)
  }
  candidates <- RASTER_FILES[[key]]
  if (!length(candidates)) return(NULL)
  f <- find_first(candidates)
  if (length(f) != 1L || is.na(f)) return(NULL)
  tryCatch(terra::rast(f), error = function(e) NULL)
}

# ---- UTILITAS KOLOM STATISTIK -------------------------------
find_col <- function(data, pattern) {
  if (is.null(data)) return(NA_character_)
  n <- names(data)
  hit <- grep(pattern, n, ignore.case = TRUE, value = TRUE)
  if (length(hit)) hit[1] else NA_character_
}

numeric_col <- function(data, col) {
  if (is.na(col) || !col %in% names(data)) return(rep(NA_real_, nrow(data)))
  suppressWarnings(as.numeric(gsub(",", ".", as.character(data[[col]]),
                                  fixed = TRUE)))
}

index_columns <- function(data, index) {
  if (is.null(data)) return(character())
  # Utamakan kolom mean, kemudian statistik indeks lain.
  n <- names(data)
  idx <- switch(index,
    NDVI = "ndvi",
    NDWI = "ndwi",
    NDBI = "ndbi",
    NDCI = "ndci",
    ""
  )
  grep(idx, n, ignore.case = TRUE, value = TRUE)
}

mean_column <- function(data, index) {
  cols <- index_columns(data, index)
  if (!length(cols)) return(NA_character_)
  means <- cols[grepl("mean|rata", cols, ignore.case = TRUE)]
  if (length(means)) return(means[1])
  # Jika kolom tidak memiliki kata mean, gunakan kolom indeks pertama.
  cols[1]
}

# ---- UI ------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Ponorogo Spectral Explorer", titleWidth = 290),
  dashboardSidebar(
    width = 290,
    sidebarMenu(
      id = "tabs",
      menuItem("Beranda", tabName = "home", icon = icon("home")),
      menuItem("Peta Interaktif", tabName = "map", icon = icon("globe")),
      menuItem("Statistik Desa", tabName = "stats", icon = icon("table")),
      menuItem("Grafik", tabName = "charts", icon = icon("bar-chart")),
      menuItem("Tentang & Data", tabName = "about", icon = icon("info-circle"))
    ),
    tags$hr(),
    selectInput("index", "Pilih layer peta",
                choices = names(RASTER_FILES), selected = "NDVI"),
    helpText("Raster dibaca langsung dari folder proyek lokal.")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side {background-color:#f4f7fb;}
      .main-header .logo {font-weight:700;}
      .small-box {border-radius:12px; overflow:hidden;}
      .box {border-radius:12px; border-top:3px solid #2c7fb8;}
      .hero {padding:28px; color:white; border-radius:14px;
             background:linear-gradient(120deg,#12304a,#287c8e);}
      .hero h1 {font-weight:800; margin-top:0;}
      .info-box-text {
  font-size: 12px !important;
  white-space: nowrap !important;
}
      .muted {color:#65758b;}
    "))),
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = FALSE,
              div(class = "hero",
                  h1("Analisis Indeks Spektral"),
                  h3("Kabupaten Ponorogo, Jawa Timur"),
                  p("Eksplorasi NDVI, NDWI, NDBI, NDCI, dan klasifikasi tutupan lahan berbasis Sentinel-2.")
              )
          )
        ),
        
        fluidRow(
          infoBox(
            "Kecamatan", "21",
            icon = icon("map"),
            color = "aqua",
            fill = TRUE, width = 3
          ),
          infoBox(
            "Indeks Spektral", "4 Indeks",
            icon = icon("th"),
            color = "green",
            fill = TRUE, width = 3
          ),
          infoBox(
            "Kelas Lahan", "4 Kelas",
            icon = icon("globe"),
            color = "purple",
            fill = TRUE, width = 3
          ),
          infoBox(
            "Citra Satelit", "Sentinel-2",
            icon = icon("camera"),
            color = "yellow",
            fill = TRUE, width = 3
          )
        ),
    
        fluidRow(
          box(width = 7, title = "Ringkasan indeks", status = "primary",
              plotOutput("overview_plot", height = 320)),
          box(width = 5, title = "Status data", status = "info",
              uiOutput("data_status"))
        ),
        fluidRow(
          box(width = 12, title = "Cara menggunakan", status = "primary",
              tags$ol(
                tags$li("Pilih indeks atau klasifikasi pada menu sebelah kiri."),
                tags$li("Buka Peta Interaktif untuk melihat raster jika file TIFF tersedia."),
                tags$li("Gunakan Statistik Desa untuk mencari dan memfilter tabel."),
                tags$li("Buka Grafik untuk membandingkan statistik antar desa.")
              ))
        )
      ),
      tabItem(
        tabName = "map",
        fluidRow(
          box(width = 9, title = "Peta interaktif Ponorogo", status = "primary",
              leafletOutput("map", height = "72vh")),
          box(width = 3, title = "Informasi layer", status = "info",
              uiOutput("layer_info"),
              tags$hr(),
              p(class = "muted",
                "Jika raster belum tersedia di folder, peta dasar tetap tampil."))
        )
      ),
      tabItem(
        tabName = "stats",
        fluidRow(
          box(width = 12, title = "Statistik desa", status = "primary",
              fluidRow(
                column(4, selectInput("district", "Filter kecamatan",
                                      choices = "Semua", selected = "Semua")),
                column(4, textInput("search_village", "Cari nama desa",
                                    placeholder = "Ketik nama desa...")),
                column(4, br(), downloadButton("download_stats", "Unduh CSV hasil filter"))
              ),
              DTOutput("stats_table"))
        )
      ),
      tabItem(
        tabName = "charts",
        fluidRow(
          box(width = 4, title = "Pengaturan grafik", status = "primary",
              selectInput("chart_index", "Indeks statistik",
                          choices = c("NDVI", "NDWI", "NDBI", "NDCI")),
              selectInput("chart_type", "Jenis grafik",
                          choices = c("10 desa dengan nilai tertinggi" = "top",
                                      "Distribusi nilai" = "hist",
                                      "Rata-rata per kecamatan" = "district"))
          ),
          box(width = 8, title = "Visualisasi statistik", status = "primary",
              plotOutput("index_plot", height = 480))
        )
      ),
      tabItem(
        tabName = "about",
        fluidRow(
          box(width = 12, title = "Tentang proyek", status = "primary",
              h3("Analisis Indeks Spektral & Klasifikasi Tutupan Lahan"),
              p("Wilayah kajian: Kabupaten Ponorogo, Jawa Timur."),
              p("Indeks yang disiapkan: NDVI untuk vegetasi, NDWI untuk perairan,
                NDBI untuk area terbangun, dan NDCI sesuai konfigurasi proyek."),
              p("Catatan: interpretasi kelas dan indeks harus mengikuti metode,
                rumus, serta ambang batas yang digunakan dalam pengolahan citra."),
              h4("File yang dibaca aplikasi"),
              verbatimTextOutput("file_report"),
              h4("Catatan"),
              p("Aplikasi tidak mengunggah file ke internet. Data dibaca dari
                folder lokal saat aplikasi dijalankan.")
          )
        )
      )
    )
  )
)

# ---- SERVER --------------------------------------------------
server <- function(input, output, session) {

  output$n_rows <- renderValueBox({
    n <- if (is.null(stats_raw)) 0 else nrow(stats_raw)
    valueBox(comma(n), "Baris statistik", icon = icon("list"),
             color = if (n > 0) "aqua" else "yellow")
  })

  output$n_villages <- renderValueBox({
    n <- if (is.null(stats_raw) || !"NAMOBJ" %in% names(stats_raw)) 0 else
      dplyr::n_distinct(stats_raw$NAMOBJ, na.rm = TRUE)
    valueBox(comma(n), "Nama desa unik", icon = icon("map-marker"),
             color = "green")
  })

  output$n_districts <- renderValueBox({
    n <- if (is.null(stats_raw) || !"WADMKC" %in% names(stats_raw)) 0 else
      dplyr::n_distinct(stats_raw$WADMKC, na.rm = TRUE)
    valueBox(comma(n), "Kecamatan", icon = icon("building"),
             color = "purple")
  })

  output$raster_count <- renderValueBox({
    n <- sum(vapply(names(RASTER_FILES), function(k) !is.null(load_raster(k)),
                    logical(1)))
    valueBox(paste0(n, "/", length(RASTER_FILES)), "Raster ditemukan",
             icon = icon("globe"), color = "blue")
  })

  output$data_status <- renderUI({
    raster_lines <- lapply(names(RASTER_FILES), function(k) {
      ok <- !is.null(load_raster(k))
      tags$li(
        icon(if (ok) "check-circle" else "exclamation-circle"),
        paste(k, if (ok) "— ditemukan" else "— belum ditemukan")
      )
    })
    csv_ok <- !is.null(stats_raw)
    vec_ok <- !is.null(village_sf)
    tagList(
      p(icon(if (csv_ok) "check-circle" else "exclamation-circle"),
        if (csv_ok) "CSV statistik berhasil dibaca" else "CSV statistik belum ditemukan"),
      p(icon(if (vec_ok) "check-circle" else "exclamation-circle"),
        if (vec_ok) "Batas administrasi ditemukan" else "Batas administrasi belum ditemukan"),
      tags$ul(raster_lines)
    )
  })

  output$overview_plot <- renderPlot({
    validate(need(!is.null(stats_raw), "CSV belum ditemukan. Letakkan CSV di folder yang sama dengan app.R."))
    available <- c("NDVI", "NDWI", "NDBI", "NDCI")
    vals <- lapply(available, function(idx) {
      col <- mean_column(stats_raw, idx)
      if (is.na(col)) return(data.frame(Index = idx, Mean = NA_real_))
      x <- numeric_col(stats_raw, col)
      data.frame(Index = idx, Mean = mean(x, na.rm = TRUE))
    })
    df <- bind_rows(vals) %>% filter(is.finite(Mean))
    validate(need(nrow(df) > 0, "Kolom statistik indeks belum dikenali dari CSV."))
    ggplot(df, aes(x = Index, y = Mean, fill = Index)) +
      geom_col(width = .65, show.legend = FALSE) +
      geom_hline(yintercept = 0, color = "grey65") +
      theme_minimal(base_size = 13) +
      labs(x = NULL, y = "Rata-rata nilai kolom", title = "Ringkasan indeks") +
      theme(plot.title = element_text(face = "bold"))
  })

  # Peta dibangun ulang saat layer berubah. Cara ini menghindari
  # pemanggilan leafletProxy sebelum widget peta siap.
  output$map <- renderLeaflet({
    selected_layer <- input$index

    m <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap.Mapnik) %>%
      leaflet::setView(lng = 111.5, lat = -7.87, zoom = 10)

    r <- load_raster(selected_layer)
    if (is.null(r)) {
      message("Raster tidak ditemukan untuk layer: ", selected_layer)
      return(m)
    }

    is_class <- identical(selected_layer, "Klasifikasi Tutupan Lahan")

    # Ambil kelas yang paling sering muncul saat agregasi raster klasifikasi.
    modal_value <- function(x, ...) {
      x <- x[is.finite(x)]
      if (!length(x)) return(NA_real_)
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }

    tryCatch({
      # Perkecil raster sebelum reproyeksi agar gambar web tidak terlalu besar.
      # Indeks menggunakan mean; klasifikasi menggunakan modus (kelas terbanyak).
      n_original <- terra::ncell(r)
      if (length(n_original) == 1L && is.finite(n_original) &&
          n_original > 400000) {
        fact <- max(1L, ceiling(sqrt(n_original / 400000)))
        aggregate_fun <- if (is_class) modal_value else mean
        r <- terra::aggregate(
          r, fact = fact, fun = aggregate_fun, na.rm = TRUE
        )
      }

      # Data indeks bersifat kontinu; klasifikasi harus memakai nearest-neighbour.
      project_method <- if (is_class) "near" else "bilinear"
      rr <- terra::project(r, "EPSG:4326", method = project_method)

      vals <- terra::values(rr, mat = FALSE)
      vals <- vals[is.finite(vals)]
      if (!length(vals)) stop("Raster tidak memiliki nilai valid.")

      
      if (is_class && length(unique(vals)) <= 20L) {
        class_values <- sort(unique(vals))
        
        # Nama setiap kelas
        class_name_map <- c(
          "1" = "Vegetasi",
          "2" = "Lahan",
          "3" = "Perairan",
          "4" = "Awan"
        )
        
        # Warna setiap kelas
        class_color_map <- c(
          "1" = "#2E7D32",
          "2" = "#BCA47A",
          "3" = "#2196F3",
          "4" = "#D9DDE3"
        )
        
        class_keys <- as.character(class_values)
        
        class_labels <- unname(class_name_map[class_keys])
        missing_labels <- is.na(class_labels)
        class_labels[missing_labels] <- paste(
          "Kelas", class_values[missing_labels]
        )
        
        class_colors <- unname(class_color_map[class_keys])
        missing_colors <- is.na(class_colors)
        class_colors[missing_colors] <- "#808080"
        
        pal <- leaflet::colorFactor(
          palette = class_colors,
          domain = class_values,
          na.color = "transparent"
        )
        
        legend_values <- class_values
        legend_labels <- class_labels
        
      } else {
        palette_name <- if (selected_layer == "NDVI") "YlGn" else
          if (selected_layer == "NDWI") "Blues" else
            if (selected_layer == "NDBI") "OrRd" else
              if (selected_layer == "NDCI") "PuRd" else "viridis"
        
        value_range <- range(vals, finite = TRUE)
        
        if (!all(is.finite(value_range))) {
          stop("Rentang nilai raster tidak valid.")
        }
        
        if (value_range[1] == value_range[2]) {
          value_range <- value_range + c(-1e-8, 1e-8)
        }
        
        pal <- leaflet::colorNumeric(
          palette = palette_name,
          domain = value_range,
          na.color = "transparent"
        )
        
        legend_values <- vals
        legend_labels <- NULL
      }

      m <- leaflet::addRasterImage(
        map = m,
        x = rr,
        colors = pal,
        opacity = 0.78,
        project = FALSE,
        maxBytes = 8 * 1024^2
      )
      m <- leaflet::addLegend(
        map = m,
        pal = pal,
        values = legend_values,
        labels = legend_labels,
        title = selected_layer,
        position = "bottomright"
      )
      m
    }, error = function(e) {
      msg <- paste0("Gagal menampilkan ", selected_layer, ": ",
                    conditionMessage(e))
      message(msg)
      showNotification(msg, type = "error", duration = NULL)
      m
    })
  })


  output$layer_info <- renderUI({
    r <- load_raster(input$index)
    if (is.null(r)) {
      return(tagList(
        h4(input$index),
        p("Raster belum ditemukan."),
        p("Letakkan file dengan nama yang sesuai di folder proyek:"),
        tags$code(paste(RASTER_FILES[[input$index]], collapse = " atau "))
      ))
    }
    e <- terra::ext(r)
    tagList(
      h4(input$index),
      p(strong("Dimensi: "), paste(nrow(r), "baris ×", ncol(r), "kolom")),
      p(strong("CRS: "), terra::crs(r, proj = TRUE)),
      p(strong("Resolusi: "), paste(terra::res(r), collapse = " × ")),
      p(strong("Rentang koordinat: "), paste(round(c(e$xmin, e$xmax, e$ymin, e$ymax), 4),
                                             collapse = ", "))
    )
  })

  filtered_stats <- reactive({
    req(!is.null(stats_raw))
    d <- stats_raw
    if ("WADMKC" %in% names(d) && !is.null(input$district) &&
        input$district != "Semua") {
      d <- d[toupper(as.character(d$WADMKC)) == toupper(input$district), , drop = FALSE]
    }
    if ("NAMOBJ" %in% names(d) && nzchar(input$search_village)) {
      d <- d[grepl(input$search_village, d$NAMOBJ, ignore.case = TRUE), , drop = FALSE]
    }
    d
  })

  observe({
    choices <- if (
      is.null(stats_raw) ||
      !"WADMKC" %in% names(stats_raw)
    ) {
      "Semua"
    } else {
      c(
        "Semua",
        sort(unique(na.omit(as.character(stats_raw$WADMKC))))
      )
    }
    
    updateSelectInput(
      session,
      "district",
      choices = choices,
      selected = "Semua"
    )
  })

  output$stats_table <- renderDT({
    validate(need(!is.null(stats_raw), "CSV belum ditemukan. Letakkan CSV di folder yang sama dengan app.R."))
    datatable(filtered_stats(), filter = "top", rownames = FALSE,
              extensions = "Buttons",
              options = list(pageLength = 15, scrollX = TRUE,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv", "excel"),
                             autoWidth = TRUE))
  })

  output$download_stats <- downloadHandler(
    filename = function() paste0("Statistik_Desa_Ponorogo_filter_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_stats(), file, row.names = FALSE,
                                       fileEncoding = "UTF-8")
  )

  output$index_plot <- renderPlot({
    validate(need(!is.null(stats_raw), "CSV belum ditemukan."))
    idx <- input$chart_index
    col <- mean_column(stats_raw, idx)
    validate(need(!is.na(col), paste("Kolom statistik untuk", idx,
                                     "belum ditemukan di CSV.")))
    d <- stats_raw
    d$.value <- numeric_col(d, col)
    d <- d[is.finite(d$.value), , drop = FALSE]
    validate(need(nrow(d) > 0, "Tidak ada nilai numerik yang dapat digrafikkan."))

    if (input$chart_type == "hist") {
      ggplot(d, aes(x = .value)) +
        geom_histogram(bins = 30, color = "white", fill = "#247ba0") +
        theme_minimal(base_size = 13) +
        labs(title = paste("Distribusi", idx), x = col, y = "Jumlah baris")
    } else if (input$chart_type == "district") {
      validate(need("WADMKC" %in% names(d), "Kolom WADMKC tidak ditemukan."))
      agg <- d %>% group_by(WADMKC) %>%
        summarise(Value = mean(.value, na.rm = TRUE), .groups = "drop") %>%
        arrange(Value)
      ggplot(agg, aes(x = reorder(WADMKC, Value), y = Value)) +
        geom_col(fill = "#247ba0") + coord_flip() +
        theme_minimal(base_size = 12) +
        labs(title = paste("Rata-rata", idx, "per kecamatan"),
             x = NULL, y = col)
    } else {
      validate(need("NAMOBJ" %in% names(d), "Kolom NAMOBJ tidak ditemukan."))
      d <- d %>% arrange(desc(.value)) %>% slice_head(n = 10)
      ggplot(d, aes(x = reorder(NAMOBJ, .value), y = .value)) +
        geom_col(fill = "#247ba0") + coord_flip() +
        theme_minimal(base_size = 12) +
        labs(title = paste("10 desa dengan nilai", idx, "tertinggi"),
             x = NULL, y = col)
    }
  })

  output$file_report <- renderPrint({
    cat("Folder data:", normalizePath(DATA_DIR, winslash = "/", mustWork = FALSE), "\n\n")
    cat("CSV statistik:\n")
    print(find_first(CSV_CANDIDATES))
    cat("\nRaster:\n")
    for (k in names(RASTER_FILES)) {
      cat(k, ":", find_first(RASTER_FILES[[k]]), "\n")
    }
    cat("\nVektor administrasi:", find_first(VECTOR_CANDIDATES), "\n")
  })
}

shinyApp(ui, server)
