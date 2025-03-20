## Author: Youman/Lucy Mo
## tobihane@bu.edu
## BU CDSBF 591
## Final Project

library(shiny)
library(ggplot2)
library(DT)
library(pheatmap)
library(dplyr)
library(reshape2)
library(igraph)
library(pheatmap)

# Increase the limit of data
options(shiny.maxRequestSize = 20 * 1024^2)

ui <- fluidPage(
  titlePanel("BF591 - RShiny Project"),
  tabsetPanel(
    # TAB 1: Sample Information Exploration
    tabPanel("Sample Info",
             sidebarLayout(
               sidebarPanel(
                 h4("Upload Sample Info"),
                 fileInput("sample_file", 
                           "Choose Sample Info CSV File",
                           accept = c(".csv")),
                 
                 # UI elements
                 uiOutput("plotColumnUI"),
                 uiOutput("groupByColumnUI"),
                 radioButtons("plot_type", "Plot Type:",
                              choices = c("Histogram" = "hist",
                                          "Density" = "density",
                                          "Violin" = "violin"),
                              selected = "hist")
               ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Summary", 
                                      h3("Data Summary"),
                                      verbatimTextOutput("data_summary"),
                                      br(),
                                      h3("Column Summary"),
                                      tableOutput("column_summary")
                             ),
                             tabPanel("Table", 
                                      DTOutput("sample_table")
                             ),
                             tabPanel("Plots",
                                      plotOutput("data_plot")
                             )
                 )
               )
             )
    ),
    
    # TAB 2: Counts Matrix Exploration
    tabPanel("Counts",
             sidebarLayout(
               sidebarPanel(
                 h4("Upload Normalized Counts Matrix"),
                 fileInput("counts_file", "Choose Counts CSV File", accept = ".csv"),
                 
                 h4("Filtering Controls"),
                 sliderInput("var_percentile", "Variance Percentile Threshold:", min = 0, max = 100, value = 50),
                 sliderInput("nonzero_thresh", "Min # of Samples with Non-Zero Counts:", 
                             min = 0, max = 10, value = 3),
                 
                 checkboxInput("log_transform_heatmap", "Log-transform Counts for Heatmap", value = TRUE),
                 
                 h4("PCA Options"),
                 selectInput("pca_plot_type", "PCA Plot Type:",
                             choices = c("PC1 vs PC2" = "scatter", "Top N PCs Beeswarm" = "beeswarm")),
                 numericInput("pca_top_n", "Number of Top PCs (for beeswarm):", value = 5, min = 2, step = 1),
                 
                 numericInput("pca_x", "X-axis PC:", value = 1, min = 1, step = 1),
                 numericInput("pca_y", "Y-axis PC:", value = 2, min = 1, step = 1)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Summary", verbatimTextOutput("filter_summary")),
                   tabPanel("Diagnostic Plots",
                            h3("Median Count vs Variance"),
                            plotOutput("plot_median_vs_variance"),
                            h3("Median Count vs Number of Zeros"),
                            plotOutput("plot_median_vs_zeros")
                   ),
                   tabPanel("Heatmap", plotOutput("plot_heatmap", height = "600px")),
                   tabPanel("PCA", plotOutput("plot_pca"))
                 )
               )
             )
    ),
    
    # TAB 3: Differential Expression
    tabPanel("Differential Expression",
             sidebarLayout(
               sidebarPanel(
                 h4("Upload DE Results"),
                 fileInput("de_file", "Choose DE Results CSV File", accept = ".csv"),
                 textInput("gene_search", "Filter by Gene Name (regex):", "")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Table", DTOutput("de_table")),
                   tabPanel("Volcano Plot", plotOutput("de_volcano"))
                 )
               )
             )
    )
    
  )
)






server <- function(input, output, session) {

  # SECTION 1: Sample Information Exploration
  sampleData <- reactive({
    req(input$sample_file)
    df <- read.csv(input$sample_file$datapath, header = TRUE, stringsAsFactors = FALSE)
    df
  })
  
  output$data_summary <- renderPrint({
    req(sampleData())
    df <- sampleData()
    cat("Number of rows:", nrow(df), "\n")
    cat("Number of columns:", ncol(df), "\n\n")
    str(df)
  })
  
  output$column_summary <- renderTable({
    req(sampleData())
    df <- sampleData()
    summary_list <- lapply(names(df), function(col) {
      col_data <- df[[col]]
      col_type <- class(col_data)[1]
      
      if (is.numeric(col_data)) {
        mean_val <- round(mean(col_data, na.rm = TRUE), 2)
        sd_val <- round(sd(col_data, na.rm = TRUE), 2)
        val_summary <- paste0(mean_val, " (± ", sd_val, ")")
      } else if (is.factor(col_data) || is.character(col_data)) {
        distinct_vals <- unique(col_data)
        if (length(distinct_vals) > 5) {
          val_summary <- paste0(paste(distinct_vals[1:5], collapse = ", "), ", ...")
        } else {
          val_summary <- paste(distinct_vals, collapse = ", ")
        }
      } else {
        distinct_vals <- unique(col_data)
        val_summary <- paste(distinct_vals, collapse = ", ")
      }
      
      data.frame(
        "Column Name" = col,
        "Type" = col_type,
        "Mean (sd) or Distinct Values" = val_summary,
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, summary_list)
  })
  
  output$sample_table <- renderDT({
    req(sampleData())
    datatable(sampleData(), options = list(scrollX = TRUE))
  })
  
  output$plotColumnUI <- renderUI({
    req(sampleData())
    df <- sampleData()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    if (length(numeric_cols) == 0) {
      return(helpText("No numeric columns available for plotting."))
    }
    selectInput("plot_col", "Column to Plot:", choices = numeric_cols, selected = numeric_cols[1])
  })
  
  output$groupByColumnUI <- renderUI({
    req(sampleData())
    df <- sampleData()
    group_cols <- names(df)[!sapply(df, is.numeric)]
    group_cols <- c("None", group_cols)
    selectInput("group_col", "Group By:", choices = group_cols, selected = "None")
  })
  
  output$data_plot <- renderPlot({
    req(sampleData(), input$plot_col)
    df <- sampleData()
    plot_col <- input$plot_col
    group_col <- input$group_col
    plot_type <- input$plot_type
    
    if (group_col != "None") {
      df[[group_col]] <- as.factor(df[[group_col]])
    }
    
    p <- ggplot(df, aes_string(x = plot_col, fill = if (group_col != "None") group_col else NULL))
    
    if (plot_type == "hist") {
      p <- p + geom_histogram(position = "dodge", bins = 30, alpha = 0.7)
    } else if (plot_type == "density") {
      p <- p + geom_density(alpha = 0.7)
    } else if (plot_type == "violin") {
      if (group_col == "None") {
        df$DummyGroup <- factor("All")
        p <- ggplot(df, aes_string(x = "DummyGroup", y = plot_col)) + geom_violin(fill = "steelblue", alpha = 0.7)
      } else {
        p <- ggplot(df, aes_string(x = group_col, y = plot_col, fill = group_col)) + geom_violin(alpha = 0.7)
      }
    }
    p + theme_minimal() +
      labs(title = paste("Plot of", plot_col, ifelse(group_col != "None", paste("by", group_col), "")))
  })
  
  

  # SECTION 2: COUNTS MATRIX

  countsData <- reactive({
    req(input$counts_file)
    df <- read.csv(input$counts_file$datapath, header = TRUE, stringsAsFactors = FALSE)
    rownames(df) <- df[[1]]
    df <- df[,-1]
    df
  })
  
  geneStats <- reactive({
    req(countsData())
    mat <- countsData()
    var_vals <- apply(mat, 1, var, na.rm = TRUE)
    median_vals <- apply(mat, 1, median, na.rm = TRUE)
    zero_counts <- apply(mat, 1, function(x) sum(x == 0))
    
    data.frame(
      gene = rownames(mat),
      variance = var_vals,
      median_count = median_vals,
      zero_count = zero_counts,
      stringsAsFactors = FALSE
    )
  })
  
  filteredGenes <- reactive({
    req(geneStats())
    gs <- geneStats()
    mat <- countsData()
    
    var_threshold <- quantile(gs$variance, input$var_percentile/100)
    keep_by_var <- gs$variance >= var_threshold
    keep_by_nonzero <- (ncol(mat) - gs$zero_count) >= input$nonzero_thresh
    
    keep <- keep_by_var & keep_by_nonzero
    gs$keep <- keep
    gs
  })
  
  output$filter_summary <- renderPrint({
    req(filteredGenes(), countsData())
    gs <- filteredGenes()
    mat <- countsData()
    total_genes <- nrow(mat)
    total_samples <- ncol(mat)
    passing_genes <- sum(gs$keep)
    passing_percent <- round((passing_genes / total_genes) * 100, 2)
    failing_genes <- total_genes - passing_genes
    failing_percent <- 100 - passing_percent
    
    cat("Number of samples:", total_samples, "\n")
    cat("Total number of genes:", total_genes, "\n")
    cat("Number and % of genes passing filter:", passing_genes, "(", passing_percent, "%)\n")
    cat("Number and % of genes not passing filter:", failing_genes, "(", failing_percent, "%)\n")
  })
  
  output$plot_median_vs_variance <- renderPlot({
    req(filteredGenes())
    gs <- filteredGenes()
    ggplot(gs, aes(x = median_count, y = variance, color = keep)) +
      geom_point(alpha = 0.5) +
      scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "blue")) +
      scale_y_log10() +
      theme_minimal() +
      labs(title = "Median Count vs Variance", x = "Median Count", y = "Variance (log scale)", color = "Pass Filter")
  })
  
  output$plot_median_vs_zeros <- renderPlot({
    req(filteredGenes())
    gs <- filteredGenes()
    ggplot(gs, aes(x = median_count, y = zero_count, color = keep)) +
      geom_point(alpha = 0.5) +
      scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "blue")) +
      theme_minimal() +
      labs(title = "Median Count vs Number of Zeros", x = "Median Count", y = "Number of Zero Counts", color = "Pass Filter")
  })
  
  output$plot_heatmap <- renderPlot({
    req(filteredGenes(), countsData())
    gs <- filteredGenes()
    mat <- countsData()
    mat_filt <- mat[gs$keep, , drop = FALSE]
    
    if (input$log_transform_heatmap) {
      mat_filt <- log2(mat_filt + 1)
    }
    
    pheatmap(mat_filt, cluster_rows = TRUE, cluster_cols = TRUE, show_rownames = FALSE, 
             scale = "row", main = "Clustered Heatmap of Filtered Genes")
  })
  
  output$plot_pca <- renderPlot({
    req(filteredGenes(), countsData())
    gs <- filteredGenes()
    mat <- countsData()
    mat_filt <- mat[gs$keep, , drop = FALSE]
    
    pca_res <- prcomp(t(mat_filt), center = TRUE, scale. = TRUE)
    pve <- (pca_res$sdev^2 / sum(pca_res$sdev^2)) * 100
    pve_labels <- paste0("PC", 1:length(pve), " (", round(pve, 2), "%)")
    
    if (input$pca_plot_type == "scatter") {
      pc_x <- input$pca_x
      pc_y <- input$pca_y
      df_pca <- data.frame(pca_res$x, Sample = rownames(pca_res$x))
      ggplot(df_pca, aes_string(x = paste0("PC", pc_x), y = paste0("PC", pc_y))) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(x = pve_labels[pc_x], y = pve_labels[pc_y],
             title = paste("PCA Scatter:", pve_labels[pc_x], "vs", pve_labels[pc_y]))
      
    } else {
      top_n <- input$pca_top_n
      df_pca <- as.data.frame(pca_res$x[, 1:top_n, drop = FALSE])
      df_pca_long <- reshape2::melt(df_pca, variable.name = "PC", value.name = "Score")
      levels(df_pca_long$PC) <- pve_labels[1:top_n]
      
      ggplot(df_pca_long, aes(x = PC, y = Score)) +
        geom_jitter(width = 0.2, alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Top", top_n, "Principal Components (Beeswarm)"),
             x = "Principal Component", y = "Score")
    }
  })
  
  # SECTION 3: DIFFERENTIAL EXPRESSION
  deData <- reactive({
    req(input$de_file)
    df <- read.csv(input$de_file$datapath, header = TRUE, stringsAsFactors = FALSE)
    df
  })
  
  filteredDE <- reactive({
    req(deData())
    df <- deData()
    
    if (input$gene_search != "") {
      df <- df[grepl(input$gene_search, df[[1]], ignore.case = TRUE), ]
    }
    df
  })
  
  output$de_table <- renderDT({
    req(filteredDE())
    datatable(filteredDE(), options = list(scrollX = TRUE, pageLength = 25), filter = "top")
  })
  
  # Render Volcano Plot
  output$de_volcano <- renderPlot({
    req(filteredDE())
    df <- filteredDE()
    
    # Check for column names and adjust
    logFC_col <- NULL
    pval_col <- NULL
    
    # Attempt to identify log fold change and p-value columns
    if ("log2FoldChange" %in% colnames(df)) {
      logFC_col <- "log2FoldChange"
    } else if ("logFC" %in% colnames(df)) {
      logFC_col <- "logFC"
    }
    
    if ("P.value" %in% colnames(df)) {
      pval_col <- "P.value"
    } else if ("adj.P.Val" %in% colnames(df)) {
      pval_col <- "adj.P.Val"
    } else if ("P.Value" %in% colnames(df)) {
      pval_col <- "P.Value"
    }
    
    # If columns are missing, show a warning on the plot
    if (is.null(logFC_col) || is.null(pval_col)) {
      plot.new()
      text(0.5, 0.5, "DE results must contain log fold change (logFC) and p-value (P.value) columns.")
      return(NULL)
    }
    
    # Prepare volcano plot data
    df$negLogP <- -log10(df[[pval_col]])
    
    # Generate the volcano plot
    ggplot(df, aes(x = df[[logFC_col]], y = negLogP)) +
      geom_point(alpha = 0.5) +
      theme_minimal() +
      labs(
        title = "Volcano Plot",
        x = "log2(Fold Change)",
        y = "-log10(P-value)"
      ) +
      theme(plot.title = element_text(hjust = 0.5)) # Center the title
  })
}

shinyApp(ui, server)

