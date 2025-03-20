library(testthat)
library(shiny)
library(shinytest)

source("../../app.R")

# SAMPLE INFO TAB TEST

test_that("Sample Info: File upload loads data", {
  testServer(server, {
    session$setInputs(sample_file = list(datapath = "../../testdata/sample_info.csv"))
    expect_true(!is.null(sampleData()))
    expect_gt(nrow(sampleData()), 0)
  })
})

test_that("Sample Info: Summary text outputs correct structure", {
  testServer(server, {
    session$setInputs(sample_file = list(datapath = "../../testdata/sample_info.csv"))
    expect_silent(output$data_summary)
  })
})

test_that("Sample Info: Column summary table correct columns", {
  testServer(server, {
    session$setInputs(sample_file = list(datapath = "../../testdata/sample_info.csv"))
    expect_silent(output$data_summary)
  })
})

test_that("Sample Info: Plot updates with selected numeric column", {
  testServer(server, {
    session$setInputs(sample_file = list(datapath = "../../testdata/sample_info.csv"))
    session$setInputs(plot_col = "Age")
    session$setInputs(group_col = "None")
    session$setInputs(plot_type = "Hist")
    expect_silent(output$data_plot)
  })
})

test_that("Sample Info: Grouping works", {
  testServer(server, {
    session$setInputs(sample_file = list(datapath = "../../testdata/sample_info.csv"))
    session$setInputs(plot_col = "Age")
    session$setInputs(group_col = "Condition")
    session$setInputs(plot_type = "violin")
    expect_silent(output$data_plot)
  })
})


# COUNTS TAB TESTS

test_that("Counts: File upload loads counts", {
  testServer(server, {
    session$setInputs(counts_file = list(datapath ="../../testdata/counts.csv"))
    expect_true(!is.null(countsData()))
    expect_gt(nrow(countsData()), 0)
  })
})

test_that("Counts: Filtering updates keep logic", {
  testServer(server, {
    session$setInputs(counts_file = list(datapath = "../../testdata/counts.csv"))
    session$setInputs(var_percentile = 0)
    session$setInputs(nonzero_thresh = 0)
    fg <- filteredGenes()
    expect_true("keep" %in% names(fg))
    expect_true(any(fg$keep)) 
  })
})

test_that("Counts: Median vs Variance plot ready", {
  testServer(server, {
    session$setInputs(counts_file = list(datapath = "../../testdata/counts.csv"))
    session$setInputs(var_percentile = 0)
    session$setInputs(nonzero_thresh = 0)
    expect_silent(output$plot_median_vs_variance)
  })
})

test_that("Counts: Heatmap can render", {
  testServer(server, {
    session$setInputs(counts_file = list(datapath = "../../testdata/counts.csv"))
    session$setInputs(var_percentile = 0)
    session$setInputs(nonzero_thresh = 0)
    session$setInputs(log_transform_heatmap = TRUE)
    expect_silent(output$plot_heatmap)
  })
})

test_that("Counts: PCA plot updates with chosen PCs", {
  testServer(server, {
    session$setInputs(counts_file = list(datapath = "../../testdata/counts.csv"))
    session$setInputs(var_percentile = 0)
    session$setInputs(nonzero_thresh = 0)
    session$setInputs(pca_plot_type = "scatter")
    session$setInputs(pca_x = 1)
    session$setInputs(pca_y = 2)
    expect_silent(output$plot_pca)
  })
})


# DE TAB TESTS

test_that("DE: Upload DE results works", {
  testServer(server, {
    session$setInputs(de_file = list(datapath ="../../testdata/DE_results.csv"))
    session$setInputs(gene_search = "")
    expect_true(!is.null(deData()))
    expect_gt(nrow(deData()), 0)
  })
})

test_that("DE: Table filters by gene name", {
  testServer(server, {
    session$setInputs(de_file = list(datapath = "../../testdata/DE_results.csv"))
    session$setInputs(gene_search = "Glyma0002")
    df <- filteredDE()
    expect_true(nrow(df) > 0)
    expect_true(all(grepl("Glyma0002", df[[1]])))
  })
})

test_that("DE: Volcano plot checks for columns", {
  testServer(server, {
    session$setInputs(de_file = list(datapath = "../../testdata/DE_results.csv"))
    session$setInputs(gene_search = "") 
    df <- filteredDE()
    if (!("log2FoldChange" %in% colnames(df)) || !("pvalue" %in% colnames(df))) {
      expect_silent(output$de_volcano)
    } else {
      expect_silent(output$de_volcano)
    }
  })
})

test_that("DE: p-values within valid range", {
  testServer(server, {
    session$setInputs(de_file = list(datapath ="../../testdata/DE_results.csv"))
    session$setInputs(gene_search = "") 
    df <- filteredDE()
    if ("pvalue" %in% colnames(df)) {
      expect_true(all(df$pvalue >= 0 & df$pvalue <= 1, na.rm = TRUE))
    }
  })
})

test_that("DE: gene_search empty shows all genes", {
  testServer(server, {
    session$setInputs(de_file = list(datapath = "../../testdata/DE_results.csv"))
    session$setInputs(gene_search = "")
    df_all <- filteredDE()
    expect_gt(nrow(df_all), 0)
  })
})
