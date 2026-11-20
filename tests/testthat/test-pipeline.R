test_that("end-to-end suppression pipeline runs cleanly in tempdir", {
  hiers <- list(
    var1 = remove_bogus(read_hier("minimal")),
    var2 = remove_bogus(read_hier("table_header"))
  )
  df <- create_microdata(hiers, n_ids = 60, n_unique = 10, rnd_seed = 123, sample_seed = 123)

  # 1. check_primary & check_sdcTable
  expect_output(
    chk <- check_primary(df, hiers, do_check_sdcTable = TRUE),
    "n_cells"
  )
  expect_true(isTRUE(chk$ok_primary))

  # 2. Initialize file-backed pipeline in temporary directory
  tmp_dir <- file.path(tempdir(), "test_merged")
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  fname <- "unit_test_case"
  expect_output(
    initialize_gauss(fname, df, hiers, path = tmp_dir, overwrite = TRUE),
    "initialize_gauss"
  )
  rds_file <- file.path(tmp_dir, paste0(fname, ".rds"))
  expect_true(file.exists(rds_file))

  # 3. Run GAUSS secondary suppression
  add_gauss(fname, path = tmp_dir)
  obj <- readRDS(rds_file)
  expect_true("suppressed_gauss" %in% names(obj$df_merged))
  expect_true("gauss" %in% obj$df_merged$method)

  # 4. Run sdcTable secondary suppression
  expect_output(
    add_sdcTable(fname, path = tmp_dir, method = "SIMPLEHEURISTIC"),
    "protectTable"
  )
  obj <- readRDS(rds_file)
  expect_true("suppressed_simpleheuristic" %in% names(obj$df_merged))

  # 5. Compute safety intervals
  expect_output(
    add_intervals(fname, path = tmp_dir),
    "INTERVAL CALCULATIONS"
  )
  obj <- readRDS(rds_file)
  expect_true(all(c("lo_gauss", "up_gauss", "lo_simpleheuristic", "up_simpleheuristic") %in% names(obj$df_merged)))

  # 6. Compute unsafe cell flags
  expect_output(
    add_unsafe(fname, path = tmp_dir),
    "UNSAFE CALCULATIONS"
  )
  obj <- readRDS(rds_file)
  expect_true("unsafe_simpleheuristic" %in% names(obj$df_merged))

  # 7. Run comparative analysis
  analysis <- add_analysis(fname, path = tmp_dir, output = "analysis")
  expect_type(analysis, "list")
  expect_true(all(c("no_secondaries", "Protectedness", "Suppressions_by_Class") %in% names(analysis)))
  expect_equal(analysis$n_primary, sum(obj$df_merged$primary_gauss))
})
