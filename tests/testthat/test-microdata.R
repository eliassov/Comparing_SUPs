test_that("rpareto2 generates bounded positive whole numbers", {
  set.seed(42)
  x <- rpareto2(100, shape = 1.5, max_response = 1000)

  expect_length(x, 100)
  expect_true(all(x >= 1))
  expect_true(all(x <= 1000))
  expect_equal(max(x), 1000)
  expect_equal(x, floor(x))
})

test_that("add_response is reproducible and preserves global .Random.seed", {
  df <- data.frame(id = 1:20)

  set.seed(999)
  seed_before <- .Random.seed
  res1 <- add_response(df, shape = 1.1, rnd_seed = 123)
  seed_after <- .Random.seed

  expect_identical(seed_before, seed_after)
  expect_true("response" %in% names(res1))

  res2 <- add_response(df, shape = 1.1, rnd_seed = 123)
  expect_identical(res1$response, res2$response)
})

test_that("create_microdata and info_microdata work as expected", {
  hiers <- list(
    var1 = read_hier("minimal"),
    var2 = read_hier("table_header")
  )

  df <- create_microdata(hiers, n_ids = 50, n_unique = 8, rnd_seed = 10, sample_seed = 20)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 50)
  expect_equal(names(df), c("var1", "var2", "id", "response"))
  expect_equal(nrow(unique(df[c("var1", "var2")])), 8)

  # Capture printed output of info_microdata and verify returned data frame
  expect_output(
    info_df <- info_microdata(df, hiers, return_frame = TRUE),
    "n_inner_cells"
  )
  expect_s3_class(info_df, "data.frame")
  expect_equal(nrow(info_df), 8)
  expect_equal(sum(info_df$freq), 50)
})

test_that("create_microdata_from_filename validates input and creates data", {
  expect_error(create_microdata_from_filename(123), "single non-missing character")
  expect_error(create_microdata_from_filename("unknown_case"), "Unknown filename")

  # Test smallest benchmark configuration (oyl_28b: n_ids = 2950, n_unique = 1000)
  df_bench <- create_microdata_from_filename("oyl_28b")
  expect_s3_class(df_bench, "data.frame")
  expect_equal(nrow(df_bench), 2950)
})
