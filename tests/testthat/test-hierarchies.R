test_that("read_hier loads packaged hierarchies correctly", {
  h_min <- read_hier("minimal")
  h_tab <- read_hier("table_header")

  expect_s3_class(h_min, "data.frame")
  expect_equal(ncol(h_min), 2)
  expect_true(nrow(h_min) > 0)
  expect_true(all(grepl("^@+$", h_min[[1]])))

  expect_s3_class(h_tab, "data.frame")
  expect_equal(ncol(h_tab), 2)
})

test_that("simple_hier creates expected hierarchy structure", {
  h <- simple_hier(n = 4, code_name = "B")

  expect_s3_class(h, "data.frame")
  expect_equal(nrow(h), 5)
  expect_equal(h$level, c("@", rep("@@", 4)))
  expect_equal(h$name[1], "TOTAL")
  expect_equal(h$name[-1], c("B001", "B002", "B003", "B004"))
})

test_that("remove_bogus removes single-child codes and fixes @ levels", {
  # Construct a hierarchy where "Sub1" only has a single child "Sub1_A" (bogus)
  hier_with_bogus <- data.frame(
    levels = c("@", "@@", "@@@", "@@", "@@@", "@@@"),
    codes = c("Total", "Sub1", "Sub1_A", "Sub2", "Sub2_A", "Sub2_B"),
    stringsAsFactors = FALSE
  )

  cleaned <- remove_bogus(hier_with_bogus)

  # The single child ("Sub1_A") that maps 1-to-1 to "Sub1" is removed
  expect_false("Sub1_A" %in% cleaned$codes)
  expect_true("Sub1" %in% cleaned$codes)
  expect_equal(cleaned$levels[cleaned$codes == "Sub1"], "@@")
  # Valid parent Sub2 and its two children remain intact
  expect_true(all(c("Sub2", "Sub2_A", "Sub2_B") %in% cleaned$codes))
})

test_that("add_mean_n_at calculates mean_n_at and HiTaS_Class accurately", {
  h1 <- simple_hier(2, "A")
  h2 <- simple_hier(2, "B")
  hiers <- list(var1 = h1, var2 = h2)

  out <- expand.grid(
    var1 = c("TOTAL", "A001"),
    var2 = c("TOTAL", "B001"),
    stringsAsFactors = FALSE
  )

  res <- add_mean_n_at(out, hiers, include_HiTaS_Class = TRUE)

  expect_true(all(c("mean_n_at", "HiTaS_Class") %in% names(res)))
  # (TOTAL, TOTAL) -> (1, 1) -> mean = 1, HiTaS_Class = 0
  row_tot <- res$var1 == "TOTAL" & res$var2 == "TOTAL"
  expect_equal(res$mean_n_at[row_tot], 1)
  expect_equal(res$HiTaS_Class[row_tot], 0)

  # (A001, B001) -> (2, 2) -> mean = 2, HiTaS_Class = 2
  row_inner <- res$var1 == "A001" & res$var2 == "B001"
  expect_equal(res$mean_n_at[row_inner], 2)
  expect_equal(res$HiTaS_Class[row_inner], 2)
})
