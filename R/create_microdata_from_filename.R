


#
# To recreate the microdata of the oyl files
#


if (FALSE) {  # Example 
  a <- create_microdata_from_filename("oyl_26d")
  head(a)
  
  f <- paste0("oyl_", 1:28)
  ff <- rep(f, each = 4)
  filenames <- c(paste0(ff, c("a", "b", "c", "d")))
  filenames
  
  b <- create_microdata_from_filename(filenames[58])
  tail(b)
}



#' Recreate Microdata from Benchmark Case Filename
#'
#' Recreates the exact simulated microdata for a specific benchmark test case
#' (e.g., "oyl_1a", "oyl_26d") by looking up its original configuration settings
#' and structural hierarchies.
#'
#' @param filename A single, non-missing character string representing the benchmark
#'   case identifier (e.g., "oyl_26d").
#'
#' @return A data frame containing the generated mock microdata for the specified benchmark case.
#' @export
create_microdata_from_filename <- function(filename) {
  if (!is.character(filename) || length(filename) != 1L || is.na(filename)) {
    stop("filename must be a single non-missing character string.", call. = FALSE)
  }

  configs <- list(
    oyl_1a = list(group = "oyl_1", n_ids = 120000, n_unique = 30000),
    oyl_1b = list(group = "oyl_1", n_ids = 100000, n_unique = 30000, shape = 2),
    oyl_1c = list(group = "oyl_1", n_ids = 500000, n_unique = 52900),
    oyl_1d = list(group = "oyl_1", n_ids = 350000, n_unique = 52900, shape = 2),
    oyl_2a = list(group = "oyl_2", n_ids = 350000, n_unique = 30000),
    oyl_2b = list(group = "oyl_2", n_ids = 250000, n_unique = 30000, shape = 2),
    oyl_2c = list(group = "oyl_2", n_ids = 250000, n_unique = 38000),
    oyl_2d = list(group = "oyl_2", n_ids = 200000, n_unique = 38000, shape = 2),
    oyl_3a = list(group = "oyl_3", n_ids = 2500000, n_unique = 50000),
    oyl_3b = list(group = "oyl_3", n_ids = 1500000, n_unique = 50000, shape = 2),
    oyl_3c = list(group = "oyl_3", n_ids = 2500000, n_unique = 44000),
    oyl_3d = list(group = "oyl_3", n_ids = 1500000, n_unique = 44000, shape = 2),
    oyl_4a = list(group = "oyl_4", n_ids = 5000000, n_unique = 25000),
    oyl_4b = list(group = "oyl_4", n_ids = 3000000, n_unique = 25000, shape = 2),
    oyl_4c = list(group = "oyl_4", n_ids = 100000, n_unique = 35000),
    oyl_4d = list(group = "oyl_4", n_ids = 87000, n_unique = 35000, shape = 2),
    oyl_5a = list(group = "oyl_5", n_ids = 1000000, n_unique = 25000),
    oyl_5b = list(group = "oyl_5", n_ids = 600000, n_unique = 25000, shape = 2),
    oyl_5c = list(group = "oyl_5", n_ids = 10000000, n_unique = 20000),
    oyl_5d = list(group = "oyl_5", n_ids = 5500000, n_unique = 20000, shape = 2),
    oyl_6a = list(group = "oyl_6", n_ids = 40000, n_unique = 15000),
    oyl_6b = list(group = "oyl_6", n_ids = 34000, n_unique = 15000, shape = 2),
    oyl_6c = list(group = "oyl_6", n_ids = 250000, n_unique = 19000),
    oyl_6d = list(group = "oyl_6", n_ids = 200000, n_unique = 19000, shape = 2),
    oyl_7a = list(group = "oyl_7", n_ids = 1000000, n_unique = 22000),
    oyl_7b = list(group = "oyl_7", n_ids = 600000, n_unique = 22000, shape = 2),
    oyl_7c = list(group = "oyl_7", n_ids = 150000, n_unique = 25000),
    oyl_7d = list(group = "oyl_7", n_ids = 120000, n_unique = 25000, shape = 2),
    oyl_8a = list(group = "oyl_8", n_ids = 300000, n_unique = 21500),
    oyl_8b = list(group = "oyl_8", n_ids = 200000, n_unique = 21500, shape = 2),
    oyl_8c = list(group = "oyl_8", n_ids = 23000, n_unique = 12000),
    oyl_8d = list(group = "oyl_8", n_ids = 21000, n_unique = 12000, shape = 2),
    oyl_9a = list(group = "oyl_9", n_ids = 100000, n_unique = 6000),
    oyl_9b = list(group = "oyl_9", n_ids = 75000, n_unique = 6000, shape = 2),
    oyl_9c = list(group = "oyl_9", n_ids = 10000, n_unique = 4000),
    oyl_9d = list(group = "oyl_9", n_ids = 9000, n_unique = 4000, shape = 2),
    oyl_10a = list(group = "oyl_10", n_ids = 10000000, n_unique = 7500),
    oyl_10b = list(group = "oyl_10", n_ids = 5000000, n_unique = 7500, shape = 2),
    oyl_10c = list(group = "oyl_10", n_ids = 100000, n_unique = 5000),
    oyl_10d = list(group = "oyl_10", n_ids = 80000, n_unique = 5000, shape = 2),
    oyl_11a = list(group = "oyl_11", n_ids = 10000, n_unique = 4000),
    oyl_11b = list(group = "oyl_11", n_ids = 8500, n_unique = 4000, shape = 2),
    oyl_11c = list(group = "oyl_11", n_ids = 4000000, n_unique = 5000),
    oyl_11d = list(group = "oyl_11", n_ids = 2000000, n_unique = 5000, shape = 2),
    oyl_12a = list(group = "oyl_12", n_ids = 20000, n_unique = 5000),
    oyl_12b = list(group = "oyl_12", n_ids = 16000, n_unique = 5000, shape = 2),
    oyl_12c = list(group = "oyl_12", n_ids = 23000, n_unique = 3500),
    oyl_12d = list(group = "oyl_12", n_ids = 20000, n_unique = 3500, shape = 2),
    oyl_13a = list(group = "oyl_13", n_ids = 10000, n_unique = 3000),
    oyl_13b = list(group = "oyl_13", n_ids = 8500, n_unique = 3000, shape = 2),
    oyl_13c = list(group = "oyl_13", n_ids = 20000, n_unique = 4048),
    oyl_13d = list(group = "oyl_13", n_ids = 17000, n_unique = 4048, shape = 2),
    oyl_14a = list(group = "oyl_14", n_ids = 18000, n_unique = 1300),
    oyl_14b = list(group = "oyl_14", n_ids = 11000, n_unique = 1300, shape = 2),
    oyl_14c = list(group = "oyl_14", n_ids = 2000000, n_unique = 1600),
    oyl_14d = list(group = "oyl_14", n_ids = 800000, n_unique = 1600, shape = 2),
    oyl_15a = list(group = "oyl_15", n_ids = 5000, n_unique = 1700),
    oyl_15b = list(group = "oyl_15", n_ids = 4300, n_unique = 1700, shape = 2),
    oyl_15c = list(group = "oyl_15", n_ids = 15000, n_unique = 1600),
    oyl_15d = list(group = "oyl_15", n_ids = 10000, n_unique = 1600, shape = 2),
    oyl_16a = list(group = "oyl_16", n_ids = 10000, n_unique = 3000),
    oyl_16b = list(group = "oyl_16", n_ids = 8200, n_unique = 3000, shape = 2),
    oyl_16c = list(group = "oyl_16", n_ids = 25000, n_unique = 2200),
    oyl_16d = list(group = "oyl_16", n_ids = 18000, n_unique = 2200, shape = 2),
    oyl_17a = list(group = "oyl_17", n_ids = 100000, n_unique = 15000),
    oyl_17b = list(group = "oyl_17", n_ids = 78000, n_unique = 15000, shape = 2),
    oyl_17c = list(group = "oyl_17", n_ids = 200000, n_unique = 10000),
    oyl_17d = list(group = "oyl_17", n_ids = 160000, n_unique = 10000, shape = 2),
    oyl_18a = list(group = "oyl_18", n_ids = 80000, n_unique = 13000),
    oyl_18b = list(group = "oyl_18", n_ids = 65000, n_unique = 13000, shape = 2),
    oyl_18c = list(group = "oyl_18", n_ids = 500000, n_unique = 14000),
    oyl_18d = list(group = "oyl_18", n_ids = 350000, n_unique = 14000, shape = 2),
    oyl_19a = list(group = "oyl_19", n_ids = 80000, n_unique = 14000),
    oyl_19b = list(group = "oyl_19", n_ids = 62000, n_unique = 14000, shape = 2),
    oyl_19c = list(group = "oyl_19", n_ids = 300000, n_unique = 19000),
    oyl_19d = list(group = "oyl_19", n_ids = 200000, n_unique = 19000, shape = 2),
    oyl_20a = list(group = "oyl_20", n_ids = 5000000, n_unique = 9000),
    oyl_20b = list(group = "oyl_20", n_ids = 3000000, n_unique = 9000, shape = 2),
    oyl_20c = list(group = "oyl_20", n_ids = 500000, n_unique = 11000),
    oyl_20d = list(group = "oyl_20", n_ids = 350000, n_unique = 11000, shape = 2),
    oyl_21a = list(group = "oyl_21", n_ids = 50000, n_unique = 5000),
    oyl_21b = list(group = "oyl_21", n_ids = 35000, n_unique = 5000, shape = 2),
    oyl_21c = list(group = "oyl_21", n_ids = 100000, n_unique = 3500),
    oyl_21d = list(group = "oyl_21", n_ids = 70000, n_unique = 3500, shape = 2),
    oyl_22a = list(group = "oyl_22", n_ids = 300000, n_unique = 4500),
    oyl_22b = list(group = "oyl_22", n_ids = 250000, n_unique = 4500, shape = 2),
    oyl_22c = list(group = "oyl_22", n_ids = 15000000, n_unique = 4000),
    oyl_22d = list(group = "oyl_22", n_ids = 10000000, n_unique = 4000, shape = 2),
    oyl_23a = list(group = "oyl_23", n_ids = 10000, n_unique = 2500),
    oyl_23b = list(group = "oyl_23", n_ids = 8000, n_unique = 2500, shape = 2),
    oyl_23c = list(group = "oyl_23", n_ids = 20000, n_unique = 3000),
    oyl_23d = list(group = "oyl_23", n_ids = 16000, n_unique = 3000, shape = 2),
    oyl_24a = list(group = "oyl_24", n_ids = 200000, n_unique = 3500),
    oyl_24b = list(group = "oyl_24", n_ids = 120000, n_unique = 3500, shape = 2),
    oyl_24c = list(group = "oyl_24", n_ids = 100000, n_unique = 3100),
    oyl_24d = list(group = "oyl_24", n_ids = 70000, n_unique = 3100, shape = 2),
    oyl_25a = list(group = "oyl_25", n_ids = 100000, n_unique = 1000),
    oyl_25b = list(group = "oyl_25", n_ids = 50000, n_unique = 1000, shape = 2),
    oyl_25c = list(group = "oyl_25", n_ids = 500000, n_unique = 750),
    oyl_25d = list(group = "oyl_25", n_ids = 300000, n_unique = 750, shape = 2),
    oyl_26a = list(group = "oyl_26", n_ids = 80000, n_unique = 1400),
    oyl_26b = list(group = "oyl_26", n_ids = 40000, n_unique = 1400, shape = 2),
    oyl_26c = list(group = "oyl_26", n_ids = 5000, n_unique = 1100),
    oyl_26d = list(group = "oyl_26", n_ids = 4000, n_unique = 1100, shape = 2),
    oyl_27a = list(group = "oyl_27", n_ids = 80000, n_unique = 1650),
    oyl_27b = list(group = "oyl_27", n_ids = 65000, n_unique = 1650, shape = 2),
    oyl_27c = list(group = "oyl_27", n_ids = 9000, n_unique = 1300),
    oyl_27d = list(group = "oyl_27", n_ids = 6500, n_unique = 1300, shape = 2),
    oyl_28a = list(group = "oyl_28", n_ids = 4000, n_unique = 1000),
    oyl_28b = list(group = "oyl_28", n_ids = 2950, n_unique = 1000, shape = 2),
    oyl_28c = list(group = "oyl_28", n_ids = 5000, n_unique = 850),
    oyl_28d = list(group = "oyl_28", n_ids = 3800, n_unique = 850, shape = 2)
  )

  config <- configs[[filename]]
  if (is.null(config)) {
    stop("Unknown filename: ", filename, call. = FALSE)
  }

  hiers <- switch(
    config$group,
    oyl_1 = list(
      var1 = read_hier("region_mid")[1:130, ],
                    var2 = read_hier("nace_2"),
                    var3 = simple_hier(5, "A")
    ),
    oyl_2 = list(
      var1 = read_hier("region_large")[1:400, ],
                    var2 = read_hier("nace_2")[1:15, ],
                    var3 = read_hier("table_header")
    ),
    oyl_3 = list(
      var1 = read_hier("region_large")[1:200, ],
                    var2 = read_hier("nace_2")[1:30, ],
                    var3 = read_hier("table_header")
    ),
    oyl_4 = list(
      var1 = read_hier("large"),
                    var2 = read_hier("table_header"),
                    var3 = simple_hier(20, "A")
    ),
    oyl_5 = list(
      var1 = read_hier("region_mid")[c(1, 355:446), ],
                    var2 = read_hier("nace_2")[1:98, ],
                    var3 = simple_hier(4, "A")
    ),
    oyl_6 = list(
      var1 = read_hier("region_large")[c(1, 10670:10906), ],
                    var2 = read_hier("nace_2")[c(1, 99:111), ],
                    var3 = read_hier("table_header")
    ),
    oyl_7 = list(
      var1 =  read_hier("region_large")[c(1, 10670:10906), ],
                    var2 = read_hier("nace_2")[c(1, 60:89), ],
                    var3 = simple_hier(6, "A")
    ),
    oyl_8 = list(
      var1 = read_hier("large"),
                    var2 = read_hier("table_header"),
                    var3 = simple_hier(10, "A")
    ),
    oyl_9 = list(
      var1 = read_hier("region_mid")[c(1, 132:200), ],
                    var2 = read_hier("nace_2")[c(1,70:111), ],
                    var3 = simple_hier(4, "A")
    ),
    oyl_10 = list(
      var1 = simple_hier(8, "A"),
                    var2 = read_hier("nace_2"),
                    var3 = read_hier("table_header")
    ),
    oyl_11 = list(
      var1 =  read_hier("nace_2"),
                    var2 = read_hier("nace_2")[c(1, 60:68), ],
                    var3 = read_hier("table_header")
    ),
    oyl_12 = list(
      var1 = read_hier("large"),
                    var2 = read_hier("table_header"),
                    var3 = simple_hier(3, "A")
    ),
    oyl_13 = list(
      var1 = read_hier("table_header"),
                    var2 = read_hier("nace_2"),
                    var3 = simple_hier(4, "A")
    ),
    oyl_14 = list(
      var1 = simple_hier(2, "A"),
                    var2 = read_hier("large")[1:100, ],
                    var3 = read_hier("table_header")
    ),
    oyl_15 = list(
      var1 =  read_hier("nace_2")[c(1:45), ],
                    var2 = read_hier("nace_2")[c(1, 60:68), ],
                    var3 = read_hier("table_header")
    ),
    oyl_16 = list(
      var1 = read_hier("region_mid")[1:100, ],
                    var2 = read_hier("table_header"),
                    var3 = simple_hier(4, "A")
    ),
    oyl_17 = list(
      var1 = read_hier("nace_3"),
                    var2 = read_hier("region_mid")[1:72,]
    ),
    oyl_18 = list(
      var1 = read_hier("nace_2")[1:98, ],
                    var2 = read_hier("large")
    ),
    oyl_19 = list(
      var1 =  read_hier("region_large")[1:3000, ],
                    var2 = simple_hier(7, "A")
    ),
    oyl_20 = list(
      var1 = read_hier("large")[1:87, ],
                    var2 = read_hier("large")
    ),
    oyl_21 = list(
      var1 = read_hier("region_mid"),
                    var2 = read_hier("nace_2")[1:18, ]
    ),
    oyl_22 = list(
      var1 = read_hier("region_large")[c(1:500), ],
                    var2 = read_hier("table_header")
    ),
    oyl_23 = list(
      var1 =  read_hier("nace_3"),
                    var2 = read_hier("nace_2")[1:20, ]
    ),
    oyl_24 = list(
      var1 = read_hier("nace_3")[1:80,],
                    var2 = read_hier("nace_3")[c(1, 148:245),]
    ),
    oyl_25 = list(
      var1 = read_hier("nace_2")[1:8, ],
                    var2 = read_hier("region_mid")[1:299, ]
    ),
    oyl_26 = list(
      var1 = read_hier("table_header"),
                    var2 = read_hier("large")[1:198, ]
    ),
    oyl_27 = list(
      var1 =  read_hier("nace_3")[1:197, ],
                    var2 = read_hier("table_header")
    ),
    oyl_28 = list(
      var1 = read_hier("nace_3"),
                    var2 = simple_hier(5, "A")
    )
  )

  hiers <- lapply(hiers, remove_bogus)

  args <- config[names(config) != "group"]
  do.call(create_microdata, c(list(hiers), args))
}
