# afford_verdict() + afford_capacity() on afford_index()-shaped toy data,
# plus afford_bands() differencing of the stretch columns.

toy <- data.frame(
  GEOID = c("a", "b", "c", "d"), tenure = "rent", ami_tier = "VLI",
  accessible = c(60, 30, 10, NA), total = c(100, 100, 100, 0),
  supply = c(0.6, 0.3, 0.1, NA),
  accessible_stretch = c(90, 60, 30, NA), supply_stretch = c(0.9, 0.6, 0.3, NA),
  reg_hh_tier = 1000, reg_hh_total = 2000,
  available_turnover = c(12, 6, 0.5, NA), available_vacancy = c(3, 1, 0.2, NA),
  stringsAsFactors = FALSE)

test_that("afford_verdict classifies on the two burden lines (majority rule)", {
  v <- afford_verdict(toy)
  expect_s3_class(v$verdict, "factor")
  expect_identical(levels(v$verdict),
                   c("affordable", "roughly affordable", "not affordable"))
  expect_identical(as.character(v$verdict),
                   c("affordable", "roughly affordable", "not affordable", NA))
})

test_that("afford_verdict share_cut moves the classes", {
  v <- afford_verdict(toy, share_cut = 0.25)
  # tract c: supply .1 < .25 but stretch .3 >= .25 -> roughly affordable
  expect_identical(as.character(v$verdict[v$GEOID == "c"]), "roughly affordable")
  # tract b: supply .3 >= .25 -> affordable outright
  expect_identical(as.character(v$verdict[v$GEOID == "b"]), "affordable")
})

test_that("afford_verdict errors without the stretch columns", {
  expect_error(afford_verdict(toy[, setdiff(names(toy), "supply_stretch")]),
               "supply_stretch")
})

test_that("afford_capacity does the free-will arithmetic", {
  cp <- afford_capacity(toy)
  expect_equal(nrow(cp), 1)
  expect_equal(cp$n_tracts, 4)
  expect_equal(cp$tier_households, 1000)
  expect_equal(cp$affordable_units, 100)              # 60 + 30 + 10
  expect_equal(cp$per100_affordable, 10)
  expect_equal(cp$shortfall, 900)
  expect_equal(cp$affordable_stretch_units, 180)
  expect_equal(cp$per100_stretch, 18)
  expect_equal(cp$open_per_year, 18.5)                # 12 + 6 + 0.5
  expect_equal(cp$open_now, 4.2)
  expect_equal(cp$tracts_lt1_open, 1)
})

test_that("afford_capacity works without stretch/availability, warns on dupes", {
  slim <- toy[, c("GEOID", "tenure", "ami_tier", "accessible", "reg_hh_tier")]
  cp <- afford_capacity(slim)
  expect_false(any(c("per100_stretch", "open_per_year") %in% names(cp)))
  expect_equal(cp$per100_affordable, 10)
  expect_warning(afford_capacity(rbind(slim, slim[1, ])), "double-count")
})

test_that("afford_capacity reports entry columns when present", {
  toy2 <- toy
  toy2$accessible_entry <- c(30, 15, 5, NA)
  toy2$available_entry_turnover <- c(6, 3, 0.25, NA)
  cp <- afford_capacity(toy2)
  expect_equal(cp$affordable_entry_units, 50)
  expect_equal(cp$per100_entry, 5)
  expect_equal(cp$open_entry_per_year, 9.25)
  expect_equal(cp$per100_open_entry_year, 0.925)
})

test_that("afford_bands differences the stretch columns too", {
  tb <- data.frame(
    GEOID = "a", tenure = "rent",
    ami_tier = factor(c("ELI", "VLI"), levels = c("ELI", "VLI")),
    accessible = c(10, 30), accessible_stretch = c(20, 50), total = 100,
    supply = c(0.1, 0.3), supply_stretch = c(0.2, 0.5),
    reg_hh_tier = c(100, 250), reg_hh_total = 1000,
    vacancy_rate = 0.05, turnover_rate = 0.2, stringsAsFactors = FALSE)
  b <- afford_bands(tb)
  vli <- b[b$ami_tier == "VLI", ]
  expect_equal(vli$accessible, 20)                    # 30 - 10
  expect_equal(vli$accessible_stretch, 30)            # 50 - 20
  expect_equal(vli$supply_stretch, 0.3)
  expect_equal(vli$reg_hh_tier, 150)
  # ELI (lowest tier) unchanged
  expect_equal(b$accessible_stretch[b$ami_tier == "ELI"], 20)
})
