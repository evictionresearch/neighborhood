# hud_ami(): offline tests against the bundled FY2024 HUD snapshot.

test_that("hud_ami returns the published HUD AMI level with provenance", {
  out <- hud_ami("53", "033", 2024, ami_source = "hud")
  expect_equal(nrow(out), 1L)
  expect_identical(names(out), c("GEOID", "NAME", "year", "ami", "ami_source"))
  expect_identical(out$GEOID, "53033")
  expect_equal(out$year, 2024)
  expect_identical(out$ami_source, "hud")
  snap <- readRDS(system.file("extdata", "hud_il_2024.rds",
                              package = "neighborhood"))
  expect_equal(out$ami, as.numeric(snap$median_income[snap$GEOID == "53033"]))
})

test_that("hud_ami vectorizes over counties and years", {
  out <- hud_ami("06", c("073", "075"), years = c(2024, 2024),
                 ami_source = "hud")
  expect_equal(nrow(out), 4L)                    # 2 counties x 2 (repeated) years
  expect_setequal(unique(out$GEOID), c("06073", "06075"))
  expect_true(all(out$ami > 0))
})

test_that("hud_ami validates inputs", {
  expect_error(hud_ami("53", "033", years = "2024"))
  expect_error(hud_ami("53", "033", years = NA_real_))
  expect_error(hud_ami("53", "033", 2024, ami_source = "nope"))
})
