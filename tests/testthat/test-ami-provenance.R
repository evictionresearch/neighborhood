# The ami_source recorded on results must be the source actually resolved,
# never the "auto" request. Offline via the bundled FY2024 snapshot.

test_that("ami_cutoffs stamps an explicit source as the attribute", {
  out <- ami_cutoffs("53", "033", 2024, ami_source = "hud")
  expect_identical(attr(out, "ami_source"), "hud")
})

test_that("ami_source = 'auto' resolves and stamps the winning source", {
  out <- ami_cutoffs("53", "033", 2024, ami_source = "auto")
  src <- attr(out, "ami_source")
  expect_true(src %in% c("hud", "hud_acs", "acs_fmr", "acs"))
  # King County is in the bundled snapshot, so auto lands on exact HUD offline
  expect_identical(src, "hud")
  # and the cutoffs match the explicit-source call
  expect_equal(out$cut_VLI, ami_cutoffs("53", "033", 2024, ami_source = "hud")$cut_VLI)
})
