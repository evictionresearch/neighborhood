# inflow_index(): revealed low-income destinations from HPRM net migration.
# No-network -- small hand-built HPRM + afford_index-shaped frames.

test_that("overall uses dis_value; tier uses the matched raw nmr_pred", {
  hp <- data.frame(GEOID = c("A", "B", "C"),
                   dis_value   = c(200, 30, -100),
                   nmr_pred_el = c(60, -10, 5),
                   nmr_pred_vl = c(80, 40, -50),
                   nmr_pred_l  = c(300, 20, -5))
  ov <- inflow_index(hp)                                  # overall -> dis_value
  expect_equal(ov$inflow, c(200, 30, -100))
  expect_equal(ov$growth, c(TRUE, FALSE, FALSE))          # > +50
  expect_equal(as.character(ov$inflow_cat), c("growth", "break-even", "outflow"))

  vl <- inflow_index(hp, tier = "VLI")                    # -> nmr_pred_vl
  expect_equal(vl$inflow, c(80, 40, -50))
  expect_equal(as.character(vl$inflow_cat), c("growth", "break-even", "outflow"))
  el <- inflow_index(hp, tier = "ELI")                    # -> nmr_pred_el
  expect_equal(el$inflow, c(60, -10, 5))
})

test_that("growth_cut is configurable", {
  hp <- data.frame(GEOID = c("A", "B"), dis_value = c(120, 60),
                   nmr_pred_el = 1, nmr_pred_vl = 1, nmr_pred_l = 1)
  expect_equal(inflow_index(hp, growth_cut = 100)$growth, c(TRUE, FALSE))  # 120>100, 60<100
})

test_that("joins onto an afford_index result and preserves its columns", {
  x <- data.frame(GEOID = c("A", "B"), ami_tier = c("VLI", "VLI"), supply = c(0.4, 0.6))
  hp <- data.frame(GEOID = c("A", "B"), dis_value = c(100, -20),
                   nmr_pred_el = c(1, 1), nmr_pred_vl = c(70, -30), nmr_pred_l = c(1, 1))
  j <- inflow_index(hp, tier = "VLI", x = x)
  expect_true(all(c("supply", "inflow", "growth", "inflow_cat") %in% names(j)))
  expect_equal(j$inflow, c(70, -30))
  expect_equal(j$supply, c(0.4, 0.6))
})

test_that("tier-specific inflow errors when the raw nmr_pred fields are absent", {
  hp <- data.frame(GEOID = "A", dis_value = 100)          # dis_value only
  expect_equal(inflow_index(hp)$inflow, 100)               # overall still works
  expect_error(inflow_index(hp, tier = "VLI"), "nmr_pred")
})

test_that("accepts lower-case geoid and errors on missing GEOID", {
  hp <- data.frame(geoid = "A", dis_value = 75, nmr_pred_el = 1, nmr_pred_vl = 1, nmr_pred_l = 1)
  expect_equal(inflow_index(hp)$GEOID, "A")
  expect_error(inflow_index(data.frame(z = 1)), "GEOID")
})
