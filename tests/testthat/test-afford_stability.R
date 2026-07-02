# afford_stability(): stability from HPRM anchors, availability folded into the chain.
# All no-network -- small hand-built afford_index-shaped frames + HPRM data.frames.

test_that("stability maps EDR/EER on published anchors and grades tracts", {
  x <- data.frame(GEOID = c("A", "B", "C"), accessible = c(100, 100, 100),
                  available_turnover = c(10, 10, 10), supply = c(0.5, 0.5, 0.5))
  hp <- data.frame(GEOID = c("A", "B", "C"),
                   dis_value = c(50, -125, -300),   # anchors: +50->1, -125->0.5, -300->0
                   ev_value  = c(0.8, 1.4, 2.0))     # anchors: 0.8->1, 1.4->0.5, 2.0->0
  out <- afford_stability(x, hp)
  expect_equal(out$s_edr, c(1, 0.5, 0))
  expect_equal(out$s_eer, c(1, 0.5, 0))
  expect_equal(out$stability, c(1, 0.5, 0))
  expect_equal(out$stable_dest, c(10, 5, 0))              # available_turnover * stability
  expect_equal(out$stable, c(TRUE, FALSE, FALSE))          # stability >= stable_cut (0.8)
  expect_equal(as.character(out$stable_cat),
               c("stable", "elevated", "precarious"))
})

test_that("stability_source switches between EDR, EER, and the weighted blend", {
  x <- data.frame(GEOID = "A", accessible = 100, available_turnover = 10, supply = 0.5)
  hp <- data.frame(GEOID = "A", dis_value = -300, ev_value = 0.8)  # EDR extreme, EER stable
  expect_equal(afford_stability(x, hp, stability_source = "edr")$stability, 0)
  expect_equal(afford_stability(x, hp, stability_source = "eer")$stability, 1)
  expect_equal(afford_stability(x, hp, stability_source = "both")$stability, 0.5)
  expect_equal(afford_stability(x, hp, stability_source = "both",
                                edr_weight = 0.75)$stability, 0.25)
})

test_that("availability = 'none' uses accessible and CA is flagged out-of-sample", {
  x <- data.frame(GEOID = c("06001", "53001"), accessible = c(100, 100),
                  available_turnover = c(10, 10), supply = c(0.5, 0.5))
  hp <- data.frame(GEOID = c("06001", "53001"), dis_value = c(50, 50), ev_value = c(0.8, 0.8))
  none <- afford_stability(x, hp, availability = "none")
  expect_equal(none$attainable, c(100, 100))          # accessible, not turnover
  expect_equal(none$stable_dest, c(100, 100))           # x stability 1.0
  turn <- afford_stability(x, hp)
  expect_equal(turn$attainable, c(10, 10))            # available_turnover
  expect_equal(turn$eer_out_of_sample, c(TRUE, FALSE)) # 06 = California
})

test_that("missing availability columns raise a helpful error", {
  x <- data.frame(GEOID = "A", accessible = 100, supply = 0.5)  # no available_turnover
  hp <- data.frame(GEOID = "A", dis_value = 50, ev_value = 0.8)
  expect_error(afford_stability(x, hp), "available_turnover")
  expect_error(afford_stability(x, hp, availability = "vacancy"), "available_vacancy")
})

test_that("hprm accepts a data.frame with geoid (lower) and errors on bad input", {
  x <- data.frame(GEOID = "A", accessible = 100, available_turnover = 10, supply = 0.5)
  hp <- data.frame(geoid = "A", dis_value = 50, ev_value = 0.8)   # lower-case geoid
  expect_equal(afford_stability(x, hp)$stability, 1)
  expect_error(afford_stability(x, data.frame(GEOID = "A", dis_value = 1)), "ev_value")
  expect_error(afford_stability(data.frame(x = 1), hp), "GEOID")
})
