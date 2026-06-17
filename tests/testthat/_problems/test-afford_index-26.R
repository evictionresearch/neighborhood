# Extracted from test-afford_index.R:26

# test -------------------------------------------------------------------------
f <- neighborhood:::.afi_price_income_factor(0.06, 30, down_pct = 0.20,
                                               tax_ins_rate = 0.0125, burden = 0.30)
expect_equal(f, ((0.80 * 0.071935) + 0.0125) / 0.30, tolerance = 1e-4)
