# Extracted from test-afford_index.R:18

# test -------------------------------------------------------------------------
expect_equal(neighborhood:::.afi_mortgage_constant(0.06, 30), 0.071935, tolerance = 1e-4)
