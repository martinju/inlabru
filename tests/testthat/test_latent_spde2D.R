context("Latent models - 2D SPDE - Group parameter (test_latent_spde2D.R)")

latent_spde2D_group_testdata <- function() {
  set.seed(123)

  # Load and reduce data set
  data(mrsea)
  mrsea$points <- mrsea$points[mrsea$points$season == 1 |
    mrsea$points$season == 2, ]
  mrsea$samplers <- mrsea$samplers[mrsea$samplers$season == 1 |
    mrsea$samplers$season == 2, ]

  # The estimation is numerically unreliable when the spatial
  # domain is represented in metres, and has been seen to produce
  # different results on different systems (e.g. Travis CI).
  # Transform m to km:
  crs_km <- inla.CRS("+proj=utm +zone=32 +ellps=WGS84 +units=km")
  mrsea$mesh <- inla.spTransform(mrsea$mesh, crs_km)
  mrsea$samplers <- sp::spTransform(mrsea$samplers, crs_km)

  # Integration points
  ips <- ipoints(mrsea$samplers, group = "season")

  # Run the model
  matern <- inla.spde2.pcmatern(mrsea$mesh,
    prior.sigma = c(0.1, 0.01),
    prior.range = c(10, 0.01)
  )

  cmp <- coordinates + season ~
  mySmooth(
    map = coordinates, model = matern,
    group = season, ngroup = 2
  ) + Intercept
  fit <- lgcp(cmp, mrsea$points,
    ips = ips,
    options = list(control.inla = list(int.strategy = "eb"))
  )

  list(
    mrsea = mrsea,
    matern = matern,
    cmp = cmp,
    fit = fit
  )
}

test_that("Latent models: SPDE with group parameter (spatiotemporal)", {
  skip_on_cran()
  library(INLA)
  data <- latent_spde2D_group_testdata()

  # Check Intercept
  expect_equal(data$fit$summary.fixed["Intercept", "mean"], -9.231591, hitol)

  # Check SPDE
  expect_equal(
    data$fit$summary.random$mySmooth$mean[c(1, 250, 550)],
    c(-1.27410, -1.96819, 0.7479571), hitol
  )
  expect_equal(
    data$fit$summary.random$mySmooth$sd[c(1, 250, 550)],
    c(1.439, 1.635, 0.935), hitol
  )
})
