test_that("Default cutoff plot", {
  p <- plot_cutoff()
  vdiffr::expect_doppelganger("Default cutoff plot", p$final_plot)
})

# test_that("Default cutoff plot with table", {
#   p <- plot_cutoff(add_table = TRUE)
#   vdiffr::expect_doppelganger("Default cutoff plot with table", p$final_plot)
# })


testthat::expect_message(
  p <- plot_cutoff(
    prevalence = 0.001
    )
  )


test_that("Remove FP", {
  p <- plot_cutoff()
  p2 = remove_layers_cutoff_plot(p$final_plot, delete_what = "FP")
  vdiffr::expect_doppelganger("Remove FP", p2$final_plot)
})

test_that("Remove FN", {
  p <- plot_cutoff()
  p2 = remove_layers_cutoff_plot(p$final_plot, delete_what = "FN")
  vdiffr::expect_doppelganger("Remove FN", p2$final_plot)
})


test_that("Remove FP not silent", {
  p <- plot_cutoff()
  p2 = suppressMessages(remove_layers_cutoff_plot(
    p$final_plot, 
    delete_what = "TN", 
    silent = FALSE))
  vdiffr::expect_doppelganger("Remove FP", p2$final_plot)
})

test_that("Remove FN not silent", {
  p <- plot_cutoff()
  p2 = suppressMessages(remove_layers_cutoff_plot(
    p$final_plot, 
    delete_what = "FN", 
    silent = FALSE))
   vdiffr::expect_doppelganger("Remove FN", p2$final_plot)
})



# Takes too long
# p <- plot_cutoff(n_people = 10^7)

# The table creates a tempfile so it fails with covr::package_coverage()
# testthat::test_that("Save plots with tables", {
#   suppressMessages(p <- plot_cutoff(add_table = TRUE, output_filename = "temp.png"))
#   
#   file_name_test <- "temp.png"
#   testthat::expect_true(file.exists(file_name_test))
#   file.remove(file_name_test)
# })
# 
# 
testthat::test_that("Save plots", {
  suppressMessages(p <- plot_cutoff(output_filename = "temp.png"))

  file_name_test <- "temp.png"
  testthat::expect_true(file.exists(file_name_test))
  file.remove(file_name_test)
})
