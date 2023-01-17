# This function calls set_dt_single_thread within a test function
# and returns the number of threads used during and after function call

test_set_dt_threads <- function() {
  test_function <- function() {
    set_dt_single_thread()

    data.table::getDTthreads()
  }

  out <- list(
    function_threads = test_function(),
    exit_threads = data.table::getDTthreads()
  )

  return(out)
}

dt_recognised_threads <- data.table::getDTthreads()

if (dt_recognised_threads >= 2) {
  data.table::setDTthreads(2)


  test_that("set_dt_single_thread returns correct number of threads after call", {
    expect_equal(2L, test_set_dt_threads()$exit_threads)
  })

  test_that("set_dt_single_thread uses only one thread during function", {
    expect_equal(1L, test_set_dt_threads()$function_threads)
  })
} else {
  test_that("set_dt_single_thread uses only one thread during function", {
    expect_equal(1L, test_set_dt_threads()$function_threads)
  })
}
