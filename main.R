source("header.R")

do_validate <- function(dat) {
  wrt <- est_mongo_conn("Write")
  outprice_now <- dat$result %>%
    map(function(x) {
      fromJSON(x[[1]])$house_out_price
    }) %>%
    unlist()
  
  dat$outprice_now <- outprice_now
  # extract coefficients
  coefs <- lm(sales_wish_sum_price_yuan ~ outprice_now, data = dat)$coefficients %>% round(digits = 5)
  
  # save result to db
  list(
    timestamp = Sys.time(),
    intercept = coefs[["(Intercept)"]],
    slope = coefs[["outprice_now"]]
  ) %>% wrt$insert()
}

make_plot <- function(dat) {
  outprice_now <- dat$result %>%
    map(function(x) {
      fromJSON(x[[1]])$house_out_price
    }) %>%
    unlist()
  
  dat$outprice_now <- outprice_now
  
  dat %>%
    ggplot(aes(sales_wish_sum_price_yuan, outprice_now)) +
    geom_point() +
    geom_smooth(method = lm, se = F)
}

# GET -----------------------------------------------------------------

#' @png
#* @get /trigger
function(req) {
  cat_rule("Receive trigger. Prepare to validate.")
  
  # establish a db connection
  rd <- est_mongo_conn("Read")
  
  # extract all data
  dat <- rd$find()
  
  if (any(dat$error != "")) {
    # report
    cat_boxx("Not all results return true. Please check.", col = "red")
    stop("Require further attention.")
  }
  
  status <- dat$result %>% map(function(x) {
    fromJSON(x[[1]])$status
  }) %>%
    unlist()
  
  if (any(status == 0)) {
    # report
    cat_boxx("Not all results return true. Please check.", col = "red")
    stop("Require further attention.")
  }
  
  do_validate(dat)
  make_plot(dat) %>% print()
  cat_boxx("*** ALL TEST PASSED ***", col = "green")
}
