require(dplyr)
require(reshape2)
require(readxl)
require(stringr)
require(mgcv)

require(stats); require(graphics)
require(compiler)

# TODO:dynamically generate input labels
ACM_all <- NULL
src <- NULL
sheets <- NULL
gender_labels <- c("Total", "Female", "Male")
age_group_labels <- c("Total", "0-44", "45-64", "65-74", "75-84", "85 and over")
age_group_labels <- c("Total")

# version of textInput with more size options.
# specify class = 'input-small' or class='input-mini' in
# addition to other textInput args
customTextInput <- function(inputId, label, value = "",
                            labelstyle = "display:inline;", ...) {
  tagList(
    tags$label(label, `for` = inputId, style = labelstyle),
    tags$input(
      id = inputId, type = "text", value = value,
      ...
    )
  )
}

customNumericInput <- function(inputId, label, value = 0,
                               labelstyle = "display:inline;", ...) {
  tagList(
    tags$label(label, `for` = inputId, style = labelstyle),
    tags$input(
      id = inputId, type = "number", value = value,
      ...
    )
  )
}

# version of selectInput...shorter box and label
# inline lapply allows us to add each element of
# choices as an option in the select menu
inlineSelectInput <- function(inputId, label, choices, ...) {
  if (is.null(label)) {
    labeldisp <- "display: none;"
  } else {
    labeldisp <- "display: inline;"
  }

  tagList(
    tags$label(label, `for` = inputId, style = labeldisp),
    tags$select(
      id = inputId, choices = choices, ...,
      class = "shiny-bound-input inlineselect",
      lapply(choices, tags$option)
    )
  )
}

# disable widgets when they should not be usable
disableWidget <- function(id, session, disabled = TRUE) {
  if (disabled) {
    session$sendCustomMessage(
      type = "jsCode",
      list(code = paste("$('#", id, "').prop('disabled',true)",
        sep = ""
      ))
    )
  } else {
    session$sendCustomMessage(
      type = "jsCode",
      list(code = paste("$('#", id, "').prop('disabled',false)",
        sep = ""
      ))
    )
  }
}

attr.info <- function(df, colname, numattrs, breaks) {
  lvls <- length(unique(df[[colname]]))
  if (colname %in% numattrs & lvls > 9) {
    tab <- hist(df[[colname]], breaks = breaks, plot = FALSE)
    barname <- paste(tab$breaks[1:2], collapse = "-")
    for (i in seq(length(tab$breaks) - 2)) {
      barname <- append(barname, paste(tab$breaks[i + 1] + 1,
        tab$breaks[i + 2],
        sep = "-"
      ))
    }
    tab <- tab$counts
    names(tab) <- barname
  } else {
    tab <- table(df[[colname]])
  }
  return(tab)
}

calculate_age <- function(src) { return(sort(unique(src$AGE_GROUP)))}

calculate_spline <- function(src) {

# src <- src[src$SEX %in% c("Female","Male","Total") & src$AGE_GROUP=="Total",]

# src <- src[src$PERIOD <= 52, ]
  src <- src[order(src$REGION, src$SEX, src$AGE_GROUP, src$YEAR, src$PERIOD),]
  if(is.null(src$NO_DEATHS)){
    if(is.null(src$DEATHS)){
      stop("The data should have a column called 'NO_DEATHS', containing the number of all-cause deaths for that period.")
    }else{
      src$NO_DEATHS <- as.numeric(src$DEATHS)
    }
  }else{
    src$NO_DEATHS <- as.numeric(src$NO_DEATHS)
  }

  nys  <- 30 # A max number of years to select from
  minyear <- min(src$YEAR, na.rm=TRUE)-1
  dom <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  moy <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  if (max(src$PERIOD, na.rm = TRUE) == 12) {
    day <- cumsum(c(0, dom))[src$PERIOD] + 15
   #DATE <- cumsum(c(0, 365, 366, 365, 365, 365, 366, 365, 365))[src$YEAR - minyear] + day
    DATE <- cumsum(c(0, rep(365, nys)))[src$YEAR - minyear] + day
  } else {
    day <- cumsum(c(0, rep(7,53)))[src$PERIOD] + 3.5
    DATE <- cumsum(c(0, rep(365, nys)))[src$YEAR - minyear] + day
  }

  src$DATE <- DATE

  out <- src %>% dplyr::filter(YEAR >= "2020") #add in 2021 -
  out <- rbind(out,out)
  wm_ident <- ifelse(max(src$PERIOD, na.rm = TRUE) == 12, "Month", "Week")
  l_period <- ifelse(max(src$PERIOD, na.rm = TRUE) == 12, 12, 53)
  out$ESTIMATE <- out$NO_DEATHS
  out$LOWER_LIMIT <- out$NO_DEATHS
  out$UPPER_LIMIT <- out$NO_DEATHS
  out$SE_CUM_EXPECTED <- out$NO_DEATHS
  out$EXCESS_DEATHS <- out$NO_DEATHS
  out$P_SCORE <- out$NO_DEATHS

  year_predict = sort(unique(out$YEAR))
  nyear_predict = length(year_predict)

  pattern <- unique(paste(out$SEX, out$AGE_GROUP))
  n_pat <- length(pattern)

  for (j in 1:n_pat) {
    patt_src <- src[paste(src$SEX, src$AGE_GROUP) == pattern[j],]
    # Remove COVID period
    hist_src <- patt_src[patt_src$COVID!=1,]
    if (sum(hist_src$NO_DEATHS, na.rm = TRUE) == 0) next
    if (l_period > 51) {
      day <- cumsum(c(0, rep(7,53)))[patt_src$PERIOD] + 3.5
      day <- patt_src$DAYS
      aDATE <- min(patt_src$YEAR, na.rm=TRUE):max(patt_src$YEAR, na.rm=TRUE)
      aDATE <- cumsum(c(0, rep(365, nys)))[patt_src$YEAR - 2014] + day
      src_pandemic <- patt_src
      src_pandemic$DAYS[src_pandemic$DAYS == 0] <- 7
      num.cycle <- max(patt_src$PERIOD[patt_src$DAYS > 0])
      loc_DATE <- aDATE[patt_src$YEAR < "2020"]
      hist_src$logdays <- log(hist_src$DAYS)
      fit <- mgcv::gam(NO_DEATHS ~ offset(logdays) + YEAR + s(PERIOD, bs = "cc", fx = TRUE, k = 9),
        knots = list(PERIOD = c(0, num.cycle)), method = "REML",
        family = nb(), data = hist_src, subset = DAYS > 0
      )
      src_pandemic$logdays <- log(src_pandemic$DAYS)
    } else {
      day <- cumsum(c(0, dom))[patt_src$PERIOD] + 15
      DATE <- cumsum(c(0, rep(365, nys)))[src$YEAR - minyear] + day
      src_pandemic <- patt_src
      aDATE <- DATE[paste(src$SEX, src$AGE_GROUP) == pattern[j]]
      num.cycle <- 12
      loc_DATE <- aDATE[patt_src$YEAR < "2020"]
      days <- dom[hist_src$PERIOD]
      days[14] <- 29
      hist_src$logdays <- log(days)
      fit <- mgcv::gam(NO_DEATHS ~ offset(logdays) + YEAR + s(PERIOD, bs = "cc", fx = TRUE, k = 5),
        knots = list(PERIOD = c(0, num.cycle)), method = "REML",
        family = nb(), data = hist_src
      )
      days <- dom[src_pandemic$PERIOD]
      days[14] <- 29 # Feb 2016
      if(length(days) > 61) days[62] <- 29 # Feb 2020
      src_pandemic$logdays <- log(days)
    }
    t.start <- Sys.time()

    ave_deaths <- as.numeric(tapply(hist_src$NO_DEATHS,hist_src$PERIOD,mean,na.rm=TRUE))
    var_deaths <- as.numeric(tapply(hist_src$NO_DEATHS,hist_src$PERIOD,var,na.rm=TRUE))
    num_deaths <- as.numeric(tapply(hist_src$NO_DEATHS,hist_src$PERIOD,function(x){sum(!is.na(x))}))
    if(length(num_deaths) >= 53 & num_deaths[53] < 3) var_deaths[53] <- var_deaths[52]
    if(num_deaths[1] < 3) var_deaths[1] <- var_deaths[2]

    ave_deaths <- rep(ave_deaths,nyear_predict+50)[1:nrow(src_pandemic)]
    var_deaths <- rep(var_deaths,nyear_predict+50)[1:nrow(src_pandemic)]
    num_deaths <- rep(num_deaths,nyear_predict+50)[1:nrow(src_pandemic)]
    ave_deaths_lower <- ave_deaths - qnorm(0.975)*sqrt(var_deaths*(1 + 1 / num_deaths))
    ave_deaths_upper <- ave_deaths + qnorm(0.975)*sqrt(var_deaths*(1 + 1 / num_deaths))
    var_cum_deaths <- sqrt(cumsum(var_deaths))

    src_pandemic$loc_DATE <- aDATE
    if(TRUE){
      estim <- mgcv::predict.gam(fit, newdata = src_pandemic, se.fit = TRUE)
      # covariance of predictors
      Terms <- list(stats::delete.response(fit$pterms))
      mf <- model.frame(Terms[[1]],src_pandemic,xlev=fit$xlevels)
      Xfrag <- mgcv::PredictMat(fit$smooth[[1]],src_pandemic)
      X <- matrix(0,nrow(src_pandemic),ncol(Xfrag)+2)
      X[,1:2] <- model.matrix(Terms[[1]],mf,contrasts=NULL)
      X[,-c(1:2)] <- Xfrag
      V = X%*%fit$Vp%*%t(X)
      #
      estim.median <- estim$fit
      estim.lower <- estim$fit # [5*12-1+(1:12)]
      estim.upper <- estim$fit
      theta <- fit$family$getTheta(TRUE)
      set.seed(1)
      if(FALSE){
       for (i in 1:length(estim.median)) {
        a <- rnorm(n = 10000, mean = estim$fit[i], sd = estim$se.fit[i])
        estim.median[i] <- mean(qnbinom(mu = exp(a), size = theta, p = 0.5))
        estim.lower[i] <- mean(qnbinom(mu = exp(a), size = theta, p = 0.025))
        estim.upper[i] <- mean(qnbinom(mu = exp(a), size = theta, p = 0.975))
       }
      }else{
       a <- mvtnorm::rmvnorm(n = 2000, mean = estim$fit, sigma = V)
       estim.median <- apply(a,2,function(x){mean(qnbinom(mu = exp(x), size = theta, p = 0.5))})
       estim.lower <- apply(a,2,function(x){mean(qnbinom(mu = exp(x), size = theta, p = 0.025))})
       estim.upper <- apply(a,2,function(x){mean(qnbinom(mu = exp(x), size = theta, p = 0.975))})
       # compute the covariance matrix of predictions
       b <- cov(matrix(rnbinom(n=nrow(a)*ncol(a), mu=exp(a), size=theta),ncol=ncol(a)))
       # compute the variances of cumulative deaths
       cv <- rep(0, length(estim$fit))
       for(i in 1:length(cv)){
        cv[i] <- sum(b[1:i, 1:i])
       }
       # convert to the SD of cumulative deaths
       scv <- pmax(0, sqrt(cv))
      }
    }else{
      estim <- mgcv::predict.gam(fit, newdata = src_pandemic, se.fit = TRUE, type = "response")
      estim.median <- estim$fit
      estim.lower <- estim$fit - 1.96*estim$se.fit
      estim.upper <- estim$fit + 1.96*estim$se.fit
    }
    scv[is.na(scv)] <- 0
    estim.median[estim.median < 0] <- 0
    estim.upper[estim.upper < 0] <- 0
    estim.lower[estim.lower < 0] <- 0

    for (iyear_predict in seq_along(year_predict)) {
     for (k in 0:(l_period - 1)) {
      y <- year_predict[iyear_predict]
      a <- src_pandemic$YEAR == y & src_pandemic$PERIOD == (k + 1)
      while (!any(a) & y >= 2017) {
        y <- y - 1
        a <- src_pandemic$YEAR == y & src_pandemic$PERIOD == (k + 1)
      }
      y <- year_predict[iyear_predict]
      while (!any(a) & y >= 2017) {
        y <- y - 1
        a <- src_pandemic$YEAR == y & src_pandemic$PERIOD == (k - 1)
      }
      if (!any(a)) {
        a <- src_pandemic$YEAR == year_predict[iyear_predict] & src_pandemic$PERIOD == k
      }
      out[(iyear_predict-1)*l_period + nyear_predict*l_period * (j - 1) + k + 1, "ESTIMATE"] <- estim.median[a]
      out[(iyear_predict-1)*l_period + nyear_predict*l_period * (j - 1) + k + 1, "LOWER_LIMIT"] <- estim.lower[a]
      out[(iyear_predict-1)*l_period + nyear_predict*l_period * (j - 1) + k + 1, "UPPER_LIMIT"] <- estim.upper[a]
      out[(iyear_predict-1)*l_period + nyear_predict*l_period * (j - 1) + k + 1, "SE_CUM_EXPECTED"] <- scv[a]
     }
    }
    for (iyear_predict in seq_along(year_predict)) {
     for (k in 0:(l_period - 1)) {
      y <- year_predict[iyear_predict]
      a <- src_pandemic$YEAR == y & src_pandemic$PERIOD == (k + 1)
      while (!any(a) & y >= 2017) {
        y <- y - 1
        a <- src_pandemic$YEAR == y & src_pandemic$PERIOD == (k + 1)
      }
      y <- year_predict[iyear_predict]
      while (!any(a) & y >= 2017) {
        y <- y - 1
        a <- src_pandemic$YEAR == y & src_pandemic$PERIOD == (k - 1)
      }
      if (!any(a)) {
        a <- src_pandemic$YEAR == year_predict[iyear_predict] & src_pandemic$PERIOD == k
      }
      out[nyear_predict*l_period*n_pat + (iyear_predict-1)*l_period + nyear_predict*l_period * (j - 1) + k + 1, "ESTIMATE"] <- ave_deaths[a]
      out[nyear_predict*l_period*n_pat + (iyear_predict-1)*l_period + nyear_predict*l_period * (j - 1) + k + 1, "LOWER_LIMIT"] <- ave_deaths_lower[a]
      out[nyear_predict*l_period*n_pat + (iyear_predict-1)*l_period + nyear_predict*l_period * (j - 1) + k + 1, "UPPER_LIMIT"] <- ave_deaths_upper[a]
      out[(iyear_predict-1)*l_period + nyear_predict*l_period * (j - 1) + k + 1, "SE_CUM_EXPECTED"] <- var_cum_deaths[a]
     }
    }

#   message(paste0(out[l_period * (j - 1) + k + 1, "SEX"], " ", out[l_period * (j - 1) + k + 1, "AGE_GROUP"], " finished (", round(difftime(
#     Sys.time(),
#     t.start
#   ), 1), " seconds)"))
    message(paste0(out[l_period * (j - 1) + k + 1, "SEX"], " ", out[l_period * (j - 1) + k + 1, "AGE_GROUP"],

       " percentage of 2015-2019 outside the 95% prediction intervals is ", 
      round(100*(sum(estim.lower[1:60] > src_pandemic$NO_DEATHS[1:60]) + sum(estim.upper[1:60] < src_pandemic$NO_DEATHS[1:60])) / 120,1),
      "; for the average ",
      round(100*(sum(ave_deaths_lower[1:60] > src_pandemic$NO_DEATHS[1:60]) + sum(ave_deaths_upper[1:60] < src_pandemic$NO_DEATHS[1:60])) / 120,1),
      "."))
  }
 
  out$WM_IDENTIFIER <- rep(wm_ident)

  out[, "EXCESS_DEATHS"] <- out$NO_DEATHS - out$ESTIMATE
  out[, "P_SCORE"] <- 100*(out$NO_DEATHS - out$ESTIMATE) / out$ESTIMATE
  out[, "EXPECTED"] <- out$ESTIMATE
  out[, "EXCESS_UPPER"] <- out$NO_DEATHS - out$LOWER_LIMIT
  out[, "EXCESS_LOWER"] <- out$NO_DEATHS - out$UPPER_LIMIT

# names(out)[c(14:15)] <- c("SERIES", "NO_DEATHS")
# out$SERIES <- factor(rep(c("Cyclical spline", "Historical average"),rep(nyear_predict*l_period*n_pat,2)))
  out$SERIES <- factor(rep(c("Cyclical spline", "Historical average"),rep(nyear_predict*l_period*n_pat,2)))
 #out$SERIES <- factor(rep(c("Cyclical spline", "Historical average"),rep(nrow(out)/2,2)))

# l_SERIES <- levels(out$SERIES)
# names_SERIES <- c("NO_DEATHS", "ESTIMATE")
# new_names_SERIES <- c("Current deaths", "Cyclical spline", "Historical average")
# series <- as.numeric(out$SERIES)
# series[3*l_period*n_pat+(1:(l_period*n_pat))][a] <- 3
# out$SERIES <- factor(new_names_SERIES[series], levels=new_names_SERIES) 

  if (any(out$SERIES == "Unknown series, plz check")) message("Unknown series, plz check")

  out <- out[, c("REGION", "WM_IDENTIFIER", "YEAR", "PERIOD", "SEX", "AGE_GROUP", "SERIES", "NO_DEATHS", "EXPECTED", "LOWER_LIMIT", "UPPER_LIMIT", "EXCESS_DEATHS", "P_SCORE", "EXCESS_LOWER", "EXCESS_UPPER", "SE_CUM_EXPECTED")]

  message("Computation of the expected deaths completed successfully.")

  attr(out, "num_deaths") <- sum(!is.na(hist_src$NO_DEATHS)) / l_period
  attr(out, "SE_cumulative_deaths") <- scv
# print(scv)

  return(out)
}

calculate_spline <- cmpfun(calculate_spline)

calculate_spline_age <- function(src) {

# src <- src[src$SEX %in% c("Female","Male","Total") & src$AGE_GROUP=="Total",]

# load(file="src.RData")
# save(src,file="/tmp/src.RData")

# src <- src[src$SEX %in% c("Female","Male","Total") & src$AGE_GROUP!="Total",]

  src <- src[src$PERIOD <= 52, ]
  src <- src[order(src$REGION, src$SEX, src$AGE_GROUP, src$YEAR, src$PERIOD),]
  if(is.null(src$NO_DEATHS)){
    if(is.null(src$DEATHS)){
      stop("The data should have a column called 'NO_DEATHS', containing the number of all-cause deaths for that period.")
    }else{
      src$NO_DEATHS <- as.numeric(src$DEATHS)
    }
  }else{
    src$NO_DEATHS <- as.numeric(src$NO_DEATHS)
  }

  minyear <- min(src$YEAR, na.rm=TRUE)-1
  dom <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  moy <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  if (max(src$PERIOD, na.rm = TRUE) == 12) {
    day <- cumsum(c(0, dom))[src$PERIOD] + 15
   #DATE <- cumsum(c(0, 365, 366, 365, 365, 365, 366, 365, 365))[src$YEAR - minyear] + day
    DATE <- min(src$YEAR, na.rm=TRUE):max(src$YEAR, na.rm=TRUE)
    DATE <- c(366, 365, 365, 365)[round(DATE-4*trunc((DATE+0.5) / 4))+1]
    DATE <- cumsum(c(0, DATE))[src$YEAR - minyear] + day
  } else {
    day <- cumsum(c(0, rep(7,52)))[src$PERIOD] + 3.5
   #DATE <- cumsum(c(0, 365, 366, 365, 365, 365, 366, 365, 365))[src$YEAR - minyear] + day
    DATE <- min(src$YEAR, na.rm=TRUE):max(src$YEAR, na.rm=TRUE)
    DATE <- c(366, 365, 365, 365)[round(DATE-4*trunc((DATE+0.5) / 4))+1]
    DATE <- cumsum(c(0, DATE))[src$YEAR - minyear] + day
  }

  out <- src %>% dplyr::filter(YEAR >= "2020")
  out <- rbind(out,out)
  wm_ident <- ifelse(max(src$PERIOD, na.rm = TRUE) == 12, "Month", "Week")
  l_period <- ifelse(max(src$PERIOD, na.rm = TRUE) == 12, 12, 52)
  out$ESTIMATE <- out$NO_DEATHS
  out$LOWER_LIMIT <- out$NO_DEATHS
  out$UPPER_LIMIT <- out$NO_DEATHS
  out$EXCESS_DEATHS <- out$NO_DEATHS
  out$P_SCORE <- out$NO_DEATHS

  year_predict = sort(unique(out$YEAR))
  nyear_predict = length(year_predict)

# pattern <- unique(paste(out$AREA, out$SEX, out$AGE_GROUP))
  pattern <- unique(paste(out$AREA, out$SEX))
# pattern <- pattern[-c(grep("Total",pattern),grep("Unknown",pattern))]
  n_pat <- length(pattern)

  sel <- 0
  for (j in 1:n_pat) {
    patt_src <- src[paste(out$AREA, src$SEX) == pattern[j], ]

    ages <- c("<5",">=80","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
              "50-54","55-59","60-64","65-69","70-74","75-79")
#   ages <- ages[c(grep("-",ages),grep("and",ages))]
#   nage <- c(2,12,17,22,27,32,37,42,47,7,52,57,62,67,72,77,82,87,92,97,100)
    nage <- c(2,85,17,22,27,32,37,42,47,52,57,62,67,72,77)
    patt_src$AGE <- nage[match(patt_src$AGE_GROUP,ages)]
    ages <- sort(unique(patt_src$AGE_GROUP))

    hist_src <- patt_src[patt_src$YEAR < "2020",]

    if (sum(hist_src$NO_DEATHS, na.rm = TRUE) == 0) next
    if (l_period > 51) {
      day <- cumsum(c(0, rep(7,52)))[patt_src$PERIOD] + 3.5
     #aDATE <- cumsum(c(0, 365, 366, 365, 365, 365, 366, 365, 365))[patt_src$YEAR - minyear] + day
      aDATE <- min(patt_src$YEAR, na.rm=TRUE):max(patt_src$YEAR, na.rm=TRUE)
      aDATE <- c(366, 365, 365, 365)[round(aDATE-4*trunc((aDATE+0.5) / 4))+1]
      aDATE <- cumsum(c(0, aDATE))[patt_src$YEAR - minyear] + day
      src_pandemic <- patt_src
      num.cycle <- 52
#     loc_DATE <- aDATE[patt_src$YEAR < "2020"]
#     days <- diff(c(0, loc_DATE))
#     days[1] <- 7
      hist_src$logdays <- log(7)
      fit <- mgcv::gam(NO_DEATHS ~ offset(logdays) + YEAR + s(AGE) + s(PERIOD,bs="cc", fx = TRUE, k = 9),
        knots=list(PERIOD=c(0,num.cycle)), method="REML", family=nb(), data=hist_src,
        subset=AGE_GROUP %in% ages)
      days <- dom[src_pandemic$PERIOD]
      src_pandemic$logdays <- log(7)
    } else {
      day <- cumsum(c(0, dom))[patt_src$PERIOD] + 15
     #DATE <- cumsum(c(0, 365, 366, 365, 365, 365, 366, 365, 365))[patt_src$YEAR - minyear] + day
      DATE <- min(patt_src$YEAR, na.rm=TRUE):max(patt_src$YEAR, na.rm=TRUE)
      DATE <- c(366, 365, 365, 365)[round(DATE-4*trunc((DATE+0.5) / 4))+1]
      DATE <- cumsum(c(0, DATE))[patt_src$YEAR - minyear] + day
      src_pandemic <- patt_src
      aDATE <- DATE[paste(out$AREA, src$SEX, src$AGE_GROUP) == pattern[j]]
      num.cycle <- 12
#     loc_DATE <- aDATE[patt_src$YEAR < "2020"]
      days <- dom[hist_src$PERIOD]
      days[14] <- 29
      hist_src$logdays <- log(days)
#     fit <- mgcv::gam(NO_DEATHS ~ offset(logdays) + YEAR + s(PERIOD, bs = "cc", fx = TRUE, k = 5),
#       knots = list(PERIOD = c(0, num.cycle)), method = "REML",
#       family = nb(), data = hist_src
#     )

#     fit_F <- mgcv::gam(NO_DEATHS ~ offset(logdays) + YEAR + s(AGE) + s(PERIOD,bs="cc", fx = TRUE, k = 5),
#       knots=list(PERIOD=c(0,len.cycle)), method="REML", family=nb(), data=hist_src,
#       subset=AGE_GROUP %in% ages & SEX == "Female")
      fit <- mgcv::gam(NO_DEATHS ~ offset(logdays) + YEAR + s(AGE) + s(PERIOD,bs="cc", fx = TRUE, k = 5),
        knots=list(PERIOD=c(0,num.cycle)), method="REML", family=nb(), data=hist_src,
        subset=AGE_GROUP %in% ages)
      days <- dom[src_pandemic$PERIOD]
      days[14] <- 29 # Feb 2016
      if(length(days) > 61) days[62] <- 29 # Feb 2020
      src_pandemic$logdays <- log(days)
    }
    t.start <- Sys.time()

    ave_deaths <- as.numeric(tapply(hist_src$NO_DEATHS,hist_src$PERIOD,mean,na.rm=TRUE))
    var_deaths <- as.numeric(tapply(hist_src$NO_DEATHS,hist_src$PERIOD,var,na.rm=TRUE))
    num_deaths <- as.numeric(tapply(hist_src$NO_DEATHS,hist_src$PERIOD,function(x){sum(!is.na(x))}))
    if(length(num_deaths) >= 53 & num_deaths[53] < 3) var_deaths[53] <- var_deaths[52]
    if(num_deaths[1] < 3) var_deaths[1] <- var_deaths[2]

    ave_deaths <- rep(ave_deaths,nyear_predict+50)[1:nrow(src_pandemic)]
    var_deaths <- rep(var_deaths,nyear_predict+50)[1:nrow(src_pandemic)]
    num_deaths <- rep(num_deaths,nyear_predict+50)[1:nrow(src_pandemic)]
    ave_deaths_lower <- ave_deaths - qnorm(0.975)*sqrt(var_deaths*(1 + 1 / num_deaths))
    ave_deaths_upper <- ave_deaths + qnorm(0.975)*sqrt(var_deaths*(1 + 1 / num_deaths))

#   src_pandemic$loc_DATE <- aDATE
    if(TRUE){
      estim <- mgcv::predict.gam(fit, newdata = src_pandemic, se.fit = TRUE)
      estim.median <- estim$fit
      estim.lower <- estim$fit # [5*12-1+(1:12)]
      estim.upper <- estim$fit
      theta <- fit$family$getTheta(TRUE)
      set.seed(1)
      if(FALSE){
       for (i in 1:length(estim.median)) {
        a <- rnorm(n = 10000, mean = estim$fit[i], sd = estim$se.fit[i])
        estim.median[i] <- mean(qnbinom(mu = exp(a), size = theta, p = 0.5))
        estim.lower[i] <- mean(qnbinom(mu = exp(a), size = theta, p = 0.025))
        estim.upper[i] <- mean(qnbinom(mu = exp(a), size = theta, p = 0.975))
       }
      }else{
       a <- matrix(rnorm(n = 1000*length(estim$fit), mean = estim$fit, sd = estim$se.fit),ncol=1000)
       estim.median <- apply(a,1,function(x){mean(qnbinom(mu = exp(x), size = theta, p = 0.5))})
       estim.lower <- apply(a,1,function(x){mean(qnbinom(mu = exp(x), size = theta, p = 0.025))})
       estim.upper <- apply(a,1,function(x){mean(qnbinom(mu = exp(x), size = theta, p = 0.975))})
      }
    }else{
      estim <- mgcv::predict.gam(fit, newdata = src_pandemic, se.fit = TRUE, type = "response")
      estim.median <- estim$fit
      estim.lower <- estim$fit - 1.96*estim$se.fit
      estim.upper <- estim$fit + 1.96*estim$se.fit
    }
    estim.median[estim.median < 0] <- 0
    estim.upper[estim.upper < 0] <- 0
    estim.lower[estim.lower < 0] <- 0

    f <- cbind(estim.median,estim.upper,estim.lower)[src_pandemic$YEAR >= "2020",]  
    if(sel==0){
     sels <- 1
     sel <- nrow(f)
    }else{
     sels <- sel + 1
     sel <- sel + nrow(f)
    }
    out[sels:sel,c("ESTIMATE","LOWER_LIMIT","UPPER_LIMIT")] <- f
    f <- cbind(ave_deaths,ave_deaths_upper,ave_deaths_lower)[src_pandemic$YEAR >= "2020",]  
    out[nrow(out)/2+(sels:sel),c("ESTIMATE","LOWER_LIMIT","UPPER_LIMIT")] <- f[out[nrow(out)/2+(sels:sel),"PERIOD",]]

    message(paste0(out[l_period * (j - 1) + 1, "SEX"], " ", out[l_period * (j - 1) + 1, "AGE_GROUP"],
       " percentage of 2015-2019 outside the 95% prediction intervals is ", 
      round(100*(sum(estim.lower[1:60] > src_pandemic$NO_DEATHS[1:60]) + sum(estim.upper[1:60] < src_pandemic$NO_DEATHS[1:60])) / 120,1),
      "; for the average ",
      round(100*(sum(out[,"LOWER_LIMIT"] > src_pandemic$NO_DEATHS[1:60]) + sum(ave_deaths_upper[1:60] < src_pandemic$NO_DEATHS[1:60])) / 120,1),
      "."))
  }

  out$WM_IDENTIFIER <- rep(wm_ident)

  out[, "EXCESS_DEATHS"] <- out$NO_DEATHS - out$ESTIMATE
  out[, "P_SCORE"] <- 100*(out$NO_DEATHS - out$ESTIMATE) / out$ESTIMATE
  out[, "EXPECTED"] <- out$ESTIMATE

# names(out)[c(14:15)] <- c("SERIES", "NO_DEATHS")
# out$SERIES <- factor(rep(c("Cyclical spline", "Historical average"),rep(nyear_predict*l_period*n_pat,2)))
   out$SERIES <- factor(rep(c("Cyclical spline", "Historical average"),rep(nrow(out)/2,2)))

# l_SERIES <- levels(out$SERIES)
# names_SERIES <- c("NO_DEATHS", "ESTIMATE")
# new_names_SERIES <- c("Current deaths", "Cyclical spline", "Historical average")
# series <- as.numeric(out$SERIES)
# series[3*l_period*n_pat+(1:(l_period*n_pat))][a] <- 3
# out$SERIES <- factor(new_names_SERIES[series], levels=new_names_SERIES) 

  if (any(out$SERIES == "Unknown series, plz check")) message("Unknown series, plz check")

  out <- out[, c("REGION", "WM_IDENTIFIER", "YEAR", "PERIOD", "SEX", "AGE_GROUP", "SERIES", "NO_DEATHS", "EXPECTED", "LOWER_LIMIT", "UPPER_LIMIT", "EXCESS_DEATHS", "P_SCORE")]

  message("Computation of the expected deaths completed successfully.")

  attr(out, "num_deaths") <- sum(!is.na(hist_src$NO_DEATHS)) / l_period
  return(out)
}

calculate_spline_age <- cmpfun(calculate_spline_age)
