require(utils)
require(grDevices)
require(graphics)
require(stats)
require(methods)

require("readxl")
require("writexl")
require("RColorBrewer")
require("ggplot2")
require("lattice")
require("latticeExtra")

# data(ExampleCountries, package = "ACMCalculator")
# load("./data/ExampleCountries.RData")
# load("./data/ExampleCountriesXLSX.RData")
BRGcol <- "darkred"
CUGcol <- "darkorange"
obsblue <- "#076EC3"
histblue <- "#83B6E1"
tgray3 <- adjustcolor("gray", alpha.f = 0.3)
tgray7 <- adjustcolor("gray", alpha.f = 0.7)

shinyServer(
  function(input, output, session) {
    oldoptions <- options()
    on.exit(options(oldoptions))
    options(digits = 3)


    # Reactive Expressions ----------------------------------------------------
    # These expressions contain most of the code from the ergm package that we will
    # be using. Objects created with a reactive expression can be accessed from any
    # other reactive expression or render functions and they only get re-run when
    # their values are outdated. Since many of our render functions will be calling
    # the same ergm objects, using reactive expressions will help the app run much
    # faster.


    values <- reactiveValues()

    # when two options are available to the user, or when we need to know if one
    # variable is outdated this reactive value will keep track of the state
    state <- reactiveValues(
      symmdir = FALSE, plotperc_dd = FALSE,
      plotperc_gd = FALSE, gof = 0
    )

    # move to Help page when user clicks Help link button
    observe({
      if (input$helpLink == 0) {
        return()
      }
      isolate({
        updateTabsetPanel(session, "navbar", selected = "tab6")
      })
    })

    # move to Data panel when user clicks Get Started button
    observe({
      if (input$startButton == 0) {
        return()
      }
      isolate({
        updateTabsetPanel(session, "navbar", selected = "tab2")
      })
    })

    # update active tab in navbar when arrows are clicked
    leftarrowclicks <- reactive({
      input$dataleft + input$plotleft
    })
    rightarrowclicks <- reactive({
      input$dataright + input$plotright
    })
    observe({
      if (leftarrowclicks() == 0) {
        return()
      }
      tabOptions <- c("tab1", "tab2", "tab3", "tab4", "tab5", "tab6")
      current <- isolate(which(input$navbar == tabOptions))
      updateTabsetPanel(session, "navbar", selected = tabOptions[current - 1])
    })
    observe({
      if (rightarrowclicks() == 0) {
        return()
      }
      tabOptions <- c("tab1", "tab2", "tab3", "tab4", "tab5", "tab6")
      current <- isolate(which(input$navbar == tabOptions))
      updateTabsetPanel(session, "navbar", selected = tabOptions[current + 1])
    })


    ## Data Selection ------------------------------------------------------

    output$selectsheet <- renderUI({
      sheets <- ""
      if (input$filetype == 1) {
        # input$rawdatafile comes as a dataframe with name, size, type and datapath
        # datapath is stored in 4th column of dataframe
        filepath <- input$rawdatafile[1, 4]
        filename <- input$rawdatafile[1, 1]
        fileext <- substr(filename, nchar(filename) - 3, nchar(filename))

        validate(
          need(
            fileext %in% c("xls", "xlsx", "XLS", "XLSX"),
            "Upload an Excel file"
          )
        )
        try({
          ACM_sheets <- readxl::excel_sheets(path = paste(filepath))
        })
        sheets <- c()
        for (i in seq_along(ACM_sheets)) {
          if (ACM_sheets[i] != "Instructions") {
            ACM_all <- readxl::read_excel(path = paste(filepath), sheet = ACM_sheets[i])
            if (ACM_all[4, 3] == "WEEKS") {
              len.header <- 6
            } else {
              len.header <- 5
            }
            is.data <- apply(!is.na(as.matrix(ACM_all[len.header:nrow(ACM_all), 3:ncol(ACM_all)])), 1, sum)
            skip <- max(is.data) < 12
            if (!skip) {
              sheets <- c(sheets, ACM_sheets[i])
            }
          }
        }
      }
      selectizeInput("chosesheet",
        label = NULL,
        choices = c("Choose a region" = "", sheets)
      )
    })

    output$selectbuiltinsheet <- renderUI({
      sheets <- ""
      if (input$filetype == 2 & input$samplecountry != "") {
        country_name <- c("Australia", "Japan", "Republic_of_Korea", "New_Zealand", "Philippines")[
          match(input$samplecountry, c(
            "Australia", "Japan",
            "Republic of Korea", "New Zealand", "Philippines"
          ))
        ]
        filepath <- paste0("XLSX/", country_name, "_built_in_data.xlsx")
        try({
          ACM_sheets <- readxl::excel_sheets(path = paste(filepath))
        })
        sheets <- c()
        for (i in seq_along(ACM_sheets)) {
          if (ACM_sheets[i] != "Instructions") {
            ACM_all <- readxl::read_excel(path = paste(filepath), sheet = ACM_sheets[i])
            if (!is.null(ACM_all)) {
              if (ACM_all[4, 3] == "WEEKS") {
                len.header <- 6
              } else {
                len.header <- 5
              }
              is.data <- apply(!is.na(as.matrix(ACM_all[len.header:nrow(ACM_all), 3:ncol(ACM_all)])), 1, sum)
              skip <- max(is.data) < 12
              if (!skip) {
                sheets <- c(sheets, ACM_sheets[i])
              }
            }
          }
        }
      }
      selectizeInput("chosebuiltinsheet",
        label = NULL,
        choices = c("Choose a region" = "", sheets)
      )
    })

    output$covidstartdate <- renderUI({
      dateInput("COVIDstartdate",
        label = "COVID start date",
        value = "2020-01-01",
        min = "2019-12-01",
        startview = "month"
      )
    })
    # output$age <- renderUI({
    #   selectizeInput('age_list', label=NULL,
    #     choices=output_age() )
    # })

    output$age <- renderUI({
      oage <- output_age()
      selectInput("age", "Select Age Group", oage)
    })

    output$EDage <- renderUI({
      oage <- output_age()
      selectInput("EDage", "Select Age Group", oage)
    })

    output$EPage <- renderUI({
      oage <- output_age()
      selectInput("EPage", "Select Age Group", oage)
    })

    #     output$genderlabels <- renderUI({
    #      if (is.null(input$rawdatafile)) {
    #        genderlabels <- NULL
    #      } else {
    #        filepath <- input$rawdatafile[1, 4]
    #        filename <- input$rawdatafile[1, 1]
    #        fileext <- substr(filename, nchar(filename) - 3, nchar(filename))
    #
    #         validate(
    #            need(
    #              fileext %in% c("xls", "xlsx", "XLS", "XLSX"),
    #              "Upload an Excel file"
    #            )
    #          )
    #          try({
    #            ACM_all <- readxl::read_excel(path=paste(filepath), sheet = input$chosesheet)
    #          })
    #          max.types <- dim(ACM_all)[1]
    #          max.times <- dim(ACM_all)[2]
    #          is.data <- apply(!is.na(as.matrix(ACM_all[5:nrow(ACM_all),3:ncol(ACM_all)])),1,sum)
    #          skip <- max(is.data) < 12
    #          a <- as.matrix(ACM_all[5:nrow(ACM_all),3:ncol(ACM_all)])[is.data > 12,]
    #          mode(a) <- "numeric"
    #          a <- round(a)
    #          age <- as.data.frame(ACM_all[,1])[seq(5,nrow(a)+2,by=3),1]
    #          sex <- as.data.frame(ACM_all[5:nrow(ACM_all), 2])[is.data > 12,]
    #          genderlabels <- sort(unique(age))
    #       }
    #       selectizeInput('gender', label=NULL,
    #         choices=c("Select Sex" = '', genderlabels ))
    #     })

    ACMinit <- reactive({
      # input$rawdatafile comes as a dataframe with name, size, type and datapath
      # datapath is stored in 4th column of dataframe
      ACM_var <- NULL
      if (is.null(input$rawdatafile) & is.null(input$samplecountry)) {
        ACM_var <- NULL
      } else {
        if (input$filetype == 1 && !is.null(input$chosesheet) && input$chosesheet != "") {
          filepath <- input$rawdatafile[1, 4]
          filename <- input$rawdatafile[1, 1]
          fileext <- substr(filename, nchar(filename) - 3, nchar(filename))

          ACM_var <- NULL
          validate(
            need(
              fileext %in% c("xls", "xlsx", "XLS", "XLSX"),
              "Upload an Excel file"
            )
          )
          try({
            ACM_all <- readxl::read_excel(path = paste(filepath), sheet = input$chosesheet)
          })
        } else {
          if (input$filetype == 2 && !is.null(input$chosebuiltinsheet) && input$chosebuiltinsheet != "") {
            country_name <- c("Australia", "Japan", "Republic_of_Korea", "New_Zealand", "Philippines")[
              match(input$samplecountry, c(
                "Australia", "Japan",
                "Republic of Korea", "New Zealand", "Philippines"
              ))
            ]
            filepath <- paste0("XLSX/", country_name, "_built_in_data.xlsx")
            try({
              ACM_all <- readxl::read_excel(path = paste(filepath), sheet = input$chosebuiltinsheet)
            })
          } else {
            return(ACM_var)
          }
        }
        if (ACM_all[4, 3] == "WEEKS") {
          len.header <- 6
        } else {
          len.header <- 5
        }
        max.types <- dim(ACM_all)[1]
        max.times <- dim(ACM_all)[2]
        is.data <- apply(!is.na(as.matrix(ACM_all[len.header:nrow(ACM_all), 3:ncol(ACM_all)])), 1, sum, na.rm = TRUE)
        is.time <- apply(!is.na(as.matrix(ACM_all[len.header:nrow(ACM_all), 3:ncol(ACM_all)])), 2, sum, na.rm = TRUE) > 0
        skip <- max(is.data) < 12
        # last.data <- which.max(seq_along(is.time)[is.time]) + 2
        # last.data <- which.max(cumsum(is.time)) + 3
        last.data <- which.max(cumsum(is.time)) + 2
        year <- max(as.numeric(ACM_all[1, 3:last.data]))
        last.year <- match(year + 1, ACM_all[1, ]) - 1
        if (is.na(last.year)) last.year <- max.times
        a <- as.matrix(ACM_all[len.header:nrow(ACM_all), 3:last.year])[is.data > 12, , drop = FALSE]
        mode(a) <- "numeric"
        a <- round(a)
        #       VIP recode missing as 0
        # a[is.na(a) & col(a) <= (last.data-2)] <- 0
        age <- as.data.frame(ACM_all[, 1])[seq(len.header, max(len.header, nrow(ACM_all)), by = 3), 1]
        age <- age[age != ""]
        # Set the COVID period
        covidstart <- grep("START", as.character(ACM_all[ifelse(len.header == 6, 4, 2),]),fixed=TRUE)
        covidend   <- grep("END",   as.character(ACM_all[ifelse(len.header == 6, 4, 2),]),fixed=TRUE)
        if(length(covidstart)==0){
          covidstart <- which(as.character(ACM_all[1,])=="2020" & as.character(ACM_all[ifelse(len.header == 6, 5, 3),])=="1")
          if(len.header==6){
            if(length(covidstart)==0){ covidstart <- min(268, ncol(a)+2) }
          }else{
            if(length(covidstart)==0){ covidstart <- min(65, ncol(a)+2) }
          }
        }
        if(length(covidend  )==0){
          if(len.header==6){
            covidend   <- which(as.character(ACM_all[1,])=="2023" & as.character(ACM_all[ifelse(len.header == 6, 5, 3),])=="18")
            if(length(covidend  )==0){ covidend <- min(444, ncol(a)+2) }
          }else{
            covidend   <- which(as.character(ACM_all[1,])=="2023" & as.character(ACM_all[ifelse(len.header == 6, 5, 3),])=="5")
            if(length(covidend  )==0){ covidend <- min(101, ncol(a)+2) }
          }
        }
        covid <- matrix(0, ncol=ncol(a), nrow=nrow(a))
        covid[,(covidstart-2):(covidend-2)] <- 1
        #
        ACM_var <- data.frame(
          REGION = rep(input$chosesheet, length(a)),
          AGE_GROUP = rep(rep(age, rep(3, length(age)))[is.data > 12], rep(ncol(a), 3 * length(age))[is.data > 12])[1:length(a)],
          SEX = rep(as.data.frame(ACM_all[len.header:nrow(ACM_all), 2])[is.data > 12, ], rep(ncol(a), nrow(a))),
          YEAR = rep(as.numeric(ACM_all[1, 3:last.year]), nrow(a)),
          DAYS = rep(as.numeric(ACM_all[3, 3:last.year]), nrow(a)),
          PERIOD = rep(as.numeric(ACM_all[ifelse(len.header == 6, 5, 3), 3:last.year]), nrow(a)),
          COVID = as.vector(t(covid)),
          NO_DEATHS = as.vector(t(a))
        )
      }
      return(ACM_var)
    })

    #     output$genderlabels <- renderUI({
    #      if (is.null(input$rawdatafile)) {
    #        genderlabels <- NULL
    #      } else {
    #        filepath <- input$rawdatafile[1, 4]
    #        filename <- input$rawdatafile[1, 1]
    #        fileext <- substr(filename, nchar(filename) - 3, nchar(filename))
    #
    #         validate(
    #            need(
    #              fileext %in% c("xls", "xlsx", "XLS", "XLSX"),
    #              "Upload an Excel file"
    #            )
    #          )
    #          try({
    #            ACM_all <- readxl::read_excel(path=paste(filepath), sheet = input$chosesheet)
    #          })
    #          max.types <- dim(ACM_all)[1]
    #          max.times <- dim(ACM_all)[2]
    #          is.data <- apply(!is.na(as.matrix(ACM_all[5:nrow(ACM_all),3:ncol(ACM_all)])),1,sum)
    #          skip <- max(is.data) < 12
    #          a <- as.matrix(ACM_all[5:nrow(ACM_all),3:ncol(ACM_all)])[is.data > 12,]
    #          mode(a) <- "numeric"
    #          a <- round(a)
    #          age <- as.data.frame(ACM_all[,1])[seq(5,nrow(a)+2,by=3),1]
    #          sex <- as.data.frame(ACM_all[5:nrow(ACM_all), 2])[is.data > 12,]
    #          genderlabels <- sort(unique(age))
    #       }
    #       selectizeInput('gender', label=NULL,
    #         choices=c("Select Sex" = '', genderlabels ))
    #     })

    Countryname <- reactive({
      name <- input$rawdatafile[1, 1]
      if (input$filetype == 2) {
        name <- input$samplecountry
      }
      name
    })

    # compute the spline model  for the expected deaths
    output_spline <- reactive({
      if (!is.data.frame(ACMinit())) {
        return()
      }
      src <- ACMinit()
      src_total <- src[src$SEX %in% c("Female", "Male", "Total") & src$AGE_GROUP == "Total", ]
      src_age <- src[src$SEX %in% c("Female", "Male", "Total") & src$AGE_GROUP != "Total", ]
      if (all(is.na(src_total))) src_total <- matrix(0, nrow = 0, ncol = 0)
      if (all(is.na(src_age))) src_age <- matrix(0, nrow = 0, ncol = 0)
      if (nrow(src_age) > 0 & nrow(src_total) > 0) {
        src <- rbind(calculate_spline(src_total), calculate_spline(src_age))
      } else {
        if (nrow(src_total) > 0) {
          src <- calculate_spline(src_total)
        } else {
          src <- calculate_spline(src_age)
        }
      }
      src
    })

    output_age <- reactive({
      if (!is.data.frame(ACMinit())) {
        return()
      }
      calculate_age(ACMinit())
    })

    #   output$download_t <- renderUI({
    #     if(input$template_country != ""){
    #     filename =
    #      paste0(c("AUS", "PHL", "PYF", "Data Entry Template - monthly", "Data Entry Template - weekly")[
    #             match(input$template_country, c(
    #             "Australia", "Philippines", "French Polynesia", "Generic Monthly", "Generic Weekly"))],".xlsx")
    #      file.copy(paste0("./XLSX/",c("AUS", "PHL", "PYF", "Data Entry Template - monthly", "Data Entry Template - weekly")[
    #             match(input$template_country, c(
    #             "Australia", "Philippines", "French Polynesia", "Generic Monthly", "Generic Weekly"))],".xlsx"),
    #             to = paste0(path.expand("~"),"/Downloads/",
    #              c("AUS", "PHL", "PYF", "Data Entry Template - monthly", "Data Entry Template - weekly")[
    #             match(input$template_country, c(
    #             "Australia", "Philippines", "French Polynesia", "Generic Monthly", "Generic Weekly"))],".xlsx"))
    #     return(paste0(filename," downloaded to",path.expand("~"),"/Downloads"))
    #     }else{
    #     return(paste0("The file will be downloaded to ",path.expand("~"),"/Downloads"))
    #     }
    #   })

    output$download_templates <- downloadHandler(
      filename = function() {
        paste0(c(
          "Australia (empty template).xlsx", "Philippines (empty template).xlsx", "French Polynesia (empty template).xlsx",
          "Data Entry Template - monthly.xlsx", "Data Entry Template - weekly.xlsx",
          "Australia_built_in_data.xlsx", "Japan_built_in_data.xlsx",
          "Republic_of_Korea_built_in_data.xlsx", "New_Zealand_built_in_data.xlsx",
          "Philippines_built_in_data.xlsx"
        )[
          match(input$template_country, c(
            "Australia (empty template)", "Philippines (empty template)", "French Polynesia (empty template)",
            "Generic Monthly template", "Generic Weekly template",
            "Australia (filled up to August 2020)", "Japan (filled up to August 2020)", "Republic of Korea (filled up to August 2020)", "New Zealand (filled up to August 2020)", "Philippines (filled up to August 2020)"
          ))
        ])
      },
      content = function(file) {
        file.copy(
          from = paste0(
            "./XLSX/",
            c(
              "Australia (empty template).xlsx", "Philippines (empty template).xlsx", "French Polynesia (empty template).xlsx",
              "Data Entry Template - monthly.xlsx", "Data Entry Template - weekly.xlsx",
              "Australia_built_in_data.xlsx", "Japan_built_in_data.xlsx",
              "Republic_of_Korea_built_in_data.xlsx", "New_Zealand_built_in_data.xlsx",
              "Philippines_built_in_data.xlsx"
            )[
              match(input$template_country, c(
                "Australia (empty template)", "Philippines (empty template)", "French Polynesia (empty template)",
                "Generic Monthly template", "Generic Weekly template",
                "Australia (filled up to August 2020)", "Japan (filled up to August 2020)", "Republic of Korea (filled up to August 2020)", "New Zealand (filled up to August 2020)", "Philippines (filled up to August 2020)"
              ))
            ]
          ),
          to = file
        )
      }
    )

    output$EDdownload <- downloadHandler(
      filename = function() {
        paste(Countryname(), "_ED.xlsx", sep = "")
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        writexl::write_xlsx(x = output_spline(), path = file)
      }
    )

    output$ACMplotdownload <- downloadHandler(
      filename = function() {
        paste(Countryname(), "_plot.pdf", sep = "")
      },
      content = function(file) {
        pdf(file = file, height = 8, width = 20)
        ACM_var <- output_spline()
        validate(
          need(
            (input$age %in% output_age()),
            "Please select an Age Group from the above pull-down list."
          )
        )
        c_data <- ACM_var[ACM_var$SEX == input$gender & ACM_var$AGE_GROUP == input$age, ]
        caption <- ""
        if (attr(ACM_var, "num_deaths") < 4.5) {
          caption <- paste0(
            "WARNING: The model is based on less than 5 years of historical data (",
            round(attr(ACM_var, "num_deaths"), 1), " years). The statistical uncertainty may be high."
          )
        }
        if (nrow(c_data) < 2) {
          if (ACM_var$WM_IDENTIFIER[1] == "Month") {
            p <- ACM_var[1:12, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "Recorded")) +
              geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
              geom_line(aes(x = PERIOD, y = EXPECTED, colour = "Expected")) +
              scale_colour_manual(
                name = "",
                values = c(Recorded = "black", Expected = "indianred")
              ) +
              scale_x_continuous(
                name = "Month (2020-present)",
                labels = c(
                  "JAN", "FEB", "MAR", "APR",
                  "MAY", "JUN", "JUL", "AUG",
                  "SEP", "OCT", "NOV", "DEC"
                ),
                breaks = 1:12
              ) +
              scale_y_continuous(name = "Monthly Deaths") +
              labs(
                title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
                subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
              ) +
              theme_bw()
          } else {
            p <- ACM_var[1:53, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "Recorded")) +
              geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
              geom_line(aes(x = PERIOD, y = EXPECTED, colour = "Expected")) +
              scale_colour_manual(
                name = "",
                values = c(Recorded = "black", Expected = "indianred")
              ) +
              scale_x_continuous(name = "Week (2020-present)") +
              scale_y_continuous(name = "Weekly Deaths") +
              labs(
                title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
                subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
              ) +
              theme_bw()
          }
          p <- p + theme(plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic", color = "darkred"))
          print(p)
          dev.off()
        } else {
          name_PERIOD <- ifelse(ACM_var$WM_IDENTIFIER[1] == "Month", "Month (2020-present)", "Week (2020-present)")
          # Spline Regression
          if (input$check_spline & !input$check_avg) {
            c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
            c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
            last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
            subtitle <- paste0("deaths from ", bquote(2020), " compared to negative binomial regression on available data from the prior period")
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel[1:last_deaths, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_line(aes(x = YEARPERIOD, y = NO_DEATHS, colour = "Recorded")) +
              geom_ribbon(aes(x = YEARPERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
              geom_line(aes(x = YEARPERIOD, y = EXPECTED, colour = "Expected")) +
              scale_colour_manual(
                name = "",
                values = c(Recorded = "black", Expected = "indianred")
              ) +
              scale_y_continuous(name = "Deaths") + # , limits = c(0, NA)) +
              labs(
                title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, caption = caption
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic")
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }

          # historical average
          if (input$check_avg & !input$check_spline) {
            subtitle <- paste0("deaths from ", bquote(2020), " compared to historical average on available data from the prior period")
            c_data_sel <- c_data[c_data$SERIES == "Historical average", ]
            c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
            last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel[1:last_deaths, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_line(aes(x = YEARPERIOD, y = EXPECTED, colour = "Average")) +
              geom_ribbon(aes(x = YEARPERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "cyan2", colour = "cyan2") +
              geom_line(aes(x = YEARPERIOD, y = NO_DEATHS, colour = "Recorded")) +
              scale_colour_manual(
                name = "",
                values = c(Recorded = "black", Average = "cyan2")
              ) +
              scale_y_continuous(name = "Deaths") + # , limits = c(0, NA)) +
              labs(
                title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, size = 12, caption = caption
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic", size = 9)
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }

          # Both neg binom and hist avg
          if (input$check_avg & input$check_spline) {
            subtitle <- paste0("deaths from 2020 compared to negative binomial regression and historical average on available data from the prior period")
            last_deaths <- nrow(c_data) / 2 - which.max(!is.na(rev(c_data[1:(nrow(c_data) / 2), "NO_DEATHS"]))) + 1
            c_data_sel <- c_data[c(1:last_deaths, (nrow(c_data) / 2 + (1:last_deaths))), ]
            c_data_sel$YEARPERIOD <- rep(1:last_deaths, 2)
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_line(aes(x = YEARPERIOD, y = NO_DEATHS, colour = "Recorded")) +
              geom_line(aes(x = YEARPERIOD, y = EXPECTED, group = SERIES, colour = SERIES)) +
              scale_colour_manual(
                name = "", labels = c("Expected", "Average", "Recorded"),
                values = c("indianred", "cyan2", "black")
              ) +
              scale_y_continuous(name = "Deaths") + # , limits = c(0, NA)) +
              labs(
                title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, size = 16, caption = caption
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic", size = 9)
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }

          # neither box checked, just show actual
          if (!input$check_avg & !input$check_spline) {
            c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
            last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
            subtitle <- paste0("Recorded deaths from 2020")
            c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel[1:last_deaths, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_line(aes(x = YEARPERIOD, y = NO_DEATHS), colour = "black") +
              scale_x_continuous(name = name_PERIOD) +
              scale_y_continuous(name = "Deaths") + # , limits = c(0, NA)) +
              labs(
                title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, size = 12
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic", size = 9)
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }
          print(p)
          dev.off()
        }
      }
    )

    output$EDplotdownload <- downloadHandler(
      filename = function() {
        paste(Countryname(), "_plot.pdf", sep = "")
      },
      content = function(file) {
        pdf(file = file, height = 10, width = 10)
        ACM_var <- output_spline()
        validate(
          need(
            (input$EDage %in% output_age()),
            "Please select an Age Group from the above pull-down list."
          )
        )
        c_data <- ACM_var[ACM_var$SEX == input$EDgender & ACM_var$AGE_GROUP == input$EDage, ]
        caption <- ""
        if (attr(ACM_var, "num_deaths") < 4.5) {
          caption <- paste0(
            "WARNING: The model is based on less than 5 years of historical data (",
            round(attr(ACM_var, "num_deaths"), 1), " years). The statistical uncertainty may be high."
          )
        }
        if (nrow(c_data) < 2) {
          if (ACM_var$WM_IDENTIFIER[1] == "Month") {
            # lower <- c_data[c_data$SERIES == "Cyclical spline","LOWER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
            # upper <- c_data[c_data$SERIES == "Cyclical spline","UPPER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
            lower <- ACM_var[ACM_var$SERIES == "Cyclical spline", "LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]
            upper <- ACM_var[ACM_var$SERIES == "Cyclical spline", "UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]
            p <- ACM_var[1:12, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              # geom_line(aes(x = PERIOD, y = EXPECTED), colour = "indianred") +
              # geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
              # geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
              geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
                fill = "indianred", alpha = 0.1
              ) +
              geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1 * lower[1:12], ymax = -1 * upper[1:12]),
                linetype = 1, colour = "indianred"
              ) +
              scale_colour_manual(
                name = "",
                values = c(excess_from_expected = "indianred")
              ) +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_x_continuous(
                name = "Month (2020-present)",
                labels = c(
                  "JAN", "FEB", "MAR", "APR",
                  "MAY", "JUN", "JUL", "AUG",
                  "SEP", "OCT", "NOV", "DEC"
                ),
                breaks = 1:12
              ) +
              scale_y_continuous(name = "Monthly Excess Deaths") +
              labs(
                title = paste0("All Cause Mortality for ", input$EDgender, " ", input$EDage, " in ", Countryname(), " during the Pandemic"),
                subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
              ) +
              theme_bw()
          } else {
            # lower <- c_data[c_data$SERIES == "Cyclical spline","LOWER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
            # upper <- c_data[c_data$SERIES == "Cyclical spline","UPPER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
            lower <- ACM_var[ACM_var$SERIES == "Cyclical spline", "LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]
            upper <- ACM_var[ACM_var$SERIES == "Cyclical spline", "UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]
            p <- ACM_var[1:53, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              # geom_line(aes(x = PERIOD, y = EXPECTED), colour = "indianred") +
              # geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
              # geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
              geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
                fill = "indianred", alpha = 0.1
              ) +
              geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1 * lower[1:53], ymax = -1 * upper[1:53]),
                linetype = 1, colour = "indianred"
              ) +
              scale_colour_manual(
                name = "",
                values = c(excess_from_expected = "indianred")
              ) +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_x_continuous(name = "Week (2020-present)") +
              scale_y_continuous(name = "Weekly Excess Deaths") +
              labs(
                title = paste0("All Cause Mortality for ", input$EDgender, " ", input$EDage, " in ", Countryname(), " during the Pandemic"),
                subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
              ) +
              theme_bw()
          }
          p <- p + theme(plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic", color = "darkred"))
          print(p)
          dev.off()
        } else {
          name_PERIOD <- ifelse(ACM_var$WM_IDENTIFIER[1] == "Month", "Month (2020-present)", "Week (2020-present)")
          # Spline Regression
          if (input$EDcheck_spline & !input$EDcheck_avg) {
            c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
            c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
            last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
            subtitle <- paste0("excess deaths from ", bquote(2020), " compared to negative binomial regression on available data from the prior period")
            lower <- c_data_sel[1:last_deaths, "LOWER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]
            upper <- c_data_sel[1:last_deaths, "UPPER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel[1:last_deaths, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              # geom_ribbon(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
              # geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected")) +
              geom_col(aes(x = YEARPERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
                fill = "indianred", alpha = 0.1
              ) +
              geom_errorbar(aes(x = YEARPERIOD, y = EXCESS_DEATHS, ymin = -1 * lower, ymax = -1 * upper),
                linetype = 1, colour = "indianred"
              ) +
              scale_colour_manual(
                name = "",
                values = c(excess_from_expected = "indianred")
              ) +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_y_continuous(name = "Excess Deaths") +
              labs(
                title = paste0("Excess Mortality for ", input$EDgender, " ", input$EDage, " in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, size = 12, caption = caption
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic", size = 9)
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }

          # historical average
          if (input$EDcheck_avg & !input$EDcheck_spline) {
            c_data_sel <- c_data[c_data$SERIES == "Historical average", ]
            c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
            last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
            subtitle <- paste0("excess deaths from ", bquote(2020), " compared to historical average on available data from the prior period")
            lower <- c_data_sel[1:last_deaths, "LOWER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]
            upper <- c_data_sel[1:last_deaths, "UPPER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel[1:last_deaths, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              # geom_ribbon(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), linetype = 2, alpha = 0.1, fill = "cyan2", colour = "cyan2") +
              # geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_average")) +
              geom_col(aes(x = YEARPERIOD, y = EXCESS_DEATHS, colour = "excess_from_average"),
                fill = "cyan2", alpha = 0.1
              ) +
              geom_errorbar(aes(x = YEARPERIOD, y = EXCESS_DEATHS, ymin = -1 * lower, ymax = -1 * upper),
                linetype = 1, colour = "cyan2"
              ) +
              scale_colour_manual(
                name = "",
                values = c(excess_from_average = "cyan2")
              ) +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_y_continuous(name = "Excess Deaths") +
              labs(
                title = paste0("Excess Mortality for ", input$EDgender, " ", input$EDage, " in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, size = 12, caption = caption
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic", size = 9)
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }

          # Both neg binom and hist avg
          if (input$EDcheck_avg & input$EDcheck_spline) {
            subtitle <- paste0("excess deaths from 2020 compared to negative binomial regression and historical average on available data from the prior period")
            last_deaths <- nrow(c_data) / 2 - which.max(!is.na(rev(c_data[1:(nrow(c_data) / 2), "NO_DEATHS"]))) + 1
            c_data_sel <- c_data[c(1:last_deaths, (nrow(c_data) / 2 + (1:last_deaths))), ]
            c_data_sel$YEARPERIOD <- rep(1:last_deaths, 2)
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel %>%
              ggplot() +
              theme_grey(base_size = 24) +
              # geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, group = SERIES, colour = SERIES)) +
              geom_col(aes(x = YEARPERIOD, y = EXCESS_DEATHS, group = SERIES, colour = SERIES),
                fill = c(
                  rep("indianred", nrow(c_data_sel) / 2),
                  rep("cyan2", nrow(c_data_sel) / 2)
                ),
                position = "dodge", alpha = 0.1
              ) +
              scale_colour_manual(
                name = "", values = c("indianred", "cyan2"),
                labels = c("excess from expected", "excess from average")
              ) +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_y_continuous(name = "Excess Deaths") +
              labs(
                title = paste0("Excess Mortality for ", input$EDgender, " ", input$EDage, "  in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, size = 12, caption = caption
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic", size = 9)
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }

          # neither box checked, just show actual
          if (!input$EDcheck_avg & !input$EDcheck_spline) {
            c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
            last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
            subtitle <- paste0("Recorded deaths from 2020")
            c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel[1:last_deaths, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_line(aes(x = YEARPERIOD, y = NO_DEATHS), colour = "black") +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_y_continuous(name = "All Cause Deaths") +
              labs(
                title = paste0("All Cause Mortality for ", input$EDgender, " ", input$EDage, " in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, size = 12
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic", size = 9)
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }
          print(p)
          dev.off()
        }
      }
    )

    output$EPplotdownload <- downloadHandler(
      filename = function() {
        paste(Countryname(), "_P-score_plot.pdf", sep = "")
      },
      content = function(file) {
        pdf(file = file, height = 10, width = 10)
        ACM_var <- output_spline()
        validate(
          need(
            (input$EPage %in% output_age()),
            "Please select an Age Group from the above pull-down list."
          )
        )
        c_data <- ACM_var[ACM_var$SEX == input$EPgender & ACM_var$AGE_GROUP == input$EPage, ]
        caption <- ""
        if (attr(ACM_var, "num_deaths") < 4.5) {
          caption <- paste0(
            "WARNING: The model is based on less than 5 years of historical data (",
            round(attr(ACM_var, "num_deaths"), 1), " years). The statistical uncertainty may be high."
          )
        }
        if (nrow(c_data) < 2) {
          if (ACM_var$WM_IDENTIFIER[1] == "Month") {
            lower <- 100 * (ACM_var[ACM_var$SERIES == "Cyclical spline", "LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]) / ACM_var[ACM_var$SERIES == "Cyclical spline", "EXPECTED"]
            upper <- 200 * (ACM_var[ACM_var$SERIES == "Cyclical spline", "UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]) / ACM_var[ACM_var$SERIES == "Cyclical spline", "EXPECTED"]
            p <- ACM_var[1:12, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_col(aes(x = PERIOD, y = P_SCORE, colour = "P_score_from_expected"),
                fill = "indianred", alpha = 0.1
              ) +
              geom_errorbar(aes(x = PERIOD, y = P_SCORE, ymin = -1 * lower[1:12], ymax = -1 * upper[1:12]),
                linetype = 1, colour = "indianred"
              ) +
              scale_colour_manual(
                name = "",
                values = c(P_score_from_expected = "indianred")
              ) +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_x_continuous(
                name = "Month (2020-present)",
                labels = c(
                  "JAN", "FEB", "MAR", "APR",
                  "MAY", "JUN", "JUL", "AUG",
                  "SEP", "OCT", "NOV", "DEC"
                ),
                breaks = 1:12
              ) +
              scale_y_continuous(name = "Monthly P-score of Excess Deaths") +
              labs(
                title = paste0("All Cause Mortality for ", input$EPgender, " ", input$EPage, " in ", Countryname(), " during the Pandemic"),
                subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
              ) +
              theme_bw()
          } else {
            lower <- 100 * (ACM_var[ACM_var$SERIES == "Cyclical spline", "LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]) / ACM_var[ACM_var$SERIES == "Cyclical spline", "EXPECTED"]
            upper <- 100 * (ACM_var[ACM_var$SERIES == "Cyclical spline", "UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]) / ACM_var[ACM_var$SERIES == "Cyclical spline", "EXPECTED"]
            p <- ACM_var[1:53, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_col(aes(x = PERIOD, y = P_SCORE, colour = "P_score_from_expected"),
                fill = "indianred", alpha = 0.1
              ) +
              geom_errorbar(aes(x = PERIOD, y = P_SCORE, ymin = -1 * lower[1:53], ymax = -1 * upper[1:53]),
                linetype = 1, colour = "indianred"
              ) +
              scale_colour_manual(
                name = "",
                values = c(P_score_from_expected = "indianred")
              ) +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_x_continuous(name = "Week (2020-present)") +
              scale_y_continuous(name = "Weekly P-score of Excess Deaths") +
              labs(
                title = paste0("All Cause Mortality for ", input$EPgender, " ", input$EPage, " in ", Countryname(), " during the Pandemic"),
                subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
              ) +
              theme_bw()
          }
          p <- p + theme(plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic", color = "darkred"))
          print(p)
          dev.off()
        } else {
          name_PERIOD <- ifelse(ACM_var$WM_IDENTIFIER[1] == "Month", "Month (2020-present)", "Week (2020-present)")
          # Spline Regression
          if (input$EPcheck_spline & !input$EPcheck_avg) {
            c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
            c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
            last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
            subtitle <- paste0("P-score of excess deaths from ", bquote(2020), " compared to negative binomial regression on available data from the prior period")
            lower <- 100 * (c_data_sel[1:last_deaths, "LOWER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]) / c_data_sel[1:last_deaths, "EXPECTED"]
            upper <- 100 * (c_data_sel[1:last_deaths, "UPPER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]) / c_data_sel[1:last_deaths, "EXPECTED"]
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel[1:last_deaths, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_col(aes(x = YEARPERIOD, y = P_SCORE, colour = "P_score_from_expected"),
                fill = "indianred", alpha = 0.1
              ) +
              geom_errorbar(aes(x = YEARPERIOD, y = P_SCORE, ymin = -1 * lower, ymax = -1 * upper),
                linetype = 1, colour = "indianred"
              ) +
              scale_colour_manual(
                name = "",
                values = c(P_score_from_expected = "indianred")
              ) +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_y_continuous(name = "P-score of Excess Deaths") +
              labs(
                title = paste0("P-score of Excess Mortality for ", input$EPgender, " ", input$EPage, " in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, size = 12, caption = caption
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic", size = 9)
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }

          # historical average
          if (input$EPcheck_avg & !input$EPcheck_spline) {
            c_data_sel <- c_data[c_data$SERIES == "Historical average", ]
            c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
            last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
            subtitle <- paste0("P-score of excess deaths from ", bquote(2020), " compared to historical average on available data from the prior period")
            lower <- 100 * (c_data_sel[1:last_deaths, "LOWER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]) / c_data_sel[1:last_deaths, "EXPECTED"]
            upper <- 100 * (c_data_sel[1:last_deaths, "UPPER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]) / c_data_sel[1:last_deaths, "EXPECTED"]
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel[1:last_deaths, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_col(aes(x = YEARPERIOD, y = P_SCORE, colour = "P_score_from_average"),
                fill = "cyan2", alpha = 0.1
              ) +
              geom_errorbar(aes(x = YEARPERIOD, y = P_SCORE, ymin = -1 * lower, ymax = -1 * upper),
                linetype = 1, colour = "cyan2"
              ) +
              scale_colour_manual(
                name = "",
                values = c(P_score_from_average = "cyan2")
              ) +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_y_continuous(name = "P-score of Excess Deaths") +
              labs(
                title = paste0("P-score of Excess Mortality for ", input$EPgender, " ", input$EPage, " in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, size = 12, caption = caption
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic", size = 9)
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }

          # Both neg binom and hist avg
          if (input$EPcheck_avg & input$EPcheck_spline) {
            subtitle <- paste0("P-score of excess deaths from 2020 compared to negative binomial regression and historical average on available data from the prior period")
            last_deaths <- nrow(c_data) / 2 - which.max(!is.na(rev(c_data[1:(nrow(c_data) / 2), "NO_DEATHS"]))) + 1
            c_data_sel <- c_data[c(1:last_deaths, (nrow(c_data) / 2 + (1:last_deaths))), ]
            c_data_sel$YEARPERIOD <- rep(1:last_deaths, 2)
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_col(aes(x = YEARPERIOD, y = P_SCORE, group = SERIES, colour = SERIES),
                fill = c(
                  rep("indianred", nrow(c_data_sel) / 2),
                  rep("cyan2", nrow(c_data_sel) / 2)
                ),
                position = "dodge", alpha = 0.1
              ) +
              scale_colour_manual(
                name = "", values = c("indianred", "cyan2"),
                labels = c("P-score of excess from expected", "P-score of excess from average")
              ) +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_y_continuous(name = "P-score of Excess Deaths") +
              labs(
                title = paste0("P-score of Excess Mortality for ", input$EPgender, " ", input$EPage, "  in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, size = 12, caption = caption
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic", size = 9)
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }

          # neither box checked, just show actual
          if (!input$EPcheck_avg & !input$EPcheck_spline) {
            c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
            last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
            subtitle <- paste0("Recorded deaths from 2020")
            c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
            x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
            x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
            x_labels <- x_labels[x_breaks <= last_deaths]
            x_breaks <- x_breaks[x_breaks <= last_deaths]
            p <- c_data_sel[1:last_deaths, ] %>%
              ggplot() +
              theme_grey(base_size = 24) +
              geom_line(aes(x = YEARPERIOD, y = NO_DEATHS), colour = "black") +
              geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
              scale_y_continuous(name = "All Cause Deaths") +
              labs(
                title = paste0("All Cause Mortality for ", input$EPgender, " ", input$EPage, " in ", Countryname(), " during the Pandemic"),
                subtitle = subtitle, size = 12
              ) +
              theme(
                plot.title = element_text(color = "black", size = 14, face = "bold"),
                plot.subtitle = element_text(color = "blue"),
                plot.caption = element_text(color = "red", face = "italic", size = 9)
              ) +
              if (name_PERIOD == "Month (2020-present)") {
                scale_x_continuous(
                  name = name_PERIOD,
                  labels = rep(c(
                    "JAN", "FEB", "MAR", "APR",
                    "MAY", "JUN", "JUL", "AUG",
                    "SEP", "OCT", "NOV", "DEC"
                  ), 30)[1:last_deaths],
                  breaks = 1:last_deaths
                )
              } else {
                scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
              }
          }
          print(p)
          dev.off()
        }
      }
    )

    ## Data Descriptives (Plots) ------------------------------------------------------

    # Output Expressions -------------------------------------------------------

    # Every piece of content that gets displayed in the app has to be
    # rendered by the appropriate `render*` function, e.g. `renderPrint` for text
    # and `renderPlot` for plots. Most of the render functions here call
    # reactive objects that were created above. I have divided the output objects
    # into sections depending on what tab of the app they are called from.


    ## Data Upload -------------------------------------------------------------



    output$datadesc <- renderUI({
      country <- input$samplecountry
      text <- div()
      if (country == "Australia") {
        text <- div(
          p("This is the data from Australia."), br(),
          p(
            "It is weekly data from January 1, 2015 through week 30 of 2020. Each row corresponds to an All Cause Mortality count for the",
            "year in 'YEAR' and the week number in that year given by 'PERIOD'. The column variables are",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'NO_DEATHS'."
          ),
          p(
            "The All Cause Mortality counts are disaggregated by 'SEX' ('Female', 'Male' and 'Total' (i.e., both combined)).",
            "Similarly, they are also disaggregated by 'AGE_GROUP'. The 'REGION' variable is just set to 'Australia' as this",
            "data is not disaggregated by sub-national regions (e.g., states)."
          )
        )
      }
      if (country == "Japan") {
        text <- div(
          p("This is the data from Japan."), br(),
          p(
            "It is monthly data from January 2015 through June 2020. Each row corresponds to an All Cause Mortality count for the",
            "year in 'YEAR' and the month number in that year given by 'PERIOD'. The column variables are",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'NO_DEATHS'."
          ),
          p(
            "The All Cause Mortality counts are disaggregated by 'SEX' ('Female', 'Male' and 'Total' (i.e., both combined)).",
            "Similarly, they are also disaggregated by 'AGE_GROUP'. The 'REGION' variable is just set to 'Japan' as this",
            "data is not disaggregated by sub-national regions."
          )
        )
      }
      if (country == "Republic of Korea") {
        text <- div(
          p("This is the data from Republic of Korea."), br(),
          p(
            "It is monthly data from January 2015 through September 2020. Each row corresponds to an All Cause Mortality count for the",
            "year in 'YEAR' and the month number in that year given by 'PERIOD'. The column variables are",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'NO_DEATHS'."
          ),
          p(
            "The All Cause Mortality counts are not disaggregated by 'SEX' (i.e., 'Total' is all sexes combined).",
            "Similarly, they are not disaggregated by 'AGE_GROUP'. The 'REGION' variable is just set to 'Republic of Korea' as this",
            "data is not disaggregated by sub-national regions."
          )
        )
      }
      if (country == "New Zealand") {
        text <- div(
          p("This is the data from New Zealand."), br(),
          p(
            "It is weekly data from the beginning of January 2015 through week 46 of 2020. Each row corresponds to an All Cause Mortality count for the",
            "year in 'YEAR' and the week number in that year given by 'PERIOD'. The column variables are",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'NO_DEATHS'."
          ),
          p(
            "The All Cause Mortality counts are disaggregated by 'SEX' ('Female', 'Male' and 'Total' (i.e., both combined)).",
            "Similarly, they are also disaggregated by 'AGE_GROUP'. The 'REGION' variable is just set to 'New Zealand' as this",
            "data is not disaggregated by sub-national regions."
          )
        )
      }
      if (country == "Philippines") {
        text <- div(
          p("This is the data from Philippines."), br(),
          p(
            "It is monthly data from January 2015 through August 2020. Each row corresponds to an All Cause Mortality count for the",
            "year in 'YEAR' and the month number in that year given by 'PERIOD'. The column variables are",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'NO_DEATHS'."
          ),
          p(
            "The All Cause Mortality counts are not disaggregated by 'SEX' (i.e., 'Total' is all sexes combined).",
            "Similarly, they are not disaggregated by 'AGE_GROUP'."
          )
        )
      }

      text
    })

    # output$iristbl = DT::renderDT(
    #      iris, options = list(lengthChange = FALSE)
    #      )
    # output$iris_table = DT::renderDataTable({
    #      datatable(iris(),  extensions = 'Responsive')
    #      })
    output$ACM_table <- shiny::renderDataTable({
      # ACMinit()
      acmtable <- ACMinit()
      acmtable$ISO3 <- NULL
      acmtable
    })
    output$spline_table <- shiny::renderDataTable({
      # output_spline()
      acmtable <- output_spline()
      acmtable$ISO3 <- NULL
      names(acmtable)[names(acmtable) == "WM_IDENTIFIER"] <- "WEEK/MONTH"
      names(acmtable)[names(acmtable) == "AREA"] <- "REGION/AREA"
      names(acmtable)[names(acmtable) == "NO_DEATHS"] <- "DEATHS"
      names(acmtable)[names(acmtable) == "EXPECTED"] <- "EXPECTED_DEATHS"
      names(acmtable)[names(acmtable) == "LOWER_LIMIT"] <- "95%_CI_LOWER"
      names(acmtable)[names(acmtable) == "UPPER_LIMIT"] <- "95%_CI_UPPER"
      acmtable
    })
    output$iris_table <- shiny::renderDataTable(
      {
        iris
      },
      options = list(pageLength = 10)
    )

    output$rawdatafile <- renderPrint({
      raw <- matrix(nrow = 2, ncol = 1)
      rownames(raw) <- c("name:", "size:")
      if (!is.null(input$rawdatafile)) {
        raw[1, 1] <- input$rawdatafile[1, 1]
        raw[2, 1] <- paste(input$rawdatafile[1, 2], " bytes")
      }
      write.table(raw, quote = FALSE, col.names = FALSE)
    })

    # data summary panel under data tab
    output$ACMsum <- renderPrint({
      if (is.null(ACMinit())) {
        return(cat(paste0("Please load the data under the 'Upload All Cause Mortality data' tab on the left\n and use the 'Browse' menu to select an Excel file\n and then 'Choose a region' to specify the region (required).")))
      }
      # ACM_var <- ACMinit()
      acmtable <- ACMinit()
      acmtable$ISO3 <- NULL
      names(acmtable)[names(acmtable) == "WM_IDENTIFIER"] <- "WEEK/MONTH"
      names(acmtable)[names(acmtable) == "AREA"] <- "REGION/AREA"
      #     names(acmtable)[names(acmtable) == "NO_DEATHS"] <- "DEATHS_IN_2020"
      names(acmtable)[names(acmtable) == "WM_IDENTIFIER"] <- "WEEK/MONTH"
      names(acmtable)[names(acmtable) == "EXPECTED"] <- "EXPECTED_DEATHS"
      if (class(acmtable) != "data.frame") {
        return(str(acmtable))
      }
      return(str(acmtable))
    })

    ## Plotting All and Expected deaths ------------------------------------------------------


    output$ACMplot <- renderPlot({
      ACM_var <- output_spline()
      validate(
        need(
          (input$age %in% output_age()),
          "Please select an Age Group from the above pull-down list."
        )
      )
      c_data <- ACM_var[ACM_var$SEX == input$gender & ACM_var$AGE_GROUP == input$age, ]
      caption <- ""
      if (attr(ACM_var, "num_deaths") < 4.5) {
        caption <- paste0(
          "WARNING: The model is based on less than 5 years of historical data (",
          round(attr(ACM_var, "num_deaths"), 1), " years). The statistical uncertainty may be high."
        )
      }
      if (nrow(c_data) < 2) {
        if (ACM_var$WM_IDENTIFIER[1] == "Month") {
          p <- ACM_var[1:12, ] %>%
            ggplot() +
            geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "Recorded")) +
            geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            geom_line(aes(x = PERIOD, y = EXPECTED, colour = "Expected")) +
            scale_colour_manual(
              name = "",
              values = c(Recorded = "black", Expected = "indianred")
            ) +
            scale_x_continuous(
              name = "Month (2020-present)",
              labels = c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ),
              breaks = 1:12
            ) +
            scale_y_continuous(name = "Monthly Deaths") +
            labs(
              title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        } else {
          p <- ACM_var[1:53, ] %>%
            ggplot() +
            geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "Recorded")) +
            geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            geom_line(aes(x = PERIOD, y = EXPECTED, colour = "Expected")) +
            scale_colour_manual(
              name = "",
              values = c(Recorded = "black", Expected = "indianred")
            ) +
            scale_x_continuous(name = "Week (2020-present)") +
            scale_y_continuous(name = "Weekly Deaths") +
            labs(
              title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        }
        p <- p + theme(plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic", color = "darkred"))
        return(p)
      }
      name_PERIOD <- ifelse(ACM_var$WM_IDENTIFIER[1] == "Month", "Month (2020-present)", "Week (2020-present)")
      # Spline Regression
      if (input$check_spline & !input$check_avg) {
        subtitle <- paste0("deaths from ", bquote(2020), " compared to negative binomial regression on available data from the prior period")
        c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
        c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
        last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel[1:last_deaths, ] %>%
          ggplot() +
          geom_line(aes(x = YEARPERIOD, y = NO_DEATHS, colour = "Recorded")) +
          geom_ribbon(aes(x = YEARPERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
          geom_line(aes(x = YEARPERIOD, y = EXPECTED, colour = "Expected")) +
          scale_colour_manual(
            name = "",
            values = c(Recorded = "black", Expected = "indianred")
          ) +
          scale_y_continuous(name = "Deaths") + # , limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12, caption = caption
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }

      # historical average
      if (input$check_avg & !input$check_spline) {
        subtitle <- paste0("deaths from ", bquote(2020), " compared to historical average on available data from the prior period")
        c_data_sel <- c_data[c_data$SERIES == "Historical average", ]
        last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
        c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel[1:last_deaths, ] %>%
          ggplot() +
          geom_line(aes(x = YEARPERIOD, y = EXPECTED, colour = "Average")) +
          geom_ribbon(aes(x = YEARPERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "cyan2", colour = "cyan2") +
          geom_line(aes(x = YEARPERIOD, y = NO_DEATHS, colour = "Recorded")) +
          scale_colour_manual(
            name = "",
            values = c(Recorded = "black", Average = "cyan2")
          ) +
          scale_y_continuous(name = "Deaths") + # , limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12, caption = caption
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }

      # Both neg binom and hist avg
      if (input$check_avg & input$check_spline) {
        subtitle <- paste0("deaths from 2020 compared to negative binomial regression and historical average on available data from the prior period")
        last_deaths <- nrow(c_data) / 2 - which.max(!is.na(rev(c_data[1:(nrow(c_data) / 2), "NO_DEATHS"]))) + 1
        c_data_sel <- c_data[c(1:last_deaths, (nrow(c_data) / 2 + (1:last_deaths))), ]
        c_data_sel$YEARPERIOD <- rep(1:last_deaths, 2)
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel %>%
          ggplot() +
          geom_line(aes(x = YEARPERIOD, y = NO_DEATHS, colour = "Recorded")) +
          geom_line(aes(x = YEARPERIOD, y = EXPECTED, group = SERIES, colour = SERIES)) +
          scale_colour_manual(
            name = "", labels = c("Expected", "Average", "Recorded"),
            values = c("indianred", "cyan2", "black")
          ) +
          scale_y_continuous(name = "Deaths") + # , limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 16, caption = caption
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }

      # neither box checked, just show actual
      if (!input$check_avg & !input$check_spline) {
        c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
        last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
        subtitle <- paste0("Recorded deaths from 2020")
        c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel[1:last_deaths, ] %>%
          ggplot() +
          geom_line(aes(x = YEARPERIOD, y = NO_DEATHS), colour = "black") +
          scale_x_continuous(name = name_PERIOD) +
          scale_y_continuous(name = "Deaths") + # , limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality for ", input$gender, " ", input$age, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }
      p
    })

    output$EDplot <- renderPlot({
      ACM_var <- output_spline()
      validate(
        need(
          (input$EDage %in% output_age()),
          "Please select an Age Group from the above pull-down list."
        )
      )
      c_data <- ACM_var[ACM_var$SEX == input$EDgender & ACM_var$AGE_GROUP == input$EDage, ]
      caption <- ""
      if (attr(ACM_var, "num_deaths") < 4.5) {
        caption <- paste0(
          "WARNING: The model is based on less than 5 years of historical data (",
          round(attr(ACM_var, "num_deaths"), 1), " years). The statistical uncertainty may be high."
        )
      }
      if (nrow(c_data) < 2) {
        if (ACM_var$WM_IDENTIFIER[1] == "Month") {
          # lower <- c_data[c_data$SERIES == "Cyclical spline","LOWER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          # upper <- c_data[c_data$SERIES == "Cyclical spline","UPPER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          lower <- ACM_var[ACM_var$SERIES == "Cyclical spline", "LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]
          upper <- ACM_var[ACM_var$SERIES == "Cyclical spline", "UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]
          p <- ACM_var[1:12, ] %>%
            ggplot() +
            # geom_line(aes(x = PERIOD, y = EXPECTED), colour = "indianred") +
            # geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            # geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
            geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
              fill = "indianred", alpha = 0.1
            ) +
            geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1 * lower[1:12], ymax = -1 * upper[1:12]),
              linetype = 1, colour = "indianred"
            ) +
            scale_colour_manual(
              name = "",
              values = c(excess_from_expected = "indianred")
            ) +
            geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
            scale_x_continuous(
              name = "Month (2020-present)",
              labels = c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ),
              breaks = 1:12
            ) +
            scale_y_continuous(name = "Monthly Excess Deaths") +
            labs(
              title = paste0("All Cause Mortality for ", input$EDgender, " ", input$EDage, " in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        } else {
          # lower <- c_data[c_data$SERIES == "Cyclical spline","LOWER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          # upper <- c_data[c_data$SERIES == "Cyclical spline","UPPER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          lower <- ACM_var[ACM_var$SERIES == "Cyclical spline", "LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]
          upper <- ACM_var[ACM_var$SERIES == "Cyclical spline", "UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]
          p <- ACM_var[1:53, ] %>%
            ggplot() +
            # geom_line(aes(x = PERIOD, y = EXPECTED), colour = "indianred") +
            # geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            # geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
            geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
              fill = "indianred", alpha = 0.1
            ) +
            geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1 * lower[1:53], ymax = -1 * upper[1:53]),
              linetype = 1, colour = "indianred"
            ) +
            scale_colour_manual(
              name = "",
              values = c(excess_from_expected = "indianred")
            ) +
            geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
            scale_x_continuous(name = "Week (2020-present)") +
            scale_y_continuous(name = "Weekly Excess Deaths") +
            labs(
              title = paste0("All Cause Mortality for ", input$EDgender, " ", input$EDage, " in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        }
        p <- p + theme(plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic", color = "darkred"))
        return(p)
      }
      name_PERIOD <- ifelse(ACM_var$WM_IDENTIFIER[1] == "Month", "Month (2020-present)", "Week (2020-present)")
      # Spline Regression
      if (input$EDcheck_spline & !input$EDcheck_avg) {
        c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
        c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
        last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
        subtitle <- paste0("excess deaths from ", bquote(2020), " compared to negative binomial regression on available data from the prior period")
        lower <- c_data_sel[1:last_deaths, "LOWER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]
        upper <- c_data_sel[1:last_deaths, "UPPER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel[1:last_deaths, ] %>%
          ggplot() +
          # geom_ribbon(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
          # geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected")) +
          geom_col(aes(x = YEARPERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
            fill = "indianred", alpha = 0.1
          ) +
          geom_errorbar(aes(x = YEARPERIOD, y = EXCESS_DEATHS, ymin = -1 * lower, ymax = -1 * upper),
            linetype = 1, colour = "indianred"
          ) +
          scale_colour_manual(
            name = "",
            values = c(excess_from_expected = "indianred")
          ) +
          geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
          scale_y_continuous(name = "Excess Deaths") +
          labs(
            title = paste0("Excess Mortality for ", input$EDgender, " ", input$EDage, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12, caption = caption
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }

      # historical average
      if (input$EDcheck_avg & !input$EDcheck_spline) {
        c_data_sel <- c_data[c_data$SERIES == "Historical average", ]
        c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
        last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
        subtitle <- paste0("excess deaths from ", bquote(2020), " compared to historical average on available data from the prior period")
        lower <- c_data_sel[1:last_deaths, "LOWER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]
        upper <- c_data_sel[1:last_deaths, "UPPER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel[1:last_deaths, ] %>%
          ggplot() +
          # geom_ribbon(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), linetype = 2, alpha = 0.1, fill = "cyan2", colour = "cyan2") +
          # geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_average")) +
          geom_col(aes(x = YEARPERIOD, y = EXCESS_DEATHS, colour = "excess_from_average"),
            fill = "cyan2", alpha = 0.1
          ) +
          geom_errorbar(aes(x = YEARPERIOD, y = EXCESS_DEATHS, ymin = -1 * lower, ymax = -1 * upper),
            linetype = 1, colour = "cyan2"
          ) +
          scale_colour_manual(
            name = "",
            values = c(excess_from_average = "cyan2")
          ) +
          geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
          scale_y_continuous(name = "Excess Deaths") +
          labs(
            title = paste0("Excess Mortality for ", input$EDgender, " ", input$EDage, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12, caption = caption
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }

      # Both neg binom and hist avg
      if (input$EDcheck_avg & input$EDcheck_spline) {
        subtitle <- paste0("excess deaths from 2020 compared to negative binomial regression and historical average on available data from the prior period")
        last_deaths <- nrow(c_data) / 2 - which.max(!is.na(rev(c_data[1:(nrow(c_data) / 2), "NO_DEATHS"]))) + 1
        c_data_sel <- c_data[c(1:last_deaths, (nrow(c_data) / 2 + (1:last_deaths))), ]
        c_data_sel$YEARPERIOD <- rep(1:last_deaths, 2)
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel %>%
          ggplot() +
          # geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, group = SERIES, colour = SERIES)) +
          geom_col(aes(x = YEARPERIOD, y = EXCESS_DEATHS, group = SERIES, colour = SERIES),
            fill = c(
              rep("indianred", nrow(c_data_sel) / 2),
              rep("cyan2", nrow(c_data_sel) / 2)
            ),
            position = "dodge", alpha = 0.1
          ) +
          scale_colour_manual(
            name = "", values = c("indianred", "cyan2"),
            labels = c("excess from expected", "excess from average")
          ) +
          geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
          scale_y_continuous(name = "Excess Deaths") +
          labs(
            title = paste0("Excess Mortality for ", input$EDgender, " ", input$EDage, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12, caption = caption
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }

      # neither box checked, just show actual
      if (!input$EDcheck_avg & !input$EDcheck_spline) {
        c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
        last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
        subtitle <- paste0("Recorded deaths from 2020")
        c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel[1:last_deaths, ] %>%
          ggplot() +
          geom_line(aes(x = YEARPERIOD, y = NO_DEATHS), colour = "black") +
          geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
          scale_y_continuous(name = "All Cause Deaths") +
          labs(
            title = paste0("All Cause Mortality for ", input$EDgender, " ", input$EDage, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }
      p
    })

    output$EPplot <- renderPlot({
      ACM_var <- output_spline()
      validate(
        need(
          (input$EPage %in% output_age()),
          "Please select an Age Group from the above pull-down list."
        )
      )
      c_data <- ACM_var[ACM_var$SEX == input$EPgender & ACM_var$AGE_GROUP == input$EPage, ]
      caption <- ""
      if (attr(ACM_var, "num_deaths") < 4.5) {
        caption <- paste0(
          "WARNING: The model is based on less than 5 years of historical data (",
          round(attr(ACM_var, "num_deaths"), 1), " years). The statistical uncertainty may be high."
        )
      }
      if (nrow(c_data) < 2) {
        if (ACM_var$WM_IDENTIFIER[1] == "Month") {
          lower <- 100 * (ACM_var[ACM_var$SERIES == "Cyclical spline", "LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]) / ACM_var[ACM_var$SERIES == "Cyclical spline", "EXPECTED"]
          upper <- 200 * (ACM_var[ACM_var$SERIES == "Cyclical spline", "UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]) / ACM_var[ACM_var$SERIES == "Cyclical spline", "EXPECTED"]
          p <- ACM_var[1:12, ] %>%
            ggplot() +
            geom_col(aes(x = PERIOD, y = P_SCORE, colour = "P_score_from_expected"),
              fill = "indianred", alpha = 0.1
            ) +
            geom_errorbar(aes(x = PERIOD, y = P_SCORE, ymin = -1 * lower[1:12], ymax = -1 * upper[1:12]),
              linetype = 1, colour = "indianred"
            ) +
            scale_colour_manual(
              name = "",
              values = c(P_score_from_expected = "indianred")
            ) +
            geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
            scale_x_continuous(
              name = "Month (2020-present)",
              labels = c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ),
              breaks = 1:12
            ) +
            scale_y_continuous(name = "Monthly P-score of Excess Deaths") +
            labs(
              title = paste0("All Cause Mortality for ", input$EPgender, " ", input$EPage, " in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        } else {
          lower <- 100 * (ACM_var[ACM_var$SERIES == "Cyclical spline", "LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]) / ACM_var[ACM_var$SERIES == "Cyclical spline", "EXPECTED"]
          upper <- 100 * (ACM_var[ACM_var$SERIES == "Cyclical spline", "UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline", "NO_DEATHS"]) / ACM_var[ACM_var$SERIES == "Cyclical spline", "EXPECTED"]
          p <- ACM_var[1:53, ] %>%
            ggplot() +
            # geom_line(aes(x = PERIOD, y = EXPECTED), colour = "indianred") +
            # geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            # geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
            geom_col(aes(x = PERIOD, y = P_SCORE, colour = "P_score_from_expected"),
              fill = "indianred", alpha = 0.1
            ) +
            geom_errorbar(aes(x = PERIOD, y = P_SCORE, ymin = -1 * lower[1:53], ymax = -1 * upper[1:53]),
              linetype = 1, colour = "indianred"
            ) +
            scale_colour_manual(
              name = "",
              values = c(P_score_from_expected = "indianred")
            ) +
            geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
            scale_x_continuous(name = "Week (2020-present)") +
            scale_y_continuous(name = "Weekly P-score of Excess Deaths") +
            labs(
              title = paste0("All Cause Mortality for ", input$EPgender, " ", input$EPage, " in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        }
        p <- p + theme(plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic", color = "darkred"))
        return(p)
      }
      name_PERIOD <- ifelse(ACM_var$WM_IDENTIFIER[1] == "Month", "Month (2020-present)", "Week (2020-present)")
      # Spline Regression
      if (input$EPcheck_spline & !input$EPcheck_avg) {
        c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
        c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
        last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
        subtitle <- paste0("P-score of excess deaths from ", bquote(2020), " compared to negative binomial regression on available data from the prior period")
        lower <- 100 * (c_data_sel[1:last_deaths, "LOWER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]) / c_data_sel[1:last_deaths, "EXPECTED"]
        upper <- 100 * (c_data_sel[1:last_deaths, "UPPER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]) / c_data_sel[1:last_deaths, "EXPECTED"]
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel[1:last_deaths, ] %>%
          ggplot() +
          # geom_ribbon(aes(x = PERIOD, y = P_SCORE, ymin = -1*lower, ymax = -1*upper), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
          # geom_line(aes(x = PERIOD, y = P_SCORE, colour = "P_score_from_expected")) +
          geom_col(aes(x = YEARPERIOD, y = P_SCORE, colour = "P_score_from_expected"),
            fill = "indianred", alpha = 0.1
          ) +
          geom_errorbar(aes(x = YEARPERIOD, y = P_SCORE, ymin = -1 * lower, ymax = -1 * upper),
            linetype = 1, colour = "indianred"
          ) +
          scale_colour_manual(
            name = "",
            values = c(P_score_from_expected = "indianred")
          ) +
          geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
          scale_y_continuous(name = "P-score of Excess Deaths") +
          labs(
            title = paste0("P-score of Excess Mortality for ", input$EPgender, " ", input$EPage, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12, caption = caption
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }

      # historical average
      if (input$EPcheck_avg & !input$EPcheck_spline) {
        c_data_sel <- c_data[c_data$SERIES == "Historical average", ]
        c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
        last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
        subtitle <- paste0("P-score of excess deaths from ", bquote(2020), " compared to historical average on available data from the prior period")
        lower <- 100 * (c_data_sel[1:last_deaths, "LOWER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]) / c_data_sel[1:last_deaths, "EXPECTED"]
        upper <- 100 * (c_data_sel[1:last_deaths, "UPPER_LIMIT"] - c_data_sel[1:last_deaths, "NO_DEATHS"]) / c_data_sel[1:last_deaths, "EXPECTED"]
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel[1:last_deaths, ] %>%
          ggplot() +
          # geom_ribbon(aes(x = PERIOD, y = P_SCORE, ymin = -1*lower, ymax = -1*upper), linetype = 2, alpha = 0.1, fill = "cyan2", colour = "cyan2") +
          # geom_line(aes(x = PERIOD, y = P_SCORE, colour = "P_score_from_average")) +
          geom_col(aes(x = YEARPERIOD, y = P_SCORE, colour = "P_score_from_average"),
            fill = "cyan2", alpha = 0.1
          ) +
          geom_errorbar(aes(x = YEARPERIOD, y = P_SCORE, ymin = -1 * lower, ymax = -1 * upper),
            linetype = 1, colour = "cyan2"
          ) +
          scale_colour_manual(
            name = "",
            values = c(P_score_from_average = "cyan2")
          ) +
          geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
          scale_y_continuous(name = "P-score of Excess Deaths") +
          labs(
            title = paste0("P-score of Excess Mortality for ", input$EPgender, " ", input$EPage, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12, caption = caption
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }

      # Both neg binom and hist avg
      if (input$EPcheck_avg & input$EPcheck_spline) {
        subtitle <- paste0("P-score of excess deaths from 2020 compared to negative binomial regression and historical average on available data from the prior period")
        last_deaths <- nrow(c_data) / 2 - which.max(!is.na(rev(c_data[1:(nrow(c_data) / 2), "NO_DEATHS"]))) + 1
        c_data_sel <- c_data[c(1:last_deaths, (nrow(c_data) / 2 + (1:last_deaths))), ]
        c_data_sel$YEARPERIOD <- rep(1:last_deaths, 2)
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel %>%
          ggplot() +
          # geom_line(aes(x = PERIOD, y = P_SCORE, group = SERIES, colour = SERIES)) +
          geom_col(aes(x = YEARPERIOD, y = P_SCORE, group = SERIES, colour = SERIES),
            fill = c(
              rep("indianred", nrow(c_data_sel) / 2),
              rep("cyan2", nrow(c_data_sel) / 2)
            ),
            position = "dodge", alpha = 0.1
          ) +
          scale_colour_manual(
            name = "", values = c("indianred", "cyan2"),
            labels = c("P-score of excess from expected", "P-score of excess from average")
          ) +
          geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
          scale_y_continuous(name = "P-score of Excess Deaths") +
          labs(
            title = paste0("P-score of Excess Mortality for ", input$EPgender, " ", input$EPage, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12, caption = caption
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }

      # neither box checked, just show actual
      if (!input$EPcheck_avg & !input$EPcheck_spline) {
        c_data_sel <- c_data[c_data$SERIES == "Cyclical spline", ]
        last_deaths <- nrow(c_data_sel) - which.max(!is.na(rev(c_data_sel[, "NO_DEATHS"]))) + 1
        subtitle <- paste0("Recorded deaths from 2020")
        c_data_sel$YEARPERIOD <- 1:nrow(c_data_sel)
        x_breaks <- rep(c(1, seq(5, 50, by = 5)), 30) + rep(53 * (0:29), rep(11, 30))
        x_labels <- paste(x_breaks - 53 * trunc(x_breaks / 53))
        x_labels <- x_labels[x_breaks <= last_deaths]
        x_breaks <- x_breaks[x_breaks <= last_deaths]
        p <- c_data_sel[1:last_deaths, ] %>%
          ggplot() +
          geom_line(aes(x = YEARPERIOD, y = NO_DEATHS), colour = "black") +
          geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
          scale_y_continuous(name = "All Cause Deaths") +
          labs(
            title = paste0("All Cause Mortality for ", input$EPgender, " ", input$EPage, " in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme(
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            plot.subtitle = element_text(color = "blue"),
            plot.caption = element_text(color = "red", face = "italic", size = 12)
          ) +
          if (name_PERIOD == "Month (2020-present)") {
            scale_x_continuous(
              name = name_PERIOD,
              labels = rep(c(
                "JAN", "FEB", "MAR", "APR",
                "MAY", "JUN", "JUL", "AUG",
                "SEP", "OCT", "NOV", "DEC"
              ), 30)[1:last_deaths],
              breaks = 1:last_deaths
            )
          } else {
            scale_x_continuous(name = name_PERIOD, breaks = x_breaks, labels = x_labels)
          }
      }
      p
    })
  }
)
