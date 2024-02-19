
# Everything that gets displayed inside the app is enclosed in a call to `shinyUI`.
# The first thing to be specified is the type of page to display. The `navbarPage`
# includes a navigation bar at the top of the page and each tab leads to different
# pages of content.

shinyUI(
  navbarPage(
    #theme="mycosmo.css",
    title=NULL,
    id= 'navbar', windowTitle = 'WPRO all-cause-of-mortality and excess death calculator', collapsible=TRUE,



# Front Page (About) ------------------------------------------------------


tabPanel(title=span('WPRO all-cause-of-mortality and excess death calculator', id="sWtitle"),
         value='tab1',
         fluidRow(
          column(2,
                 actionButton("aboutButton", label = "About the calculator",
                              class = "btn active", width="80%"), br(),
                 actionButton("citeButton", label = "Citing the calculator",
                              class = "btn", width="80%"), br(),
                 actionButton('startButton', label='Get Started',
                              class="btn btn-primary")
          ),
   column(6, style="padding: 0 30px 0 0;",
          div(id="aboutbox",
            p("Welcome to the",strong("WPRO online-calculator for excess deaths in countries"),"!"),

            p("This calculator has been developed by the", a('World Health Organization, Western Pacific Region',
                href='https://www.who.int/westernpacific/',
               target='_blank'), "in conjunction with the", a('Department of Statistics at UCLA',
                href='http://statistics.ucla.edu/', target='_blank'),"."),

            p("This tool aims to estimate the",em("expected all-cause mortality"),"counts for each week or month starting at",
              "January 1, 2020 onward in the counter-factual situation where there had not been a pandemic.",
              "Monitoring the all-cause mortality trends is an important component of multisource surveillance for COVID-19.",
              "The",em("excess mortality"),"is defined to be the difference between the reported counts and expected counts for that week or month.",
              "in the Western Pacific region"),

            p("This interface is useful as part of the dialogue between the WHO WPRO and countries about the",
              "impact of the COVID-19 pandemic on all-cause mortality (ACM) in individual Member countries and territories",
              "in the Western Pacific region"),

            p("Tracking all-cause mortality trends is an important component of multisource surveillance for COVID-19.",
              "Excess deaths have been observed in several countries during the COVID19 pandemic.",
              "Evidence is needed to support timely and dynamic decision-making and policy development."),

            p("This tool will allow easy tracking and analysis of ACM and excess deaths.",
              "It is for use by member countries and does not require the data to be seen by the WHO WPRO.",
              "In fact, all analysis is done on the computer where it is run. An internet connection is only required",
              "to install the software (already done if you are reading this message!)."),

            p("A typical analysis will move sequentially through the tabs at the top of the page",
              "(starting with", actionLink("startButton", "Get Started"),".",
              "Click on the help icon at the top of any page for guidance."),
            p("Bug Reports/comments/suggestions/requests? Please share them with us.",
              "They are best submitted through our", a('GitHub site,',
                                                       href='https://github.com/WorldHealthOrganization/ACMcalculator',
                                                       target='_blank'),
              "or by email to us (see", actionLink("helpLink", "Help"), "tab)."),
            p("This web application",
              "is written with the Shiny framework and development is via GitHub.  More information",
              "on Shiny and our GitHub repository can be found in the",
              "resource links on the right.")
          ),
          div(id="citebox",
            tabsetPanel(
              tabPanel("BibTeX",
                p(strong("ACMCalculator")),
                tags$pre(id='scitation','@Manual{handcock:ACMCalculator,
                  title = {ACMCalculator: Software tools for the Statistical Analysis of Excess Mortality from All Cause Mortality Data
                  author = {Mark S. Handcock},
                  year = {2021},
                  address = {Los Angeles, CA},
                  url = {http://hpmrg.org/}
                }'),
                p(strong("ACMCalculator")),
                tags$pre(id='swcitation',"@Manual{beylerian:ACMCalculator,
                  title = {\\pkg{ACMCalculator}: A Graphical User Interface for Analyzing Excess Mortality from All Cause Mortality Data},
                  author = {Mark S. Handcock},
                  year = {2021},
                  note = {\\proglang{R}~package version~0.1},
                  address = {Los Angeles, CA},
                  url = {https://cran.r-project.org/web/packages/WPROACM/}
                }")
              ),
              tabPanel("Other",
                p(strong("ACMCalculator")),
                tags$pre("Mark S. Handcock (2021). ACMCalculator: A Graphical User Interface for Analyzing Excess Mortality from All Cause Mortality Data. URL http://hpmrg.org"),
                
                p(strong("ACMCalculator")),
                tags$pre("Mark S. Handcock (2021).
                ACMCalculator: A Graphical User Interface for Analyzing Excess Mortality from All Cause Mortality Data.")
                       )
            ),
            p('If you use ACMCalculator, please cite it'),
            )
          ),
   column(4,
          wellPanel(
              h5(tags$u('Resources')),
              div(a("The calculator on GitHub", href="https://github.com/WorldHealthOrganization/ACMcalculator",
                    target="_blank")),
              div(a("WPRO all-cause mortality dashboard", href="https://lynx.wpro.who.int/viz/allcausedeath.asp",
                    target="_blank")),
              div(a("Shiny: a web application framework for R", href="http://shiny.rstudio.com/",
                    target="_blank"))
          )
#         fluidRow(a(img(src = 'WHO-WPRO_Logo_PMS_2925.png', width = 300),
#            href = 'https://www.who.int/westernpacific/', target = '_blank'),
#            style="margin-left:15px;"),
#         br(),
#         fluidRow(a(img(src = 'UCLADepartmentofStatisticsSmall.png', width = 400),
#            href = 'http://statistics.ucla.edu/', target = '_blank'),
#            style="margin-left:15px;")
         )
   )
 ),


# Data Upload -------------------------------------------------------------


# Before the code for what is displayed on the Data Upload page,
# various javaScript and CSS files that will be useful later in the
# script are linked. For example, since network plotting and model
# fitting do not happen instantly (especially for large networks),
# a loading icon will help to assure users that the app is still working
# on producing output. The file busy.js controls the behavior of the
# loading message and style.css controls the appearance. To display
# the loading message on subsequent tabs, we only need to include the
# div statement within those tabs.

tabPanel(title='Data', value='tab2',
         #busy.js is for calculation in progress boxes
         #alert.js is for popup boxes,
         #jquery libraries are loaded from google cdn, needed for autocomplete
         #this tagList command has to go inside a tabPanel
         tagList(
           tags$head(
             tags$link(rel="stylesheet", type="text/css",href="style.css"),
             #tags$link(rel="stylesheet", type="text/css",href="autocomplete.css"),
             tags$link(rel="stylesheet", type="text/css",
                       href="//ajax.googleapis.com/ajax/libs/jqueryui/1.11.1/themes/smoothness/jquery-ui.css"),
             tags$script(type="text/javascript", src="//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"),
             #tags$script(type="text/javascript", src="autocomplete.js"),
             tags$script(type="text/javascript", src="//ajax.googleapis.com/ajax/libs/jqueryui/1.11.1/jquery-ui.min.js"),
             tags$script(type="text/javascript", src="busy.js"),
             tags$script(type="text/javascript", src="alert.js"),
             tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",
                              function(message) {
                              console.log(message)
                              eval(message.code);
                              });'))
           )
         ),

# Conditional panels are only displayed when a specified condition is true.
# The condition is a javascript expression that can refer to the current
# values of input or output objects. When the condition is false, the panel
# does not take up any space in the UI.


fluidRow(
  column(7,
    tabsetPanel(id='datatabs',
      tabPanel('Upload All Cause Mortality data', br(),
         wellPanel(
           fluidRow(
             column(8,
              tabPanel('All cause mortality in 2020', br(),
                      div(id="viewdata",
                        p("This tool is designed to estimate the weekly or monthly excess deaths in countries in the Western Pacific Region",
                          "during the COVID-19 pandemic, using all-cause of mortality data."),
                        p("The", strong("5-years historical average"),"and", strong("expected deaths forecasted by negative-binomial regression"),
                          ", and their 95% confidence interval (95% CI) are calculated from the deaths observed in 2015-2019.")
                         )
                      )
             ),
             column(8,
                    p(class="helper", id="Excelhelp", icon("question-circle"),
                      span("What format does the Excel file need to be in?", style="font-size:0.85em;"),
                       br(),'The calculator needs to read in a *.xls or *.xlsx file of all-cause mortality data.',
                       'The file should be saved from the WHO standardized Excel template.',
                       br(),
                       'As a guide, the calculator has built-in example data using the WHO standardized Excel template.', 
                       'To download an example template, select the template you want from this pull-down list and click "Download template":'),
                    selectizeInput('template_country', label=NULL,
                                choices=c("Choose a country specific or generic template" = '', 
                 'Australia (empty template)', 'Philippines (empty template)', 'French Polynesia (empty template)',
                 'Generic Monthly template', 'Generic Weekly template',
                 'Australia (filled up to August 2020)', 'Japan (filled up to August 2020)', 'Republic of Korea (filled up to August 2020)', 'New Zealand (filled up to August 2020)', 'Philippines (filled up to August 2020)')),
                 downloadButton('download_templates',"Download template"), br()," ",br(),
                    p(class="helper", id="Excelhelpweeks", icon("question-circle"),
                      span("I have weekly data. Where do I enter the number of days in each week?", style="font-size:0.85em;"),
                 br(), "Please remember to enter in the Excel file the number of days in each week, especially for weeks 1 and 53. These should be entered for each year (2015, 2016, ...) on row 4 in the column for that week. They are pre-entered as 7.")
             ),
             column(8,
                    br(), p("Once you have familiarized yourself with the template, use this pull-down list below to upload your own data in a WHO standardized Excel sheet or use a built-in example data set"),
                    selectInput('filetype',label='Open a data set',
                                 choices=c(
                                           'Excel spreadsheet of All Cause Mortality data (*.xls or *.xlsx)' = 1,
                                           'built-in example All Cause Mortality data'= 2
                                          )
                    ),
             conditionalPanel(condition = 'input.filetype < 2',
               column(8,
                    br(),
                    fileInput(inputId='rawdatafile', label=NULL, accept='text'),
                    verbatimTextOutput('rawdatafile')
                    )
             ),
             conditionalPanel(condition = 'input.filetype < 2',
               column(8,
                    br(style="line-height:26px;"),
                    p(class="helper", id="SNIhelp", icon("question-circle"),
                       'Use the pull-down menu below to choose a region the country. Some countries only have one region.'),
                    uiOutput('selectsheet')
                    )
             ),
             conditionalPanel(condition = 'input.filetype == 2',
                column(12,
                    br(style="line-height:26px;"),
                    p(class="helper", id="BIhelp", icon("question-circle"),
                      span("These are example Excel files from some countries with data from January 1, 2015 through August 2020.", style="font-size:0.85em;"),
                       br(),'Use the pull-down menu below to choose a country'),#, em("Choose a country"),
                    selectizeInput('samplecountry', label=NULL,
                                choices=c("Choose a country" = '', 
                                          'Australia','Japan',
                                          'Republic of Korea', 'New Zealand', 'Philippines'))
                )
             ),
             conditionalPanel(condition = 'input.filetype == 2',
                column(12,
                    br(style="line-height:26px;"),
                    p(class="helper", id="SIhelp", icon("question-circle"),
                       'Use the pull-down menu below to choose a region the country. Some countries only have one region.'),
                    uiOutput('selectbuiltinsheet'),
                )
               )
             )
           )),
           conditionalPanel(condition = 'input.filetype == 2 & input.samplecountry != ""',
             wellPanel(uiOutput("datadesc")),
             wellPanel(
                    br(style="line-height:26px;"),
                    p(class="helper", id="SIhelp", icon("question-circle"),
                       'You can view the data by clicking in the ', em("View Data"),' tab at top of this page.',
                       'You can then try the calculator out on this data to see an analysis similar to that for your own country.'),
                      )
           )
         ),
    tabPanel('View Data', br(),
          div(id="viewdata",
            p("Below is displayed the all cause of mortality data as read in. It should display column variables called",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'DEATHS'.",
            "The row correspond to the time periods from 2015, designated by the 'YEAR' and 'PERIOD' variables.",
            "It is ok for a few values should be missing (denoted NA). If your files does not look like this, try to load the built-in",
            "example All Cause Mortality data under the 'Upload All Cause Mortality data' tab on the top left. If you view that",
            "it will show what to expect."),
            p("If your data does not look correct, try to correct it by using the WHO standardized Excel template under the",
               em("Data"),"tab on the top left."), 
            p("A typical analysis will move sequentially through the tabs at the top of the page",
              "Click on the help icon at the top of any page for guidance."),
            p("If you are having trouble getting the data in",
              "email us (see", actionLink("helpLink", "Help"), "tab)."),
            ),
           wellPanel(
             dataTableOutput("ACM_table")
           )
         )
# tabPanel('Modify Attributes', br(),
#          wellPanel(
#            p('In the future we will build in functions that will ',
#              'allow you to modify the attributes of your network.',
#              'This will include options like:'),
#            tags$ul(
#              tags$li('Applying a function (e.g.', code('sqrt()'), ') to an attribute'),
#              tags$li('Recoding (mapping a set of attributes onto a new set)'),
#              tags$li('Conditional transformations (', code('do...if...'),')'))
#            #uiOutput('modifyattrchooser')
#            )
#          )
  )
),

column(4,
tabsetPanel(
  tabPanel('Data Summary', br(),
           verbatimTextOutput('ACMsum')
           ))
  )
),

icon('question-circle', class='fa-2x helper-btn'),
div(class="helper-box", style="display:none",
    p('Upload a file of all-cause mortality data. The data should be a *.xls or *xlsx file',
    'saved from the WHO standardized Excel template. Click', a("here",
      href = "https://www.who.int/westernpacific/", target = "_blank"), ' to download.')),
actionLink('dataleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
actionLink('dataright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
),


# Data Descriptives ----------------------------------------------------

# There are no calls to selectInput for the options to color code or size the nodes,
# even though they appear in the app. Most widget functions are called in ui.R, but
# this means that all the options passed to them must be static. If the options depend
# on user input (the coloring and sizing menus depend on which network the user
# selects), the widget must be rendered in server.R and output in ui.R with
# iuOutput.

    tabPanel(
      title = "Plots", value = "tab3",
      # include progress box when this tab is loading
      div(
        class = "busy",
        p("Calculation in progress... This might take a few minutes."),
        img(src = "ajax-loader.gif")
      ),
      fluidRow(
        column(9,
          tabsetPanel(id = "plottabs",
            tabPanel(
              "All Cause Mortality Plot", br(),
              wellPanel(
                fluidRow(
                  selectInput("gender", "Select Sex", gender_labels),
                  #selectInput("gender", "Select Sex", sort(as.character(unique(ACM_var$SEX)))),
                  #selectInput("age", "Select Age Group", output_age),
                 #div(id="agebox", strong("Select Age Group")),
                 #textInput("age", "Type in the Age Group (choosing from the list above)", age_group_labels),
                 #selectInput("age", "Select Age Group", age_group_labels),
                 #p(strong("Pull-down list of Age Groups:")),
                  uiOutput('age'),
                 #selectInput("age", "Select Age Group", sort(as.character(unique(ACM_var$AGE_GROUP)))),
                  checkboxInput('check_avg', '5-year Average'),
                  checkboxInput('check_spline', "Negative Binomial Regression"),
                  plotOutput('ACMplot'),
                  downloadButton("ACMplotdownload", label = "Download plot as PDF", class = "btn-sm")
                ) ),
            ),
            tabPanel(
              "Excess Mortality Plot", br(),
              wellPanel(
                fluidRow(
                  div(id="barplotbox",
                      "The excess mortality is displayed here using a bar graph.",
                      "The height of each bar represents the excess death for that month/week.",
                      "The lines above and below the end of each bar indicate what the height ",
                      "of the bar would be using the 95% confidence interval upper bound and lower bound, respectively."),
                  selectInput("EDgender", "Select Sex", gender_labels),
               #  selectInput("EDage", "Select Age Group", age_group_labels),
               #  textInput("EDage", "Type in the Age Group (choosing from the list below)", age_group_labels),
               #  p(strong("Pull-down list of Age Groups:")),
                  uiOutput('EDage'),
                  checkboxInput('EDcheck_avg', '5-year Average'),
                  checkboxInput('EDcheck_spline', "Negative Binomial Regression"),
                  plotOutput('EDplot'),
                  downloadButton("EDplotdownload", label = "Download Excess mortality plot as PDF", class = "btn-sm")
                ) ),
            ),
            tabPanel(
              "P-score of Excess Mortality Plot", br(),
              wellPanel(
                fluidRow(
                  div(id="barplotbox",
                      "The p-score of the excess mortality is displayed here using a bar graph.",
                      "The p-score is the percentage the excess mortality is of the expected mortality.",
                      "For example, a value of 4 means that the excess mortality is 4% of the expected mortality.",
                      "A value of -1 means that the excess mortality is negative and 1% of the expected mortality.",
                      "The height of each bar represents the p-score of the excess death for that month/week.",
                      "The lines above and below the end of each bar indicate what the height ",
                      "of the bar would be using the 95% confidence interval upper bound and lower bound, respectively."),
                  selectInput("EPgender", "Select Sex", gender_labels),
               #  selectInput("EPage", "Select Age Group", age_group_labels),
               #  textInput("EPage", "Type in the Age Group (choosing from the list below)", age_group_labels),
               #  p(strong("Pull-down list of Age Groups:")),
                  uiOutput('EPage'),
                  checkboxInput('EPcheck_avg', '5-year Average'),
                  checkboxInput('EPcheck_spline', "Negative Binomial Regression"),
                  plotOutput('EPplot'),
                  downloadButton("EPplotdownload", label = "Download Excess mortality plot as PDF", class = "btn-sm")
                ) )
            )
          )
        ) ),
        div(id = "plottabhelp", class = "helper-btn", icon("question-circle", "fa-2x")),
        div(
          class = "helper-box", style = "display:none",
          p(
            "Use the plots to gain insight to the expected and excess deaths.",
            "Click the boxes on the left to get different models for the expected deaths and download a PDF of any of the plots."
          )
        ),
        actionLink("plotleft", icon = icon("arrow-left", class = "fa-2x"), label = NULL),
        actionLink("plotright", icon = icon("arrow-right", class = "fa-2x"), label = NULL)
      ),
# Expected Deaths --------------------------------------------------------------------

tabPanel(title='Expected Deaths',  value='tab4',

         #include progress bar when this tab is loading
         div(class = "busy",
             p("Calculation in progress... This might take a few minutes."),
             img(src="ajax-loader.gif")
         ),

  fluidRow(
    dataTableOutput("spline_table"),
##   DT::dataTableOutput("iris_table")

column(2,
    downloadButton('EDdownload', label = "Download data, including expected and excess deaths", class = "btn-sm"))

) ),

# Methods --------------------------------------------------------------------

tabPanel(title='Methods', value='tab5',
   column(6, style="padding: 0 30px 0 0;",
          div(id="methodsbox",
            p("This page has a description of the statistical methods used in the calculator to compute the expected and excess deaths in countries."),
            
            HTML("<br/>"),
            
            p(strong("All-cause mortality"),
              "is defined as the total number of recorded deaths across all causes."),
            
            p(strong("Excess death"),
              "is defined as the difference between the number of all-cause deaths during 2020-21 and the expected number of deaths."),
            
            p(strong("Expected death"), "is defined as the expected number of deaths in 2020-21 if no pandemic had occurred.",
              "The expected number of deaths is calculated in two different ways, using either a",
              strong("negative binomial regression"), "or the", strong("historical five year average"),
              ", both of which are based on the years 2015-2019."),
            
            HTML("<br/>"),

            p(strong(em("Negative-binomial regression"))),

            p("This particular negative-binomial regression model is a generalized additive model (GAM) in that it uses smoothing functions",
              "for the predictor variables. Since the date and period are input as discrete values, they are smoothed using cubic splines,",
              "a common smoothing technique."),

            p(strong(em("Historical 5-year average"))),

            p("The 5-years historical average is based on, and the 95% confidence interval (95% CI) are calculated",
              "from, the deaths observed in 2015-2019."),
            
            HTML("<br/>"),

            p("Below is a detailed description of the methods in statistical language.",
              "It is in PDF format and can be saved for separate study."),
          ),
           tags$iframe(style="height:600px; width:100%; scrolling=yes", 
                     src="ACMCalculator_Methodology_210407.pdf")
         )
         ),

tabPanel(title='Help and Resources', value='tab6',
         sidebarLayout(position = 'right',
                       sidebarPanel(
                         h5(tags$u('Resources')),
                         div(title = "Wiki page for the calculator",
                             a("About WPRO all-cause-of-mortality and excess death calculator",
                               href = "https://github.com/WorldHealthOrganization/ACMcalculator/wiki",
                               target = "_blank")),
          #              div(title="WPRO dashboard",
          #                  a("WPRO dashboard",
          #                    href = "https://who.maps.arcgis.com/apps/opsdashboard/index.html#/345dfdc82b5c4f6a815f1d54a05d18e", 
          #                    target = "_blank")
                         div(title="WPRO all-cause mortality dashboard",
                             a("WPRO all-cause mortality dashboard",
                               href="https://lynx.wpro.who.int/viz/allcausedeath.asp",
                               target="_blank")
                         ),
                         div(title=paste("Information on the methodology used",
                                         "in the calculator"),
                             a("About the methodology used in the tool.",
                               href = "https://github.com/WorldHealthOrganization/ACMcalculator/wiki/Methodology-used-in-ACMCalculator/", target = "_blank")
                         ),
                         br(),
                         div(a("WPRO all-cause-of-mortality and excess death calculator on GitHub", href="https://github.com/WorldHealthOrganization/ACMcalculator",
                               target="_blank")),
                         div(a("Shiny: a web application framework for R", href="http://shiny.rstudio.com/",
                               target="_blank"))
                       ),
                       mainPanel(
                         h5(tags$u('Help with the WPRO all-cause-of-mortality and excess death calculator')),
                         p("This app is maintained on GitHub. To request new features or report a bug,",
                           "please interact with the",
                           a("repository", href='https://github.com/WorldHealthOrganization/ACMcalculator',
                             target="_blank"),
                           "or email us at", 
                             a(actionButton(inputId = "email1", label = "duanm@who.int",
                               icon = icon("envelope", lib = "font-awesome")),
                               href="mailto:duanm@who.int"),
                           a(actionButton(inputId = "email2", label = "wproncovinfoplan@who.int", 
                                          icon = icon("envelope", lib = "font-awesome")),
                             href="mailto:wproncovinfoplan@who.int")),
                         
                         h5(tags$u('How to Install the app off-line')),
                         p("This app can be installed to run entirely on your local machine. Running",
                           "the app locally is the same as using this online version, except",
                           "that no internet connection is necessary and your data does not have to",
                           "be uploaded to an external location.",
                           "To install this app locally for off-line use, follow these simple steps."),
                         
                         p("1. Open the",strong("RGui")," app (You can search for it by name)."), 
                         p("Note: This is not the Rstudio app, which should not be used."),
                         p("2. In the",strong("R Console"), "window, cut-and-paste the following command:"),
                         p('source("https://faculty.stat.ucla.edu/handcock/ACMCalculator.R")'),
                         p("and hit Enter."),
                         p("3. The app will install itself and close RGui. This may take a few minutes. Once R is closed",
                           "go to your Documents directory and find the file named",
                           strong("ACMCalculator"),"."),
                         p("4. Double-click on the file, and the app will open within your default browser.",
                           "Note that although a web browser is being used to display the app,",
                           "no information is being sent over the internet and indeed",
                           "the app is operating offline. There may be a delay of a few seconds before the app appears.",
                           "If the app fails to open the",
                           "first time, simply close the browser and double-click the",
                           strong("ACMCalculator"), " file again."),
                         p("4. In the future, when you want to run the app, just double-click on the ",strong("ACMCalculator")," file and the app will open within your default browser.")
                         ))
         )


  )
)
