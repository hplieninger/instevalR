library(shiny)
library(shinyAce)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(DT)

shinyUI(navbarPage(title = "Shiny InstevalR",
                   windowTitle = "Shiny InstevalR",
                   theme = shinytheme("yeti"),

    ##### App #####

    tabPanel("App",

             shinyjs::useShinyjs(),

        h2("Plot several InstEvaL course evaluations"),

        sidebarLayout(
            sidebarPanel(

                fileInput("files", "Upload CSV-File(s)",
                          multiple = TRUE,
                          accept = c(
                              # "text/csv",
                              # "text/comma-separated-values,text/plain",
                              ".csv",
                              ".xls",
                              ".xlsx")
                ),

                checkboxInput("sim", "Simulate Data", FALSE),

                uiOutput("course_list"),

                tags$hr(),

                selectInput("domains", label = strong("Domain to plot"),
                            choices = list("Overall" = 1,
                                           "Major scales" = 2,
                                           "Minor scales" = 3,
                                           "Settings" = 4,
                                           "Students presentations" = 5,
                                           "Misc" = 6),
                            selected = 2),

                uiOutput("topic_list"),

                tags$hr(),

                selectInput("plottype", label = strong("Type of plot"),
                            choices = list("Barplot" = "barplot",
                                           "Points"  = "points",
                                           "Boxplot" = "boxplot",
                                           "Violin plot" = "violin"),
                            selected = "barplot"),

                selectInput("xaxis", label = strong("Variable on the x-axis"),
                            choices = list("Course" = "course",
                                           "Topic"  = "topic"),
                            selected = "course"),

                selectInput("topic", label = strong("Show topics in"),
                            choices = list("Panels" = "facet",
                                           "Colors" = "group"),
                            selected = "facet"),

                selectInput("course", label = strong("Show courses in"),
                            choices = list("Panels" = "facet",
                                           "Colors" = "group"),
                            selected = "facet"),

                checkboxInput("errorbar", strong("Show errorbars"), TRUE),

                checkboxInput("add_line", strong("Add connecting lines"), FALSE),

                checkboxInput("xvar_as_legend", strong("Show additional legend"), FALSE),

                radioButtons("lang", strong("Language"),
                             choices = list("German" = "de",
                                            "English" = "en"),
                             selected = "de", inline = TRUE),

                textInput("names", label = strong("Course labels"), value = NULL,
                          placeholder = "label 1; label 2; label 3"),

                sliderInput("angle",
                            strong("Tick labels' angle"),
                            0, 360, 30, 1),

                tags$hr(),

                h5("Plot specifications for download"),

                radioButtons(inputId = "var3", label = strong("Select the file type"), choices = list("pdf", "png"),
                             selected = "pdf", inline = TRUE),

                # sliderInput("width",
                #             strong("Plot width (in inch)"),
                #             1, 15, 8, .1),

                sliderInput("height",
                            strong("Plot height (in inch)"),
                            1, 15, 5, .1),

                selectInput("ratio", label = strong("Aspect ratio"),
                            choices = list("2.414:1" = "2.414",
                                           "1.618:1" = "1.618",
                                           "3:2" = "1.5",
                                           "4:3" = "1.333333",
                                           "1:1" = "1",
                                           "3:4" = "0.75",
                                           "2:3" = "0.6666667",
                                           "1:1.618" = "0.618047",
                                           "1:2.414" = "0.4142502"),
                            selected = "1.618")
            ),

            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", br(),
                                     bsAlert("file_error"),
                                     bsAlert("welcome"),
                                     plotOutput("plot1"),
                                     br(),
                                     bsAlert("alert"),
                                     br(),

                                     downloadButton("downPlot1", "Download current plot")
                                     ),
                            tabPanel("Table", br(),
                                     DT::dataTableOutput("table1")
                                     )
                            )
            )
        )
    ),

    # ABOUT -------------------------------------------------------------------

    tabPanel("About", icon = icon("dot-circle-o"),

        strong("Shiny InstevalR"),

        p("InstEvaL is an online system for course evaluations, which is used at",
          "some German universities (e.g., U Mannheim).",
          a("InstEvaL", href="https://insteval.uni-mannheim.de/", target="_blank"),
          "provides instructors with feedback for every course that is evaluated",
          "and with the accompanying raw data. Users may upload their raw data to",
          "Shiny InstevalR to produce a handful of interesting plots."),
        p("This app was developed using",
          a("Shiny.", href="http://shiny.rstudio.com/", target="_blank")),
        p("Feel free to contact the developer, Hansjörg Plieninger, at",
          a("GitHub.", href="https://github.com/hplieninger/instevalR/issues", target="_blank"),
          "or at plieninger[a]uni-mannheim.de for questions, comments, bug reports, or feature requests."),

        strong("instevalR"),

        p("Shiny InstevalR is part of the R package", strong("instevalR"), "that is available from",
          a("GitHub.", href="https://github.com/hplieninger/instevalR", target="_blank"),
          "You may use this package to run instevalR locally and to fine-tune the plots."),

        # br(),
        #
        # strong("List of Packages Used"),
        #
        # code("library(shiny)"),br(),
        # code("library(shinyAce)"),br(),

        # br(),
        #
        # strong("Acknowledgments"),

        # br(),
        #
        # strong("Authors"),

        # br(),
        # strong("Bug Reports"),

        strong("License"),

        p("Shiny InstevalR: Plotting several InstEvaL course evaluations"),

        p("Copyright (C) 2018 Hansjörg Plieninger"),

        p("This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version."),

        p("This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details."),

        p("You should have received a copy of the GNU General Public License along with this program. If not, see", a("http://www.gnu.org/licenses/gpl.html", href="http://www.gnu.org/licenses/gpl.html", target="_blank")),

        p("A simple explanation may be found at:" , a("https://www.tldrlegal.com/l/gpl-3.0", href="https://tldrlegal.com/license/gnu-general-public-license-v3-(gpl-3)", target="_blank"))

        # br(),
        #
        # strong("R session info"),
        #
        # verbatimTextOutput("info1.out")

        )
    ))
