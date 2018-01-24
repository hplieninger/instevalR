library(shiny)
library(shinyAce)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(shinyjs)
library(shinyBS)
library(instevalR)
library(DT)

shinyServer(function(input, output, session) {

    # observeEvent(input$lang, reset("topics"))

    # onclick("files", reset("sim"))

    # observe({
    #     if (!is.null(input$files)) {
    #         reset("sim")
    #     }
    # })

    ##### data #####

    dat_1 <- reactive({

        if (input$sim == TRUE) {
            shinyjs::disable("names")
            closeAlert(session, "welcome_alert")
            return(sim_eval(N_files = 3, N_id = 25, lang = input$lang))
        } else if (!is.null(input$files)) {
            closeAlert(session, "welcome_alert")

            shinyjs::enable("names")
            inFile <- input$files

            if (any(!tools::file_ext(inFile$datapath) %in% c("csv", "xls", "xlsx"))) {
                createAlert(session, "file_error", title = "Error",
                            content = "Please upload only files with extensions csv, xls, or xlsx.",
                            style = "danger")
                return(NULL)
            }

            x0 <- grepl(paste(c("kommentare", "fragen"), collapse = "|"),
                        x = inFile$name)

            inFile <- inFile[!x0, ]

            if (any(x0)) {
                createAlert(session, "file_error", title = "Error",
                            content = paste0("Ignoring the following files:<br>",
                                             paste0(input$files[x0, "name"], collapse = "<br>")),
                            style = "danger")
                # return(NULL)
            }

            tmp1 <- trimws(unlist(strsplit(input$names, ";")))
            tmp1 <- tmp1[!(tmp1 == "")]
            if (length(tmp1) == nrow(inFile)) {
                tmp_names <- tmp1
            } else if (all(grepl("^InstEvaL-Rohdaten-vlg", inFile$name))) {
                    tmp_names <- regmatches(inFile$name, regexpr("\\d+", inFile$name))
            } else {
                    tmp_names <- sub(".csv$|.xlsx$|.xls$", "", inFile$name)
            }

            tmp2 <- read_eval(inFile$datapath, names = tmp_names, shiny = TRUE)

            dat <- join_eval(tmp2, lang = input$lang
                             # , names = tmp_names
                             )
            return(dat)
        } else {
            createAlert(session, "welcome", "welcome_alert", title = "Welcome to Shiny InstevalR",
                        # content = "foo",
                        content = "<p style='font-size:110%;text-align:justify'> This shiny app can be used to plot several course evaluations from InstEvaL.</p>
                                   <p style='font-size:110%;text-align:justify'> First, <strong>please download the raw data of your course evaluations</strong>
                                      (log in to  <a href='https://insteval.uni-mannheim.de/'>InstEvaL</a> &rArr; Results &rArr; Raw data; the CSV-files have names ending with 'evaluationen.csv').
                                      Second, you can upload your files using the Browse-Button on the left.</p>
                                   <p style='font-size:110%;text-align:justify'> Alternatively, you may use random data using the 'Simulate Data'-checkbox on the left.</p>
                                   <p style='font-size:110%;text-align:justify'> Furthermore, instead of using this app, you may also use instevalR locally,
                                      which will give you even more control over the plot; see <a href='https://github.com/hplieninger/instevalR'>https://github.com/hplieninger/instevalR</a>.</p>",
                        style = "primary")
            return(NULL)
        }
    })

    output$topic_list <- renderUI({
        tmp1 <- levels(dat_1()$topic)
        selectizeInput("topics", strong("Topics to plot"),
                       choices = tmp1,
                       multiple = TRUE)
    })

    output$course_list <- renderUI({
        tmp1 <- levels(dat_1()$course)
        selectizeInput("courses", strong("Courses to plot"),
                       choices = tmp1,
                       multiple = TRUE)
    })

    ##### table #####

    output$table1 <- DT::renderDataTable({
        if (is.null(dat_1()))
            return(NULL)

        datatable_eval(dat_1())
        # tmp1 <- length(levels(dat_1()$course))
        # DT::datatable(dplyr::arrange(describe_eval(dat_1()), topic, course),
        #               options = list(lengthMenu = c(tmp1, 2*tmp1, 4*tmp1, 8*tmp1),
        #                              pageLength = tmp1,
        #                              autoWidth = F,
        #                              dom = "Bftlpr",
        #                              scrollX = T,
        #                              # fixedColumns = list(leftColumns = 2),
        #                              colReorder = TRUE,
        #                              # scrollY = T,
        #                              fixedHeader = TRUE,
        #                              # fixedColumns = list(leftColumns = 3),
        #                              # buttons = I("colvis"),
        #                              buttons = list(list(extend = "colvis",
        #                                                  columns = 3:11))),
        #               extensions = c("ColReorder",
        #                              # "FixedColumns",
        #                              "FixedHeader",
        #                              "Buttons"),
        #               rownames = FALSE, filter = "top") %>%
        #     DT::formatRound(c("Mean", "SD", "SE", "LL", "UL", "Q25", "Q50", "Q75")) %>%
        #     DT::formatStyle(c("course", "topic"), fontWeight = "bold")
    })

    ##### plot #####

    plot1 <- reactive({
        if (is.null(dat_1()))
            return(NULL)

        if (input$xaxis == "course") {
            course_as <- "x-axis"
            if (input$topic == "facet") {
                topic_as <- "facet"
            } else {
                topic_as <- "group"
            }
        } else if (input$xaxis == "topic") {
            topic_as <- "x-axis"
            if (input$course == "facet") {
                course_as <- "facet"
            } else {
                course_as <- "group"
            }
        }

        observe({
            if ("group" %in% c(course_as, topic_as)) {
                shinyjs::reset("xvar_as_legend")
                shinyjs::disable("xvar_as_legend")
            } else {
                shinyjs::enable("xvar_as_legend")
            }
        })

        observe({
            if (input$xvar_as_legend == TRUE) {
                shinyjs::disable("angle")
            } else {
                shinyjs::enable("angle")
            }
        })

        plot_eval(data = dat_1(),
                  plottype = input$plottype,
                  courses = input$courses,
                  errorbar = input$errorbar,
                  course_as = course_as,
                  topic_as  = topic_as,
                  domains = as.integer(input$domains),
                  topics = input$topics,
                  add_line = input$add_line,
                  lang = input$lang,
                  xvar_as_legend = input$xvar_as_legend
                  # control = list(angle = input$angle)
                  ) +
            ggthemes::theme_igray() +
            theme(axis.text.x = element_text(angle = input$angle, hjust = 1))
    })

    output$plot1 <- renderPlot({
        if (is.null(plot1()))
            return(NULL)
        plot1()
    })

    observe({
        if (input$plottype %in% c("boxplot", "violin") & input$errorbar == TRUE) {
            createAlert(session, "alert", "exampleAlert", title = NULL,
                        content = "Errorbars are not available for boxplot and violin plots.",
                        style = "info")

        } else {
            closeAlert(session, "exampleAlert")
        }
    })

    observe({
        if (input$xaxis == "course") {
            shinyjs::disable("course")
            shinyjs::enable("topic")
        } else if (input$xaxis == "topic") {
            shinyjs::enable("course")
            shinyjs::disable("topic")
        }
    })

    ##### download plot #####

    output$downPlot1 <- downloadHandler(
        filename =  function() {
            paste("Shiny InstevalR", input$var3, sep = ".")
        },
        content = function(file) {
            ht <- as.numeric(input$height)
            wd <- as.numeric(input$height)*as.numeric(input$ratio)
            if (input$var3 == "png")
                png(file, units = "in", width = wd, height = ht, res = 600)
            else
                cairo_pdf(file, width = wd, height = ht)
            print(plot1())
            dev.off()
        }
    )

    # info1  <- reactive({
    #     info1  <- paste("This analysis was performed on ", format(Sys.time(), "%d %b %Y at %H:%M:%S"), ".", sep = "")
    #     info2  <- paste(strsplit(R.version$version.string, " \\(")[[1]][1], " was used for this session.", sep = "")
    #     info3  <- paste("R packages used:")
    #     info4  <- paste("  shiny", packageVersion("shiny"))
    #     info5  <- paste("  ggplot2", packageVersion("ggplot2"))
    #     info6  <- paste("  ggthemes", packageVersion("ggthemes"))
    #     info7  <- paste("  magrittr", packageVersion("magrittr"))
    #     info8  <- paste("  shinyAce", packageVersion("shinyAce"))
    #     info9  <- paste("  shinythemes", packageVersion("shinythemes"))
    #     info10 <- paste("  shinyjs", packageVersion("shinyjs"))
    #     info11 <- paste("  shinyBS", packageVersion("shinyBS"))
    #
    #
    #     cat(sprintf(info1), "\n")
    #     cat(sprintf(info2), "\n")
    #     cat(sprintf(info3), "\n")
    #     cat(sprintf(info4), "\n")
    #     cat(sprintf(info5), "\n")
    #     cat(sprintf(info6), "\n")
    #     cat(sprintf(info7), "\n")
    #     cat(sprintf(info8), "\n")
    #     cat(sprintf(info9), "\n")
    #     cat(sprintf(info10), "\n")
    #     cat(sprintf(info11), "\n")
    #
    # })
    #
    # output$info1.out <- renderPrint({
    #     info1()
    # })
})
