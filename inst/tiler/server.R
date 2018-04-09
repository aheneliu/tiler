server <- function(input, output, session) {

    ## --------------------------------------------------
    ## Define variables
    ## --------------------------------------------------
    ## The current x and y variables
    xvar <- NULL
    yvar <- NULL

    ## The brushed selection
    selection     <- list()
    selected_rows <- c()

    ## Points that can be selected
    selectable_points <- c()

    current_mode <- "explore" ## start in explore mode

    selected_points_brush        <- c()
    selected_points_tiles        <- c()
    selected_points_focus_tiles  <- c()
    selected_points_classes      <- c()


    ## The dataset
    dataset   <- NULL ## original
    dataset_1 <- NULL ## randomised 1
    dataset_2 <- NULL ## randomised 2

    ## The tilings
    tiling_1 <- NULL
    tiling_2 <- NULL

    tlist    <- NULL
    ilist    <- NULL
    flist    <- NULL

    thePlot <- NULL
    
    ## Column group id
    colgroup_df <<- NULL
    colgroups <<- NULL

    ## Colours
    col_original <- "#000000"
    col_1        <- "#00AA00"
    col_2        <- "#3477DB"

    ## Numeric columns in the dataset
    n_numeric_cols <- NULL

    select_options_class_df <- NULL

    ## Use different sets of focus tiles depending on the mode
    ## and switch between these
    tiles_focus <- list()
    tiles_focus$explore <- list("tiling_1" = list(), "tiling_2" = list())
    tiles_focus$focus   <- list("tiling_1" = list(), "tiling_2" = list())
    tiles_focus$compare <- list("tiling_1" = list(), "tiling_2" = list())

    ## --------------------------------------------------
    ## Reactive bindings
    ## --------------------------------------------------

    makeReactiveBinding("selected_rows")
    makeReactiveBinding("tiling_1")
    makeReactiveBinding("tiling_2")

    makeReactiveBinding("xvar")
    makeReactiveBinding("yvar")

    makeReactiveBinding("colgroup_df")
    makeReactiveBinding("colgroups")

    makeReactiveBinding("flist")

    makeReactiveBinding("selected_points_brush")
    makeReactiveBinding("selected_points_tiles")
    makeReactiveBinding("selected_points_focus_tiles")
    makeReactiveBinding("selected_points_classes")

    makeReactiveBinding("current_mode")

    ## --------------------------------------------------
    ## Some helper functions -- should be moved from here!
    ## --------------------------------------------------

    init_tilings <- function() {
        ## Create empty tilings, both having columnwise tiles
        tiling_1 <<- init_tiling(dim(dataset), columnwise_tiles = TRUE)
        tiling_2 <<- init_tiling(dim(dataset), columnwise_tiles = TRUE)

        ## Add the focus tiles to the explore mode -- these cannot be manipulated!
        tiles_focus[["explore"]][["tiling_1"]] <<- list(create_tile(rowset = seq.int(nrow(dataset)), colset = seq.int(ncol(dataset)), dims = dim(dataset)))

        ## No need to add focus tiles to tiling_2 in the explore mode, since the tiling is initialised with columnwise tiles
        set_mode("explore")
        rebuild_tilings()
        randomize_data()

        ## find the initially best pair of variables
        set_best_pair()
    }

    rebuild_tilings <- function() {
        tiling_1 <<- rebuild_tiling(tiling_1)
        tiling_2 <<- rebuild_tiling(tiling_2)
    }

    randomize_data <- function() {
        dataset_1 <<- permute_data(dataset, tiling_1)
        dataset_2 <<- permute_data(dataset, tiling_2)
    }

    set_best_pair <- function() {
        tmp <- find_best_variable_pair(dataset_1, dataset_2, method = "max_change_in_correlation", data = TRUE)

        xvar <<- tmp$names[1]
        yvar <<- tmp$names[2]

        updateSelectInput(session, "xvar", NULL, choices =  names(dataset), selected = xvar)
        updateSelectInput(session, "yvar", NULL, choices =  names(dataset), selected = yvar)
    }

    set_mode <- function(tool_mode) {
        tiling_1$tile_list_focus <<- tiles_focus[[tool_mode]][["tiling_1"]]
        tiling_2$tile_list_focus <<- tiles_focus[[tool_mode]][["tiling_2"]]

        rebuild_tilings()
    }

    s.seq.int <- function(x) {
        if (length(x) > 0)
            seq.int(length(x))
        else
            ""
    }
    clear_selection <- function() {
        session$resetBrush("selected_points")
        selection     <<- NULL
        selected_rows <<- NULL

        selected_points_brush   <<- c()
        selected_points_tiles   <<- c()
        selected_points_classes <<- c()

        ## clear tile and class selectors
        updateSelectInput(session, "selected_tiles", NULL, choices =  tlist, selected = NULL)
        updateSelectInput(session, "selected_classes", NULL, choices = select_options_class_df$key, selected = NULL)

    }

    add_tile_to_tilings <- function(target) {
        if ( (length(input$selected_columns) > 0) & (length(selected_rows) > 0)) {

            tile <- create_tile(rowset = selected_rows, colset = which(names(dataset) %in% input$selected_columns), dims = dim(dataset))

            tiling_1 <<- add_tile(tiling_1, tile, target = target)
            tiling_2 <<- add_tile(tiling_2, tile, target = target)

            clear_selection()

        } else {

            msg <- ""

            if (length(selected_rows) <= 0)
                msg <- paste0(msg, "No rows selected for the tile.<br/>")
            if (length(input$selected_columns) <= 0)
                msg <- paste0(msg, "No columns selected for the tile.")
            msg <- HTML(msg)

            showModal(modalDialog(
                title = "Error in tile creation",
                msg,
                easyClose = TRUE,
                footer = modalButton("OK"),
                fade = FALSE
            ))
        }
    }

    print_tiles <- function(tiling, target) {
        if (length(tiling[[target]]) > 0) {
            out <- ""

            for (i in seq.int(length(tiling[[target]]))) {
                tile <- tiling[[target]][[i]]
                colset <- attr(tile, "colset")
                out <- paste0(out, "<b>Tile ", i, "</b><br/>")
                out <- paste0(out, "<b>&nbsp;&nbsp;&nbsp; variables: </b>", paste0(names(dataset)[colset], collapse = ", "), "<br/>")
                out <- paste0(out, "<b>&nbsp;&nbsp;&nbsp; rows: </b>", length(attr(tile, "rowset")), "<br/>")
            }
        } else {
            out <- "<b>No tiles.</b>"
        }
        out
    }


    list_datasets <- function() {
        fnames <- list.files("data/", pattern = "*.rds")
        gsub(".rds", "", fnames)
    }


    read_dataset <- function(fname) {
        readRDS(file.path("data", paste0(fname, ".rds")))
    }

    
    summarize_selection <- function() {
        out <- ""


        if (length(selected_rows) > 0) {


            df <- dataset[selected_rows, sapply(dataset, is.factor), drop = FALSE]
            if (ncol(df) == 0)
                return("")

            for (cn in names(df))
                df[[cn]] <- paste0(cn,  ".", as.character(df[[cn]]))

            all_classes <- 100 * table(unlist(df)) / nrow(df)
            all_classes <- all_classes[order(all_classes, decreasing = TRUE)]
            all_classes <- all_classes[1:min(5, length(all_classes)), drop = FALSE]
            all_classes_df <- as.data.frame(all_classes)
            if (prod(dim(all_classes_df)) == 1) {
                all_classes_df <- data.frame("Variable.Class" = rownames(all_classes_df), "Percentage" = all_classes_df[[1]])
            } else {
                names(all_classes_df) <- c("Variable.Class", "Percentage")
            }

            out <- "<div id='selection_info_header'>Class membership of selected points (in %)</div>"

            out <- paste0(out, "<table class='infotable'>")
            out <- paste0(out, paste0("<tr class='headerrow'><td>", names(all_classes_df)[1], "</td><td>", names(all_classes_df)[2], "</td></tr>"))

            for (i in seq.int(nrow(all_classes_df)))
                out <- paste0(out, paste0("<tr><td class='infotablelabel'>", all_classes_df[i, 1], "</td><td>", sprintf("%.0f", all_classes_df[i, 2]), "</td></tr>"))

            out <- paste0(out, "</table>")

        }
        out
    }


    make_class_df <- function(classname, vals) {
        data.frame("class" = rep(classname, length(vals)), "variable" = vals, "key" = paste0(rep(classname, length(vals)), "-", vals))
    }


    ## --------------------------------------------------
    ## Update list of selectable datasets
    ## --------------------------------------------------

    updateSelectInput(session, "dataset_name", label = "Dataset", choices = list_datasets(), selected = "")

    ## --------------------------------------------------
    ## Load data
    ## --------------------------------------------------
    observeEvent(input$load_data, {
        if (input$dataset_name == "") {
            showModal(modalDialog(
                title = "Error loading dataset",
                "No dataset selected.",
                easyClose = TRUE,
                footer = modalButton("OK"),
                fade = FALSE
            ))
        } else {

            ## Reset data and tilings
            dataset   <<- NULL ## original
            dataset_1 <<- NULL ## randomised 1
            dataset_2 <<- NULL ## randomised 2

            tiling_1  <<- NULL
            tiling_2  <<- NULL

            selection     <<- list()
            selected_rows <<- c()

            xvar <<- NULL
            yvar <<- NULL

            ## Read the dataset
            dataset <<- read_dataset(input$dataset_name)

            n_numeric_cols <<- sum(sapply(dataset, is.numeric))

            ## Data table view of the data
            output$data_view <- DT::renderDataTable( DT::datatable(dataset, options = list(autoWidth = FALSE, scrollX = TRUE), width = "300") )

            ## The class variables
            class_variable_id <- which(sapply(dataset, is.factor))


            if (length(class_variable_id) > 0) {
                select_options_class_df <<- do.call(rbind, lapply(names(dataset)[class_variable_id], function(i) make_class_df(i, levels(dataset[[i]]))))
                updateSelectInput(session, "selected_classes", NULL, choices = select_options_class_df$key, selected = NULL)
            }

            ## Allow all points and all columns to be selectable initially
            updateSelectInput(session, "selected_columns", "Selected columns", choices = names(dataset), selected = c(xvar, yvar))

            ## Column group id
            colgroup_df <<- data.frame("id" = seq.int(ncol(dataset)), "variable" = colnames(dataset), "group" = seq.int(length(names(dataset))), stringsAsFactors = FALSE)
            colgroups   <<- tapply(colgroup_df$variable, colgroup_df$group, function(x) x)

            updateSelectInput(session, "select_column_group", NULL, choices =  names(dataset), selected = NULL)

            ## Start in Explore mode
            shinyjs::disable("add_focus")

            ## Initialise tilings
            init_tilings()
        }
    })


    ## --------------------------------------------------
    ## Tool mode
    ## --------------------------------------------------
    observeEvent(input$select_tool_mode, {

        if (input$tool_mode == "explore") {
            shinyjs::disable("add_focus")
            output$tool_mode_msg <- renderText({paste0("<ol><li>Select points</li> <li>Add tiles</li> <li>Update background</li> <li>request New View</li></ol>") })
        }

        if (input$tool_mode == "focus") {
            shinyjs::enable("add_focus")
            output$tool_mode_msg <- renderText({paste0("<ol><li>Select points</li> <li>Add tiles and Focus-tiles</li> <li>Update background</li> <li>request New View</li></ol>") })
        }

        if (input$tool_mode == "compare") {
            shinyjs::enable("add_focus")
            output$tool_mode_msg <- renderText({paste0("<ol><li>Select points</li> <li>Partition the columns in the focus area in the Column groups -tab before adding a focus tile</li> <li>Add tiles and Focus-tiles</li> <li>Update background</li> <li>request New View</li></ol>") })
        }

        current_mode <<- input$tool_mode
        set_mode(current_mode)

    })


    ## Summary of the variables in the data
    output$data_names <- renderText({
        input$load_data

        out <- paste0("<table>")
        for (i in seq.int(names(dataset))) {
            out <- paste0(out, "<tr><td><b>", i, ".&nbsp;&nbsp;&nbsp;</b></td><td>", " ", paste0(names(dataset)[i]), "</td></tr>")
        }
        out <- paste0(out, "</table>")
        out
    })



    ## --------------------------------------------------
    ## Selected variables
    ## --------------------------------------------------

    selected_variables <- reactive({
        input$load_data

        if (! is.null(input$xvar))
            xvar <<- input$xvar
        if (! is.null(input$yvar))
            yvar <<- input$yvar
    })

    observe({
        xvar
        yvar
        updateSelectInput(session, "selected_columns", "Selected columns", choices = names(dataset), selected = c(xvar, yvar))
    })


    ## --------------------------------------------------
    ## Plots
    ## --------------------------------------------------
    ## Primary plot
    output$main_plot_2 <- output$main_plot <- renderPlot({
                              selected_variables()
                              input$update_background

                              ## reset brush
                              session$resetBrush("selected_points")

                              alpha_data <- 1

                              ## --------------------------------------------------
                              ## fused dataset for drawing connected lines
                              ## between background 1 and 2
                              ## --------------------------------------------------
                              data_fuse <- cbind(dataset_1[, c(xvar, yvar)], dataset_2[, c(xvar, yvar)])
                              names(data_fuse) <- c("startx", "starty", "stopx", "stopy")

                              ## --------------------------------------------------
                              ## fused dataset for drawing connected lines
                              ## --------------------------------------------------

                              ## initialise plot
                              p <- ggplot(dataset, aes_string(x = xvar, y = yvar))


                              ## --------------------------------------------------
                              ## Draw lines connecting the data points
                              ## Highlight lines originating from brushed points
                              ## --------------------------------------------------

                              if (input$show_lines) {
                                  p <- p + geom_segment(data = data_fuse, aes(x = startx,y = starty, xend = stopx, yend = stopy), colour = "lightgray", alpha = 0.3)

                                  if (length(selected_rows) > 0) {
                                      p <- p + geom_segment(data = data_fuse[selected_rows,], aes(x = startx,y = starty, xend = stopx, yend = stopy), colour = "red", alpha = 0.1)
                                  }
                              }

                              ## --------------------------------------------------
                              ## original data
                              ## --------------------------------------------------

                              if (input$show_plot_original) {
                                  p <- p + geom_point(data = dataset, aes_string(x = xvar, y = yvar), colour = col_original, size = 2.3)
                              }


                              ## --------------------------------------------------
                              ## background 2
                              ## --------------------------------------------------

                              if (input$show_plot_background_2) {
                                  p <- p + geom_point(data = dataset_2, aes_string(x = xvar, y = yvar), colour = col_2, alpha = alpha_data)
                              }

                              ## --------------------------------------------------
                              ## background 1
                              ## --------------------------------------------------

                              if (input$show_plot_background_1) {
                                  p <- p + geom_point(data = dataset_1, aes_string(x = xvar, y = yvar), colour = col_1, alpha = alpha_data)
                              }
                              
                              ## --------------------------------------------------
                              ## Brushed points
                              ## --------------------------------------------------

                              if (any(input$show_plot_background_1, input$show_plot_background_2, input$show_plot_original)) {
                                  if (length(selected_rows) > 0) {
                                      p <- p + geom_point(data = dataset[selected_rows, ], colour = "red")
                                  }
                              }


                              ## --------------------------------------------------
                              ## Theme
                              ## --------------------------------------------------
                              p <- p + theme_bw()

                              thePlot <<- p
                              
                              p

                              ## --------------------------------------------------
                          })

    ## Secondary plot

    make_lpp <- function(colour_1, colour_2, colour_3 = "#510087", d, fd) {
        col_1 <- colour_1
        col_2 <- colour_2
        col_3 <- colour_3
        cdata <- d
        fuseddata <- fd

        return (
            function(x1, x2, ...) {
            format_label <- function(v) sprintf("%.2f", v)

            ## determine indices of the current plot
            i <- which(apply(fuseddata, 2, function(y) identical(x1, y)))
            j <- which(apply(fuseddata, 2, function(y) identical(x2, y)))
            
            usr <- par("usr"); on.exit(par(usr))
            par(usr = c(0, 1, 0, 1))

            rid <- which(colnames(cdata$c1) == cdata$top_k[i])
            cid <- which(colnames(cdata$c1) == cdata$top_k[j])

            text(x = 0.5, y = 0.8, format_label(cdata$c1[rid, cid]), cex = 2, col = col_1)
            text(x = 0.5, y = 0.6, format_label(cdata$c2[rid, cid]),  cex = 2, col = col_2)
            text(x = 0.5, y = 0.15, paste0("d = ", format_label(cdata$d[rid, cid])),  cex = 2, col = col_3)
        }
        )
    }


    output$pair_plot_2 <- output$pair_plot <- renderPlot({

                              selected_variables()
                              input$update_background

                              tmp <- find_top_k_variables(dataset_1, dataset_2, k = min(5, n_numeric_cols), data = TRUE)

                              fused_data <- rbind(dataset_2[, tmp$top_k], dataset_1[, tmp$top_k])
                              colour_vector <- c(rep(col_2, nrow(dataset_1)), rep(col_1, nrow(dataset_2)))

                              mylpp <- make_lpp(colour_1 = col_1, colour_2 = col_2, d = tmp, fd = fused_data)

                              pairs(fused_data, col = colour_vector, lower.panel = mylpp)
                          })

    ## --------------------------------------------------

    ## Number of selected rows
    output$n_selected_rows <- renderText({paste0("<b>Selected rows: </b>", length(selected_rows))})

    ## Number of tiles
    output$n_tiles <- renderText({paste0("<b>Tiles: </b>", length(tiling_1$tile_list_user))})

    ## Info on selection
    output$selection_info <- renderText({ summarize_selection() })


    ## --------------------------------------------------
    ## Main function for handling all selections
    ## --------------------------------------------------
    observe({
        selected_points_brush
        selected_points_tiles
        selected_points_focus_tiles
        selected_points_classes

        selected_rows <<- unique(c(selected_points_brush, selected_points_tiles, selected_points_focus_tiles, selected_points_classes))
    })

    ## --------------------------------------------------
    ## Handle brushing (selection of points in the plot)
    ## --------------------------------------------------
    observeEvent(input$selected_points, {
        selection <<- c(selection, list(input$selected_points))
        selected_points_brush <<- unique(unlist(lapply(selection, function(p) which(brushedPoints(dataset, p, allRows = TRUE)$selected))))
    })

    observeEvent(input$resetSelection, {
        session$resetBrush("selected_points")
        selection     <<- NULL
        selected_rows <<- NULL

        selected_points_brush   <<- c()
        selected_points_tiles   <<- c()
        selected_points_classes <<- c()

        ## clear tile and class selectors
        updateSelectInput(session, "selected_tiles", NULL, choices =  tlist, selected = NULL)
        updateSelectInput(session, "selected_classes", NULL, choices = select_options_class_df$key, selected = NULL)

    })

    observeEvent(input$selectAll, {
        selected_rows <<- seq.int(nrow(dataset))
    })

    observeEvent(input$selectInvert, {
        selected_rows <<- setdiff(seq.int(nrow(dataset)), selected_rows)
    })


    ## --------------------------------------------------
    ## Handle selection of classes and tiles
    ## --------------------------------------------------

    ## Select tile
    observe({input$selected_tiles
        if ((length(input$selected_tiles) > 0))
            selected_points_tiles <<- unique(unlist(sapply(input$selected_tiles, function(i) attr(tiling_1$tile_list_user[[as.numeric(i)]], "rowset"))))
        else
            selected_points_tiles <<- c()
    })

    ## Select focus tile
    observe({input$selected_focus_tiles
        if ((length(input$selected_focus_tiles) > 0))
            selected_points_focus_tiles <<- unique(unlist(sapply(input$selected_focus_tiles, function(i) attr(tiling_1$tile_list_focus[[as.numeric(i)]], "rowset"))))
        else
            selected_points_focus_tiles <<- c()
    })


    ## Select class
    observe({input$selected_classes
        if (length(input$selected_classes) > 0)
            selected_points_classes <<- unique(c(unlist(apply(subset(select_options_class_df, key %in% input$selected_classes), 1, function(i) which(dataset[, i[["class"]]] == i[["variable"]])))))
        else
            selected_points_classes <<- c()
    })


    ## Update dropdowns with available tiles
    observe({
        tiling_1

        tlist <<- s.seq.int(tiling_1$tile_list_user)
        flist <<- s.seq.int(tiling_1$tile_list_focus)

        updateSelectInput(session, "selected_tiles", NULL, choices = tlist, selected = "")
        updateSelectInput(session, "selected_focus_tiles", NULL, choices = flist, selected = "")

    })


    ## --------------------------------------------------



    ## --------------------------------------------------
    ## Add new tiles
    ## --------------------------------------------------

    observeEvent(input$add_tile, {
        add_tile_to_tilings(target = "tile_list_user")
    })


    observeEvent(input$add_focus, {
        ## add_tile_to_tilings(target = "tile_list_focus")
        ## In the focus and compare modes, create focus
        ## that spans all rows and all columns in the current focus tile
        ##
        ## Currently limit the number of selected focus regions to 1

        if ( (length(input$selected_columns) > 0) & (length(selected_rows) > 0)) {

            rs <- selected_rows
            cs <- which(names(dataset) %in% input$selected_columns)

            ## tiling 1 -- one big tile spanning the entire focus region
            tile_t1 <- create_tile(rowset = rs, colset = cs, dims = tiling_1$dims)

            ## tiling 2 -- columnwise tiles in the focus region according to the column grouping
            if (current_mode == "focus") {
                cgs <- as.list(cs)
            }

            if (current_mode == "compare") {
                cg_df <- colgroup_df[colgroup_df$id %in% cs,]
                cgs <- unname(as.list(tapply(cg_df$id, cg_df$group, function(x) x)))
            }

            tile_t2 <- create_columnwise_tiles(tiling_2, rowset = rs, colgroups = cgs)

            tiling_1$tile_list_focus <<- list(tile_t1)
            tiling_2$tile_list_focus <<- tile_t2
        }

        tiles_focus[[current_mode]][["tiling_1"]] <<- tiling_1$tile_list_focus
        tiles_focus[[current_mode]][["tiling_2"]] <<- tiling_2$tile_list_focus

        clear_selection()

        rebuild_tilings()

    })


    ## --------------------------------------------------
    ## Update the background distributions
    ## --------------------------------------------------

    observeEvent(input$update_background, {
        dataset_1 <<- permute_data(dataset, tiling_1)
        dataset_2 <<- permute_data(dataset, tiling_2)
    })

    ## --------------------------------------------------
    ## Get new view
    ## --------------------------------------------------
    observeEvent(input$update_view, {

        ## find the initially best pair of variables
        randomize_data()
        set_best_pair()

        updateSelectInput(session, "xvar", "X", choices = names(dataset), selected = xvar)
        updateSelectInput(session, "yvar", "Y", choices = names(dataset), selected = yvar)

        updateSelectInput(session, "selected_columns", "Selected columns", choices = names(dataset), selected = c(xvar, yvar))

    })

    ## --------------------------------------------------
    ## Save the view
    ## --------------------------------------------------
    output$download_plot <- downloadHandler(
        filename = "plot.pdf" ,
        content = function(file) {
        ggsave(file, plot = thePlot, device = "pdf", width = as.numeric(strsplit(input$imagesize, "x")[[1]][1]), height = as.numeric(strsplit(input$imagesize, "x")[[1]][2]), units = "mm")
    }
    )

    
    ## --------------------------------------------------
    ## Delete tiles
    ## --------------------------------------------------
    observeEvent(input$delete_tiles, {

        ## ------------------------------
        ## Delete user and focus tiles
        ## ------------------------------
        tile_ids_user   <- as.numeric(input$selected_tiles)
        tile_ids_focus  <- as.numeric(input$selected_focus_tiles)

        selected_points_tiles        <<- c()
        selected_points_focus_tiles  <<- c()

        updateSelectInput(session, "selected_tiles", NULL, choices =  setdiff(tlist, tile_ids_user), selected = NULL)
        updateSelectInput(session, "selected_focus_tiles", NULL, choices =  setdiff(flist, tile_ids_focus), selected = NULL)

        tmp1 <- tiling_1
        tmp2 <- tiling_2

        if (length(input$selected_tiles) > 0) {
            tmp1 <- remove_tiles(tmp1, tile_ids_user, target = "tile_list_user")
            tmp2 <- remove_tiles(tmp2, tile_ids_user, target = "tile_list_user")
        }

        if (length(input$selected_focus_tiles) > 0) {
            tmp1 <- remove_tiles(tmp1, tile_ids_focus, target = "tile_list_focus")
            tmp2 <- remove_tiles(tmp2, tile_ids_focus, target = "tile_list_focus")
        }

        shinyjs::delay(2000, tiling_1 <<- tmp1)
        shinyjs::delay(2000, tiling_2 <<- tmp2)
    })


    ## --------------------------------------------------
    ## Column groups
    ## --------------------------------------------------

    output$column_groups <- renderRHandsontable({
        if (! is.null(colgroup_df)) {
            rhandsontable(colgroup_df[order(colgroup_df$group), c("variable", "group")]) %>% hot_col("variable", readOnly = TRUE) %>% hot_col("group", readOnly = TRUE) %>% hot_col("group", format = "0") %>% hot_cols(columnSorting = TRUE)
        }
    })


    output$column_group_list <- renderText({
        if (! is.null(colgroup_df)) {
            out <- ""

            for (i in seq.int(length(colgroups))) {
                out <- paste0(out, "<b>Group ", i, "</b><br/>")
                out <- paste0(out, "&nbsp;&nbsp;&nbsp;", paste0(colgroups[[i]], collapse = ", "), "<br/>")
            }
            out
        }
    })


    ## Assign column group id
    observeEvent(input$assign_column_group_id, {
        if ((length(input$select_column_group) > 0) & (input$column_group_id != "")) {
            colgroup_df[(colgroup_df$variable %in% input$select_column_group), "group"] <<- as.numeric(input$column_group_id)
        }
        colgroups <<- tapply(colgroup_df$variable, colgroup_df$group, function(x) x)
    })


    ## --------------------------------------------------
    ## Tile lists
    ## --------------------------------------------------

    output$tile_list_user <- renderText({
        print_tiles(tiling_1, "tile_list_user")
    })

    output$tile_list_focus <- renderText({
        print_tiles(tiling_1, "tile_list_focus")
    })

    ## --------------------------------------------------

    
    ## --------------------------------------------------
    ## Stop shiny when the browser window is closed
    ## --------------------------------------------------
    
    session$onSessionEnded(function() {
                stopApp()
            })
}
