ui <- navbarPage("Tiler", theme = "bootstrap.css",
                 ## --------------------------------------------------
                 ## Data panel
                 ## --------------------------------------------------
                 tabPanel("Data", value = "panel_data",
                          sidebarLayout(
                              sidebarPanel(
                                  shinyjs::useShinyjs(), ## use shinyjs

                                  ## dataset
                                  HTML("<div class='ui_group_header_block'>Load dataset</div>"),
                                  div(style="display: inline-block;vertical-align:top; width: 300px;",
                                      selectInput('dataset_name_internal', 'Internal dataset', choices = NULL, selected = NULL)
                                      ),
                                  
                                  HTML("<div style='min-height: 10px'></div>"),
                                  fileInput("dataset_name_external", "External Dataset", accept = c(".Rds", ".rds", ".RDS")),

                                  HTML("<div class='separator'></div>"),
                                  
                                  actionButton("load_data", "Load dataset"),
                                  actionButton("clear_load", "Clear dataset selection"),

                                  HTML("<div style='min-height: 50px'></div>"),
                                  HTML("<div class='ui_group_header_block'>Variables in the data</div>"),
                                  htmlOutput("data_names")
                              ),

                              mainPanel(
                                  DT::dataTableOutput("data_view")
                              )
                          )
                          ),
                 ## --------------------------------------------------
                 ## Explore panel
                 ## --------------------------------------------------
                 tabPanel("Explore", value = "panel_explore" ,
                          sidebarLayout(
                              sidebarPanel(

                                  ## ------------------------------
                                  ## Mode
                                  ## ------------------------------
                                  HTML("<div class='ui_group'>"),
                                  HTML("<div class='ui_group_header'>Mode</div>"),
                                  div(style="display: inline-block;vertical-align:top; width: 200px;",
                                      selectInput('tool_mode', NULL, choices = c("explore", "focus", "compare"), selected = "explore")
                                      ),
                                  div(style="display: inline-block; vertical-align:top; width: 200px;",
                                      actionButton("select_tool_mode", "Change")
                                      ),
                                  htmlOutput("tool_mode_msg"),

                                  HTML("</div>"),

                                  ## ------------------------------
                                  ## Visualisation
                                  ## ------------------------------
                                  HTML("<div class='separator'></div>"),
                                  HTML("<div class='ui_group'>"),
                                  HTML("<div class='ui_group_header'>Visualisation</div>"),
                                  HTML("<div style='min-height: 15px; min-width: 50px; display: block'></div>"),
                                  div(style="display: inline-block;vertical-align:top; width: 20px;",
                                      HTML("<b>X</b>")
                                      ),
                                  div(style="display: inline-block;vertical-align:top; width: 200px;",
                                      selectInput('xvar', NULL, choices = NULL, selected = NULL)
                                      ),

                                  HTML("<div style='min-height: 5px; min-width: 50px; display: inline-block'></div>"),

                                  div(style="display: inline-block;vertical-align:top; width: 20px;",
                                      HTML("<b>Y</b>")
                                      ),

                                  div(style="display: inline-block;vertical-align:top; width: 200px;",
                                      selectInput('yvar', NULL, choices = NULL, selected = NULL)
                                      ),

                                  ## Checkboxes to mark which plots to show
                                  HTML("<div class='separator'></div>"),
                                  HTML("<b>Show samples</b>"),
                                  HTML("<div style='min-height: 5px'></div>"),
                                  div(id="label_plot_original", style="display: inline-block;vertical-align:top; width: 100px;",
                                      checkboxInput("show_plot_original", "Original", value = FALSE, width = "100px")
                                      ),
                                  div(id="label_plot_background_1", style="display: inline-block;vertical-align:top; width: 130px;",
                                      checkboxInput("show_plot_background_1", "Hypothesis 1", value = TRUE, width = "130px")
                                      ),
                                  div(id="label_plot_background_2", style="display: inline-block;vertical-align:top; width: 130px;",
                                      checkboxInput("show_plot_background_2", "Hypothesis 2", value = TRUE, width = "130px")
                                      ),
                                  div(style="display: inline-block;vertical-align:top; width: 100px;",
                                      checkboxInput("show_lines", "Lines", value = TRUE, width = "100px")
                                      ),

                                  HTML("</div>"),


                                  ## ------------------------------
                                  ## Data selection
                                  ## ------------------------------
                                  HTML("<div class='separator'></div>"),
                                  HTML("<div class='ui_group'>"),
                                  HTML("<div class='ui_group_header'>Select</div>"),

                                  ## Select tile or class
                                  HTML("<div style='display: inline-block; margin-left: 15px'></div>"),
                                  div(style="display: inline-block;vertical-align:top; width: 130px;",
                                      selectizeInput('selected_tiles', "Tiles", choices = NULL, selected = NULL, multiple = TRUE)
                                      ),
                                  HTML("<div style='display: inline-block; margin-left: 5px'></div>"),
                                  
                                  div(style="display: inline-block;vertical-align:top; width: 130px;",
                                      selectizeInput('selected_focus_tiles', "Focus-tiles", choices = NULL, selected = NULL, multiple = TRUE)
                                      ),


                                  HTML("<div style='display: inline-block; margin-left: 5px'></div>"),

                                  div(style="display: inline-block;vertical-align:top; width: 130px;",
                                      selectizeInput('selected_classes', "Classes", choices = NULL, selected = NULL, multiple = TRUE, options = list(" allowEmptyOption" = TRUE))
                                      ),


                                  HTML("<div style='min-height: 15px'></div>"),

                                  HTML("<div style='display: inline-block; margin-left: 110px'></div>"),

                                  div(style="display: inline-block;vertical-align:top; width: 160px;",
                                      htmlOutput("n_selected_rows")
                                      ),
                                  HTML("<div style='display: inline-block; margin-left: 10px'></div>"),

                                  div(style="display: inline-block;vertical-align:top; width: 250px;",
                                      selectizeInput('selected_columns', 'Selected columns', choices = NULL, selected = NULL, multiple = TRUE, width = "250")
                                      ),

                                  ## Select buttons
                                  HTML("<div style='display: inline-block; margin-left: 70px'></div>"),

                                  div(style="display: inline-block;vertical-align:top; width: 110px;",
                                      actionButton("selectAll", "All", width = "100px")
                                      ),
                                  div(style="display: inline-block;vertical-align:top; width: 110px;",
                                      actionButton("resetSelection", "None", width = "100px")
                                      ),

                                  div(style="display: inline-block;vertical-align:top; width: 110px;",
                                      actionButton("selectInvert", "Invert", width = "100px")
                                      ),


                                  HTML("</div>"),


                                  ## ------------------------------
                                  ## Tiles
                                  ## ------------------------------
                                  HTML("<div class='separator'></div>"),
                                  HTML("<div class='ui_group'>"),
                                  HTML("<div class='ui_group_header'>Tiles</div>"),

                                  actionButton("add_tile", "Add tile"),
                                  HTML("<div style='display: inline-block; margin-left: 5px'></div>"),
                                  actionButton("add_focus", "Add focus tile"),
                                  HTML("<div style='display: inline-block; margin-left: 5px'></div>"),
                                  actionButton("delete_tiles", "Delete selection", width = "150px"),

                                  div(style="display: inline-block;vertical-align:top;",
                                      htmlOutput("n_tiles")
                                      ),

                                  HTML("</div>"),


                                  ## ------------------------------
                                  ## Navigation
                                  ## ------------------------------
                                  HTML("<div class='separator'></div>"),
                                  HTML("<div class='ui_group'>"),
                                  HTML("<div class='ui_group_header'>Navigation</div>"),
                                  actionButton("update_background", "Update background"),
                                  actionButton("update_view", "New view"),
                                  downloadButton("download_plot", "Save view"),
                                  div(style="display: inline-block;vertical-align:top;",
                                      textInput("imagesize", NULL, value = "", width = 100, placeholder = "200x150")
                                      ),
                                  HTML("</div>"),

                                  ## ------------------------------
                                  ## Info on selection
                                  ## ------------------------------
                                  HTML("<div class='ui_group'>"),
                                  HTML("<div class='ui_group_header'>Selection info</div>"),
                                  htmlOutput("selection_info"),
                                  HTML("</div>")
                                  
                              ), ## end sidebarpanel

                              
                              mainPanel(
                                  plotOutput("main_plot",
                                             width = "100%",
                                             height = "700px",
                                             brush = brushOpts(id = "selected_points", delay = 5000)),

                                  div(style="display: inline-block;vertical-align:top; width: 100%; float:right",
                                  plotOutput("pair_plot", width = "100%", height = "600px")
                                      )



                              )
                          ) ## end -- sidebarlayout
                          ), ## end -- tabpanel
                 ## --------------------------------------------------
                 ## Tiles panel
                 ## --------------------------------------------------

                 tabPanel("Tiles", value = "panel_tiles",
                          sidebarLayout(
                              sidebarPanel(
                                  h3("Tiles"),
                                  htmlOutput("tile_list_user"),
                                  h3("Focus-tiles"),
                                  htmlOutput("tile_list_focus")
                              ),

                              mainPanel(
                                  plotOutput("main_plot_2",
                                             width = "100%",
                                             height = "700px")
                              )
                          )
                          ),

                 ## --------------------------------------------------
                 ## Column grouping panel
                 ## --------------------------------------------------

                 tabPanel("Column groups", value = "panel_groups",
                          sidebarLayout(
                              sidebarPanel(
                                  h3("Edit column groups"),
                                  HTML("Mark columns with the same number to assign them to the same group.<br/>"),

                                  div(style="display: inline-block;vertical-align:top; width: 200px;",
                                      selectizeInput('select_column_group', 'Selected columns', choices = NULL, selected = NULL, multiple = TRUE, width = "200")
                                      ),

                                  HTML("<div style='display: inline-block; margin-left: 20px'></div>"),
                                  
                                  div(style="display: inline-block;vertical-align:top; width: 200px;",
                                      textInput("column_group_id", "Column group ID", value = "", width = 200, placeholder = NULL)
                                      ),

                                  HTML("<div style='display: inline-block; margin-left: 20px'></div>"),
                                  
                                  div(style="display: inline-block;vertical-align:top; width: 50px;",
                                      actionButton("assign_column_group_id", "Assign")
                                     )

                              ),

                              mainPanel(
                                  div(style="display: inline-block;vertical-align:top; width: 400px;",
                                      rHandsontableOutput("column_groups", width = 350)
                                      ),
                                  div(style="display: inline-block;vertical-align:top; width: 400px;",
                                      htmlOutput("column_group_list")
                                      )
                              )
                          )
                          )


                 ## --------------------------------------------------
                 ) ## end of page
