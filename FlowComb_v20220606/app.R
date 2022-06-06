
# launch from command line: R -e "shiny::runApp('//central-cluster/algroup/Orian Bricard/210326_ORB609_Combinatorial_flow_data_analysis/vFJSA-0.1',  launch.browser = TRUE)"

required_packages = c( "shiny", "shinyFiles", "shinyWidgets", "shinybusy", "htmlwidgets", "ggplot2", "gridExtra", "scales", "dplyr", "gtools", "ggrepel")

for (req.package in required_packages){
    if(!requireNamespace(req.package, quietly=TRUE)){
      install.packages(req.package, repos='http://cran.us.r-project.org')
    }
  }


library(shiny)
library(shinyFiles)
library(shinyWidgets)
library(shinybusy)
library(htmlwidgets)
library(ggplot2)
library(gridExtra)
library(scales)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
options(shiny.maxRequestSize=1000*1024^2)
library(gtools)
library(ggrepel)


### PrmOI for Parameter of Interest
### PopOI for Population of Interest



## FlowJo plugin needs to set full path of samples_and_groups.csv file with flowjo_samples_and_groups_path



## Shiny app


# UI --------------------
ui <- navbarPage("FlowComb",
                 
 # Files selection and group label =========================

    tabPanel("Files selection",
             
             h3("Select files for each group and label them"),
             
             hr(),
             fluidRow(column(1),
                      column(2, h3("Reference group")),
                      column(3, uiOutput("refgroup_name_ui")),
                      column(1),
                      column(5, fileInput("refgroup_files",  label = tags$span(
                        "csv files", 
                        tags$i(
                          class = "fa fa-info-circle", 
                          style = "color:#0072B2;font-size: 12px",
                          title = "0-1023 scale\nexpected format from FlowJo population export with csv channel values option"
                        )), multiple = TRUE, accept = c(".csv")))),
             fluidRow(column(1), tableOutput("refgroup_contents")),
             
             hr(),
             
             fluidRow(column(1),
                      column(2, h3("Second group"),),
                      column(3, uiOutput("group2_name_ui")),
                      column(1),
                      column(5, fileInput("group2_files",  label = tags$span(
                        "csv files", 
                        tags$i(
                          class = "fa fa-info-circle", 
                          style = "color:#0072B2;font-size: 12px",
                          title = "0-1023 scale\nexpected format from FlowJo population export with csv channel values option"
                        )), multiple = TRUE, accept = c(".csv")))),
             
             fluidRow(column(1), tableOutput("group2_contents")),
             
             hr(),
             
             h3("or provide samples and groups file"),
             
             fluidRow(column(1),
                      column(1),
                      column(5, fileInput("samples_and_groups_file",  label = tags$span(
                        "csv file", 
                        tags$i(
                          class = "fa fa-info-circle", 
                          style = "color:#0072B2;font-size: 12px",
                          title = "header should contain name, group and datapath"
                        )), multiple = FALSE, accept = c(".csv")))),

             hr(),
             
             h3("or load from FlowJo"),
             fluidRow(column(3, actionButton("load_from_FlowJo", "load from FlowJo", width = "100%"), offset = 2)),
             
             hr(),
             
             
             fluidRow(column(4, uiOutput("import_data_buttonUI"),offset =4))
    
             ),
  
 # Parameters of interest panel =========================

    tabPanel("Parameters of interest",
             fluidRow(column(10,  h3("Select parameters of interest",tags$i(
               class = "fa fa-info-circle", 
               style = "color:#0072B2;font-size: 14px",
               title = "generally exclude parameters that have been used to define the total populations (e.g. viability, CD45)"
             )))),
             hr(),
             fluidRow(column(2, actionButton("Deselect_all_button", "Deselect All", width = "100%"), offset = 1)),
             fluidRow(column(2, uiOutput("PrmOIs_selector"),offset = 1), 
                      column(1), 
                      column(4, h4(textOutput("PrmOIs_count")),  style="padding:50px;"))
             ),

 # Threshold setting panel =========================

    tabPanel("Threshold setting",
             fluidRow(column(10, h3("Adjust positive population threshold for each parameter",  tags$i(
               class = "fa fa-info-circle", 
               style = "color:#0072B2;font-size: 12px",
               title = "click on a graph to setup threshold value for the x-axis parameter"
             )))),
             hr(),
             fluidRow(column(1),
                      column(2, h4("Threshold guide file (optional)",  tags$i(
                        class = "fa fa-info-circle", 
                        style = "color:#0072B2;font-size: 12px",
                        title = "use this option when the thresholds are easier to determine on a sample or cell population that you don't want to include in the analysis"
                      ))), column(3, fileInput("threshold_guide_file",  label = tags$span(
                        "csv file", 
                        tags$i(
                          class = "fa fa-info-circle", 
                          style = "color:#0072B2;font-size: 12px",
                          title = "0-1023 scale\nexpected format from FlowJo population export with csv channel values option"
                        )), multiple = FALSE, accept = c(".csv"))), 
                      column(2, h4("Import thresholds from file")), column(3, fileInput("imported_threshold_file",  label =  tags$span(
                        "thresholds.csv file", 
                        tags$i(
                          class = "fa fa-info-circle", 
                          style = "color:#0072B2;font-size: 12px",
                          title = "should contains parameter,number_of_pop,value,second_value columns\nautomatically generated when a FlowComb analysis has been performed"
                        )), multiple = FALSE, accept = c("thresholds.csv")))),
             
             fluidRow(column(6), 
                      column(3, uiOutput("number_of_pop_selector"))
                       ),
             fluidRow(column(1), column(2, uiOutput("threshold_plot_yvar_selector")) ),
             fluidRow(column(1),
                      column(2, uiOutput("PrmOI_threshold_selector")),
                      column(1),
                      column(6, plotOutput("ref_threshold_plot", click="ref_threshold_plot_click",  height="250px"),
                                plotOutput("group2_threshold_plot", click="group2_threshold_plot_click", height="250px"),
                                conditionalPanel(condition = "is.null(input$threshold_guide_file)",
                                      plotOutput("guide_threshold_plot", click="guide_threshold_plot_click" , height="250px")),
                                downloadButton("save_threshold", label = "Save thresholds"))
                     )
             ),
 
 # Analysis panel =========================
    tabPanel("Analysis",
             fluidRow(column(10, h3("Adjust analysis parameters"))),
             hr(),
             fluidRow(column(1), column(3,h4("Define populations of interest (optional)",  tags$i(
               class = "fa fa-info-circle", 
               style = "color:#0072B2;font-size: 12px",
               title = "A populations of interest will be analysed as an additional parameter\nThis option enable you to get deeper in the analysis from the combination of parameters defining the population of interest\nWith this option outputs are also generally easier to analyse because more annotated "
             )))),
             fluidRow(column(1),
                      column(1, textInput("PopOI_label", label = "Name")),
                      column(1, uiOutput("PopOI_1_ui")),
                      column(1, uiOutput("PopOI_2_ui")),
                      column(1, uiOutput("PopOI_3_ui")),
                      column(1, uiOutput("PopOI_4_ui")),
                      column(1, uiOutput("PopOI_5_ui")),
                      column(1, uiOutput("PopOI_6_ui")), 
                      column(1, actionButton("PopOI_add", "add", width = "100%",style = "margin-top: 25px;")),
                      column(1, actionButton("PopOI_remove", "remove last", width = "100%",style = "margin-top: 25px;")),
                      column(2, fileInput("loadPopOIs",  label = tags$span("Load PopOIs.csv file", 
                            tags$i(
                            class = "fa fa-info-circle", 
                            style = "color:#0072B2;font-size: 12px",
                            title = "should contains name and	parameters_combination columns"
                            ), multiple = FALSE, accept = c(".csv"))))
                    ),
             fluidRow(column(3),
                      column(6, tableOutput("PopOIs_table"))),
             hr(),
             fluidRow(column(1),
                      column(3, selectInput("max_combination_level", label = h4("Max combination level",  tags$i(
                        class = "fa fa-info-circle", 
                        style = "color:#0072B2;font-size: 12px",
                        title = "The combination level correspond to the number of parameters combined to define a population"
                      )), 
                         choices = list("1" = 1,"2" = 2, "3" = 3), 
                         selected = 3,
                         width = "50%")),
                      column(2, radioGroupButtons("test_type", label = h4("Statistical test"), choices = c("t.test", "glmer"))),
                      column(3, numericInput("min_parentcell_count_for_test", label = h4("Minimum mean cell count in the less populated group"),
                         value = 20,
                         width = "70%")),
                      column(3, numericInput("min_subpopcell_count_for_test", label = h4("Minimum mean subpop cell count in the group with the most populated subpop"),
                                             value = 10,
                                             width = "80%")), 
                      
                      ),
             hr(),
             fluidRow(column(1),
                      column(2, checkboxInput("saveRDS_checkbox", label = "Save analyis in:", value = TRUE))),
             fluidRow(column(1),
                      column(6, verbatimTextOutput("output_dir_txt")),
                      column(2, shinyDirButton("output_dir_selector", "Browse...", "Select location for FlowComb ouputs") )
                      ),
            # fluidRow( verbatimTextOutput("env_infos")),
             hr(),
             br(),br(),br(),
             fluidRow(column(3, offset = 5, actionButton("run", h4("Run analysis"))), 
                      column(3, fileInput("loadRDS",  label = tags$span("Load analysis RDS file", 
                             tags$i(
                             class = "fa fa-info-circle", 
                             style = "color:#0072B2;font-size: 12px",
                             title = "Use this option when you want to look at a previous analysis or run it again with different settings"
                             )), multiple = FALSE, accept = c(".rds")))),
      ),


 # Results panel =========================

 tabPanel("Results",
          
          fluidRow(column(2, h3("Frequency denominator", 
                          tags$i(
                          class = "fa fa-info-circle", style = "color:#0072B2;font-size: 12px",
                          title = 'All: the frequencies are related to all cells considered in the analysis\nParent: the frequencies are related to the cell population defined by the combination without the last parameter (e.g. A+B-C+ => A+B-) ' 
                          ))),
                   column(3, radioGroupButtons("freq_denominator", label = "", choices = c(" All ", "Parent"))), actionButton("browserbutton", "browser")),
          
          # Volcano plot ################################
          fluidRow(column(6,
                          h3("Volcano plot", 
                             tags$i(
                               class = "fa fa-info-circle", style = "color:#0072B2;font-size: 12px",
                               title = "Click on a point to label it and/or use left side options"
                             )), 
                          hr(),
                          fluidRow(column(1, downloadLink("export_volcano", label = "export"),offset = 10), 
                                   column(1,
                                          dropdownButton(
                                            tags$h3("export parameters"),
                                            numericInput("volc_width", label = "width", value = 150),
                                            numericInput("volc_height", label = "height", value = 130), 
                                            selectInput("volc_format", label = "format", 
                                                        choices = c("png",  "pdf"), 
                                                        selected = "png"),
                                            circle = FALSE, status = "default",
                                            icon = shiny::icon("cog"),
                                            tooltip = tooltipOptions(title = "export parameters")
                                          ))
                                    ),
                          column(3,
                                 checkboxGroupInput("level_selector", label = tags$span("Combination level", 
                                       tags$i(
                                       class = "fa fa-info-circle", style = "color:#0072B2;font-size: 12px",
                                       title = "Unselect combination level 3 if graph updating is to slow"
                                       )), 1:3, selected = 1:2, inline = TRUE,),
                                 fluidRow(column(9, strong("Restrict to L1HHM containing pops")), column(3,checkboxInput("restrict_volcano_to_selected_L1HHM", label = "", value = FALSE))),
                                 hr(),
                                 actionButton("click_reset",label = "reset click selection"),
                                 hr(),
                                 fluidRow(column(6, h5("Label top")),
                                          column(1, checkboxInput("label_top_checkbox", label = "", value = FALSE)),
                                          column(1,
                                                 dropdownButton(
                                                   tags$h3("Top list parameters"),
                                                   numericInput("labeltop", label = "label top", value = 5, min = 1),
                                                   numericInput("alpha", label = "alpha", value = 0.05), 
                                                   numericInput("mfc", label = "minimum fold change", value = 2),
                                                   selectInput("direction", label = "direction", 
                                                               choices = c("both",  "positive only",  "negative only"), 
                                                               selected = "both"),
                                                   circle = FALSE, status = "default",
                                                   icon = shiny::icon("cog"), width = "300px",
                                                   tooltip = tooltipOptions(title = "Top list parameters")
                                                 )
                                          )
                                 ),
                                 fluidRow(column(6, h5("Label level 1")), 
                                          column(1, checkboxInput("label_level1_checkbox", label = "", value = FALSE))),
                                 fluidRow(column(6, h5("Label HHM pop")), 
                                          column(1, checkboxInput("label_HHM_checkbox", label = "", value = TRUE))),
                                 hr(),
                                 fluidRow(column(8, h5("Dot size as mean of group total freq")),
                                          column(1, checkboxInput("dot_size_freq_checkbox", label = "", value = FALSE)))
                          ),
                          column(9, plotOutput("Freq_volcano", click ="Freq_volcano_click"))
          ),
          
          # Bar plot ################################ 
          column(6, h3("Bar plot", tags$i(
            class = "fa fa-info-circle", style = "color:#0072B2;font-size: 12px",
            title = "Display population labelled on the volcano plot\nIf you want more details about one population comparission click on the bar"
          )),
                 hr(),
                 fluidRow(column(1, downloadLink("export_barplot", label = "export"),offset = 10), 
                          column(1, uiOutput("barplot_dropdown" ))
                 ),
                 
                 column(11,plotOutput("Barplot", click ="Barplot_click"),offset = 1),
                 column(11,tableOutput("barplot_info"),offset = 1)
          )
          ),
          
          # Hierarcheatmap #############################    
          fluidRow(column(6, h3("HierarcHeatMap",tags$i(
            class = "fa fa-info-circle", style = "color:#0072B2;font-size: 12px",
            title = "Click on the number associate to a population to identify it on the volcano plot (with Label HHM pop option) and display dot plots showing the values distribution on this parameter "
          )),
                          hr(),
                          fluidRow(
                            column(3, uiOutput("HHM_stat_picker_ui")), 
                            column(1),
                            column(2, h5("sort by side first")),
                            column(1, checkboxInput("sort_by_side_first", label = "", value = FALSE)),
                            column(3),
                            column(1, downloadLink("export_HHM", label = "export")), 
                            column(1, uiOutput("HHM_dropdown" ))),
                          
                          column(12,  
                                 column(4, plotOutput("L1_Hieracheatmap", click ="L1_Hieracheatmap_click",  width = "100%",height = "100%")),
                                 column(4, plotOutput("L2_Hieracheatmap", click ="L2_Hieracheatmap_click",  width = "100%", height = "100%")),
                                 column(4, plotOutput("L3_Hieracheatmap", click ="L3_Hieracheatmap_click",  width = "100%", height = "100%"))
                          ) 
                          
          ),
          # Dot plot #######################
          column(6, h3("Dot plots",tags$i(
            class = "fa fa-info-circle", style = "color:#0072B2;font-size: 12px",
            title = "Show the values distribution of the last parameter of the combination defining the last population clicked on the HierarcHeatMap"
          )),
                 hr(),
                 fluidRow(column(2, h5("Display dotplots", tags$i(
                   class = "fa fa-info-circle", style = "color:#0072B2;font-size: 12px",
                   title = "unselect this option if the dataset contains lots of cell and graph updating is to slow"
                 )), offset = 1), 
                          column (1, checkboxInput("display_dotplots", label = "", value = TRUE)),

                          column(2, h5("y-axis: contrast parameter")),
                          column(2, uiOutput("dotplots_yvar_selector")),
                          column(2),
                          column(1, downloadLink("export_dotplots", label = "export")),
                          column(1, uiOutput("dotplots_dropdown" ))),
                 fluidRow(column(12, plotOutput("Dotplots"))) 
                 
                 )
          )
          

          #   ####################
 ),

)
 # ====================


# Server --------------------
server <- function(input, output, session) {

  
 reactV<- reactiveValues()
   
 # Files selection and group label  =========================

  reactV$import_button_status = "hidden"
  reactV$samples_and_groups_file_status = "not_provided"
  reactV$groups_names = c("refgroup", "group2")
  
  # Group name UI  ##############
  
  output$refgroup_name_ui <- renderUI({
    textInput("refgroup_name", label = "Name", value = reactV$groups_names[1]) })
  
  output$group2_name_ui <- renderUI({
    textInput("group2_name", label = "Name", value = reactV$groups_names[2]) })
  
  


  # Show import button if csv from the 2 groups provided ###########  

  observeEvent(c(input$refgroup_files,input$group2_files), {
    if( !(is.null(input$refgroup_files)|is.null(input$group2_files))){
      reactV$import_button_status = "totrigger"
    }
    
  })
  
  # Load from samples_and_groups_file file ######
  
  observeEvent(input$samples_and_groups_file, {
    reactV$perGroup_files_df = read.csv(input$samples_and_groups_file$datapath)
    reactV$groups_names = unique(reactV$perGroup_files_df$group)
    reactV$import_button_status = "totrigger"
    reactV$samples_and_groups_file_status = "provided"
  })
  
  # Load from FlowJo ######
  
  observeEvent(input$load_from_FlowJo, {
    flowjo_samples_and_groups_path = "\\\\central-cluster\\algroup\\Orian Bricard\\210326_ORB609_Combinatorial_flow_data_analysis\\vFJSA-0.2\\FlowComb_202206050947\\samples_and_groups_202206050947.csv"
    if(is.null(flowjo_samples_and_groups_path) == FALSE)
    reactV$perGroup_files_df = read.csv(flowjo_samples_and_groups_path)
    reactV$groups_names = unique(reactV$perGroup_files_df$group)
    reactV$import_button_status = "totrigger"
    reactV$samples_and_groups_file_status = "provided"
    
  })
  
 

  # Output files names ############### 

    output$refgroup_contents <- renderTable({
      if(reactV$samples_and_groups_file_status == "provided"){
        df = data.frame(files = reactV$perGroup_files_df[reactV$perGroup_files_df$group == reactV$groups_names[1],c("name")])
      }else{df = data.frame(files = input$refgroup_files[,c("name")])}
      df
     })
      
    output$group2_contents <- renderTable({
      if(reactV$samples_and_groups_file_status == "provided"){
        df = data.frame(files = reactV$perGroup_files_df[reactV$perGroup_files_df$group == reactV$groups_names[2],c("name")])
      }else{df = data.frame(files = input$group2_files[,c("name")])}
      df
    })    
 

  # Import data button  ##########
    
    output$import_data_buttonUI <- renderUI({
      if(reactV$import_button_status == "hidden")
        return(NULL)
      if(reactV$import_button_status == "totrigger"){return(actionBttn("import_data_button", h4("Click to import flow data"), style = "simple", color = "danger" ))} 
      if(reactV$import_button_status == "triggered"){return(actionBttn("import_data_button", h4("Flow data imported"), style = "simple",  color = "success" ))}
      if(reactV$import_button_status == "namestoupdate"){return(actionBttn("import_data_button", h4("Click to update names"), style = "simple", color = "danger" ))}
      if(reactV$import_button_status == "nameschanged"){return(actionBttn("import_data_button", h4("Names updated"), style = "simple",  color = "success" ))}
      
      })

  # Import data with import data button  ##########
    observeEvent(input$import_data_button, {
      
      show_modal_spinner(spin = "fading-circle")
      
      if(reactV$import_button_status == "totrigger"){
        
        reactV$groups_names = c(input$refgroup_name,input$group2_name)
        if(reactV$samples_and_groups_file_status == "provided"){
        perGroup_files_df = reactV$perGroup_files_df
        }else{
        perGroup_files_list = list(input$refgroup_files,input$group2_files)
        perGroup_files_list = perGroup_files_list[!sapply(perGroup_files_list,is.null)] #remove NULL from list
        perGroup_files_df = do.call(rbind, perGroup_files_list)
        if(length(perGroup_files_list)>0){perGroup_files_df$group = rep(reactV$groups_names, sapply(perGroup_files_list,nrow))}
        reactV$perGroup_files_df = perGroup_files_df
        }
        channel_values_matrix = do.call(rbind, lapply(perGroup_files_df$datapath, read.csv)) # 0-1023 scale ; expected format from FlowJo export population with csv channel values option
        perSample_cellcount = sapply(lapply(perGroup_files_df$datapath, read.csv),nrow)
        rowData = data.frame(group = rep(perGroup_files_df$group, perSample_cellcount),
                             sample = rep(sub(".csv$", "",perGroup_files_df$name),  perSample_cellcount))
        colData = data.frame(original_matrix_colnames = colnames(channel_values_matrix)) 
        
        if(sum(grepl( "\\.\\.\\.\\.", colData$original_matrix_colnames ))){ # signal_origin :: Id ; expected format from FlowJo export population with csv include header "Both" stain and parameter
        colData$signal_origin =  gsub("\\.\\.\\.\\..*", "",  colData$original_matrix_colnames)
        colData$parameterId =  gsub(".*\\.\\.\\.\\.", "",  colData$original_matrix_colnames)
        colData$is_labeled_parameter = !(colData$parameterId == colData$original_matrix_colnames)
        }else{ # for compatibility for non formatted header
          colData$parameterId = colData$original_matrix_colnames
          colData$is_labeled_parameter = !(colData$parameterId %in% c("FSC.A", "FSC.H","FSC.W", "SSC.A", "SSC.H", "SSC.W", "SSC.B.A","SSC.B.H","SSC.B.W", "Time", "Comp.AF.A", "AF.A"))
        }
        colData$parameterId = gsub(" |_|-|\\+|@", ".", colData$parameterId)  ## neutralize characters used as tag in the script 

        colnames(channel_values_matrix) = colData$parameterId
        
        reactV$channel_values_matrix = channel_values_matrix
        reactV$colData = colData
        reactV$rowData = rowData
        reactV$labeled_parameters = sort(colData$parameterId[colData$is_labeled_parameter])
        reactV$preselect = reactV$labeled_parameters
        
        reactV$import_button_status = "triggered"
      }
      
      if(reactV$import_button_status == "namestoupdate"){
        previous_names = reactV$groups_names
        reactV$groups_names = c(input$refgroup_name,input$group2_name)
        reactV$test_res$group_comparison = paste(reactV$groups_names[2], "vs", reactV$groups_names[1])
        reactV$perGroup_files_df$group[reactV$perGroup_files_df$group == previous_names[1]] = reactV$groups_names[1]
        reactV$perGroup_files_df$group[reactV$perGroup_files_df$group == previous_names[2]] = reactV$groups_names[2]
        reactV$perGroup_hpopstat$group[reactV$perGroup_hpopstat$group == previous_names[1]] = reactV$groups_names[1]
        reactV$perGroup_hpopstat$group[reactV$perGroup_hpopstat$group == previous_names[2]] = reactV$groups_names[2]
        reactV$perSample_hpopstat$group[reactV$perSample_hpopstat$group == previous_names[1]] = reactV$groups_names[1]
        reactV$perSample_hpopstat$group[reactV$perSample_hpopstat$group == previous_names[2]] = reactV$groups_names[2]
        reactV$rowData$group[reactV$rowData$group == previous_names[1]] = reactV$groups_names[1]
        reactV$rowData$group[reactV$rowData$group == previous_names[2]] = reactV$groups_names[2]
        
        reactV$import_button_status = "nameschanged"
      }
    
      remove_modal_spinner()
     })
    
    
    ## Update group names if changed after import
    
    observeEvent(c(input$refgroup_name,input$group2_name), {
      if(reactV$import_button_status == "triggered" & !all(reactV$groups_names == c(input$refgroup_name,input$group2_name)) ){
        report_warning("Need to update", "click on below button") 
        reactV$import_button_status = "namestoupdate"
      }
    })
   

    
    
  
# Variables of interest =================================

    reactV$preselect = c()
      
    observeEvent(input$Deselect_all_button,{
      reactV$preselect = c()
    })
    
    
    
    
    
   # Output dynamic group of buttons to select parameters of interest (PrmOIs) ####################  
    output$PrmOIs_selector <- renderUI({
      if(is.null(reactV$labeled_parameters))
        return(NULL)
      checkboxGroupButtons("PrmOIs", label = "", 
                         choices = reactV$labeled_parameters ,
                         justified = TRUE,
                         selected = reactV$preselect,
                         direction = "vertical",
                         checkIcon = list(yes = icon("ok",lib = "glyphicon"))
                         )
    })
    

  # Get PrmOIs from input #################
    observeEvent(input$PrmOIs,{
      reactV$PrmOIs = input$PrmOIs
      reactV$selectedPrmOI = reactV$PrmOIs[1] # initialization
    })
    
  # Output PrmOIs count ###################
    output$PrmOIs_count <- renderText({
      if(is.null(reactV$PrmOIs))
        return(NULL)
      paste(length(reactV$PrmOIs), "parameters selected")
    })
  
  
    
    
# Threshold setting =================================

  # Output nb of population selector ###########
    
    output$number_of_pop_selector <- renderUI({
      if(is.null(reactV$thresholds) | is.null(input$Selected_PrmOI_for_threshold))
        return(NULL)
      if(nrow(reactV$thresholds)> 0){
        if(input$Selected_PrmOI_for_threshold %in% reactV$thresholds$parameter){
          prettyRadioButtons( inputId = "number_of_pop_selector",
                              label = "", 
                              choices = c("2 populations" = 2, "3 populations" = 3),
                              selected = reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"number_of_pop"],
                              inline = TRUE)
        }else{
          prettyRadioButtons( inputId = "number_of_pop_selector",
                              label = "", 
                              choices = c("2 populations" = 2, "3 populations" = 3),
                              selected = "2",
                              inline = TRUE)
        }
      }else{
        prettyRadioButtons( inputId = "number_of_pop_selector",
                            label = "", 
                            choices = c("2 populations" = 2, "3 populations" = 3),
                            selected = "2",
                            inline = TRUE)
      }

    })
    
  # Update threshold nb of population #####
    observeEvent(input$number_of_pop_selector, {
      if(is.null(reactV$thresholds) == FALSE){
        if(nrow(reactV$thresholds)> 0){
          if(input$Selected_PrmOI_for_threshold %in% reactV$thresholds$parameter){
            reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"number_of_pop"] = input$number_of_pop_selector
            if(input$number_of_pop_selector == "2" ){
              reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"] = NA
              
            }
          }
        }
      }
      
    })
  
    
    
  # Output PrmOI selector for threshold ###############
    
    
    output$PrmOI_threshold_selector <- renderUI({
      if(is.null(reactV$labeled_parameters))
        return(NULL)

      PrmOIs = reactV$PrmOIs
      
      choiceNames_list = list()
      for(i in 1:length(PrmOIs)){
        if(PrmOIs[i] %in% reactV$thresholds$parameter){
          choiceNames_list[[i]] =  tags$span(style = "color:#28b78d", PrmOIs[i])
        }else { choiceNames_list[[i]] =  tags$span(style = "color:black", PrmOIs[i])}
      }

      
      radioGroupButtons("Selected_PrmOI_for_threshold", label = "x-axis: threshold parameter", 
                        choiceNames = choiceNames_list,
                        choiceValues = PrmOIs,
                        justified = TRUE,
                        direction = "vertical", 
                        selected = reactV$selectedPrmOI
      )
    })
    
    observeEvent(input$Selected_PrmOI_for_threshold, {
      reactV$selectedPrmOI = input$Selected_PrmOI_for_threshold
    }
    )
 
  
  # Output threshold_plot_yvar ###############
     
    output$threshold_plot_yvar_selector<- renderUI({
      if(is.null(reactV$labeled_parameters))
        return(NULL)
      selectInput("threshold_plot_yvar", label = tags$span("y-axis: contrast parameter", tags$i(
        class = "fa fa-info-circle", 
        style = "color:#0072B2;font-size: 12px",
        title = "change this parameter to better discriminate the x-axis populations"
      )),  choices = sort(colnames(reactV$channel_values_matrix) ), selected = "SSC.A")
    })
    
    
    
    

    
  # Output threshold plots ###############
    
    plot_threshold_plot <- function(df, title){
      xvar = colnames(df)[1]
      yvar = colnames(df)[2]
      xscale = c(0,1024)
      yscale = c(0,1024)
      
      g = ggplot(df, aes_string(x = xvar, y = yvar) )+
        ggtitle(title)+
        scale_x_continuous(limits = xscale)+  #labels =  math_format(10^.x) , breaks = seq(xscale[1],xscale[2]) 
        scale_y_continuous(limits = yscale)+
        geom_bin2d( aes_string(x = xvar, y = yvar), binwidth = c(2, 5)) 
        
        if(max(ggplot_build(g)$data[[1]]$count) == 1){
          g = g +scale_fill_gradientn(trans = "log10", colours="#0000FF")
        }else{
          g = g +scale_fill_gradientn(trans = "log10", colours=rev(rainbow(10, end = 4/6))) 
        } 
      
      g = g +
        geom_vline(xintercept = reactV$thresholds[reactV$thresholds$parameter == xvar,"value"], size = 1)+
        geom_hline(yintercept = reactV$thresholds[reactV$thresholds$parameter == yvar,"value"], size = 0.5)+
        theme_bw() +
        theme(plot.title = element_text(face="bold"), legend.position = "none")
      
      if(nrow(reactV$thresholds)> 0){
        if(xvar %in% reactV$thresholds$parameter){
          if(is.na( reactV$thresholds[reactV$thresholds$parameter == xvar,"second_value"]) == FALSE){
            g = g + geom_vline(xintercept = reactV$thresholds[reactV$thresholds$parameter == xvar,"second_value"], size = 1, color = "red")
          }
        }
        if(yvar %in% reactV$thresholds$parameter){
          if(is.na( reactV$thresholds[reactV$thresholds$parameter == yvar,"second_value"]) == FALSE){
            g = g + geom_hline(yintercept = reactV$thresholds[reactV$thresholds$parameter == yvar,"second_value"], size = 0.5, color = "red")
          }
        }
      }

      g
    }
    
    plot_threshold_plot_per_group <- function(group){
      xvar = input$Selected_PrmOI_for_threshold
      yvar = input$threshold_plot_yvar
      df = as.data.frame(reactV$channel_values_matrix[reactV$rowData$group == group,c(xvar,yvar)])
      plot_threshold_plot(df,paste(group, "merged samples"))
    }
    
    output$ref_threshold_plot <- renderPlot({
      if(is.null(input$Selected_PrmOI_for_threshold))  
        return(NULL)
      plot_threshold_plot_per_group(reactV$groups_names[1])
    })
    output$group2_threshold_plot <- renderPlot({
      if(is.null(input$Selected_PrmOI_for_threshold))
        return(NULL)
      plot_threshold_plot_per_group(reactV$groups_names[2])
    })
    
    
  # Generate threshold guide file per event flow data #####################
    guidefile_channel_values_matrix <- reactive({   #data from threshold guide file
      if(is.null(input$threshold_guide_file))
        return(NULL)
      threshold_guide_file = input$threshold_guide_file
      df = read.csv(input$threshold_guide_file[1,"datapath"])
      colnames(df) = gsub(".*\\.\\.\\.\\.", "",  colnames(df))
      df$filename = input$threshold_guide_file[1,"name"]
      df
    })
    
  # Output guide threshold plots ###############
    
    output$guide_threshold_plot <- renderPlot({
      if(is.null(input$Selected_PrmOI_for_threshold) | is.null(input$threshold_guide_file))
        return(NULL)
      xvar = input$Selected_PrmOI_for_threshold
      yvar = input$threshold_plot_yvar
      df = guidefile_channel_values_matrix()[,c(xvar,yvar)]
      plot_threshold_plot(df,paste("Threshold guide file:",gsub(".csv", "",df[1,"filename"])))
    })
    
    
    
    
  # Get threshold from threshold plots click ##################
    reactV$thresholds = data.frame(parameter = character(),  number_of_pop = numeric(), value = numeric(), second_value = numeric())
    
    
    observeEvent(input$guide_threshold_plot_click, {
      if(input$number_of_pop_selector == "2"){
        if(input$Selected_PrmOI_for_threshold %in% reactV$thresholds$parameter){
          reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] = input$guide_threshold_plot_click$x 
        } else{
          new_row = data.frame(parameter = input$Selected_PrmOI_for_threshold, number_of_pop = 2, value = input$guide_threshold_plot_click$x,second_value = NA  )
          reactV$thresholds = rbind(reactV$thresholds,new_row)
        }        
      }
      
      if(input$number_of_pop_selector == "3"){
        if(input$Selected_PrmOI_for_threshold %in% reactV$thresholds$parameter){
          if(is.na(reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"])){
            if(input$guide_threshold_plot_click$x > reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] ){
              reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"] = input$guide_threshold_plot_click$x
            } else{
              reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"] = reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"]
              reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] = input$guide_threshold_plot_click$x
            }
          } else {
            reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"] = NA
            reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] = input$guide_threshold_plot_click$x
          }
          
        } else{
          new_row = data.frame(parameter = input$Selected_PrmOI_for_threshold, number_of_pop = 3, value = input$guide_threshold_plot_click$x,second_value = NA  )
          reactV$thresholds = rbind(reactV$thresholds,new_row)
        }        
      }
      
    })

    
    
    
    observeEvent(input$ref_threshold_plot_click, {

      if(input$number_of_pop_selector == "2"){
        if(input$Selected_PrmOI_for_threshold %in% reactV$thresholds$parameter){
          reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] = input$ref_threshold_plot_click$x 
        } else{
          new_row = data.frame(parameter = input$Selected_PrmOI_for_threshold, number_of_pop = 2, value = input$ref_threshold_plot_click$x,second_value = NA  )
          reactV$thresholds = rbind(reactV$thresholds,new_row)
        }        
      }
      
      if(input$number_of_pop_selector == "3"){
        if(input$Selected_PrmOI_for_threshold %in% reactV$thresholds$parameter){
          if(is.na(reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"])){
            if(input$ref_threshold_plot_click$x > reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] ){
              reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"] = input$ref_threshold_plot_click$x
            } else{
              reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"] = reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"]
              reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] = input$ref_threshold_plot_click$x
            }
          } else {
            reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"] = NA
            reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] = input$ref_threshold_plot_click$x
          }

        } else{
          new_row = data.frame(parameter = input$Selected_PrmOI_for_threshold, number_of_pop = 3, value = input$ref_threshold_plot_click$x,second_value = NA  )
          reactV$thresholds = rbind(reactV$thresholds,new_row)
        }        
      }
      


    })
    observeEvent(input$group2_threshold_plot_click, {
      
      if(input$number_of_pop_selector == "2"){
        if(input$Selected_PrmOI_for_threshold %in% reactV$thresholds$parameter){
          reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] = input$group2_threshold_plot_click$x 
        } else{
          new_row = data.frame(parameter = input$Selected_PrmOI_for_threshold, number_of_pop = 2, value = input$group2_threshold_plot_click$x,second_value = NA  )
          reactV$thresholds = rbind(reactV$thresholds,new_row)
        }        
      }
      
      if(input$number_of_pop_selector == "3"){
        if(input$Selected_PrmOI_for_threshold %in% reactV$thresholds$parameter){
          if(is.na(reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"])){
            if(input$group2_threshold_plot_click$x > reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] ){
              reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"] = input$group2_threshold_plot_click$x
            } else{
              reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"] = reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"]
              reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] = input$group2_threshold_plot_click$x
            }
          } else {
            reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"second_value"] = NA
            reactV$thresholds[reactV$thresholds$parameter == input$Selected_PrmOI_for_threshold,"value"] = input$group2_threshold_plot_click$x
          }
          
        } else{
          new_row = data.frame(parameter = input$Selected_PrmOI_for_threshold, number_of_pop = 3, value = input$group2_threshold_plot_click$x,second_value = NA  )
          reactV$thresholds = rbind(reactV$thresholds,new_row)
        }        
      }
    })
  

    
    
  # Import thresholds #####################
    
      observeEvent(input$imported_threshold_file, {
         reactV$thresholds = read.csv(input$imported_threshold_file$datapath)
        }
       )  
    
    
    
  # Save thresholds ######
    
    output$save_threshold <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_thresholds.csv")
      },
      content = function(con) {
        write.csv(reactV$thresholds, con, row.names = FALSE)
      }
    )
  
 
    
    
# Analysis =================================
  # Get max combination level from input ################  
    max_combination_level <- reactive({input$max_combination_level})

  # Popfilter selection #########  
    output$Popfilter_1_ui <- renderUI({

      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))

      selectInput("Popfilter_1", label = "Select", choices = choices_list, selected = "None")
    })
    output$Popfilter_2_ui <- renderUI({
      if(is.null(input$Popfilter_1))  
        return(NULL)
      if(input$Popfilter_1 == "None")  
        return(NULL)

      
      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_1) ]
      
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_1) ]
      
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))
      

      selectInput("Popfilter_2", label = "and", choices = choices_list, selected = "None")
    })
    output$Popfilter_3_ui <- renderUI({
      if(is.null(input$Popfilter_2))  
        return(NULL)
      if(input$Popfilter_2 == "None")  
        return(NULL)

      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_1) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_2) ]
      
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_1) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_2) ]
      
    
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))
      
      selectInput("Popfilter_3", label = "and", choices = choices_list, selected = "None")
    })
    output$Popfilter_4_ui <- renderUI({
      if(is.null(input$Popfilter_3))  
        return(NULL)
      if(input$Popfilter_3 == "None")  
        return(NULL)
      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_1) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_2) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_3) ]
      
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_1) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_2) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_3) ]
      
      
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))
      
      selectInput("Popfilter_4", label = "and", choices = choices_list, selected = "None")
    })
    output$Popfilter_5_ui <- renderUI({
      if(is.null(input$Popfilter_4))  
        return(NULL)
      if(input$Popfilter_4 == "None")  
        return(NULL)
      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_1) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_2) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_3) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_4) ]
      
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_1) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_2) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_3) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_4) ]
      
      
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))
      
      selectInput("Popfilter_5", label = "and", choices = choices_list, selected = "None")
    })
    output$Popfilter_6_ui <- renderUI({
      if(is.null(input$Popfilter_5))  
        return(NULL)
      if(input$Popfilter_5 == "None")  
        return(NULL)
      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_1) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_2) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_3) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_4) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$Popfilter_5) ]
      
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_1) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_2) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_3) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_4) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$Popfilter_5) ]
      
      
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))
      
      selectInput("Popfilter_6", label = "and", choices = choices_list, selected = "None")
    })
  
  # Population of interest selection #####
    
    output$PopOI_1_ui <- renderUI({
      
      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))
      
      selectInput("PopOI_1", label = "Select", choices = choices_list, selected = "None")
    })
    output$PopOI_2_ui <- renderUI({
      if(is.null(input$PopOI_1))  
        return(NULL)
      if(input$PopOI_1 == "None")  
        return(NULL)
      
      
      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_1) ]
      
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_1) ]
      
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))
      
      
      selectInput("PopOI_2", label = "and", choices = choices_list, selected = "None")
    })
    output$PopOI_3_ui <- renderUI({
      if(is.null(input$PopOI_2))  
        return(NULL)
      if(input$PopOI_2 == "None")  
        return(NULL)
      
      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_1) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_2) ]
      
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_1) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_2) ]
      
      
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))
      
      selectInput("PopOI_3", label = "and", choices = choices_list, selected = "None")
    })
    output$PopOI_4_ui <- renderUI({
      if(is.null(input$PopOI_3))  
        return(NULL)
      if(input$PopOI_3 == "None")  
        return(NULL)
      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_1) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_2) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_3) ]
      
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_1) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_2) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_3) ]
      
      
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))
      
      selectInput("PopOI_4", label = "and", choices = choices_list, selected = "None")
    })
    output$PopOI_5_ui <- renderUI({
      if(is.null(input$PopOI_4))  
        return(NULL)
      if(input$PopOI_4 == "None")  
        return(NULL)
      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_1) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_2) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_3) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_4) ]
      
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_1) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_2) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_3) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_4) ]
      
      
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))
      
      selectInput("PopOI_5", label = "and", choices = choices_list, selected = "None")
    })
    output$PopOI_6_ui <- renderUI({
      if(is.null(input$PopOI_5))  
        return(NULL)
      if(input$PopOI_5 == "None")  
        return(NULL)
      two_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 2]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_1) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_2) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_3) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_4) ]
      two_pop_prm = two_pop_prm[two_pop_prm !=  gsub("[-+]", "", input$PopOI_5) ]
      
      three_pop_prm = reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_1) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_2) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_3) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_4) ]
      three_pop_prm = three_pop_prm[three_pop_prm !=  gsub("[-+]", "", input$PopOI_5) ]
      
      
      choices_list = as.list(c("None",sort(c(paste0(two_pop_prm,"-"), paste0(two_pop_prm,"+"), paste0(three_pop_prm,"-"), paste0(three_pop_prm,"+"), paste0(three_pop_prm,"++") ))))
      
      selectInput("PopOI_6", label = "and", choices = choices_list, selected = "None")
    })
    
    reactV$PopOIs = data.frame(name = character(), parameters_combination = character())
    
    observeEvent(input$PopOI_add, {
      pc = paste(input$PopOI_1,input$PopOI_2,input$PopOI_3,input$PopOI_4,input$PopOI_5, sep = "_" )
      pc = gsub("_None.*", "",pc)
      new_line = data.frame(name = paste0("@", input$PopOI_label), parameters_combination = pc )
      reactV$PopOIs  = rbind(reactV$PopOIs , new_line)
    })
    
    observeEvent(input$PopOI_remove, {
      reactV$PopOIs  = reactV$PopOIs[-nrow(reactV$PopOIs),]
    })
    
    output$PopOIs_table = renderTable({
      if(nrow(reactV$PopOIs) == 0)
        return(NULL)
      reactV$PopOIs})
    
    observeEvent(input$loadPopOIs, {
      reactV$PopOIs = read.csv(input$loadPopOIs$datapath)
    })
    
    reactV$output_dir = getwd()
    volumes = c(getVolumes()(), "Current working directory" = getwd())
    
    shinyDirChoose( input, id = "output_dir_selector", roots = volumes)

    observeEvent(input$output_dir_selector,{
      reactV$output_dir = parseDirPath(volumes, input$output_dir_selector)
    })
    
    output$output_dir_txt <- renderText({ reactV$output_dir})
    
    output$env_infos = renderText({ c(R.home(), .libPaths())})
  

    
    
  # Run ############
    
    observeEvent(input$run, {
      
        if(all(reactV$PrmOIs %in% reactV$thresholds$parameter) == FALSE ){ ## check that there is a threshold set for each parameter of interest
          report_warning("Threshold(s) missing", paste("Set threshold for:", paste(reactV$PrmOIs[!(reactV$PrmOIs %in% reactV$thresholds$parameter)], collapse = " ")) )
          }else{ 
            #if(is.null(input$PrmOIs)){ reactV$analyis_message = "Select data first"
            if( sum(is.na(reactV$thresholds$second_value[reactV$thresholds$number_of_pop == 3])) ){
              report_warning("2nd threshold(s) missing", paste("Select 2 populations or set 2nd threshold for:", paste( reactV$thresholds$parameter[reactV$thresholds$number_of_pop == 3 & is.na(reactV$thresholds$second_value)], collapse = ",")) )
            }else{
           
          if(is.null(reactV$channel_values_matrix)){
            report_warning("No data provided", "Provide csv files and follow tabs in order or load a previous analysis with a analysis.rds")
            return(NULL)
          }
              

          show_modal_spinner(spin = "fading-circle")  
          
          time = format(Sys.time(), "%Y%m%d%H%M")
          outdir=paste0("FlowComb_", time)
          dir.create(outdir)
          update_modal_spinner(text = paste0("result stored in ", getwd(), "/", outdir))
          
          reactV$thresholds = reactV$thresholds[!grepl("@",reactV$thresholds$parameter),] #erase previous populations of interest
            
          write.csv(reactV$perGroup_files_df, file = paste0("./", outdir, "/", "samples_and_groups_", time, ".csv")  , row.names = FALSE )
          write.csv(reactV$thresholds, file = paste0("./", outdir, "/", "thresholds_", time, ".csv") , row.names = FALSE) 
          write.csv(reactV$PopOIs, file = paste0("./", outdir, "/", "PopOIs_", time, ".csv") , row.names = FALSE) 

          number_of_pops = setNames(reactV$thresholds$number_of_pop, reactV$thresholds$parameter)
          thresholds = setNames(reactV$thresholds$value, reactV$thresholds$parameter)
          thresholds2 = setNames(reactV$thresholds$second_value, reactV$thresholds$parameter)
          
          PrmOIs = reactV$PrmOIs
          channel_values_matrix = reactV$channel_values_matrix
          channel_values_matrix = channel_values_matrix[,!grepl("@",colnames(channel_values_matrix))] #erase previous populations of interest
          L1_pops = c(paste0(names(number_of_pops), "-"), paste0(names(number_of_pops), "+"))
          if(sum(number_of_pops == "3") > 0){L1_pops = c(L1_pops,paste0(names(number_of_pops[number_of_pops == "3"]), "++") ) }
          
          colData = reactV$colData
          rowData = reactV$rowData
          
          
          # Generate aboveThreshold matrix ################
          update_modal_spinner(text = "Generate aboveThreshold matrix")
          
          thresholds2[is.na(thresholds2)] = 1025 #set NA as above max 
          
          aboveThreshold_matrix  = t(apply(channel_values_matrix[,names(thresholds)],1,function(x) x>thresholds ))*1
          aboveThreshold2_matrix = t(apply(channel_values_matrix[,names(thresholds)],1,function(x) x>thresholds2 ))*1
          aboveThresholdsum_matrix = aboveThreshold_matrix  + aboveThreshold2_matrix
          
          reactV$aboveThresholdsum_matrix = aboveThresholdsum_matrix

          
          # Generate L1_pops_belonging_matrix matrix ########

          L1_pops_belonging_matrix = matrix( rep(0,length(L1_pops)*dim(aboveThresholdsum_matrix)[1]), ncol = length(L1_pops))
          colnames(L1_pops_belonging_matrix) = L1_pops
          for(L1_pop in L1_pops){
            parameter = gsub("[-+]", "",L1_pop) 
            aboveThresholdvalue = as.numeric(gsub("11", 2, gsub("\\+", 1,gsub("-",0,gsub("[^-+]", "",L1_pop)))))
            if(aboveThresholdvalue == 0){L1_pops_belonging_matrix[,L1_pop] = as.numeric(aboveThresholdsum_matrix[,parameter] == 0)}
            if(aboveThresholdvalue == 1){L1_pops_belonging_matrix[,L1_pop] = as.numeric(aboveThresholdsum_matrix[,parameter] == 1)}
            if(aboveThresholdvalue == 2){L1_pops_belonging_matrix[,L1_pop] = as.numeric(aboveThresholdsum_matrix[,parameter] == 2)}
          }
          reactV$L1_pops_belonging_matrix = L1_pops_belonging_matrix
          
          
          # Add PopOIs as parameter #####
          update_modal_spinner(text = "Add PopOIs as parameter")

          popFinder <- function(pop){  
            popv = strsplit(pop, split = "_")[[1]]
            if(length(popv) == 1){return(L1_pops_belonging_matrix[,popv] == length(popv))}
            return(rowSums(L1_pops_belonging_matrix[,popv]) == length(popv))
          } 

          if(nrow(reactV$PopOIs) > 0){ 
            PopOI_matrix = apply(reactV$PopOIs,1,function(x) popFinder(x["parameters_combination"]))
            colnames(PopOI_matrix) = reactV$PopOIs$name
            L1_pops = c(L1_pops, paste0(colnames(PopOI_matrix),"+"))
            aboveThresholdsum_matrix = cbind(aboveThresholdsum_matrix, PopOI_matrix)
            reactV$L1_pops_belonging_matrix = L1_pops_belonging_matrix
            reactV$aboveThresholdsum_matrix = aboveThresholdsum_matrix
            reactV$channel_values_matrix = cbind(channel_values_matrix, (PopOI_matrix*500+250+30*matrix(rnorm(dim(PopOI_matrix)[1]*dim(PopOI_matrix)[2]), dim(PopOI_matrix)[1])))
            reactV$thresholds = rbind(reactV$thresholds, data.frame(parameter = reactV$PopOIs$name, number_of_pop = 2, value = 500,second_value = NA  ))
            colnames(PopOI_matrix) = paste0(colnames(PopOI_matrix),"+")
            L1_pops_belonging_matrix = cbind(L1_pops_belonging_matrix, PopOI_matrix)
          }
          
          
          # Generate per sample total count table #############
          
          reactV$perSample_totalcount = as.data.frame(as.data.frame(rowData) %>% group_by(sample) %>% summarise(n=n()))
          
          # Run main analysis ##############  
          
          perSample_totalcount = reactV$perSample_totalcount
          perSample_totalcount = setNames(perSample_totalcount$n,perSample_totalcount$sample )
          max_combination_level = input$max_combination_level
          cores = detectCores(logical = FALSE)
          
          # Get count ############
          update_modal_spinner(text = "Getting counts...")
          
          print("Getting counts...")
          
          pop_count <- function(pop){  
            popv = strsplit(pop, split = "_")[[1]]
            if(length(popv) == 1){return(sum(mt[,popv] == length(popv)))}
            return(sum(rowSums(mt[,popv]) == length(popv)))
          } 
          
          perSample_popstat = data.frame(group = character(), sample = character(),  population = character(), count = integer())  ### parameter names order of population defined by alphabetical order  

          print(paste0(Sys.time()," Getting all non repetititive populations ... "))
          update_modal_spinner(text = paste0(Sys.time()," Getting all non repetititive populations ... "))
          
          for(level in 1:max_combination_level){
            if(level == 1){non_rep_pop = L1_pops
            }else{
              all_comb = t(combn(sort(L1_pops),level))
              all_comb_var = gsub("[-+]", "",all_comb)
              no_same_var_comb = apply(all_comb_var, 1, function(x) length(unique(x)) == level )
              non_rep_pop = c(non_rep_pop, apply(all_comb[no_same_var_comb,] ,1,function(x) paste(x,collapse = "_")))
            }
          }
            

          for(sample in unique(rowData$sample) ){
             print(paste0(Sys.time()," Getting counts for ", sample, " ..."))
              update_modal_spinner(text = paste0(Sys.time()," Getting count for ", sample, " ..."))
              
              count_start = Sys.time()
              mt = L1_pops_belonging_matrix[rowData$sample == sample, ]
              group = rowData[rowData$sample == sample,"group"][1]
              count_vector = sapply(non_rep_pop, pop_count)

              perSample_popstat = rbind(perSample_popstat, data.frame(group = group, sample = sample, population = names(count_vector), count = count_vector, row.names = NULL))
              print(Sys.time() - count_start)
              update_modal_spinner(Sys.time() - count_start)
          }
          perSample_popstat$level = sapply(strsplit(perSample_popstat$population, split = "_"), length)
          
          
          
          ## Getting counts and parent count into hierarchical popstat ##########
          
          print("Getting counts and parent counts of hierarchical populations...")
          update_modal_spinner(text = "Getting counts and parent counts of hierarchical populations")
          
          start = Sys.time()
          
          for(level in 1:max_combination_level){
            if(level == 1){ rep_pop = L1_pops
            }else{
              all_rep_comb = permutations(n = length(L1_pops), r = level, v = L1_pops, repeats.allowed = FALSE)
              all_rep_comb_var = gsub("[-+]", "",all_rep_comb)
              no_same_var_rep_comb = apply(all_rep_comb_var, 1, function(x) length(unique(x)) == level )
              rep_pop =c(rep_pop, apply(all_rep_comb[no_same_var_rep_comb,] ,1,function(x) paste(x,collapse = "_")))
            }
          }
          rep_pop_vlist = strsplit(rep_pop, split = "_")
          parent_pop_vector = sapply(rep_pop_vlist, get_parentPop)
          non_rep_pop_vector = sapply(lapply(rep_pop_vlist,sort,decreasing=FALSE), function (x) paste(x, collapse = "_"))
          
          perSample_hpopstat = data.frame(group = character(), sample = character(), population = character(), parentPopulation = character(), count = integer(), parentCount = integer()) ### parameter names order of population indicates parental populations
          
          for(sample in unique(rowData$sample) ){
            subperSample_popstat = perSample_popstat[perSample_popstat$sample == sample, ]
            rownames(subperSample_popstat) = subperSample_popstat$population
            group = subperSample_popstat$group[1]
            count_vector = subperSample_popstat[non_rep_pop_vector ,"count"]
            parentCount_vector = subperSample_popstat[parent_pop_vector ,"count"]
            parentCount_vector[is.na(parentCount_vector)]= perSample_totalcount[sample]
            perSample_hpopstat = rbind(perSample_hpopstat, data.frame(group = group, sample = sample, level = level, population = rep_pop,parentPopulation = parent_pop_vector, non_rep_pop = non_rep_pop_vector,  count = count_vector, parentCount = parentCount_vector, row.names = NULL))
          }
          perSample_hpopstat$level = sapply(rep_pop_vlist, length)
          
          print(Sys.time() - start)
          update_modal_spinner(Sys.time() - start)
            
            
          ## Get frequencies  for hpopstat ##########
          
          print("Getting frequencies...")
          update_modal_spinner(text = "Getting frequencies...")
          freq_start = Sys.time()
          
          for(sample in unique(rowData$sample) ){
            perSample_hpopstat$totalFreq[perSample_hpopstat$sample == sample] = perSample_hpopstat$count[perSample_hpopstat$sample == sample]/perSample_totalcount[sample]
          }
          
          perSample_hpopstat$parentFreq = perSample_hpopstat$count / (perSample_hpopstat$parentCount+0.001) # avoid 0 division
          
          reactV$perSample_popstat = perSample_popstat
          reactV$perSample_hpopstat = perSample_hpopstat
          
          print(Sys.time() - freq_start)
          update_modal_spinner(Sys.time() - freq_start)
          
          print("Getting groups stats...")
          update_modal_spinner(text = "Getting groups basic stats...")  
          group_start = Sys.time()
          reactV$perGroup_hpopstat = as.data.frame(perSample_hpopstat %>% 
                                                     group_by(group, population,non_rep_pop) %>%
                                                     summarise(n=n(), totalFreq_mean = mean(totalFreq), totalFreq_sd = sd(totalFreq), totalFreq_sem = sd(totalFreq)/sqrt(n()),
                                                     parentFreq_mean = mean(parentFreq), parentFreq_sd = sd(parentFreq), parentFreq_sem = sd(parentFreq)/sqrt(n()) ))
          print(Sys.time() - group_start)
          update_modal_spinner(Sys.time() - group_start)
          


          ## Generate frequency test result table ######
          
          print("Performing statistical tests...")
          update_modal_spinner(text = "Performing statistical tests...")
          
          test_start = Sys.time()
          
          groups_names = reactV$groups_names
          test_type = input$test_type
          min_parentcell_count_for_test = input$min_parentcell_count_for_test
          min_subpopcell_count_for_test = input$min_subpopcell_count_for_test
          
          if(test_type == "t.test"){
            res1 = perSample_hpopstat %>% 
              select(group, population, totalFreq ) %>%
              group_by(group, population) %>% 
              summarise(totalFreq = list(totalFreq)) %>% 
              spread(group, totalFreq)  %>%
              rename(totalFreq_refgroup_values = groups_names[1]) %>%
              rename(totalFreq_group2_values = groups_names[2]) %>%
              group_by(population)  %>%
              mutate(totalFreq_refgroup_mean = mean(unlist(totalFreq_refgroup_values)),
                     totalFreq_group2_mean = mean(unlist(totalFreq_group2_values)),
                     totalFreq_l2fc = log2( (totalFreq_group2_mean+1e-6) / (totalFreq_refgroup_mean+1e-6)),
                     totalFreq_pvalue = t.test(unlist(totalFreq_group2_values), unlist(totalFreq_refgroup_values))$p.value) %>%
              select(population, totalFreq_l2fc, totalFreq_pvalue, totalFreq_refgroup_mean, totalFreq_group2_mean)
            
            res2 = perSample_hpopstat %>% 
              select(group, population, parentFreq ) %>%
              group_by(group, population) %>% 
              summarise(parentFreq = list(parentFreq))  %>% 
              spread(group, parentFreq)  %>%
              rename(parentFreq_refgroup_values = groups_names[1]) %>%
              rename(parentFreq_group2_values = groups_names[2]) %>%
              group_by(population)  %>%
              mutate(parentFreq_refgroup_mean = mean(unlist(parentFreq_refgroup_values)),
                     parentFreq_group2_mean = mean(unlist(parentFreq_group2_values)),
                     parentFreq_l2fc = log2( (parentFreq_group2_mean+1e-6) / (parentFreq_refgroup_mean+1e-6)),
                     parentFreq_pvalue = t.test(unlist(parentFreq_group2_values), unlist(parentFreq_refgroup_values))$p.value) %>%
              select(population, parentFreq_l2fc, parentFreq_pvalue, parentFreq_refgroup_mean,parentFreq_group2_mean )
            
            res_filter1 = perSample_hpopstat %>% 
              select(group, population, parentCount ) %>%
              group_by(group, population) %>% 
              summarise(parentCount = list(parentCount))  %>% 
              spread(group, parentCount)  %>%
              rename(parentCount_refgroup_values = groups_names[1]) %>%
              rename(parentCount_group2_values = groups_names[2]) %>%
              group_by(population)  %>%
              mutate(count_refgroup_mean = mean(unlist(parentCount_refgroup_values)),
                     count_group2_mean = mean(unlist(parentCount_group2_values)),
                     min_parentCount_for_test_exclusion_filter = min(c(mean(unlist(parentCount_refgroup_values)),  mean(unlist(parentCount_group2_values)))) < min_parentcell_count_for_test) %>%
              select(population,min_parentCount_for_test_exclusion_filter)
            
            res_filter2 = perSample_hpopstat %>% 
              select(group, population, count ) %>%
              group_by(group, population) %>% 
              summarise(count = list(count))  %>% 
              spread(group, count)  %>%
              rename(count_refgroup_values = groups_names[1]) %>%
              rename(count_group2_values = groups_names[2]) %>%
              group_by(population)  %>%
              mutate(min_subpop_count_for_test_exclusion_filter = max(c(mean(unlist(count_refgroup_values)), mean(unlist(count_group2_values)))) < min_subpopcell_count_for_test)%>%
              select(population,min_subpop_count_for_test_exclusion_filter)       
            
            test_res= as.data.frame(cbind(res1, res2[,-1], res_filter1[,-1], res_filter2[,-1]))
            
            test_res[as.logical(test_res$min_parentCount_for_test_exclusion_filter+ test_res$min_subpop_count_for_test_exclusion_filter),c("totalFreq_l2fc", "totalFreq_pvalue", "parentFreq_l2fc","parentFreq_pvalue")] = NA
         
            
            test_res$level = as.character(sapply(strsplit(test_res$population, split = "_"), length))
            test_res$group_comparison = paste(groups_names[2],"vs",groups_names[1])
            
            test_res$parentFreq_minuslog10pvalue = -log10(test_res$parentFreq_pvalue)
            test_res$parentFreq_distancefrom0 = sqrt(test_res$parentFreq_l2fc^2 + test_res$parentFreq_minuslog10pvalue^2)
            test_res$totalFreq_minuslog10pvalue = -log10(test_res$totalFreq_pvalue)
            test_res$totalFreq_distancefrom0 = sqrt(test_res$totalFreq_l2fc^2 + test_res$totalFreq_minuslog10pvalue^2)
            
            test_res$non_rep_pop = sapply(lapply(strsplit(test_res$population, split = "_"),sort,decreasing=FALSE), function (x) paste(x, collapse = "_"))
            test_res$non_rep_pop_logical = test_res$non_rep_pop == test_res$population

            }

          print(Sys.time() - test_start)
          update_modal_spinner(Sys.time() - test_start)
          
          
          reactV$test_res = test_res

          write.csv(reactV$test_res, file = paste0("./", outdir, "/", "test_res_", time, ".csv") , row.names = FALSE)  
          sink(paste0("./", outdir, "/", "test_res_parameters_", time, ".txt"))
          print(paste("Restricted population analyis:", Popfilter))
          print(paste("Max combination level:", max_combination_level))
          print(paste("Test:", input$test_type))
          print(paste("Min cell count:", input$min_cell_count_for_test))
          sink()
          
          update_modal_spinner(text = "saving RDS...")
          if(input$saveRDS_checkbox){
            saveRDS(reactV, file = paste0("./", outdir, "/", "analysis_", time, ".rds"))
          }
          report_success("Analysis completed", "See Results tab")
          
          print("done")
          

          
          
          remove_modal_spinner()

            }
          }
      
      })
 
    
    # Load RDS ############   
  observeEvent(input$loadRDS, {
      show_modal_spinner(spin = "fading-circle") 
      rds_reactV = readRDS(input$loadRDS$datapath)
      reactV$channel_values_matrix = rds_reactV$channel_values_matrix
      reactV$colData = rds_reactV$colData
      reactV$rowData = rds_reactV$rowData
      reactV$aboveThresholdsum_matrix = rds_reactV$aboveThresholdsum_matrix
      reactV$test_res = rds_reactV$test_res
      reactV$PopOIs = rds_reactV$PopOIs
      reactV$labeled_parameters = rds_reactV$labeled_parameters
      reactV$groups_names = rds_reactV$groups_names
      reactV$import_button_status = rds_reactV$import_button_status
      reactV$L1_pops_belonging_matrix = rds_reactV$L1_pops_belonging_matrix
      reactV$perGroup_files_df = rds_reactV$perGroup_files_df
      reactV$perGroup_hpopstat = rds_reactV$perGroup_hpopstat
      reactV$perSample_hpopstat = rds_reactV$perSample_hpopstat
      reactV$perSample_totalcount = rds_reactV$perSample_totalcount
      reactV$preselect = rds_reactV$PrmOIs
      reactV$samples_and_groups_file_status = rds_reactV$samples_and_groups_file_status
      reactV$subPrmOIs = rds_reactV$subPrmOIs
      reactV$thresholds = rds_reactV$thresholds
      reactV$PrmOIs = rds_reactV$PrmOIs
      remove_modal_spinner()
      report_success("Analysis loaded", "See Results tab")
  })
    
    
    
    
# Results =================================

  # Generate volcano data table ##########  
    volcano_df <- reactive ({
      if(is.null(reactV$test_res))
        return(NULL)

      if(input$freq_denominator == " All "){
        volcano_df = reactV$test_res[reactV$test_res$population == reactV$test_res$non_rep_pop, ]
        volcano_df$pvalue = volcano_df$totalFreq_pvalue
        volcano_df$l2fc = volcano_df$totalFreq_l2fc
        volcano_df$minuslog10pvalue = volcano_df$totalFreq_minuslog10pvalue
        volcano_df$distancefrom0 = volcano_df$totalFreq_distancefrom0
      } 
      if(input$freq_denominator == "Parent"){
        volcano_df = reactV$test_res
        volcano_df$pvalue = volcano_df$parentFreq_pvalue
        volcano_df$l2fc = volcano_df$parentFreq_l2fc
        volcano_df$minuslog10pvalue = volcano_df$parentFreq_minuslog10pvalue
        volcano_df$distancefrom0 = volcano_df$parentFreq_distancefrom0
      }
      volcano_df = volcano_df[volcano_df$level %in% as.numeric(input$level_selector),]
      if(input$restrict_volcano_to_selected_L1HHM & is.null(click$L1_Hieracheatmap_population) == FALSE){
        click$L1_Hieracheatmap_population
        volcano_df= volcano_df[grepl( click$L1_Hieracheatmap_population, volcano_df$population),]
      }
      rownames(volcano_df) = volcano_df$population
      
      return(volcano_df)
      
    })
    

    
    
  # Generate table for volcano label top ################
    volc_label_data_top <- reactive ({
      if(is.null(volcano_df()))
        return(NULL)
      volcano_df = volcano_df()
      if(input$label_top_checkbox == FALSE)
        return(volcano_df[FALSE,])
      df = volcano_df[volcano_df$minuslog10pvalue > -log10(input$alpha) & abs(volcano_df$l2fc) > log2(input$mfc) , ]
      if(input$direction == "positive only"){df = df[df$l2fc > 0,] }
      if(input$direction == "negative only"){df = df[df$l2fc < 0,] }
      if(dim(df)[1]>input$labeltop) {
        df[order(-df$distancefrom0)[1:input$labeltop],]
      } else {df}
    })
    
  # Generate table for volcano label level1 ################ 
    volc_label_data_level1 <- reactive ({
      if(is.null(volcano_df()))
        return(NULL)
      volcano_df = volcano_df()
      if(input$label_level1_checkbox == FALSE)
        return(volcano_df[FALSE,])
      df = volcano_df[volcano_df$level == "1", ]
      df
    })
    
  # Generate table for volcano label click #############
    volc_label_data <- reactiveValues() 
    volc_label_data$click = data.frame(level = character(), group_comparison = character(), population = character(), l2fc = numeric(), minuslog10pvalue = numeric(), distancefrom0 = numeric())
    
    
  # Get near point after Volcano click input and add it to table for volcano label click  ###########################     
    observeEvent(input$Freq_volcano_click, {
      nP = nearPoints(volcano_df(), input$Freq_volcano_click, maxpoints = 1, threshold = 20 )
      if(nrow(nP) > 0) {
        if(nrow(volc_label_data$click) > 0){
          if(nP$population %in% volc_label_data$click$population){
            volc_label_data$click =  volc_label_data$click[!(volc_label_data$click$population == nP$population),]
          } else{ volc_label_data$click = rbind(volc_label_data$click, nP)}
        }else{ volc_label_data$click = nP}
      }
    })
    
  # Reset volcano label based on click from reset input  #######################
    observeEvent(input$click_reset, {
      volc_label_data$click = data.frame(level = character(), group_comparison = character(), population = character(), l2fc = numeric(), minuslog10pvalue = numeric(), distancefrom0 = numeric())
      volc_label_data$intersect = data.frame(level = character(), group_comparison = character(), population = character(), l2fc = numeric(), minuslog10pvalue = numeric(), distancefrom0 = numeric())
    }) 
  
  # Get last selected HHM population if exist ########
    
    volc_label_HHM <- reactive ({
      if(is.null(click$last_Hieracheatmap_population))
        return(NULL)
      if(input$label_HHM_checkbox == TRUE){
        if(input$freq_denominator == " All "){
          pop = click$last_Hieracheatmap_population
          parameters = gsub("[-+]", "", strsplit(pop, "_")[[1]])
          sides = gsub("[^-+]", "", strsplit(pop, "_")[[1]])
          sorted_pop = paste(paste0(sort(parameters), sides[order(parameters)]), collapse = "_")
          volc_label_HHM = volcano_df()[sorted_pop,]
        }
        if(input$freq_denominator == "Parent"){
          pop = click$last_Hieracheatmap_population
          volc_label_HHM = volcano_df()[pop,]
        }
        if(is.na(volc_label_HHM[1,1])){return(NULL)}
        volc_label_HHM
      }
    })
    
    
     
    
    
  # Generate final table for volcano label ################
    volc_label_data_total = reactive({
      volc_label_data_pre_total = rbind(volc_label_data_top(), volc_label_data_level1()[!(volc_label_data_level1()$population %in% volc_label_data_top()$population),])
      if(nrow(volc_label_data$click) == 0){volc_label_data_total = volc_label_data_pre_total}
      if(nrow(volc_label_data$click) > 0){
        volc_label_data_intersect = volc_label_data$click[volc_label_data$click$population == volc_label_data_pre_total$population,] 
        volc_label_data_total = rbind(volc_label_data_pre_total[ !(volc_label_data_pre_total$population %in% volc_label_data_intersect$population),],
                                      volc_label_data$click[ !(volc_label_data$click$population %in% volc_label_data_intersect$population),])
      }
      
      volc_label_data_total
    })
    
  # Output Volcano plot #####################  
    Freq_volcano_plot <- reactive({
      if(is.null(volcano_df()))
        return(NULL)

      volcano_df = volcano_df()
      volcano_df$mean_totalFreq = (volcano_df$totalFreq_refgroup_mean + volcano_df$totalFreq_group2_mean)/2 

      max_x_scale = max(min(volcano_df$l2fc, na.rm = TRUE)*-1,max(volcano_df$l2fc, na.rm = TRUE))
      if(input$dot_size_freq_checkbox){
        g = ggplot()+
          scale_x_continuous(name = "Fold Change (log2)"  , limits = c(-max_x_scale, max_x_scale) )+
          scale_y_continuous(name = "Significance (-log10pvalue)"  )+
          scale_colour_manual(values = c("1" = "#D7191C", "2" =  "#FDAE61", "3" =  "#ABD9E9" ))+
          geom_point(data=volcano_df[order(volcano_df$level, decreasing = TRUE),], aes(x = l2fc, y = minuslog10pvalue, color = level, size = mean_totalFreq ), shape = 16, na.rm = TRUE)+
          geom_hline(yintercept = -log10(input$alpha), linetype = "dashed")+
          geom_vline(xintercept = c(-log2(input$mfc),log2(input$mfc)), linetype = "dashed")+
          theme_bw()+
          theme(legend.position = "top")
      } else {
        g = ggplot()+
          scale_x_continuous(name = "Fold Change (log2)"  , limits = c(-max_x_scale, max_x_scale) )+
          scale_y_continuous(name = "Significance (-log10pvalue)"  )+
          scale_colour_manual(values = c("1" = "#D7191C", "2" =  "#FDAE61", "3" =  "#ABD9E9" ))+
          geom_point(data=volcano_df[order(volcano_df$level, decreasing = TRUE),], aes(x = l2fc, y = minuslog10pvalue, color = level), shape = 16,  size = 3, na.rm = TRUE)+
          geom_hline(yintercept = -log10(input$alpha), linetype = "dashed")+
          geom_vline(xintercept = c(-log2(input$mfc),log2(input$mfc)), linetype = "dashed")+
          theme_bw()+
          theme(legend.position = "top")
      }
      g
      
    })
    
    output$Freq_volcano <- renderPlot({ 
      if(is.null(Freq_volcano_plot()) | is.null(volc_label_data_total()))
        return(NULL)
      p = Freq_volcano_plot() + geom_label_repel(data = volc_label_data_total(),   aes(x = l2fc, y = minuslog10pvalue, label = population), seed = 1234, alpha = 0.8, min.segment.length = 0)
        
      if(is.null(volc_label_HHM()) == FALSE){
        p = p + geom_label_repel(data = volc_label_HHM(),   aes(x = l2fc, y = minuslog10pvalue, label = population), fill = "#FFA4A4", seed = 1234, alpha = 0.8, min.segment.length = 0)
      }  
      
      reactV$volcano_plot = p
      
      p
        
        
    })
    
    
    
    
    
    
  # Generate volcano selected population per group population frequency table for barplot table ###############
    barplot_df <-  reactive({
      if(is.null(reactV$perGroup_hpopstat))
        return(NULL)
      
      perGroup_hpopstat = reactV$perGroup_hpopstat
      
      if( (is.null(volc_label_data_total()) | nrow(volc_label_data_total()) == 0) & (is.null(volc_label_HHM()) == TRUE) ){
        return(NULL)
      }
      if( (is.null(volc_label_data_total()) | nrow(volc_label_data_total()) == 0) & (is.null(volc_label_HHM()) == FALSE) ) {
        volc_label_data_total = volc_label_HHM()
      }
      if( (is.null(volc_label_data_total()) == FALSE | nrow(volc_label_data_total()) > 0) & (is.null(volc_label_HHM()) == TRUE) ){
        volc_label_data_total = volc_label_data_total()
      }
      if( (is.null(volc_label_data_total()) == FALSE | nrow(volc_label_data_total()) > 0) & (is.null(volc_label_HHM()) == FALSE) ){
        volc_label_data_total = rbind(volc_label_data_total(),volc_label_HHM())
      }
      
      reactV$barplot_element_count = nrow(volc_label_data_total)
      
      volcpop_perGroup_hpopstat = perGroup_hpopstat[perGroup_hpopstat$population %in% volc_label_data_total$population,] 
      volcpop_perGroup_hpopstat$group = factor(volcpop_perGroup_hpopstat$group, levels = sort(unique(volcpop_perGroup_hpopstat$group), decreasing = TRUE))
      volcpop_perGroup_hpopstat$population = factor(volcpop_perGroup_hpopstat$population, levels = sort(unique(volcpop_perGroup_hpopstat$population)))
      volcpop_perGroup_hpopstat
      
      if(input$freq_denominator == " All "){
        volcpop_perGroup_hpopstat$mean = volcpop_perGroup_hpopstat$totalFreq_mean
        volcpop_perGroup_hpopstat$sd = volcpop_perGroup_hpopstat$totalFreq_sd
        volcpop_perGroup_hpopstat$sem = volcpop_perGroup_hpopstat$totalFreq_sem
        return(volcpop_perGroup_hpopstat)
      }
      if(input$freq_denominator == "Parent"){
        volcpop_perGroup_hpopstat$mean = volcpop_perGroup_hpopstat$parentFreq_mean
        volcpop_perGroup_hpopstat$sd = volcpop_perGroup_hpopstat$parentFreq_sd
        volcpop_perGroup_hpopstat$sem = volcpop_perGroup_hpopstat$parentFreq_sem
        return(volcpop_perGroup_hpopstat)
      } 
      
    })
   
  # Export volcano #########   
    output$export_volcano <- downloadHandler(
      filename = function() {
        if(input$volc_format == "png"){return(paste0("volcano_",reactV$groups_names[2],"_vs_",reactV$groups_names[1],"_",format(Sys.time(), "%Y%m%d%H%M"), ".png"))}
        if(input$volc_format == "pdf"){return(paste0("volcano_",reactV$groups_names[2],"_vs_",reactV$groups_names[1],"_",format(Sys.time(), "%Y%m%d%H%M"), ".pdf"))}
      },
      content = function(file) {
        ggsave(file, reactV$volcano_plot, width = input$volc_width, height = input$volc_height, units = "mm" )
      }
    )
    
    
    
    
  # Output barplot ############### 
    Barplot <- reactive({
      if(is.null(barplot_df()))
        return(NULL)
      barplot_df = barplot_df()
      barplot_df$group = factor(barplot_df$group, levels = reactV$groups_names )
      barplot_dots_df = reactV$perSample_hpopstat[reactV$perSample_hpopstat$population %in% unique(barplot_df$population),]
      if(input$freq_denominator == " All "){
        barplot_dots_df$freq = barplot_dots_df$totalFreq
      }
      if(input$freq_denominator == "Parent"){
        barplot_dots_df$freq = barplot_dots_df$parentFreq
      }

      limit_max = (max(barplot_df$mean)+barplot_df$sem[which.max(barplot_df$mean)])*100
      if(limit_max < max(barplot_dots_df$freq)*100 ) { limit_max = max(barplot_dots_df$freq)*100}

      
      plot = ggplot()+
        scale_y_continuous(name = "Population frequency (% +/- sem)", limits = c(0,limit_max))+
        scale_x_discrete(name = "")+
        scale_fill_manual(values = c("darkgrey","#225EA8"))+
        scale_shape_manual(values = c(21,21))+
        geom_bar(data=barplot_df, stat = "identity", aes(x = population, y = mean*100, fill = group), position = position_dodge())+
        geom_errorbar(data=barplot_df, aes(x = population, ymin = (mean-sem)*100, ymax =(mean+sem)*100, group = group), width = 0.5, position = position_dodge(width = 0.9) )+
        geom_point(data = barplot_dots_df, aes(x = population, y = freq*100, shape = group ), size = 3,  position = position_dodge(width=0.9))+
        
        theme_bw()+
        theme(legend.position = "top", axis.text.x = element_text(angle = 90, hjust = 1, size = 10, face="bold"))

      plot
    })
    
    
    reactV$barplot_element_count = 1 # initiation
    output$Barplot <- renderPlot({ 
      if(is.null(Barplot()))
        return(NULL)
      Barplot() 
    }, width = function(){reactV$barplot_element_count*60+50})
    
    
  # Get Barplot click population ######################## 
    click <- reactiveValues()
    
    observeEvent(input$Barplot_click, { 
      click$barplot_population = levels(barplot_df()$population)[round(as.integer(input$Barplot_click$x+0.5))]
    })
    
  # Output barplot click population info table ############
    barplot_infotable <- reactive({
      if(is.null(click$barplot_population))
        return(NULL)
      if(sum(barplot_df$population == click$barplot_population) == 0)
        return(NULL)
      barplot_df = barplot_df()
      volcano_df = volcano_df()
      data.frame(population = click$barplot_population, 
                 means = paste0(paste0(round(barplot_df$mean[barplot_df$population == click$barplot_population & barplot_df$group == reactV$groups_names[1]]*100,1),"%"),
                                " vs ",
                                paste0(round(barplot_df$mean[barplot_df$population == click$barplot_population & barplot_df$group == reactV$groups_names[-1]]*100,1),"%")),
                 foldchange = round(2^(volcano_df$l2fc[volcano_df$population == click$barplot_population]),1),
                 l2fc = round(volcano_df$l2fc[volcano_df$population == click$barplot_population],1),
                 pvalue = scientific(volcano_df$pvalue[volcano_df$population == click$barplot_population],2),
                 distancefrom0 = round(volcano_df$distancefrom0[volcano_df$population == click$barplot_population],1))
    })
    
    output$barplot_info <- renderTable({barplot_infotable()})

    # Export barplot ######### 
    
    
    output$barplot_dropdown <- renderUI({
        dropdownButton(
          tags$h3("export parameters"),
          numericInput("barplot_width", label = "width", value = reactV$barplot_element_count*30+25),
          numericInput("barplot_height", label = "height", value = 130), 
          selectInput("barplot_format", label = "format", 
                      choices = c("png",  "pdf"), 
                      selected = "png"),
          circle = FALSE, status = "default",
          icon = shiny::icon("cog"),
          tooltip = tooltipOptions(title = "export parameters")
        )
    })
    
    output$export_barplot <- downloadHandler(
      filename = function() {
        if(input$barplot_format == "png"){return(paste0("barplot_",reactV$groups_names[2],"_vs_",reactV$groups_names[1],"_",format(Sys.time(), "%Y%m%d%H%M"), ".png"))}
        if(input$barplot_format == "pdf"){return(paste0("barplot_",reactV$groups_names[2],"_vs_",reactV$groups_names[1],"_",format(Sys.time(), "%Y%m%d%H%M"), ".pdf"))}
      },
      content = function(file) {
        ggsave(file, Barplot(), width = input$barplot_width, height = input$barplot_height, units = "mm" )
      }
    )
    
    
    
  # Output Hieracheatmaps #############
    Hieracheatmap <- reactiveValues()
    
    output$HHM_stat_picker_ui <- renderUI({
      pickerInput(inputId = "HHM_stat_picker",label = "displayed statistic", choices = setNames(c("l2fc","minuslog10pvalue","distancefrom0", "refgroup_freq", "group2_freq"),  c("Fold change (log2)","Significance (-log10pvalue)", "Distance from 0,0 on volcano", paste(reactV$groups_names[1], " mean frequency"), paste(reactV$groups_names[2], " mean frequency")) ))
    })
    

    HHM_df <- reactive ({
      if(is.null(reactV$test_res))
        return(NULL)
      HHM_df = reactV$test_res
      if(input$freq_denominator == " All "){
        HHM_df$l2fc = HHM_df$totalFreq_l2fc
        HHM_df$minuslog10pvalue = HHM_df$totalFreq_minuslog10pvalue
        HHM_df$distancefrom0 = HHM_df$totalFreq_distancefrom0
      } 
      if(input$freq_denominator == "Parent"){
        HHM_df$l2fc = HHM_df$parentFreq_l2fc
        HHM_df$minuslog10pvalue = HHM_df$parentFreq_minuslog10pvalue
        HHM_df$distancefrom0 = HHM_df$parentFreq_distancefrom0
      }

      return(HHM_df)
    })
    
    
    reactV$HHM_height = 1 # initialisation

    L1_Hieracheatmap <- reactive({
      if(is.null(HHM_df()))
        return(NULL)
      if(is.null(input$HHM_stat_picker))
        return(NULL)
      HHM_df = HHM_df()
      stat = input$HHM_stat_picker
      if(stat == "refgroup_freq" & input$freq_denominator == " All "){ stat = "totalFreq_refgroup_mean"}
      if(stat == "refgroup_freq" & input$freq_denominator == "Parent"){ stat = "parentFreq_refgroup_mean"}
      if(stat == "group2_freq" & input$freq_denominator == " All "){ stat = "totalFreq_group2_mean"}
      if(stat == "group2_freq" & input$freq_denominator == "Parent"){ stat = "parentFreq_group2_mean"}
      HHM_df$stat = HHM_df[,stat]
      max = max(HHM_df$stat)
      min = min(HHM_df$stat)
      df = HHM_df[HHM_df$level == "1",]
      df$population = factor(df$population, levels = sort(unique(df$population), decreasing = TRUE))
      if(input$sort_by_side_first == TRUE){
        df$parameter = gsub("[-+]", "",  as.vector(df$population))
        df$side = gsub("[^-+]", "",  as.vector(df$population))
        df = df[order(df$parameter, decreasing = TRUE),]
        df = df[order(df$side, decreasing = TRUE),]
        df$population = factor(df$population, levels = df$population)
      }
      g = ggplot(data=df, aes(x = level, y = population))+
        labs(fill=input$HHM_stat_picker) +
        geom_tile(aes(fill = stat))+
        geom_text(aes(label = round(stat, 1)), na.rm = TRUE)+
        theme_bw() +
        theme(legend.position="left", 
              axis.title.y = element_blank(), axis.text.y = element_text(size=12, face="bold"),  axis.ticks.y = element_blank(),
              axis.title.x = element_blank(), axis.text.x = element_blank(),  axis.ticks.x = element_blank(), 
              panel.border = element_blank(), panel.grid = element_blank())
      
      if(input$HHM_stat_picker == "l2fc"){ g = g+ scale_fill_gradient2(midpoint=0, low="blue", mid="white", high="red",limits = c(min,max))
      #} else {g = g+ scale_fill_viridis(limits = c(min,max)) }
      } else {g = g+ scale_fill_gradient(low = "white", high = "red", limits = c(min,max)) }
      
      Hieracheatmap$L1 = g
      Hieracheatmap$L1pop = levels(df$population)
      reactV$HHM_height = length(unique( HHM_df$population[HHM_df$level == "1"]))/2*50
      
      g
    })
    
    output$L1_Hieracheatmap <- renderPlot({
      if(is.null(L1_Hieracheatmap()))
        return(NULL)
      Hieracheatmap$L1
    },  width = 220, height = function(){reactV$HHM_height})
    
    
    L2_Hieracheatmap <- reactive({
      if(is.null(HHM_df()))
        return(NULL)
      if(is.null(click$L1_Hieracheatmap_population) | sum(HHM_df()$level == "2") == 0)
        return(NULL)
      HHM_df = HHM_df()
      stat = input$HHM_stat_picker
      if(stat == "refgroup_freq" & input$freq_denominator == " All "){ stat = "totalFreq_refgroup_mean"}
      if(stat == "refgroup_freq" & input$freq_denominator == "Parent"){ stat = "parentFreq_refgroup_mean"}
      if(stat == "group2_freq" & input$freq_denominator == " All "){ stat = "totalFreq_group2_mean"}
      if(stat == "group2_freq" & input$freq_denominator == "Parent"){ stat = "parentFreq_group2_mean"}
      HHM_df$stat = HHM_df[,stat]
      max = max(HHM_df$stat)
      min = min(HHM_df$stat)
      df = HHM_df[HHM_df$level == "2" & grepl(paste0(click$L1_Hieracheatmap_population,"_"), HHM_df$population, fixed = TRUE) ,]
      df$population = factor(df$population, levels = sort(unique(df$population), decreasing = TRUE))
      if(input$sort_by_side_first == TRUE){
        last_pop = unname(t(as.data.frame(strsplit(as.vector(df$population), split = "_")))[,2])
        df$parameter = gsub("[-+]", "",last_pop)
        df$side = gsub("[^-+]", "",  last_pop)
        df = df[order(df$parameter, decreasing = TRUE),]
        df = df[order(df$side, decreasing = TRUE),]
        df$population = factor(df$population, levels = df$population)
      }
       g = ggplot(data=df, aes(x = level, y = population))+
        geom_tile(aes(fill = stat))+
        geom_text(aes(label = round(stat, 1)), na.rm = TRUE)+
        theme_bw() +
        theme(legend.position="none", 
              axis.title.y = element_blank(), axis.text.y = element_text(size=12, face="bold"),  axis.ticks.y = element_blank(),
              axis.title.x = element_blank(), axis.text.x = element_blank(),  axis.ticks.x = element_blank(), 
              panel.border = element_blank(), panel.grid = element_blank())
      
      if(input$HHM_stat_picker == "l2fc"){ g = g+ scale_fill_gradient2(midpoint=0, low="blue", mid="white", high="red",limits = c(min,max))
      #} else {g = g+ scale_fill_viridis(limits = c(min,max)) }
      } else {g = g+ scale_fill_gradient(low = "white", high = "red", limits = c(min,max)) }
      
      Hieracheatmap$L2 = g
      Hieracheatmap$L2pop = levels(df$population)
      g
    })
    output$L2_Hieracheatmap <- renderPlot({ 
      if(is.null(L2_Hieracheatmap()))
        return(NULL)
      Hieracheatmap$L2
    }, width = 210, height = function(){reactV$HHM_height})
    
    
    L3_Hieracheatmap <- reactive({
      if(is.null(HHM_df()))
        return(NULL)
      if(is.null(click$L2_Hieracheatmap_population) | sum(HHM_df()$level == "3") == 0)
        return(NULL)
      HHM_df = HHM_df()
      stat = input$HHM_stat_picker
      if(stat == "refgroup_freq" & input$freq_denominator == " All "){ stat = "totalFreq_refgroup_mean"}
      if(stat == "refgroup_freq" & input$freq_denominator == "Parent"){ stat = "parentFreq_refgroup_mean"}
      if(stat == "group2_freq" & input$freq_denominator == " All "){ stat = "totalFreq_group2_mean"}
      if(stat == "group2_freq" & input$freq_denominator == "Parent"){ stat = "parentFreq_group2_mean"}
      HHM_df$stat = HHM_df[,stat]
      max = max(HHM_df$stat)
      min = min(HHM_df$stat)
      df = HHM_df[HHM_df$level == "3" & grepl(paste0(click$L2_Hieracheatmap_population,"_"), HHM_df$population, fixed = TRUE) ,]
      df$population = factor(df$population, levels = sort(unique(df$population), decreasing = TRUE))
      if(input$sort_by_side_first == TRUE){
        last_pop = unname(t(as.data.frame(strsplit(as.vector(df$population), split = "_")))[,3])
        df$parameter = gsub("[-+]", "",last_pop)
        df$side = gsub("[^-+]", "",  last_pop)
        df = df[order(df$parameter, decreasing = TRUE),]
        df = df[order(df$side, decreasing = TRUE),]
        df$population = factor(df$population, levels = df$population)
      }
      g = ggplot(data=df, aes(x = level, y = population))+
        geom_tile(aes(fill = stat))+
        geom_text(aes(label = round(stat, 1)), na.rm = TRUE)+
        theme_bw() +
        theme(legend.position="none", 
              axis.title.y = element_blank(), axis.text.y = element_text(size=12, face="bold"),  axis.ticks.y = element_blank(),
              axis.title.x = element_blank(), axis.text.x = element_blank(),  axis.ticks.x = element_blank(), 
              panel.border = element_blank(), panel.grid = element_blank())
      
      if(input$HHM_stat_picker == "l2fc"){ g = g+ scale_fill_gradient2(midpoint=0, low="blue", mid="white", high="red",limits = c(min,max))
      #} else {g = g+ scale_fill_viridis(limits = c(min,max)) }
      } else {g = g+ scale_fill_gradient(low = "white", high = "red", limits = c(min,max)) }
      
      Hieracheatmap$L3 = g
      Hieracheatmap$L3pop = levels(df$population)
      g
    })
    output$L3_Hieracheatmap <- renderPlot({ 
      if(is.null(L3_Hieracheatmap()))
        return(NULL)
      Hieracheatmap$L3
    }, width = 250, height = function(){reactV$HHM_height})
    
    
  # Get HHM click population #################
    observeEvent(input$L1_Hieracheatmap_click, {
      populations = Hieracheatmap$L1pop
      click$L1_Hieracheatmap_population = populations[round(as.integer(input$L1_Hieracheatmap_click$y+0.5))]
      click$last_Hieracheatmap_population = click$L1_Hieracheatmap_population
      Hieracheatmap$L3 = NULL
    })
    
    observeEvent(input$L2_Hieracheatmap_click, {
      populations = Hieracheatmap$L2pop
      click$L2_Hieracheatmap_population = populations[round(as.integer(input$L2_Hieracheatmap_click$y+0.5))]
      click$last_Hieracheatmap_population = click$L2_Hieracheatmap_population
    })
    
    observeEvent(input$L3_Hieracheatmap_click, {
      populations = Hieracheatmap$L3pop
      click$L3_Hieracheatmap_population = populations[round(as.integer(input$L3_Hieracheatmap_click$y+0.5))]
      click$last_Hieracheatmap_population = click$L3_Hieracheatmap_population
    })
    
    # Export HHM #########  
    
    output$HHM_dropdown <- renderUI({
      dropdownButton(
        tags$h3("export parameters"),
        numericInput("HHM_width", label = "width", value =  297/3+297/3*as.numeric(!is.null(Hieracheatmap$L2))+ 297/3*as.numeric(!is.null(Hieracheatmap$L3)) ),
        numericInput("HHM_height", label = "height", value = 210), 
        selectInput("HHM_format", label = "format", 
                    choices = c("png",  "pdf"), 
                    selected = "png"),
        circle = FALSE, status = "default",
        icon = shiny::icon("cog"),
        tooltip = tooltipOptions(title = "export parameters")
      )
    })
    
    output$export_HHM <- downloadHandler(
      filename = function() {
        if(input$HHM_format == "png"){return(paste0("HHM_",reactV$groups_names[2],"_vs_",reactV$groups_names[1],"_",format(Sys.time(), "%Y%m%d%H%M"), ".png"))}
        if(input$HHM_format == "pdf"){return(paste0("HHM_",reactV$groups_names[2],"_vs_",reactV$groups_names[1],"_",format(Sys.time(), "%Y%m%d%H%M"), ".pdf"))}
      },
      content = function(file) {
        
        if(is.null(Hieracheatmap$L2)){ ggsave(file, arrangeGrob(Hieracheatmap$L1, nrow=1), width = input$HHM_width, heigh = input$HHM_height, units = "mm" ) 
        } else {
          if(is.null(Hieracheatmap$L3)){ ggsave(file, arrangeGrob(Hieracheatmap$L1,Hieracheatmap$L2, nrow=1), width = input$HHM_width, heigh = input$HHM_height, units = "mm" ) }
          else { ggsave(file, arrangeGrob(Hieracheatmap$L1,Hieracheatmap$L2,Hieracheatmap$L3, nrow=1), width = input$HHM_width, heigh = input$HHM_height, units = "mm" )      }
        }
        
      }
    )
    
    
    
    
  # Output dotplots from HHM click population ###############
    
    
    # Output dotplots_yvar ###############
    
    output$dotplots_yvar_selector<- renderUI({
      if(is.null(reactV$labeled_parameters))
        return(NULL)
      selectInput("dotplots_yvar", label = "",  choices = sort(colnames(reactV$channel_values_matrix)), selected = "SSC.A")
    })
    
    reactV$Dotplot_height = 1 #initialisation
    output$Dotplots <- renderPlot({
      if(is.null(click$last_Hieracheatmap_population))
        return(NULL)
      if(input$display_dotplots == FALSE)
        return(NULL)
      HHM_pop = click$last_Hieracheatmap_population
      HHM_pop_vector = strsplit(HHM_pop, split = "_")[[1]]
      xscale = c(0,1024)
      yscale = c(0,1024)
      
      mat = reactV$channel_values_matrix
      rowData = reactV$rowData

      xvar = gsub("[-+]","", HHM_pop_vector[length(HHM_pop_vector)])
      yvar = input$dotplots_yvar
      
      subpop = "none"
      subpop_filter = rep(TRUE,dim(mat)[1])

      if(length(HHM_pop_vector) >= 2){
        filter_pop = HHM_pop_vector[-length(HHM_pop_vector)]
        subpop = paste(filter_pop,collapse = "_")
        parameters = gsub("[-+]", "",filter_pop)
        aboveThresholdvalues = paste(gsub("11", 2, gsub("\\+", 1,gsub("-",0,gsub("[^-+]", "",filter_pop)))),collapse = "")
        
        df = as.data.frame(as.matrix(reactV$aboveThresholdsum_matrix[,parameters]))
        df_collapse = do.call("paste",c(df,sep=""))

        subpop_filter = df_collapse == aboveThresholdvalues
      }
      submat = mat[subpop_filter,]
      subrowData = rowData[subpop_filter,]
      
      df = as.data.frame(cbind(subrowData[,c("group", "sample")], submat[,c(xvar,yvar)]))

      maxchar_sample_value = max(sapply(unique(df$sample), function(x){nchar(x)}))
      
      glist = list()
      for(i in 1:length(unique(df$sample))) {
        sample = unique(df$sample)[i]
        subdf = df[df$sample == sample,]
        totalEvent_count = dim(subdf)[1]
        if(reactV$thresholds[reactV$thresholds$parameter == xvar, "number_of_pop"] == "2"){
          posEvent_count = sum(subdf[,xvar] > reactV$thresholds[reactV$thresholds$parameter == xvar, "value"])
          posEvent_freq = posEvent_count / totalEvent_count
          negEvent_count = totalEvent_count - posEvent_count
          negEvent_freq = negEvent_count / totalEvent_count
        }
        if(reactV$thresholds[reactV$thresholds$parameter == xvar, "number_of_pop"] == "3"){
          posEvent_count = sum(subdf[,xvar] > reactV$thresholds[reactV$thresholds$parameter == xvar, "second_value"])
          posEvent_freq = posEvent_count / totalEvent_count
          intEvent_count = sum(subdf[,xvar] > reactV$thresholds[reactV$thresholds$parameter == xvar, "value"] &  subdf[,xvar] <= reactV$thresholds[reactV$thresholds$parameter == xvar, "second_value"])
          intEvent_freq = intEvent_count / totalEvent_count
          negEvent_count = totalEvent_count - posEvent_count - intEvent_count
          negEvent_freq = negEvent_count / totalEvent_count
        }

        
        ymax = 1024
        xmin = 0
        xmax = 1024

        colnames(subdf) = gsub("@", "",colnames(subdf))
        pxvar = gsub("@", "",xvar)
        pyvar = gsub("@", "",yvar)

        g = ggplot(subdf, aes_string(x = pxvar, y = pyvar) )+
          scale_x_continuous(limits = xscale )+
          scale_y_continuous(limits = yscale )+ 
          geom_bin2d( aes_string(x = pxvar, y = pyvar), binwidth = c(5, 20), na.rm = TRUE) 
          
          if(max(ggplot_build(g)$data[[1]]$count) == 1){
            g = g +scale_fill_gradientn(trans = "log10", colours="#0000FF")
          }else{
            g = g +scale_fill_gradientn(trans = "log10", colours=rev(rainbow(10, end = 4/6))) 
          } 
        
        g = g +
          labs(tag = paste0(sample, paste(rep(" ",maxchar_sample_value - nchar(sample) ),collapse = ""), "   " ))+
          geom_vline(xintercept = reactV$thresholds[reactV$thresholds$parameter == xvar,"value"], size = 1)+
          annotate(geom = "text", x = xmax, y = ymax, label = paste0(posEvent_count, " events\n", round(100*posEvent_freq,1),"%"),hjust =1, vjust = 1)+
          annotate(geom = "text", x = xmin, y = ymax, label = paste0(negEvent_count, " events\n", round(100*negEvent_freq,1),"%"),hjust =0, vjust = 1)+
          theme_bw() +
          theme(plot.tag.position = "left", plot.tag = element_text(family ="mono"), legend.position = "none" )
        
        #ggplot_build(g)$data[[1]]$count   min = #0000FF
        
        if(is.na( reactV$thresholds[reactV$thresholds$parameter == xvar,"second_value"]) == FALSE){
              xint = mean(c(reactV$thresholds[reactV$thresholds$parameter == xvar,"value"], reactV$thresholds[reactV$thresholds$parameter == xvar,"second_value"]))
              g = g + geom_vline(xintercept = reactV$thresholds[reactV$thresholds$parameter == xvar,"second_value"], size = 1, color = "red") +
                annotate(geom = "text", x = xint, y = ymax, label = paste0(intEvent_count, " events\n", round(100*intEvent_freq,1),"%"),hjust =0.5, vjust = 1)
        }
        
        
       # if(is.na( reactV$thresholds[reactV$thresholds$parameter == yvar,"second_value"]) == FALSE){
      #    g = g + geom_hline(yintercept = reactV$thresholds[reactV$thresholds$parameter == yvar,"second_value"], size = 0.5, color = "red")
      #  }
        

        if(i == 1) { g = g+ ggtitle(paste0("subpopulation:", subpop)) }
        if(i != length(unique(df$sample))) {g = g + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) }
        glist[[i]] = g
      }
      
      heights = rep(5,length(unique(df$sample)))
      heights[1] = heights[1]+1
      heights[length(unique(df$sample))] = heights[length(unique(df$sample))]+2
      
      reactV$dotplots = glist
      reactV$Dotplot_height = length(unique(rowData$sample))*150
      
      grid.arrange(grobs=glist,ncol = 1, heights=heights)
      
    }, height = function(){reactV$Dotplot_height})

    
    
    
    
    
    
    
    
  # Export dotplots ######
    
    
    output$dotplots_dropdown <- renderUI({
      dropdownButton(
        tags$h3("export parameters"),
        numericInput("dotplots_width", label = "width", value = 210),
        numericInput("dotplots_height", label = "height", value = reactV$Dotplot_height/5), 
        selectInput("dotplots_format", label = "format", 
                    choices = c("png",  "pdf"), 
                    selected = "png"),
        circle = FALSE, status = "default",
        icon = shiny::icon("cog"),
        tooltip = tooltipOptions(title = "export parameters")
      )
    })
    
    output$export_dotplots <- downloadHandler(
      filename = function() {
        if(input$dotplots_format == "png"){return(paste0("dotplots_",reactV$groups_names[2],"_vs_",reactV$groups_names[1],"_",format(Sys.time(), "%Y%m%d%H%M"), ".png"))}
        if(input$dotplots_format == "pdf"){return(paste0("dotplots_",reactV$groups_names[2],"_vs_",reactV$groups_names[1],"_",format(Sys.time(), "%Y%m%d%H%M"), ".pdf"))}
      },
      content = function(file) {
        ggsave(file, arrangeGrob(grobs =reactV$dotplots, ncol = 1), width = input$dotplots_width, height = input$dotplots_height, units = "mm" ) 
        
      }
    )
    
    
    
  # Output QC ###############

    
    
    output$table_info <- renderTable({
      volc_label_data_top()
    })    
    output$table2_info <- renderTable({
      volc_label_data$click
    })
    output$table3_info <- renderTable({
      volc_label_data$intersect
    })
    output$table4_info <- renderTable({
      volc_label_data_total()
    })
    
    
    
    observeEvent(input$browserbutton,{
      browser()
    })
    
    
    
    
    

    
}

# shinyApp------------

shinyApp(ui = ui, server = server)