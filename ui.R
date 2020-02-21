# Defining themes for graphs
themes <- c('theme_classic', 'theme_bw', 'theme_dark', 'theme_grey', 'theme_light', 'theme_linedraw', 'theme_minimal', 'theme_void')
fonts <- c('Calibri', 'Arial')

ui <- tagList(
  useShinyjs(),
  useShinyalert(),
  navbarPage(
    "Graphing App",
    
    # The input tab:
    # Inputs the raw data and the platemap
    # Define midlog values
    
    
    tabPanel("Input",
             fluidPage(
               fluidRow(
                 
                 # Inpt the machine
                 h3("Inputs:"),
                 column(
                   2,
                   selectInput(
                     'machine',
                     'Plate Reader:',
                     choices = c('', 'VersaMax', 'EnVision'),
                     selected = NULL,
                     multiple = F
                   )
                 ),
                 
                 # Input the raw data, requires a machine to be selected
                 conditionalPanel('output.machineInputted == true',
                                  column(
                                    3,
                                    fileInput(
                                      'data',
                                      'Upload plate reader output (*.csv):',
                                      multiple = FALSE,
                                      accept = c('.csv')
                                    )
                                  )
                 ),
                 
                 # Input the platemap, requires 
                 conditionalPanel('output.dataUploaded == true',
                                  column(
                                    2,
                                    fileInput(
                                      'map',
                                      'Upload plate map (*.csv):',
                                      multiple = FALSE,
                                      accept = c('.csv')
                                    )
                                  )
                 ),
                 
                 conditionalPanel(
                   "output.growthcurve == true && input.machine == 'EnVision' && output.mapUploaded == true",
                   column(
                     3,
                     numericInput(
                       'incriment',
                       'Input inciment between timepoints (min):',
                       value = 60,
                       min = 1
                     )
                   )
                 )
               ),
               
               # Defines the midlog values, either from a specific value or a CSV containing midlog values
               # Choosing whether to input value or CSV
               conditionalPanel(
                 "output.mapUploaded == true",
                 fluidRow(
                   h3("Midlog:"),
                   conditionalPanel("output.mapUploaded == true",
                                    column(
                                      3,
                                      checkboxInput('midlogcheck', 'Input csv for midlog:', value = F)
                                    )
                   ),
                   
                   # midlog value input
                   conditionalPanel(
                     "input.midlogcheck == false",
                     column(
                       3,
                       selectInput(
                         'factors',
                         'Select columns to calculate midlog values:',
                         'Please select column(s)',
                         multiple = T
                       )
                     ),
                     
                     conditionalPanel("input.factors !== null",
                                      column(
                                        3,
                                        numericInput(
                                          'midlogval',
                                          'Input midlog value:',
                                          value = 0.5,
                                          min = 0.3
                                        )
                                      )
                     )
                   ),
                   
                   # Inputting CSV of midlogs
                   conditionalPanel("input.midlogcheck == true",
                                    column(
                                      3,
                                      fileInput(
                                        'midlogcsv',
                                        'Input csv with midlog values:',
                                        multiple = FALSE,
                                        accept = c('.csv')
                                      )
                                    )
                   )
                 )
               )
             )
    ),
    
    # Tab containing all the data calculated from the raw data
    tabPanel("Data",
             fluidPage(
               fluidRow(
                 conditionalPanel("output.mapUploaded == true",
                                  column(
                                    10,
                                    tabsetPanel(
                                      type = "tabs",
                                      
                                      # Table with the fully processed data containing percentage growth means and SDs
                                      tabPanel('Growth', 
                                               dataTableOutput('df_growth')
                                      ),
                                      
                                      # Table with midlog times for every factor
                                      tabPanel('Midlogs',
                                               conditionalPanel(
                                                 "input.factors !== null || output.midlogsUploaded == true",
                                                 dataTableOutput('midlogs')
                                               )
                                      ),
                                      
                                      # Raw data inputted, not processed in any way
                                      tabPanel('Raw Data',
                                               tableOutput('raw')
                                      ),
                                      
                                      # Processed data that is ready to be run through calculations
                                      tabPanel('Formatted Data',
                                               DTOutput('linear')),
                                      
                                      # Zeroed data, will merge with processed data as this is redundent
                                      tabPanel('Zeroed Data',
                                               DTOutput('zero'))
                                    )
                                  )
                 )
               )
             )
    ),
    
    # Will allow plates to be viewed at each time point
    tabPanel("Plate View"),
    
    # Graphing tab to define data to graph and construct the graph
    tabPanel("Graphs",
             fluidPage(
               fluidRow(
                 conditionalPanel("output.growthCalculated == true",
                                  tabsetPanel(
                                    type = 'tabs',
                                    
                                    # Defining the data to use for the graph. Might find a better way to do this
                                    # as it's not ideal to split this and the graphing up
                                    tabPanel('Data Input',
                                             # Choose the type of graph to create
                                             fluidRow(
                                               column(2,
                                                      selectInput(
                                                        inputId = 'graphtype',
                                                        label = 'Select the type of graph:',
                                                        c('', 'Bar Graph', 'Dose Response'),
                                                        selected = NULL
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               conditionalPanel("output.growthCalculated == true",
                                                                column(5,
                                                                       h3('Growth Data:'),
                                                                       DTOutput('df_growth_graph')
                                                                ),
                                                                column(5,
                                                                       h3('Graph Data:'),
                                                                       'Note: Editing columns other than "Label" will cause "Label" to be updated',
                                                                       DTOutput('selected_rows')
                                                                )
                                               )
                                             )
                                    ),
                                    
                                    # Defining the graphing parameters
                                    tabPanel('Graphing',
                                             fluidRow(
                                               column(4,
                                                      conditionalPanel("output.growthCalculated == true",
                                                                       'Note: Editing columns other than "Label" will cause "Label" to be updated',
                                                                       DTOutput('df_graph')
                                                                       )
                                                      ),
                                               column(2, 
                                                      'Y Axis:',
                                                      textInput('ylab', 'Y axis label:', 'Percentage Growth'),
                                                      numericRangeInput('yaxis', 'Y axis range', value = c(0,120)),
                                                      'X Axis:',
                                                      textInput('xlab', 'X axis label:', value = ''),
                                                      numericInput('xangle', 'X axis label angle:', value = 45),
                                                      'Axis options:',
                                                      numericInput('axiswidth', 'Axis width:', value = 0.5, step = 0.1),
                                                      'Colour options:',
                                                      conditionalPanel("input.graphtype == 'Bar Graph'",
                                                                       colourInput('barcol', 'Bar colour', value = 'black'),
                                                                       colourInput('barout', 'Bar outline', value = 'black')
                                                                       ),
                                                      conditionalPanel("input.graphtype == 'Dose Response'",
                                                                       selectInput('pell_input', 'Choose colour pellete:', 
                                                                                   choices = row.names(brewer.pal.info), 
                                                                                   selected = 'Spectral'),
                                                                       uiOutput('multi_colour')
                                                                       # colourInput('pointcol', 'Point colour:', value = 'black')
                                                                       ),
                                                      colourInput('axiscol', 'Axis colour:', value = 'black'),
                                                      colourInput('textcol', 'Text colour:', value = 'black')
                                                      ),
                                               column(2,
                                                      'Text options',
                                                      numericInput('textsize', 'Text size:', value = 9),
                                                      selectInput('font', 'Font:', choices = fonts),
                                                      'Theme options:',
                                                      selectInput('theme', 'Theme:', choices = themes),
                                                      'Error Bar options',
                                                      numericInput('errwidth', 'Width:', value = 0.3, step = 0.1),
                                                      numericInput('errsize', 'Thickness:', value = 1),
                                                      conditionalPanel("input.graphtype == 'Bar Graph'",
                                                                       'Bar options:',
                                                                       numericInput('barwidth', 'Bar width:', value = 0.5)
                                                        ),
                                                      'Graph options:',
                                                      numericInput('graphwidth', 'Graph width:', value = 300),
                                                      numericInput('graphheight', 'Graph height:', value = 300)
                                                      ),
                                               column(4,
                                                      'Graph:',
                                                      plotOutput('graph_render'))
                                             )
                                    )
                                  )
                 )
               )
             )
    ),
    
    # Tab containign a help document to help use the software
    tabPanel("Help")
  )
)