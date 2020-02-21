server <- function(input, output, session){
  # Defining plate reader conditional input
  {
    machine <- reactive({
      if (nchar(input$machine) < 1){
        return(NULL)
      }
      else{
        return(input$machine)
      }
    })
    
    output$machineInputted <- reactive({
      return(!is.null(machine()))
    })
    
    outputOptions(output, 'machineInputted', suspendWhenHidden = FALSE)
  }
  
  # Defining data conditional input
  {
    df <- reactive({
      # if (is.null(input$data)){
      #   return(NULL)
      # }
      req(input$data)
      # else{
        if (endsWith(input$data$datapath, '.csv')){
          if (input$machine == 'VersaMax'){
            df <- read.csv(input$data$datapath)
            if (ncol(df) > 4){
              machine_test1 <- as.character(df[1,4])
              machine_test2 <- as.character(df[1,1])
              machine_string1 <- as.character("PlateFormat")
              machine_string2 <- as.character('Plate:')
              if (machine_string1 == machine_test1 & machine_string2 == machine_test2){
                return(df)
              }
              else{
                shinyalert('Wrong input!', 'Please input a raw VersaMax output', type = 'error')
                return(NULL)
              }
            }
            else{
              shinyalert('Wrong input!', 'Please input a raw VersaMax output', type = 'error')
              return(NULL)
            }
          }
          else if (input$machine == 'EnVision'){
            df <- read.csv(input$data$datapath)
            if ('EnVision' %in% df[,1] | 'EnVision' %in% df[,2] & 'Instrument nickname' %in% df[,1] | 'Instrument nickname' %in% df[,2]){
              return(df)
            }
            else{
              shinyalert('Wrong input!', 'Please input a raw EnVision output', type = 'error')
              return(NULL)
            }
          }
        }
        else{
          shinyalert('Wrong input!', 'Please input a .csv file!', type = 'error')
          return(NULL)
        }
      # }
    })
    
    output$dataUploaded <- reactive({
      return(!is.null(df()))
    })
    
    outputOptions(output, 'dataUploaded', suspendWhenHidden=FALSE)
    
    # Determining whether the EnVision input is growth curve or endpoint
    df_timeseries <- reactive({
      req(input$map)
        if (input$machine == 'EnVision'){
          df <- df()
          df_cols <- ncol(data.frame(df[, grep("Plt", colnames(df))]))
          if (df_cols > 1){
            return(TRUE)
          }
          else{
            return(NULL)
          }
        }
        else{
          return(NULL)
        }
    })
    
    output$growthcurve <- reactive({
      return(!is.null(df_timeseries()))
    })
    
    outputOptions(output, 'growthcurve', suspendWhenHidden=FALSE)
    
    # Rendering table for output
    output$raw <- renderTable({
      df()
    })
  }
  
  # Defining conditional plate map input
  {
    Map <- reactive({
        req(input$map)
        if (endsWith(input$map$datapath, '.csv')){
          df <- read.csv(input$map$datapath)
          if (all(c('Plate', 'Row', 'Column', 'Concentration', 'Unit') %in% names(df))){
            return(df)
          }
          else{
            shinyalert('Wrong input!', 'Please input a correct plate map file!', type = 'error')
            return(NULL)
          }
        }
        else{
          shinyalert('Wrong input!', 'Please input a .csv file!', type = 'error')
          return(NULL)
        }
    })
    
    output$mapUploaded <- reactive({
      return(!is.null(Map()))
    })
    
    outputOptions(output, 'mapUploaded', suspendWhenHidden=FALSE)
  }
  
  # Defining conditional incriment input
  {
    incriment <- reactive({
      inc <- input$incriment
      return(inc)
    })
    
    output$incrimentInputted <- reactive({
      return(!is.null(incriment))
    })
    
    outputOptions(output, 'incrimentInputted', suspendWhenHidden=FALSE)
  }
  
  # Organising raw input into single columned data frame (one OD per row)
  
  {
    linear_df <- reactive({
      req(input$map)
        df <- df()
        map <- Map()
        if (input$machine == 'VersaMax'){
          df_formatted <- versa(df)
          df_formatted <- merge(df_formatted, map, by = c('Plate', 'Column', 'Row'))
          return(df_formatted)
        }
    })
    
    # Rendering table for output
    output$linear <- renderDataTable({
      DT::datatable(linear_df(), extensions = 'Scroller', options = list(
        pageLength = 15,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE))
    })
  }
  
  {
    # Zeroing the input data
    df_zero <- reactive({
      req(input$map)
        df <- linear_df()
        df_zero <- zero(df)
        return(df_zero)
    })
    
    # Rendering table for output
    output$zero <- renderDataTable({
      df_zero()
    })
  }
  
  # Defining conditional midlog csv input
  {
    midlog_csv <- reactive({
      if (input$midlogcheck == T){
        req(input$midlogcsv)
          df <- read.csv(input$midlogcsv$datapath)
          map <- Map()
          midlog_check_df <- subset(df, select = -Midlog)
          midlog_check_cols <- colnames(midlog_check_df)
          if(all(midlog_check_cols %in% colnames(map))){
            midlog_check <- subset(map, select = midlog_check_cols)
            midlog_check <- unique(midlog_check)
            differences <- setdiff(midlog_check, midlog_check_df)
            if (nrow(differences) < 1){
              return(df)
            }
            else{
              shinyalert('Midlog file and plate map do not match!', 'Please check your midlog variable names!', type = 'error')
            }
          }
          else{
            shinyalert('Wrong column headers!', 'Please check your midlog column headers. At least one is wrong!', type = 'error')
            return(NULL)
          }
      }
    })
    
    output$midlogsUploaded <- reactive({
      return(!is.null(midlog_csv()))
    })
    
    outputOptions(output, 'midlogsUploaded', suspendWhenHidden=FALSE)
  }
  
  # Updating the factors to select for calculating midlogs
  observeEvent(input$map, {
    req(input$map)
      if (input$midlogcheck == F){
        df <- Map()
        df <- subset(df, select = -c(Plate, Column, Row, Unit, Concentration))
        df_cols <- colnames(df)
        updateSelectInput(session, "factors", choices = df_cols, label = 'Select columns to calculate midlog values:')
      }
  })
  
  # Defining factors for calculating growth
  {
    factors_midlog <- reactive({
      if (input$midlogcheck == T){
        req(input$midlogcsv)
          factors <- midlog_csv()
          factors <- subset(factors, select = -Midlog)
          factors <- colnames(factors)
          return(factors)
      }
      else if (input$midlogcheck == F){
        if (input$factors != ''){
          factors <- input$factors
          return(factors)
        }
        else{
          return(NULL)
        }
      }
      else{
        return(NULL)
      }
    })
  }
  
  # Defining all factors for graphing
  {
    factors_all <- reactive({
      req(input$map)
        df <- Map()
        cols_df <- colnames(subset(df, select = -c(Plate, Column, Row, Concentration, Unit)))
        return(cols_df)
    })
  }
  
  # Calculating midlog times for each variable
  {
    midlog_times <- reactive({
      req(factors_midlog())
        if (input$midlogcheck == T){
          midlogs <- midlog_csv()
          factors <- factors_midlog()
          df <- df_zero()
          midlog_times <- midlog(df, factors, control = 'Control', midlogs)
          return(midlog_times)
        }
        else if (input$midlogcheck == F){
          midlogs <- Map()
          factors <- factors_midlog()
          midlogs <- subset(midlogs, select = factors)
          midlogs$Midlog <- input$midlogval
          midlogs <- unique(midlogs)
          df <- df_zero()
          midlog_times <- midlog(df, factors, control = 'Control', midlogs)
          return(midlog_times)
        }
    })
    
    # Rendering table for output
    output$midlogs <- renderDataTable({
      if (!is.null(midlog_times())){
        midlog_times()
      }
      else{
        shinyalert('Input midlog values!', 'Please check your midlog values!', type = 'error')
      }
    })
  }
  
  # Calculating the percentage growth
  {
    growth <- reactive({
      req(midlog_times)
        df <- df_zero()
        midlogs <- midlog_times()
        map <- Map()
        factors <- factors_midlog()
        factors_map <- merge(midlogs,  map, by = factors)
        factors_map <- subset(factors_map, select = -c(Plate, Column, Row, Unit, Concentration))
        factors <- subset(map, select = -c(Plate, Column, Row, Unit, Concentration))
        factors <- colnames(factors)
        growth <- res_mid(df, factors, factors_map, 'Control')
        return(growth)
    })
    
    output$growthCalculated <- reactive({
      req(input$factors)
        return(!is.null(growth()))
    })
    
    outputOptions(output, 'growthCalculated', suspendWhenHidden = FALSE)
    
    # Rendering table for output
    output$df_growth <- renderDataTable({
      req(Map())
      req(growth())
          growth()
    })
  }
  
  # Creating a master data frame for graphing based on graph type selected
  {
    df_master <- reactive({
      req(growth())
      if (input$graphtype == 'Bar Graph'){
        df <- growth()
        rownames(df) <- 1:nrow(df) # so that the data table can be reliably edited
        df$Label <- ''
        return(df)
      }
      else if(input$graphtype == 'Dose Response'){
        df <- growth()
        df <- unique(subset(df, select = factors_all()))
        rownames(df) <- 1:nrow(df)
        df$Label <- ''
        return(df)
      }
      else{
        return(NULL)
      }
    })
  }
  
  # Creating data tables for graphing data
  {
    # Rendering second table for graph tab
    output$df_growth_graph <- renderDT({
      req(df_master())
        if (input$graphtype == 'Bar Graph'){
          DT::datatable(df_master(), options = list(pageLength = 10), rownames = F)
        }
      else if (input$graphtype == 'Dose Response'){
        DT::datatable(df_master(), options = list(pageLength = 10), rownames = F)
      }
    })
  }
  
  # Rendering table for selected frows from df_growth_graph on the graphing tab
  {
    # Subsetting selected rows into a reactive value
    selected_rows <- reactive({
      req(df_master())
          if (length(input$df_growth_graph_rows_selected) > 0){
            df <- reactab$df
            df_output <- df[input$df_growth_graph_rows_selected, ]
            return(df_output)
          }
          else{
            df <- reactab$df
            df_cols <- colnames(df)
            df_output <- data.frame(matrix(ncol = ncol(df), nrow = 0))
            colnames(df_output) <- df_cols
            return(df_output)
          }
    })
    
    # Rendering table of the selected rows from df_growth_graph
    output$selected_rows <- renderDT({
      req(df_master())
          if (nrow(selected_rows()) > 0){
            DT::datatable(selected_rows(), editable = 'Column', rownames = F)
          }
          else{
            return(NULL)
          }
    })
    
    # Redndering the selected rows for the graphing tab
      df_graph <- reactive({
        req(df_master())
          df <- reactab$df
          df <- df[input$df_growth_graph_rows_selected,]
          # df <- subset(df, select = c(Label, Mean, SD))
          return(df)
      })
      
      # Rendering table of the selected rows on the graphing tab
      output$df_graph <- renderDT({
        req(growth())
        if (input$graphtype == 'Bar Graph' || input$graphtype == 'Dose Response'){
          if (nrow(df_graph()) > 0){
            DT::datatable(df_graph(), editable = T, rownames = F, style = 'bootstrap', 
                          extensions = 'RowReorder', 
                          options = list(rowReorder = TRUE, order = list(c(0 , 'asc'))))
          }
          else{
            return(NULL)
          }
        }
      })
  }

  # Updating edits of the selected rows
  {
    # creating editable data frames from df_master
    reactab <- reactiveValues(df = NULL)
    
    observe({
      if (input$graphtype == 'Bar Graph' || input$graphtype == 'Dose Response'){
        if (!is.null(df_master())){
          reactab$df <- df_master()
        }
      }
    })
    
    # Editing the data frame when edited by user on the data input tab (Graph)
    observeEvent(input$selected_rows_cell_edit, {
      # Required data to edit cells
      df_selected <- selected_rows()
      df_edit <- reactab$df
      # Info required to edit cells
      info <- input$selected_rows_cell_edit
      i <- as.numeric(
        row.names(
          df_selected[info$row,]
          )
        )
      j <- which(colnames(df_edit)=="Label" )
      v <- info$value
      df_edit[i,j] <- v
      reactab$df <- df_edit
    })
    
    # Editing the data frame when edited by user on the graphing tab (Graph)
    observeEvent(input$df_graph_cell_edit, {
      # Required data to edit cells
      df_selected <- df_graph()
      df_edit <- reactab$df
      # Info required to edit cells
      info <- input$df_graph_cell_edit
      i <- as.numeric(
        row.names(
          df_selected[info$row,]
        )
      )
      j <- which( colnames(df_edit)=="Label" )
      v <- info$value
      df_edit[i,j] <- v
      reactab$df <- df_edit
    })
  }
  
  # Getting row order for graping tab table
  {
    row_order <- eventReactive(input$df_graph_rows_all, {
      rows_filtered <- input$df_graph_rows_all
      rows_displayed <- rows_filtered[1:min(length(rows_filtered), input$df_graph_state$length)]
      return(rows_displayed)
    })
  }
  
  # Getting colours for multicoloured graphs
  {
    output$multi_colour <- renderUI({
      req(df_graph())
      output <- tagList()
      df <- df_graph()
      pellete <- brewer.pal(nrow(df), input$pell_input)
      print(df)
      for (row in 1:nrow(df)){
        df_row <- df[row,]
        label <- as.character(df_row$Label)
        output[[row]] <- colourInput(label,
                                     paste(df_row$Label, ' colour:'),
                                     pellete[row])
      }
      output
    })
    
    colour_table <- reactive({
      req(df_graph())
      df <- req(df_graph())
      labels <- unique(df$Label)
      df_colour <- data.frame(matrix(nrow = 0, ncol = 2))
      colnames(df_colour) <- c('Label', 'Colour')
      for(label in labels){
        df_label <- data.frame(Label = label, 
                               Colour = input[[label]])
        df_colour <- rbind(df_colour, df_label)
      }
      print(df_colour)
      return(df_colour)
    })
  }
  
  # Plotting the graph for display
  {
    graph <- reactive({
      theme_input <- match.fun(input$theme)
      if (input$graphtype == 'Bar Graph'){
        row_order <- as.factor(row_order())
        df <- df_graph()
        df <- df[row_order,]
        order <- as.factor(df$Label)
        # df %>% rowid_to_column(var = "rowid")
        graph <-  ggplot(df, aes(x = Label, y = Mean, ymin = Mean, ymax  = Mean + SD)) +
          geom_errorbar(stat = 'identity', width = input$errwidth, size = input$errsize) +
          geom_bar(stat = 'identity', color = input$barout, fill = input$barcol, width = input$barwidth) +
          ylab(input$ylab) +
          xlab(input$xlab) +
          scale_y_continuous(limits = c(-1000, 1000),
                             expand = c(0,0)) +
          scale_x_discrete(limits = order) +
          coord_cartesian(ylim = c(input$yaxis[1], input$yaxis[2])) +
          theme_input() +
          theme(
            axis.title.x = element_text(size = input$textsize, colour = input$textcol, family = input$font),
            axis.title.y = element_text(size = input$textsize, colour = input$textcol, family = input$font),
            axis.text.x = element_text(size = input$textsize, colour = input$textcol, angle = input$xangle, family = input$font),
            axis.text.y = element_text(size = input$textsize, colour = input$textcol, family = input$font),
            axis.line = element_line(colour = input$axiscol, size = input$axiswidth),
            axis.ticks = element_line(colour = input$axiscol, size = input$axiswidth),
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.minor.y = element_blank()
          )
        return(graph)
      }
      else if (input$graphtype == 'Dose Response'){
        # Calling data and generating curve
        selected_rows <- reactab$df
        selected_rows <- selected_rows[input$df_growth_graph_rows_selected,]
        df_curve <- df_zero()
        df_curve <- merge(df_curve, selected_rows, by = factors_all())
        factors <- input$factors
        midlogs <- midlog_times()
        curve <- dose_curve_mid(df_curve, factors, factors_all(), midlogs, control = 'Control', model = 'LL.4')
        curve <- merge(curve, selected_rows, by = factors_all())
        
        # Calling data for the graph
        df <- growth()
        df <- merge(df, selected_rows, by = factors_all())
        
        # Calling colour settings
        colour_table <- colour_table()
        Labels <- as.character(colour_table$Label)
        colours <- as.character(colour_table$Colour)
        
        # Rendering graph
        graph <- ggplot() +
          geom_point(data = df, aes(Concentration, Mean, color = Label)) +
          geom_line(data = curve, aes(x = Concentration,y = Curve, color = Label)) +
          geom_errorbar(data = df,aes(x = Concentration, ymin = Mean - SD, ymax = Mean + SD, color = Label), linetype = 'solid', width = input$errwidth) +
          scale_x_log10() +
          scale_y_continuous(limits = c(-1000, 1000),
                             expand = c(0,0)) +
          coord_cartesian(ylim = c(input$yaxis[1], input$yaxis[2])) +
          ylab(input$ylab) +
          xlab(input$xlab) +
          scale_color_manual(labels = Labels,
                            values = colours) +
          theme_input() +
          theme(
            axis.title.x = element_text(size = input$textsize, colour = input$textcol, family = input$font),
            axis.title.y = element_text(size = input$textsize, colour = input$textcol, family = input$font),
            axis.text.x = element_text(size = input$textsize, colour = input$textcol, angle = input$xangle, family = input$font),
            axis.text.y = element_text(size = input$textsize, colour = input$textcol, family = input$font),
            axis.line = element_line(colour = input$axiscol, size = input$axiswidth),
            axis.ticks = element_line(colour = input$axiscol, size = input$axiswidth),
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.minor.y = element_blank()
          )
        return(graph)
      }
    })
    
    # Rendering the plot for UI output
    output$graph_render <- renderPlot({
      plot(graph())
        # g <- ggplotly(g_graphtab(), autosize=FALSE, width = input$graphwidth, height = input$graphheight, tooltip = list(textfont = input$font))
    },
    height = function(){
      input$graphheight
    },
    width = function(){
      input$graphwidth
    })
    
    # UI output to render graph with user specified height and width.
    # Can't have reactive inputs in renderPlot
    # output$g_graphtab <- renderUI({
    #   if (!is.null(graph)){
    #     plotOutput('graph_render', height = 400, width = 400)
    #   }
    # })
    
  }
}