library(shiny)
library(bslib)
library(rlang)
library(tidyverse)
library(plotly)
library(thematic)

#setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path)) 

thematic_shiny(font="auto")

#### Data To Use ####

files <- list.files(path='./Data', pattern='^goalies', full.names = TRUE)
data <- files %>%
  map_df(read_csv, show_col_types = FALSE)

data <- data %>%
  filter(situation=='all',
         games_played >20) %>%
  select(season, name, team, goals, icetime, ongoal, xGoals) %>%
  mutate(season = as.integer(season),
         GAA = as.numeric((goals*3600)/(icetime)),
         sv_p = as.numeric(1-goals/ongoal),
         xvG_delta = as.numeric(xGoals - goals)) %>%
  group_by(season) %>%
  mutate(GAA_rank = rank(GAA, ties.method='first'),
         sv_p_rank = rank(desc(sv_p), ties.method='first'),
         team = recode(team,
                       'T.B' = 'TBL',
                       'S.J' = 'SJS',
                       'N.J' = 'NJD',
                       'L.A' = 'LAK')
  )

team_cols <- read_csv('./Data/team_colours.csv', comment = '')

dat.plot <- merge(data, team_cols, by='team') %>%
  mutate(colour ='grey') %>%
  ungroup()

#### ####

#dat.plot


#### ui ##### 

ui <- fluidPage(
  theme = bs_theme(preset='simplex'),
  titlePanel(title = h1('NHL Goalie Comparison'),
             windowTitle = 'NHL Goalie Comparison'),
  sidebarLayout(
    sidebarPanel(
      selectInput('drop_metric',
                  label = 'Select Metric',
                  choices = c('GAA' = 'GAA', 'Save %' = 'sv_p'),
                  selected = ('Save %')
                  ),
      
      selectInput('drop_team',
                  label = 'Filter by Team',
                  choices = c('None', sort(unique(data$team))),
                  selected = 'None'
                  ),
      
      sliderInput('year_slider',
                  label = 'Filter Years',
                  min = min(dat.plot$season),
                  max = max(dat.plot$season),
                  value = c(min(dat.plot$season), max(dat.plot$season)),
                  step = 1,
                  sep = ""),
      helpText('An analysis of all goalies that played 20 or more games in the NHL starting with the 2008/2009 season. Data is current up to February 23, 2026 and sourced from MoneyPuck.')
      ),
  
    mainPanel(
      card(min_height = '120px',
           h3("Top Dawg"),
           textOutput('best_goalie')),
      
      
      card(min_height = '400px',
           h3(textOutput('goalie_header')),
           plotlyOutput('sv_v_gaa_plot'),
           card_footer('Black lines represent the median values for the selected range. Grey points represent all goalies in the selected date range. Hover for tooltips.')),
      
      card(min_height = '200px',
           h3(textOutput('goalie_table_header')),
           tableOutput('goalie_table'))
    )
  )
)


#### server #####

server <- function(input, output) {
  
  metricInput <- reactive({
    switch(input$drop_metric,
           'GAA' = 'GAA',
           'sv_p' = 'Save %'
    )
  })
  
  metricFun <- reactive({
    switch(input$drop_metric,
           'GAA' = 'min',
           'sv_p' = 'max')
  })
  
  dataInput <- reactive({
    input$drop_team
  })
  
  filteredTeam <- reactive({
    filter(dat.plot, between(season, input$year_slider[1], input$year_slider[2]))
  })
  
  teamInput <- reactive({
           filter(filteredTeam(), team %in% dataInput())
  })

      
  
  goalie_disp <- reactive({
    
    
    fun <- match.fun(metricFun())
    
    disp_goalie <- if (dataInput()=='None') {
      filteredTeam()
    } else {
      teamInput()
    }
    
    x <- (nrow(teamInput())==0 && dataInput()!='None')
    disp_goalie <- 
      if (x) {
        disp_goalie[0,]
      } else {
        filter(disp_goalie, .data[[input$drop_metric]]==fun(.data[[input$drop_metric]]))
      }


  })
  
  metric_disp <- reactive({
    round(select(goalie_disp(),input$drop_metric),3)
    
  })
  
  output$goalie_header <- renderText({
    ifelse(nrow(teamInput())==0 & dataInput()!='None',
           'No data for selected filters, these are all the goalies during this period',
            ifelse(dataInput()=='None',
                   'Goalie data for all teams',
                   paste('Goalie data for the ',unique(teamInput()$full_name))
            )               
    )
    
  })
  
  output$best_goalie <- renderText({
    ifelse(nrow(teamInput())==0 & dataInput()!='None',
           'No data for selected filters',
           ifelse(dataInput()=='None',
                  paste0("You haven't selected a team, but the best overall goalie is ", goalie_disp()$name, ' in the year ', goalie_disp()$season, ' with a ', metricInput(), ' of ', metric_disp() ,  '.'),
                  paste0('You have selected the ', unique(teamInput()$full_name), '.   ', 'Their best goalie is ', goalie_disp()$name, ' in the year ', goalie_disp()$season, ' with a ', metricInput(), ' of ', metric_disp() ,  '.')
      )
    )
  })
  
  output$goalie_table_header <- renderText({
    ifelse(nrow(teamInput())==0 & dataInput()!='None',
           'No data for selected filters',
            ifelse(dataInput()=='None',
                   paste0("Goalie data for all teams", " sorted by ", metricInput()),
                   paste0("Goalie data for the ", unique(teamInput()$full_name), " sorted by ", metricInput())
            )
    )
  })
  

  
  output$team_sub <- renderTable({
    teamInput()
  })
  
  
  
  output$sv_v_gaa_plot <- renderPlotly({
    
    p <- ggplot(filteredTeam(), aes(x=sv_p, y=GAA,
                               text = paste(name, '|', team, '|', season, '\nGAA:', round(GAA,2), '\nSave %', round(sv_p,3) ) ) ) +
      theme_minimal(base_size=14)+
      theme(axis.text = element_text(size=12),
            plot.background = element_rect(fill='transparent'),
            panel.background = element_rect(fill='transparent')) +
      geom_hline(aes(yintercept=median(GAA))) +
      geom_vline(aes(xintercept=median(sv_p))) +
      geom_point(size=2, shape=21, colour='grey') +
      geom_point(data = teamInput(), size=2, shape=21, stroke=1, aes(colour=col_code)) +
      geom_point(data = goalie_disp(), size=2, shape=21, stroke=1.25, colour='gold') +
      
      #annotate('text', x=0.95, y=median(dat.plot$GAA)*1.1, label=dataInput()) +
      scale_colour_identity() +
      labs(x='Save Percentage (%)', y='Goals Against Average')
    
    ggplotly(p, tooltip='text')
  })
  
  output$goalie_table <- renderTable({
    
    selection <- c('name', 'team', 'season', 'GAA', 'sv_p' )
    col_names <- c('Name', 'Team', 'Season', 'GAA', 'Save %')
    
    
    fun2 <- match.fun(paste0('slice_', metricFun()))
    
    
    tab_int <- if (dataInput() =='None') {
      filteredTeam()
    } else {
      teamInput() 
    }
    
    #fun2(tab_show, get(input$drop_metric), n=5)
    tab_show <- tab_int %>%
      select(selection) %>%
      group_modify(~fun2(., get(input$drop_metric), n=5)) %>%
      set_names(col_names)
    
  }, digits=3)

}

#### app ###

shinyApp(ui=ui, server=server)

