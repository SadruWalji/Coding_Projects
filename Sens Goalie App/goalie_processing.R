library(tidyverse)

#setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path)) 

#renv::init()
#renv::snapshot()

files <- list.files(path='./Data', pattern='^goalies', full.names = TRUE)
data <- files %>% 
  map_df(read_csv)

data <- data %>%
  filter(situation=='all',
         games_played >20) %>%
  select(season, name, team, goals, icetime, ongoal, xGoals) %>%
  mutate(season = as.integer(season),
         GAA = as.numeric((goals*3600)/(icetime)),
         sv_p = as.numeric(1-goals/ongoal),
         xvG_delta = as.numeric(xGoals - goals)) %>%
  group_by(season) %>%
  mutate(GAA_rank = rank(desc(GAA), ties.method='first'),
         sv_p_rank = rank(desc(sv_p), ties.method='first'),
         team = recode(team,
                               'T.B' = 'TBL',
                               'S.J' = 'SJS',
                               'N.J' = 'NJD', 
                               'L.A' = 'LAK')
  )
         

recent <- filter(data, season == max(data$season) & team == 'BOS')


test2 <- ifelse(nrow(recent) == 0, 'No Data', 
  'Some Data')

x <- ((nrow(recent) == 0 && recent$team != 'None'))


team_cols <- read_csv('./Data/team_colours.csv', comment = '')

dat.plot <- merge(data, team_cols, by='team') %>% 
  ungroup()

t1 = 'max'
fun3 <- (paste0('slice_',t1))

dat2 <- slice_head(dat.plot, n=5)

test1 <- fun2(dat2, order_by=get(metric), n=5)
  


p.sv_gaa <- ggplot(dat.plot, aes(x=sv_p, y=GAA)) +
  theme_minimal() +
  geom_hline(aes(yintercept=median(GAA))) +
  geom_vline(aes(xintercept=median(sv_p))) +
  geom_point(shape=21, colour='grey') +
  #geom_point(data = highlight, shape=21, aes(colour=col_code)) +
  scale_colour_identity() +
  labs(x='Save Percentage (%)', y='Goals Against Average')
p.sv_gaa

# 
# 
highlight <- 'BUF'

goalie_check <-  filter(dat.plot, team %in% 'BUF')

metric <- 'GAA'
metric2 <- max(goalie_check[[metric]])

test2 <- filter(goalie_check, .data[[metric]] == max(.data[[metric]]))
# 
# if (highlight == 'None') {
#   test2<-dat.plot
# } else {
#     test2<-goalie_check
#     }
# 
# choices = c('None', unique(data$team))

