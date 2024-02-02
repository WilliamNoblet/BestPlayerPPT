#Function Powerpoint Goalkeeper
#Create PowerPoint for Goalkeeper

CreatePPT <- function(Team = 'Argentina', Round = 7, CountryName = 'International', CompetitionName = 'FIFA World Cup', SeasonName = '2022'){ 
  
  #Packages
  if (!require("png")) install.packages("png")
  library(png)
  if (!require("plotly")) install.packages("plotly")
  library(plotly)
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  if (!require("htmlwidgets")) install.packages("htmlwidgets")
  library(htmlwidgets)
  if (!require("webshot")) install.packages("webshot")
  library(webshot)
  if (!require("StatsBombR")) install.packages("StatsBombR")
  library(StatsBombR)
  if (!require("officer")) install.packages("officer")
  library(officer)
  
  TeamMatches <- Matches %>%
    dplyr::filter(home_team.home_team_name == Team| 
                    away_team.away_team_name == Team) %>%
    dplyr::arrange(match_date)
  
  TeamMatchesNumber <- nrow(TeamMatches)
  
  IndexTeamMatches <- data.frame(index_match = (1:TeamMatchesNumber))
  
  TeamMatches <- TeamMatches %>%
    bind_cols(IndexTeamMatches) %>%
    dplyr::mutate(new_date = str_remove_all(match_date, '-'),
                  match_date_2 = match_date, 
                  kick_off_2 = kick_off) %>%
    separate(match_date_2, c('y', 'm', 'd'), sep = '-') %>%
    separate(kick_off_2, c('heure', 'minute', 'seconde', sep = '-'))
  
  Stadium <- TeamMatches %>%
    dplyr::filter(index_match == Round) %>% dplyr::select(stadium.name)
  
  Date <- TeamMatches %>%
    dplyr::filter(index_match == Round) %>% dplyr::select(new_date) 
  
  Hour <- TeamMatches %>%
    dplyr::filter(index_match == Round) %>% dplyr::select(heure)
  
  Minute <- TeamMatches %>%
    dplyr::filter(index_match == Round) %>% dplyr::select(minute)
  
  D <- TeamMatches %>%
    dplyr::filter(index_match == Round) %>% dplyr::select(d)
  
  M <- TeamMatches %>%
    dplyr::filter(index_match == Round) %>% dplyr::select(m)
  
  Y <- TeamMatches %>%
    dplyr::filter(index_match == Round) %>% dplyr::select(y)
  
  Stage <- TeamMatches %>% 
    dplyr::filter(index_match == Round) %>% dplyr::select(competition_stage.name)
  
  IndexMatches <- TeamMatches %>%
    select(match_id, index_match)
  
  ShotTeam <- StatsBombData %>%
    dplyr::filter(team.name == Team) %>%
    dplyr::left_join(IndexMatches, by = 'match_id') %>%
    dplyr::filter(type.name == 'Shot') %>%
    dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) %>%
    dplyr::filter(location.x >= 90) %>%
    dplyr::mutate(technique = '') %>%
    dplyr::mutate(formes = '') %>%
    dplyr::mutate(color = ifelse(shot.outcome.name == 'Goal', '#109618', '#dc3912')) %>%
    dplyr::mutate(formes = ifelse(shot.body_part.name == 'Head', 'circle', 'hexagon'))
  
  ShotTeam <- ShotTeam %>%
    dplyr::filter(index_match < Round)
  
  for(i in seq(from = 1, to = length(ShotTeam$id))){
    if
    (ShotTeam$shot.type.name[i] == 'Free Kick'){
      ShotTeam$technique[i] = 'Free Kick'
      ShotTeam$formes[i] = 'square'
    } else if (ShotTeam$shot.body_part.name[i] == 'Head'){
      ShotTeam$technique[i] = 'Head'
      ShotTeam$formes[i] = 'circle'
    } else {
      ShotTeam$technique[i] = 'Foot/Other'
      ShotTeam$formes[i] = 'hexagon'
    }
  }
  
  BestPlayer <- ShotTeam %>%
    dplyr::select(player.name, shot.statsbomb_xg) %>%
    group_by(player.name) %>%
    reframe(xG = sum(shot.statsbomb_xg, na.rm = TRUE),
            NbrShot = n()) %>%
    arrange(desc(xG))
  
  player_1 = paste0(BestPlayer[1,1])
  
  player_2 = paste0(BestPlayer[2, 1])
  
  player_3 = paste0(BestPlayer[3, 1])
  
  ShotPlayer1 <- ShotTeam %>%
    dplyr::filter(player.name == player_1) %>%
    dplyr::filter(type.name == 'Shot') %>%
    dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) 
  
  Shot1_tier1 <- ShotPlayer1 %>%
    dplyr::filter(location.y < 36) 
  
  Shot1_tier2 <- ShotPlayer1 %>%
    dplyr::filter(location.y >= 36 & location.y <= 44)
  
  Shot1_tier3 <- ShotPlayer1 %>%
    dplyr::filter(location.y > 44)
  
  ShotPlayer2 <- ShotTeam %>%
    dplyr::filter(player.name == player_2) %>%
    dplyr::filter(type.name == 'Shot') %>%
    dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) 
  
  Shot2_tier1 <- ShotPlayer2 %>%
    dplyr::filter(location.y < 36) 
  
  Shot2_tier2 <- ShotPlayer2 %>%
    dplyr::filter(location.y >= 36 & location.y <= 44)
  
  Shot2_tier3 <- ShotPlayer2 %>%
    dplyr::filter(location.y > 44)
  
  ShotPlayer3 <- ShotTeam %>%
    dplyr::filter(player.name == player_3) %>%
    dplyr::filter(type.name == 'Shot') %>%
    dplyr::filter(shot.type.name != 'Penalty' | is.na(shot.type.name)) 
  
  Shot3_tier1 <- ShotPlayer3 %>%
    dplyr::filter(location.y < 36) 
  
  Shot3_tier2 <- ShotPlayer3 %>%
    dplyr::filter(location.y >= 36 & location.y <= 44)
  
  Shot3_tier3 <- ShotPlayer3 %>%
    dplyr::filter(location.y > 44)
  
  #Function Map
  
  ShotMap <- function(dbase, team, player){
    shotmapxgcolors <- list(c(0, "#192780"), c(0.025, "#2a5d9f"), c(0.05, "#40a7d0"), c(0.075, "#87cdcf"), c(0.1, "#e7f8e6"), c(0.15, "#f4ef95"), c(0.2, "#fde960"),
                            c(0.3, "#f5b94d"), c(0.4, "#ed8a37"), c(0.6, "#d54f1b"), c(0.8, "#bf0000"), c(1, "#7f0000"))
    
    data_from <- dbase 
    
    Goal <- ({
      data_from %>% 
        dplyr::filter(shot.outcome.name == 'Goal')
    })
    
    Blocked <- ({
      data_from %>% 
        dplyr::filter(shot.outcome.name == 'Blocked')
    })
    
    OffT <- ({
      data_from %>% 
        dplyr::filter(shot.outcome.name == 'Off T')
    })
    
    Saved <- ({
      data_from %>% 
        dplyr::filter(shot.outcome.name == 'Saved')
    })
    
    expgoal <- ({
      data_from %>%
        select(shot.statsbomb_xg)
    })
    
    xg_goal <- round(sum(expgoal), digits = 2)
    
    shot_test <- count(dbase)
    
    goal <- ({
      data_from %>%
        filter(shot.outcome.name == 'Goal')
    })
    
    goal_test <- count(goal)
    
    plotly::plot_ly() %>%
      plotly::add_trace(y = c(70, 70, 120, 120, 70),
                        x = c(0, 80, 80, 0, 0),
                        mode = "lines",
                        line = list(color = "black", width = 1), type = 'scatter', showlegend = FALSE) %>% 
      layout(xaxis = list(range = c(0, 90), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = F),
             yaxis = list(range = c(59, 129), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = F)
      ) %>%
      plotly::add_trace(y = c(102, 120), x = c(62, 62), mode = "lines", line = list(color = "black", width = 1), type = 'scatter') %>%
      plotly::add_trace(y = c(102, 120), x = c(18, 18), mode = "lines", line = list(color = "black", width = 1), type = 'scatter') %>%
      plotly::add_trace(y = c(102, 102), x = c(18, 62), mode = "lines", line = list(color = "black", width = 1), type = 'scatter') %>%
      plotly::add_trace(y = c(114, 120), x = c(50, 50), mode = "lines", line = list(color = "black", width = 1), type = 'scatter') %>%
      plotly::add_trace(y = c(114, 120), x = c(30, 30), mode = "lines", line = list(color = "black", width = 1), type = 'scatter') %>%
      plotly::add_trace(y = c(114, 114), x = c(30, 50), mode = "lines", line = list(color = "black", width = 1), type = 'scatter') %>%
      plotly::add_trace(y = c(108), x = c(40), mode = "markers", marker = list(size = 5, color = "black"), type = 'scatter', showlegend = FALSE) %>%
      hide_legend() %>%
      add_paths(y=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), 
                x=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), color = I('black'), type = 'scatter', showlegend = FALSE) %>%
      plotly::add_trace(x = .5, y = 126, mode = 'text', type = 'scatter', text = player, textposition = 'middle right', textfont = list(color = 'red', size = 22)) %>%
      plotly::add_trace(x = .5, y = 123, mode = 'text', type = 'scatter', text = paste0(Team, ', ', CountryName), textposition = 'middle right', textfont = list(size = 15)) %>%
      plotly::add_trace(x = .5, y = 121, mode = 'text', type = 'scatter', text = paste0(CompetitionName, ' ', SeasonName), textposition = 'middle right', textfont = list(size = 15)) %>%
      plotly::add_trace(x = 80, y = 126, mode = 'text', type = 'scatter', text = paste0('Exp. goals: ', xg_goal, ' (', goal_test, ' goals /', shot_test, ' shots)')
                        , textposition = 'middle left' , textfont = list(size = 20)) %>%
      plotly::add_trace(data = Blocked, y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers',
                        showlegend = FALSE 
                        , marker = list(size = 25, color = 'LightGrey', opacity = 0.8, line = list(color = 'black', width = 1), symbol = ~formes
                                        
                        )
      ) %>%
      plotly::add_trace(data = OffT, y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers', 
                        showlegend = FALSE
                        , marker = list(size = 25, opacity = 0.8, line = list(color = 'black', width = 1), symbol = ~formes
                                        , color = ~shot.statsbomb_xg, colorscale = shotmapxgcolors, showscale = F
                                        , cmax = 1, cmin = 0
                        )
      ) %>%
      plotly::add_trace(data = Saved, y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers', 
                        showlegend = FALSE
                        , marker = list(size = 25, opacity = 0.8, line = list(color = 'black', width = 2), symbol = ~formes
                                        , color = ~shot.statsbomb_xg, colorscale = shotmapxgcolors, showscale = F
                                        , cmax = 1, cmin = 0
                        )
      ) %>%
      plotly::add_trace(data = Goal, y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers' 
                        ,showlegend = FALSE
                        , marker = list(size = 27, color = 'white', opacity = 1, line = list(color = 'black', width = 1), symbol = ~formes)) %>%
      plotly::add_trace(data = Goal, y = ~location.x, x = ~location.y, type = 'scatter', mode = 'markers' 
                        ,showlegend = T 
                        , marker = list(size = 22, opacity = 1, line = list(color = 'black', width = 1), symbol = ~formes
                                        , color = ~shot.statsbomb_xg, colorscale = shotmapxgcolors, showscale = F
                                        , cmax = 1, cmin = 0)) %>%
      plotly::add_trace(y = c(70, 70, 120, 120, 70),
                        x = c(0, 36, 36, 0, 0),
                        mode = "lines",
                        line = list(color = "#ffc000", width = 3), type = 'scatter', showlegend = FALSE) %>%
      plotly::add_trace(y = c(70, 70, 120, 120, 70),
                        x = c(44, 80, 80, 44, 44),
                        mode = "lines",
                        line = list(color = "#e50c46", width = 3), type = 'scatter', showlegend = FALSE) %>%
      plotly::add_trace(y = c(70, 70, 120, 120, 70),
                        x = c(36, 44, 44, 36, 36),
                        mode = "lines",
                        line = list(color = "#3f2b56", width = 3), type = 'scatter', showlegend = FALSE) 
  }
  
  shottraj <- function(data, color){
    
    ShotGoal <- data %>%
      dplyr::filter(shot.outcome.name == 'Goal')
    
    Shots <- data %>%
      dplyr::filter(shot.outcome.name != 'Goal')
    
    plotly::plot_ly() %>%
      plotly::add_trace(x = c(36, 36, 44, 44), y = c(0, 2.67, 2.67, 0), mode = "lines", line = list(color = "black", width = 5), type = 'scatter') %>%
      plotly::add_trace(x = c(33, 47), y = c(0, 0), mode = "lines", line = list(color = "black", width = 2), type = 'scatter') %>%
      layout(annotations = list(text = 'oui',
                                x = 20, y = 65, showarrow = FALSE),
             scene = list(aspectration = list(x = 1, y = 1)),
             xaxis = list(range = c(32, 48), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = F),
             yaxis = list(range = c(-1, 10), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = F)) %>%
      plotly::add_trace(x = c(32.5, 32.5, 47.5, 47.5, 32.5),
                        y = c(-0.5, 6, 6, -0.5, -0.5),
                        mode = "lines",
                        line = list(color = color, width = 2), type = 'scatter', showlegend = FALSE) %>%
      plotly::add_trace(data = Shots, x = ~shot.end_location.y, y = ~shot.end_location.z, type = 'scatter', mode = 'markers',
                        showlegend = FALSE
                        ,marker = list(size = ~(shot.statsbomb_xg * 70 + 5), color = '#dc3912', opacity = 0.6, line = list(color = '#dc3912'))) %>%
      plotly::add_trace(data = ShotGoal, x = ~shot.end_location.y, y = ~shot.end_location.z, type = 'scatter', mode = 'markers',
                        showlegend = FALSE
                        ,marker = list(size = ~(shot.statsbomb_xg * 70 + 5), color = '#109618', opacity = 0.6, line = list(color = '#109618'))) %>%
      hide_legend() 
  }
  
  shotmap1 <- ShotMap(ShotPlayer1, Team, player_1)
  shotmap2 <- ShotMap(ShotPlayer2, Team, player_2)
  shotmap3 <- ShotMap(ShotPlayer3, Team, player_3)
  
  shot1traj1 <- shottraj(Shot1_tier1, '#ffc000')
  shot2traj1 <- shottraj(Shot1_tier2, '#3f2b56')
  shot3traj1 <- shottraj(Shot1_tier3, '#e50c46')
  
  shot1traj2 <- shottraj(Shot2_tier1, '#ffc000')
  shot2traj2 <- shottraj(Shot2_tier2, '#3f2b56')
  shot3traj2 <- shottraj(Shot2_tier3, '#e50c46')
  
  shot1traj3 <- shottraj(Shot3_tier1, '#ffc000')
  shot2traj3 <- shottraj(Shot3_tier2, '#3f2b56')
  shot3traj3 <- shottraj(Shot3_tier3, '#e50c46')
  
  shotmap1_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shotmap1, file = "shotmap1.html")
  webshot::webshot("shotmap1.html", shotmap1_, zoom = 5)
  
  shotmap2_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shotmap2, file = "shotmap2.html")
  webshot::webshot("shotmap2.html", shotmap2_, zoom = 5)
  
  shotmap3_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shotmap3, file = "shotmap3.html")
  webshot::webshot("shotmap3.html", shotmap3_, zoom = 5)
  
  shot1traj1_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shot1traj1, file = "shot1traj1.html")
  webshot::webshot("shot1traj1.html", shot1traj1_, zoom = 5)
  
  shot2traj1_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shot2traj1, file = "shot2traj1.html")
  webshot::webshot("shot2traj1.html", shot2traj1_, zoom = 5)
  
  shot3traj1_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shot3traj1, file = "shot3traj1.html")
  webshot::webshot("shot3traj1.html", shot3traj1_, zoom = 5)
  
  shot1traj2_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shot1traj2, file = "shot1traj2.html")
  webshot::webshot("shot1traj2.html", shot1traj2_, zoom = 5)
  
  shot2traj2_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shot2traj2, file = "shot2traj2.html")
  webshot::webshot("shot2traj2.html", shot2traj2_, zoom = 5)
  
  shot3traj2_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shot3traj2, file = "shot3traj2.html")
  webshot::webshot("shot3traj2.html", shot3traj2_, zoom = 5)
  
  shot1traj3_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shot1traj3, file = "shot1traj3.html")
  webshot::webshot("shot1traj3.html", shot1traj3_, zoom = 5)
  
  shot2traj3_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shot2traj3, file = "shot2traj3.html")
  webshot::webshot("shot2traj3.html", shot2traj3_, zoom = 5)
  
  shot3traj3_ <- tempfile(pattern = "", fileext = ".png")
  htmlwidgets::saveWidget(shot3traj3, file = "shot3traj3.html")
  webshot::webshot("shot3traj3.html", shot3traj3_, zoom = 5)
  
  chemin_image <- paste0('C:/Users/User/OneDrive/Desktop/BestPlayerPPT/Logo/', Team, '.png')
  
  image <- png::readPNG(chemin_image)
  
  dpi_x <- 72  
  dpi_y <- 72  
  
  largeur_pouces <- dim(image)[2] / dpi_x
  hauteur_pouces <- dim(image)[1] / dpi_y
  
  multi <- 3.2 / hauteur_pouces
  
  new_width = largeur_pouces * multi
  
  new_left = (13.33 - new_width)/2
  
  my_ppt <- officer::read_pptx(path = 'C:/Users/User/OneDrive/Desktop/BestPlayerPPT/model_gk.pptx') #Open PowerPoint model
  
  my_ppt <- officer::add_slide(my_ppt,
                               layout = 'titreprepa',
                               master = 'Thème Office') %>%
    officer::ph_with(value = paste0(Stage, ' ', 'J.', Round, ' - ', Stadium, '   ', D, '/', M, '/', Y, ' - ', Hour, 'h', Minute),
                     location = officer::ph_location_label(ph_label = 'Soustitre')) %>%
    officer::ph_with(value = officer::external_img(src = paste0('C:/Users/User/OneDrive/Desktop/BestPlayerPPT/Logo/', Team, '.png')),
                     location = officer::ph_location(left = new_left, top = 1.85, width = new_width, height = 3.2))
  
  my_ppt <- officer::add_slide(my_ppt, 
                               layout = 'prepatirs',
                               master = 'Thème Office') %>%
    officer::ph_with(value = player_1,
                     location = officer::ph_location_label(ph_label = 'Titre 1')) %>%
    officer::ph_with(value = officer::external_img(shot1traj1_),
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 2')) %>%
    officer::ph_with(value = officer::external_img(shot2traj1_),
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 3')) %>%
    officer::ph_with(value = officer::external_img(shot3traj1_),
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 4')) %>%
    officer::ph_with(value = officer::external_img(shotmap1_), 
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 1')) 
  
  my_ppt <- officer::add_slide(my_ppt, 
                               layout = 'prepatirs',
                               master = 'Thème Office') %>%
    officer::ph_with(value = player_2,
                     location = officer::ph_location_label(ph_label = 'Titre 1')) %>%
    officer::ph_with(value = officer::external_img(shot1traj2_),
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 2')) %>%
    officer::ph_with(value = officer::external_img(shot2traj2_),
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 3')) %>%
    officer::ph_with(value = officer::external_img(shot3traj2_),
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 4')) %>%
    officer::ph_with(value = officer::external_img(shotmap2_), 
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 1'))
  
  my_ppt <- officer::add_slide(my_ppt, 
                               layout = 'prepatirs',
                               master = 'Thème Office') %>%
    officer::ph_with(value = player_3,
                     location = officer::ph_location_label(ph_label = 'Titre 1')) %>%
    officer::ph_with(value = officer::external_img(shot1traj3_),
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 2')) %>%
    officer::ph_with(value = officer::external_img(shot2traj3_),
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 3')) %>%
    officer::ph_with(value = officer::external_img(shot3traj3_),
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 4')) %>%
    officer::ph_with(value = officer::external_img(shotmap3_), 
                     location = officer::ph_location_label(ph_label = 'Espace réservé du texte 1'))
  
  print(my_ppt, target = paste0(Date, '_pregameGK_J', Round, '_',  Team, '.pptx'))
  
}

CreatePPT()

