#install.packages('DT')
library('DT')
library('ggplot2')
library('shiny')
library('bslib')
library('tidyr')
library('dplyr')
library('ggplot2')
library('htmlwidgets')
library('reshape2')
library('ggtext')
library('googlesheets4')
library('lubridate')
library('formattable')


# Authenticating connection to Google Sheets using auth file in '.secrets' (uploaded wtih app.R)...
gs4_auth(email = "tackley@gmail.com", cache = ".secrets")

# Load data (from Google Sheets)
match_table<- read_sheet("https://docs.google.com/spreadsheets/d/1Rv-7w5ddibSRMVnzR_DzI522nsj-nYV9euayV_oiIfM/edit?usp=sharing")
google_rank_table<-read_sheet("https://docs.google.com/spreadsheets/d/1IyZ6sbEGs1md9_MZKTMDuuHnOlWu0HXQJXAeleUJ2z4/edit?usp=sharing")

# Postgres example
#postgresql://postgres:[YOUR-PASSWORD]@db.bnnisnnqvsghpyktijal.supabase.co:5432/postgres
# install.packages(c("DBI","RPostgres"))  # run once if needed
library(DBI)
library(RPostgres)

# Option 1: single DSN from env
# set in R session (not global to OS)
#Sys.setenv(SUPABASE_DB_HOST = "db.bnnisnnqvsghpyktijal.supabase.co")
Sys.setenv(SUPABASE_DB_HOST = "aws-1-eu-west-1.pooler.supabase.com")
Sys.setenv(SUPABASE_DB_PORT = "5432")
Sys.setenv(SUPABASE_DB_NAME = "postgres")
#Sys.setenv(SUPABASE_DB_USER = "postgres")
Sys.setenv(SUPABASE_DB_USER = "postgres.bnnisnnqvsghpyktijal")
Sys.setenv(SUPABASE_DB_PASS = Sys.getenv("SUPABASE_PW"))   # Password stored on Connect Cloud: Admin/Settings > Variables

dsn <- Sys.getenv("SUPABASE_DB_DSN", unset = "")
if (nzchar(dsn)) {
  con <- dbConnect(RPostgres::Postgres(), dsn = dsn)
} else {
  # Option 2: components from env
  host <- Sys.getenv("SUPABASE_DB_HOST", "db.bnnisnnqvsghpyktijal.supabase.co")
  port <- as.integer(Sys.getenv("SUPABASE_DB_PORT", "5432"))
  dbname <- Sys.getenv("SUPABASE_DB_NAME", "postgres")
  user <- Sys.getenv("SUPABASE_DB_USER", "postgres")
  password <- Sys.getenv("SUPABASE_DB_PASS")
  if (!nzchar(password)) stop("Set SUPABASE_DB_PASS environment variable")
  
  con <- dbConnect(
    RPostgres::Postgres(),
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password,
    sslmode = "require"
  )
}

# Quick test
print(dbGetQuery(con, "SELECT current_database() AS db, current_user AS user, inet_server_addr() AS server_ip;"))
# 1) List available tables (schema-qualified)
print(dbListTables(con))           # lists tables in the search_path


# Functions:
makeStatTable<-function(stat_data){
  if(nrow(stat_data)==0){
    empty_table<-data.frame(c("No data to show - Please try other combinations."))
    colnames(empty_table)<-"Empty table!"
    return(empty_table)
    } else {
      tmp_player_list<-unique(stat_data$ID)
      tmp_stats_table<-data.frame(ID=tmp_player_list)
      tmp_stats_table$gp<-NA
      tmp_stats_table$gw<-NA
      tmp_stats_table$ps<-NA
      tmp_stats_table$adj_ps<-NA
      tmp_stats_table$pp<-NA
      tmp_stats_table$gw_div_gp<-NA
      tmp_stats_table$ps_div_pp<-NA
      tmp_stats_table$adj_ps_div_pp<-NA
      tmp_stats_table$beta<-NA # New stat, to be calculated with adj_ps_div_pp*(1+((gw_div_gp-0.5)/2)) ... this centres gw/gp around zero, with min/max -0.25,0.25 (would be -0.5,0.5, but divided by 2)
      tmp_stats_table$rank_4dr<-NA
      tmp_stats_table$sp<-NA
      tmp_stats_table$sa<-NA
      tmp_stats_table$sp_div_sa<-NA
      # Look for calcs:
      for (i in tmp_player_list){
        tmp_stats_table[tmp_stats_table$ID==i,]$gp<-
          length(stat_data[stat_data$ID==i,1])
        tmp_stats_table[tmp_stats_table$ID==i,]$gw<-
          length(stat_data[stat_data$ID==i &
                             stat_data$score_side>stat_data$score_opp,1])
        tmp_stats_table[tmp_stats_table$ID==i,]$ps<-
          sum(stat_data[stat_data$ID==i,]$score_side)
        tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps<-
          round(sum(stat_data[stat_data$ID==i,]$score_side_adj),2) # Rounded to 2dp ... probably should be un-rounded
        tmp_stats_table[tmp_stats_table$ID==i,]$pp<-
          sum(stat_data[stat_data$ID==i,]$score_side)+
          sum(stat_data[stat_data$ID==i,]$score_opp)
        tmp_stats_table[tmp_stats_table$ID==i,]$gw_div_gp<-
          round(tmp_stats_table[tmp_stats_table$ID==i,]$gw/
                  tmp_stats_table[tmp_stats_table$ID==i,]$gp,3)
        tmp_stats_table[tmp_stats_table$ID==i,]$ps_div_pp<-
          round(tmp_stats_table[tmp_stats_table$ID==i,]$ps/
                  tmp_stats_table[tmp_stats_table$ID==i,]$pp,3)
        tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps_div_pp<-
          round(tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps/
                  tmp_stats_table[tmp_stats_table$ID==i,]$pp,3) # Rounded to 3dp ... (see above)
        tmp_stats_table[tmp_stats_table$ID==i,]$beta<-
          round(tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps_div_pp *
                  (1+((tmp_stats_table[tmp_stats_table$ID==i,]$gw_div_gp-0.5)/2)),3) # New stat (see rationale above)
        tmp_stats_table[tmp_stats_table$ID==i,]$rank_4dr<-
          round(rank_table[rank_table$ID==i,]$rank,4)
        #NB the following assumes a maximum of ONE 'session' per DATE.
        tmp_stats_table[tmp_stats_table$ID==i,]$sp<-
          length(unique(stat_data[stat_data$ID==i,]$date))
        tmp_stats_table[tmp_stats_table$ID==i,]$sa<-
          length(unique(stat_data$date))
        tmp_stats_table[tmp_stats_table$ID==i,]$sp_div_sa<-
          round(tmp_stats_table[tmp_stats_table$ID==i,]$sp/
                  tmp_stats_table[tmp_stats_table$ID==i,]$sa*100,0)
        
      }
      return(tmp_stats_table)
    }
}
makePlot<-function(plot_data,plot_label){
  ## select data
  stats_select<-plot_data %>% select(ID,gw_div_gp,ps_div_pp,adj_ps_div_pp,beta)
  # sort by ranking:
  stats_select<- stats_select %>% arrange(desc(beta))
  plot_data<- plot_data %>% arrange(desc(beta)) # required for attendance labels
  col<-case_when(plot_data$sp_div_sa<25 ~ 'red',
                 plot_data$sp_div_sa<50 & plot_data$sp_div_sa>24 ~ 'darkred',
                 plot_data$sp_div_sa>49 ~ 'black')
  # change to long format
  stats_select_long <- melt(stats_select, id.vars="ID")
  stats_select_long$ordering <- 1:length(stats_select_long$ID)
  
  g<-ggplot(stats_select_long, aes(x=reorder(ID,ordering),y=value,fill=variable)) +
    geom_col(position='dodge')+
    geom_text(aes(y=1,label = ifelse(variable == "beta",
                                     paste0("\n",plot_data$sp,"/",
                                            plot_data$sa,"\n",
                                            plot_data$sp_div_sa,"%"),"")),
              size=2)+
    ylim(0,1)+
    scale_x_discrete(guide = guide_axis(angle = 90))+
    theme_classic() +
    ggtitle(plot_label)+
    theme(axis.text.x = element_text(size = 12))+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    theme(legend.position = "bottom", legend.box = "horizontal", legend.title=element_blank()) +
    theme(axis.text.x = element_markdown(colour=col))+ # element_markdown avoids array for colour causing error (don't know why!)
    scale_fill_manual(values=c("purple","orange","grey","black"), labels = c("games won / games played", 
                                                                      "points won / points played",
                                                                      "adj. points won / points played",
                                                                      "beta"))
  return(g)
}
makePlotVert<-function(plot_data,plot_label){
  ## select data
  stats_select<-plot_data %>% select(ID,gw_div_gp,ps_div_pp,adj_ps_div_pp,beta)
  # sort by ranking:
  stats_select<- stats_select %>% arrange(beta)
  plot_data<- plot_data %>% arrange(beta) # required for attendance labels
  col<-case_when(plot_data$sp_div_sa==50 ~ 'darkgrey',
                 plot_data$sp_div_sa<50 ~ 'red',
                 plot_data$sp_div_sa>50 ~ 'black')
  # change to long format
  stats_select_long <- melt(stats_select, id.vars="ID")
  stats_select_long$ordering <- 1:length(stats_select_long$ID)
  
  g<-ggplot(stats_select_long, aes(x=reorder(ID,ordering),y=value,fill=variable)) +
    geom_col(position='dodge')+
    geom_text(aes(y=0.9,vjust = -0.01,label = ifelse(variable == "beta",
                                                     paste0("\n",plot_data$sp,"/",
                                                            plot_data$sa,"; ",
                                                            plot_data$sp_div_sa,"%"),"")),
              size=2)+
    ylim(0,1)+
    #scale_x_discrete(guide = guide_axis(angle = 90))+
    theme_classic() +
    ggtitle(plot_label)+
    theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 10))+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    theme(axis.text.y = element_markdown(colour=col))+ # element_markdown avoids array for colour causing error (don't know why!)
    scale_fill_manual(values=c("purple","orange","grey","black"), labels = c("games won / games played", 
                                                                      "points won / points played",
                                                                      "adj. points won / points played",
                                                                      "beta"))+
    coord_flip()+
    theme(legend.position = "bottom", legend.direction = "vertical", legend.title=element_blank())
  return(g)
}
makePlot4dr<-function(plot_data){
  # order:
  stats_select<-plot_data %>% select(ID,rank_4dr,gp)
  stats_select<- stats_select %>% arrange(desc(rank_4dr))
  stats_select$ordering <- 1:length(stats_select$ID)
  
  g <- ggplot(stats_select, aes(x=reorder(ID,ordering),y=rank_4dr)) +
    geom_col()+
    scale_x_discrete(guide = guide_axis(angle = 90))+
    geom_text(aes(label = paste0(format(round(rank_4dr,4), nsmall=4))), #," (",round(gp,0)," games)")),
              hjust = 1, vjust=0.5,
              position = position_nudge(x =0, y=-0.2), color = "green", angle=90, size = 3) + 
    theme_classic() +
    theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11))+
    ggtitle('4DR ranking')+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())
  return(g)
}
makePlot4drVert<-function(plot_data){
  # order:
  stats_select<-plot_data %>% select(ID,rank_4dr,gp)
  stats_select<- stats_select %>% arrange(rank_4dr)
  stats_select$ordering <- 1:length(stats_select$ID)
  
  g <- ggplot(stats_select, aes(x=reorder(ID,ordering),y=rank_4dr)) +
    geom_col()+
    geom_text(aes(label = paste0(format(round(rank_4dr,4), nsmall=4))), #," (",round(gp,0)," games)")),
              hjust = 1, vjust=0.4, 
              position = position_nudge(x =0, y=-0.2), color = "green", size = 2.5) + 
    theme_classic() +
    theme(axis.text.x = element_text(size = 9),axis.text.y = element_text(size = 9))+
    ggtitle('4DR ranking')+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()
  return(g)
}
sequential_ranks_calc<-function(ID){
  id_count=1
  data_output<-sequential_ranks[0,] # Create empty table with same columns
  while(id_count<=length(ID)){
    dates<-unique(sequential_ranks[sequential_ranks$ID==ID[id_count],]$date)
    for(i in dates){
      one_row<-sequential_ranks[sequential_ranks$ID==ID[id_count] & sequential_ranks$date==i,] %>%
        arrange(date) %>% slice(n())
      data_output<-data_output %>% add_row(one_row)
    }
    id_count<-id_count+1
  }
  return(data_output)
}
processInputs<-function(indoor,location,day,date,eventType){
  if(indoor=="Indoor"){
    choice_indoor=1
  } else if(indoor=="Outdoor"){
    choice_indoor=0
  } else if(indoor=="Both"){
    choice_indoor=c(0,1)
  }
  # Read-in Location selection
  if(location=="All"){
    choice_location=unique(as.character(match_table$location))
  } else {
    choice_location=location
  }
  # Read-in Day selection
  if(day=="All"){
    choice_day=unique(as.character(match_table$dow))
  } else if(day=="Weekend"){
    choice_day=c("Fri","Sun")
  } else {
    choice_day=day
  }
  # Read-in Date selection
  if(date=="All"){
    choice_date=unique(match_table$date)
  } else {
    choice_date=date
  }
  # Read-in Date selection
  if(eventType=="All"){
    choice_event_type=unique(match_table$event_type)
  } else {
    choice_event_type=eventType
  }
  
  input_values <- list("choice_indoor" = choice_indoor,
                       "choice_location" = choice_location,
                       "choice_day" = choice_day,
                       "choice_date" = choice_date,
                       "choice_event_type" = choice_event_type)
  return(input_values) 
}
fourDRCalc_dragon<-function(){
  for (i in 1:game_max){
    tmp_rank_table<-rank_table # after each game, save current ranks in tmp table
    for (row_num in 1:4){
      # Create temp table for sinlge game, i
      game_table<-match_table_long[match_table_long$game==i,]
      # For row 1 (=player 1) game i, store player ranks in variables:
      own_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2]
      partner_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$partner[row_num],2]
      opponent_1_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$opp1[row_num],2]
      opponent_2_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$opp2[row_num],2]
      
      if (game_table$score_side[row_num]>game_table$score_opp[row_num]) { # i.e. player won
        prob<-exp(opponent_1_rank+opponent_2_rank)/
          (exp(own_rank+partner_rank)+exp(opponent_1_rank+opponent_2_rank))
        points_diff<-game_table$score_side[row_num] - game_table$score_opp[row_num] #points diff for winner
        points_awarded<-(0.1*prob)+(0.001*points_diff)
        rank_table[rank_table$ID==game_table$ID[row_num],2]<<-
          tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        # Add ID, rank and date to sequential ranks table:
        sequential_ranks<<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
                                                        rank4dr = rank_table[rank_table$ID==game_table$ID[row_num],2],
                                                        date=game_table$date[row_num])
        
      } else if (game_table$score_side[row_num]<game_table$score_opp[row_num]) { # i.e. player lost
        prob<-exp(own_rank+partner_rank)/
          (exp(own_rank+partner_rank)+exp(opponent_1_rank+opponent_2_rank))
        points_diff<-game_table$score_side[row_num] #points diff for loser
        points_awarded<-(-0.1*prob)+(0.001*points_diff)
        rank_table[rank_table$ID==game_table$ID[row_num],2]<<-
          tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        # Add ID, rank and date to sequential ranks table:
        sequential_ranks<<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
                                                        rank4dr = rank_table[rank_table$ID==game_table$ID[row_num],2],
                                                        date=game_table$date[row_num])
      } else { print("No-difference in score")} # therefore 4dr rank not updated
    }
  }
}
fourDRCalc_zeroSum<-function(){
  for (i in 1:game_max){
    tmp_rank_table<-rank_table # after each game, current ranks stored in tmp_table
    
    # Create table for single game, i
    game_table<-match_table_long[match_table_long$game==i,]
    
    # Store four players' starting ranks:
    player_one_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[1],2]
    player_two_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[2],2]
    player_three_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[3],2]
    player_four_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[4],2]
    
    for (row_num in 1:4){
      ## Assign pre-game ranks to players for each row (player):
      if (row_num == 1) {
        own_rank<-player_one_rank
        partner_rank<-player_two_rank
        opponent_1_rank<-player_three_rank
        opponent_2_rank<-player_four_rank
      } else if (row_num == 2) {
        own_rank<-player_two_rank
        partner_rank<-player_one_rank
        opponent_1_rank<-player_three_rank
        opponent_2_rank<-player_four_rank
      } else if (row_num == 3) {
        own_rank<-player_three_rank
        partner_rank<-player_four_rank
        opponent_1_rank<-player_one_rank
        opponent_2_rank<-player_two_rank
      } else {
        own_rank<-player_four_rank
        partner_rank<-player_three_rank
        opponent_1_rank<-player_one_rank
        opponent_2_rank<-player_two_rank
      }
      
      scalar_adj<-1 # Number to divide side ranks by to balance probability of a win
      
      if (game_table$score_side[row_num]>game_table$score_opp[row_num]) { # i.e. player won
        prob<-exp((opponent_1_rank+opponent_2_rank)/scalar_adj)/
          (exp((own_rank+partner_rank)/scalar_adj)+exp((opponent_1_rank+opponent_2_rank)/scalar_adj))
        points_diff<-(game_table$score_opp[row_num]/
                        game_table$score_side[row_num])*11 #alternative points diff, normalised to 11-pt game [8/5/25]
        points_awarded<-(0.1*prob)-(0.001*points_diff)
        ## Calculate new rank
        new_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        ## Set floor of 1.000 for ranks [Update 25022026)]:
        if(new_rank < 1) new_rank<-1
        ## Assign new rank to rank_table
        rank_table[rank_table$ID==game_table$ID[row_num],2]<<-new_rank
        #rank_table[rank_table$ID==game_table$ID[row_num],2]<<-
        #  tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        
        ## Add ID, rank and date to sequential ranks table:
        sequential_ranks<<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
                                                        rank4dr = rank_table[rank_table$ID==game_table$ID[row_num],2],
                                                        date=game_table$date[row_num])
        
      } else if (game_table$score_side[row_num]<game_table$score_opp[row_num]) { # i.e. player lost
        prob<-exp((own_rank+partner_rank)/scalar_adj)/
          (exp((own_rank+partner_rank)/scalar_adj)+exp((opponent_1_rank+opponent_2_rank)/scalar_adj))
        points_diff<-(game_table$score_side[row_num]/
                        game_table$score_opp[row_num])*11 #alternative points diff, normalised to 11-pt game [8/5/25]
        points_awarded<-(-0.1*prob)+(0.001*points_diff)
        ## Set floor of 1.000 for ranks [Update 25022026)]:
        new_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        if(new_rank < 1) new_rank<-1
        rank_table[rank_table$ID==game_table$ID[row_num],2]<<-new_rank
        #rank_table[rank_table$ID==game_table$ID[row_num],2]<<-
        #  tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        # Add ID, rank and date to sequential ranks table:
        sequential_ranks<<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
                                                        rank4dr = rank_table[rank_table$ID==game_table$ID[row_num],2],
                                                        date=game_table$date[row_num])
      } else { print("No-difference in score")} # therefore 4dr rank not updated
    }
  }
}
createLeaderBoard<-function(data_instance,data_instance_pen,row_length){
  #Select current rows that match >=50% attendance criteria and create ranks
  current_ladder_table<-data_instance[data_instance$sp_div_sa>=50,] %>%
    mutate(rating=beta) %>% select(ID,rating) %>% arrange(desc(rating)) %>%
    mutate(rank=rank(desc(rating),ties.method = 'max'))
  
  #Check 'row_length' for '0' (i.e. all rows) and that it doesn't exceed no. of rows:
  if (row_length > nrow(current_ladder_table)){
    row_length <- nrow(current_ladder_table)
  } else if (row_length == 0) {
    row_length <- nrow(current_ladder_table)
  }
  
  #Create table of penultimate session only if data available
  if (is.data.frame(data_instance_pen)){ # If the penultimate stats don't exist, this function is passed '0' by server code, i.e. not a data.frame
    penultimate_ladder_table<-data_instance_pen %>%
      mutate(rating=beta) %>% select(ID,rating) %>% arrange(desc(rating)) %>%
      filter(ID %in% current_ladder_table$ID) %>%
      mutate(rank=rank(desc(rating),ties.method = 'max'))
    
    current_ladder_table$change=NA
    for (i in current_ladder_table$ID[current_ladder_table$ID %in% 
                                      penultimate_ladder_table$ID]){
      current_ladder_table[current_ladder_table$ID == i,]$change <-
        penultimate_ladder_table[penultimate_ladder_table$ID == i,]$rank -
        current_ladder_table[current_ladder_table$ID == i,]$rank
    }
    
    # Replace 'NAs', i.e. missing from penultimate ladder, with 'new' to indicate new players
    current_ladder_table$new<-""
    if(length(current_ladder_table[is.na(current_ladder_table$change),]$change)>0){
      current_ladder_table[is.na(current_ladder_table$change),]$new<-"new"
    }
  } else {
    current_ladder_table$change<-NA
    current_ladder_table$new<-"new"
  }
  
  # Generate RANK for current (all data) and preceding week (all data minus current)
  # Identify new-entrants
  
  # Custom formatted:
  improvement_formatter <- 
    formatter("span", 
              style = x ~ style(
                font.weight = ifelse(x > 0, "bold", ifelse(x < 0, "italic", "normal")),
                color = ifelse(x > 0, "green", ifelse(x < 0, "red", "black"))),
              x ~ icontext(ifelse(x>0, "arrow-up", ifelse(x<0,"arrow-down","blank"))))
  
  # Formattable table:
  f<-formattable(current_ladder_table[1:row_length,],
                 align=c("l","c","c","c","c"),
                 col.names = c("","rating","rank", "Δ", ""),
                 list(
                   `rating` = color_tile("transparent","violet"),
                   `change` = improvement_formatter),
                 table.attr = 'style="font-size: 11px;";\"')
}
createLeaderBoard_4dr<-function(data_instance,row_length){
  #Select current rows that match >=50% attendance criteria and create ranks
  current_ladder_table<-data_instance[data_instance$sp_div_sa>=25,] %>%
    mutate(rating=rank_4dr) %>% select(ID,rating) %>% arrange(desc(rating)) %>%
    mutate(rank=rank(desc(rating),ties.method = 'max'))
  
  #Check 'row_length' for '0' (i.e. all rows) and that it doesn't exceed no. of rows:
  if (row_length > nrow(current_ladder_table)){
    row_length <- nrow(current_ladder_table)
  } else if (row_length == 0) {
    row_length <- nrow(current_ladder_table)
  }
  
  # #Create table of penultimate session only if data available
  # if (is.data.frame(data_instance_pen)){ # If the penultimate stats don't exist, this function is passed '0' by server code, i.e. not a data.frame
  #   penultimate_ladder_table<-data_instance_pen %>%
  #     mutate(rating=beta) %>% select(ID,rating) %>% arrange(desc(rating)) %>%
  #     filter(ID %in% current_ladder_table$ID) %>%
  #     mutate(rank=rank(desc(rating),ties.method = 'max'))
  #   
  #   current_ladder_table$change=NA
  #   for (i in current_ladder_table$ID[current_ladder_table$ID %in% 
  #                                     penultimate_ladder_table$ID]){
  #     current_ladder_table[current_ladder_table$ID == i,]$change <-
  #       penultimate_ladder_table[penultimate_ladder_table$ID == i,]$rank -
  #       current_ladder_table[current_ladder_table$ID == i,]$rank
  #   }
  #   
  #   # Replace 'NAs', i.e. missing from penultimate ladder, with 'new' to indicate new players
  #   current_ladder_table$new<-""
  #   if(length(current_ladder_table[is.na(current_ladder_table$change),]$change)>0){
  #     current_ladder_table[is.na(current_ladder_table$change),]$new<-"new"
  #   }
  # } else {
  #   current_ladder_table$change<-NA
  #   current_ladder_table$new<-"new"
  # }
  
  # Generate RANK for current (all data) and preceding week (all data minus current)
  # Identify new-entrants
  
  # Custom formatted:
  improvement_formatter <- 
    formatter("span", 
              style = x ~ style(
                font.weight = ifelse(x > 0, "bold", ifelse(x < 0, "italic", "normal")),
                color = ifelse(x > 0, "green", ifelse(x < 0, "red", "black"))),
              x ~ icontext(ifelse(x>0, "arrow-up", ifelse(x<0,"arrow-down","blank"))))
  
  # Formattable table:
  f<-formattable(current_ladder_table[1:row_length,],
                 align=c("l","c","c"), #"c","c"),
                 col.names = c("","rating","rank"), #"Δ", ""),
                 list(
                   `rating` = color_tile("transparent","violet")),
                   #`change` = improvement_formatter),
                 table.attr = 'style="font-size: 11px;";\"')
}

## Data re-formatting and calculation of ratios and 4DR
#----
# Add Days of the Week:
match_table$dow<-as.character(wday(match_table$date, label=TRUE))
# Find number of games played in event:
game_max<-max(match_table$game_no)
# Create empty data.frame to store results in 'long' format (i.e. one row per player per game)
match_table_long <- data.frame(ID=character(),
                               date=as.Date(character()),
                               dow=character(),
                               time=character(),
                               game=integer(),
                               partner=character(), 
                               opp1=character(), 
                               opp2=character(),
                               score_side=integer(),
                               score_opp=integer(),
                               score_method=character(),
                               location=character(),
                               indoor=integer(),
                               no_of_courts=integer(),
                               court_rank=integer(),
                               event_type=character(),
                               stringsAsFactors = FALSE)

# Reformat to long format (see above) and store in match_table_long:

for (i in 1:game_max){
  row1<-match_table[match_table$game_no==i,] %>%# mutate(date=as.Date(date,format = "%d/%m/%y")) %>%
    select(ID=p1,partner=p2,opp1=p3,opp2=p4,score_side=p1p2_score,score_opp=p3p4_score,score_method,
           location,date=date,dow=dow,indoor,no_of_courts,court_rank,event_type)
  row1$game<-i
  
  row2<-match_table[match_table$game_no==i,] %>%# mutate(date=as.Date(date,format = "%d/%m/%y")) %>%
    select(ID=p2,partner=p1,opp1=p3,opp2=p4,score_side=p1p2_score,score_opp=p3p4_score,score_method,
           location,date=date,dow=dow,indoor,no_of_courts,court_rank,event_type)
  row2$game<-i
  
  row3<-match_table[match_table$game_no==i,] %>%# mutate(date=as.Date(date,format = "%d/%m/%y")) %>%
    select(ID=p3,partner=p4,opp1=p1,opp2=p2,score_side=p3p4_score,score_opp=p1p2_score,score_method,
           location,date=date,dow=dow,indoor,no_of_courts,court_rank,event_type)
  row3$game<-i
  
  row4<-match_table[match_table$game_no==i,] %>%# mutate(date=as.Date(date,format = "%d/%m/%y")) %>%
    select(ID=p4,partner=p3,opp1=p1,opp2=p2,score_side=p3p4_score,score_opp=p1p2_score,score_method,
           location,date=date,dow=dow,indoor,no_of_courts,court_rank,event_type)
  row4$game<-i
  
  match_table_long<-bind_rows(match_table_long,row1,row2,row3,row4)
}

# All players:
player_list<-unique(match_table_long$ID)

## Rank tables
# Create table of ranks with all players as 3.000:
rank_table_all<-data.frame(ID=player_list,rank=3)
# Merge with historical ranks to replace '3.000's where known ('google_rank_table')
for(id in 1:nrow(google_rank_table)){
  rank_table_all$rank[rank_table_all$ID %in% google_rank_table$ID[id]] <- google_rank_table$rank[id]
}
# Store ranks in rank_table
rank_table<-rank_table_all
rank_table$rank<-as.numeric(rank_table$rank)

# maximum (i.e. smallest!) fraction by which points are down-adjusted
max_adj_factor<-0.8
# add adjusted score column
match_table_long$score_side_adj<-NA
# calculate adjusted scores from court 'levels' (0=courts not assigned levels)
for (i in 1:game_max){
  if (match_table_long[match_table_long$game==i,]$court_rank[1] == 0){
    match_table_long[match_table_long$game==i,]$score_side_adj<-
      match_table_long[match_table_long$game==i,]$score_side
  } else {
    match_table_long[match_table_long$game==i,]$score_side_adj<-
      match_table_long[match_table_long$game==i,]$score_side *
      (1-((match_table_long[match_table_long$game==i,]$court_rank-1)*
            ((1-max_adj_factor)/(match_table_long[match_table_long$game==i,]$no_of_courts-1))))
  }
}


# Create table to collect sequential 4DR ranks:
sequential_ranks<-data.frame(ID=character(),rank4dr=numeric(),date=as.Date(character()),
                             stringsAsFactors=FALSE)

## Decide on 4DR system ...
fourDRCalc_zeroSum() #updated - zero sum version; simultaneous game calcs (not sequential for the 4 players); div by 3
#fourDRCalc_dragon() #original


#----

## app.R ##
server <- function(input, output) {
  #Ladder leaderboards:
  output$thu_ladder <- renderFormattable({
    thu_stats<-match_table_long %>% filter(dow %in% "Thu" & event_type %in% "ladder")
    if (nrow(thu_stats) != 0){
      thu_this<-makeStatTable(thu_stats)
      last_session<-max(thu_stats$date)
      if (nrow(thu_stats[thu_stats$date != last_session,])==0){
        print("No previous dates for Thu leaderboard comparison")
        thu_prev <- 0
      } else {
        thu_prev<-makeStatTable(thu_stats[thu_stats$date != last_session,])
      }
      createLeaderBoard(thu_this,thu_prev,10)
    } else {
      formattable(as.data.frame("No Data"))
    }
  })
  # output$fri_ladder <- renderFormattable({
  #   fri_stats<-match_table_long %>% filter(dow %in% "Fri" & event_type %in% "ladder")
  #   if (nrow(fri_stats) != 0){
  #     fri_this<-makeStatTable(fri_stats)
  #     last_session<-max(fri_stats$date)
  #     if (nrow(fri_stats[fri_stats$date != last_session,])==0){
  #       print("No previous dates for Thu leaderboard comparison")
  #       fri_prev <- 0
  #     } else {
  #       fri_prev<-makeStatTable(fri_stats[fri_stats$date != last_session,])
  #     }
  #     #createLeaderBoard(fri_this,fri_prev,10)
  #     createLeaderBoard_4dr(fri_this,fri_prev,10)
  #   } else {
  #     formattable(as.data.frame("No Data"))
  #   }
  # })
  # output$sun_ladder <- renderFormattable({
  #   sun_stats<-match_table_long %>% filter(dow %in% "Sun"& event_type %in% "ladder")
  #   if (nrow(sun_stats) != 0){
  #     sun_this<-makeStatTable(sun_stats)
  #     last_session<-max(sun_stats$date)
  #     if (nrow(sun_stats[sun_stats$date != last_session,])==0){
  #       print("No previous dates for Thu leaderboard comparison")
  #       sun_prev <- 0
  #     } else {
  #       sun_prev<-makeStatTable(sun_stats[sun_stats$date != last_session,])
  #     }
  #     createLeaderBoard(sun_this,sun_prev,10)
  #   } else {
  #     formattable(as.data.frame("No Data"))
  #   }
  # })
  output$lhs_ladder <- renderFormattable({
    lhs_stats<-match_table_long %>% filter(location %in% "Llanishen HS" & event_type %in% "ladder")
    if (nrow(lhs_stats) != 0){
      lhs_this<-makeStatTable(lhs_stats)
      last_session<-max(lhs_stats$date)
      # if (nrow(lhs_stats[lhs_stats$date != last_session,])==0){
      #   print("No previous dates for Thu leaderboard comparison")
      #   lhs_prev <- 0
      # } else {
      #   lhs_prev<-makeStatTable(lhs_stats[lhs_stats$date != last_session,])
      # }
      
      createLeaderBoard_4dr(lhs_this,10)
    } else {
      formattable(as.data.frame("No Data"))
    }
  })
  
  # Reactive 'Function' to create match_table_data filtered by input selections.
  filtered_rows <- reactive({
    # Read-in Indoor/Outdoor selection
    input_vals<-processInputs(input$indoor,input$location,input$day,input$date,input$eventType) # Function returns processed inputs as a list
    list2env(input_vals,envir = .GlobalEnv) # Assigns all list components to global environment
    # Filter:
    data <- match_table_long %>% filter(location %in% choice_location &
                                          indoor %in% choice_indoor &
                                          dow %in% choice_day &
                                          event_type %in% choice_event_type &
                                          date %in% as.POSIXct(choice_date,tz='UTC'))
    data
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    # Filter data (with 'filtered_rows') and generate stats table with custom makeStatTable function
    data_instance<-makeStatTable(filtered_rows())
    # Return 'data' from curly parenthases:
    data_instance
  },rownames= FALSE))
  
  output$plot_ratios <- renderPlot( 
    {
      # Read-in Indoor/Outdoor selection
      #input_vals<-processInputs(input$indoor,input$location,input$day,input$date) # Function returns processed inputs as a list
      #list2env(input_vals,envir = .GlobalEnv) # Assigns all list components to global environment

      # Filter data (with 'filtered_rows') and generate stats table with custom makeStatTable function
      data_instance<-makeStatTable(filtered_rows())
      
      ratio_plot<-makePlot(data_instance,paste0('Session / Day: ', input$day,
                                                  ', Location: ', input$location,
                                                  ', Indoor/Outdoor: ', input$indoor,
                                                  ', Date: ', input$date))
      print(ratio_plot)
    }
  ) 
  output$plot_ratios_tall <- renderPlot( 
    {
      # Read-in Indoor/Outdoor selection
      #input_vals<-processInputs(input$indoor,input$location,input$day,input$date) # Function returns processed inputs as a list
      #list2env(input_vals,envir = .GlobalEnv) # Assigns all list components to global environment
      
      # Filter data (with 'filtered_rows') and generate stats table with custom makeStatTable function
      data_instance<-makeStatTable(filtered_rows())
      
      ratio_plot<-makePlotVert(data_instance,paste0('Session / Day: ', input$day,
                                                      ', Location: ', input$location,
                                                      ', Indoor/Outdoor: ', input$indoor,
                                                      ', Date: ', input$date))
      print(ratio_plot)
    }
  )
  output$plot_4dr <- renderPlot( 
    {
      all_data<-makeStatTable(match_table_long)
      fourDR_plot<-makePlot4dr(all_data)
      print(fourDR_plot)
    } 
  )
  output$plot_4dr_tall <- renderPlot( 
    {
      all_data<-makeStatTable(match_table_long)
      fourDR_plot<-makePlot4drVert(all_data)
      print(fourDR_plot)
    } 
  )
  output$plot_4dr_byParticipant <- renderPlot(
    {
      # Read-in Indoor/Outdoor selection
      #input_vals<-processInputs(input$indoor,input$location,input$day,input$date) # Function returns processed inputs as a list
      #list2env(input_vals,envir = .GlobalEnv) # Assigns all list components to global environment
      
      # Filter data (with 'filtered_rows') and generate stats table with custom makeStatTable function
      data_instance<-makeStatTable(filtered_rows()) # NB this will exclude non-ladder players (i.e. if someone only played in a tournament) but this is critical because it has to duplicate the content of the table created above, so that the row numbers accord.
      
      rows_index<-input$table_rows_selected
      plot_data<-sequential_ranks_calc(c(data_instance$ID[rows_index]))
      g<-ggplot(plot_data, aes(x=date, y=rank4dr)) +
        ggtitle("Player 4DRs")+
        theme_classic()+
        geom_line(aes(color=ID))+
        geom_point()+
        theme(legend.position = "bottom", legend.direction = "horizontal", legend.title=element_blank())
      print(g)
    }
  )
}

ui <- page_fluid(
  title = "CPC Stats",
  #tags$head(includeHTML("google_analytics.html")), #Google analytics tie-in
  div(img(src='logo.png', width="20%",style="max-width:150px; height:auto;"), style="text-align: center;"),
  titlePanel(h1("Clwb Picl Stats", align="center")),
  div(tags$a("Return to Main site",href="https://cardiffpickleballclub.wordpress.com/"), style="text-align: center;"),
  
  # Leaderboards:
  card(card_header("Ladder Leaderboards"),
       div(p(paste0("(Last updated: ",ymd(max(match_table$date)),")"),style="font-size: 12px;")),
  layout_columns(
    card(card_header("LLC Thursday"),
         formattableOutput("thu_ladder")),
    # card(card_header("Friday"),
    #      formattableOutput("fri_ladder")),
    # card(card_header("Sunday"),
    #      formattableOutput("sun_ladder"))
    card(card_header("LHS Weekend"),
               formattableOutput("lhs_ladder"))
  )),
  # Selection drop-downs:
  card(
  layout_column_wrap(
    layout_column_wrap(width="100px",
                       selectInput("day",
                                   "Session / Day:",
                                   c("All","Weekend",
                                     unique(as.character(match_table$dow)))), # TO-DO - restrict to Thu/Fri/Sun
                       selectInput("eventType",
                                   "Event-type:",selected = "ladder",
                                   c("All",
                                     unique(as.character(match_table$event_type)))),
                       selectInput("location",
                                   "Location:",
                                   c("All",
                                     unique(as.character(match_table$location))))),
    layout_column_wrap(width="100px",
                       selectInput("indoor",
                                   "Indoor / Outdoor:",
                                   c("Both",
                                     "Indoor","Outdoor")),
                       selectInput("date",
                                   "Date:",
                                   c("All",
                                     unique(as.character(match_table$date)))))
  ),
  layout_columns(checkboxInput("rotate", "Rotate charts? (ideal for smartphones)", FALSE))
  ),
  
  
  # Datetable and plots:
  div(style="font-size: 12px;",
  navset_card_underline(
    title = "Select a tab ...",
    nav_panel("Table and player 4DR plot",
              layout_column_wrap(width = "600px",
                                 card(div(p(paste0("gp = games played; gw = games won;
                                                   ps = points scored; pp = points played;
                                                   sp = sessions played; sa = sessions available"),style="font-size: 12px;")),
                                 card(DT::dataTableOutput("table"))),
                                 card(max_height = 300,
                                      conditionalPanel(
                                        condition= "input.table_rows_selected!=0",
                                        plotOutput("plot_4dr_byParticipant")),
                                      conditionalPanel(
                                        condition= "input.table_rows_selected==0",
                                        tags$h4("Select players from table to display individual 4DR plots",style="font-size: 12px;"))))),
    nav_panel("Ratio plot",
              conditionalPanel(
                condition = "input.rotate == true",
                plotOutput("plot_ratios_tall",height="700px")),
              conditionalPanel(
                condition = "input.rotate == false",
                plotOutput("plot_ratios"))),
    nav_panel("All 4DR plot",
              conditionalPanel(
                condition = "input.rotate == true",
                plotOutput("plot_4dr_tall",height="1000px")),
              conditionalPanel(
                condition = "input.rotate == false",
                plotOutput("plot_4dr"))
              ),
    )),
  
)


shinyApp(ui = ui, server = server)