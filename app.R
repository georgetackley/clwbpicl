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
library(DBI)
library(RPostgres)


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
          length(unique(as_date(stat_data[stat_data$ID==i,]$date_time)))
        tmp_stats_table[tmp_stats_table$ID==i,]$sa<-
          length(unique(as_date(stat_data$date_time)))
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
  print(sequential_ranks[1:5,])
  print(summary(sequential_ranks))
  while(id_count<=length(ID)){
    #dates<-unique(sequential_ranks[sequential_ranks$ID==ID[id_count],]$date_time)
    dates<-unique(as_date(sequential_ranks[sequential_ranks$ID==ID[id_count],]$date_time)) # Isolates unique dates from date-times for each participant (ID)
    for(i in dates){
      one_row<-sequential_ranks[sequential_ranks$ID==ID[id_count],]
      one_row<-one_row[one_row$date_time==
                         max(one_row$date_time[one_row$date_time<(as_date(i)+dhours(24))]),]
      one_row<- one_row %>% arrange(ymd_hms(date_time)) %>% slice(n())
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
    choice_date<-unique(match_table$date_time)
  } else {
    choice_date<-match_table$date_time[match_table$date_time>as_date(date) & 
                                         match_table$date_time<as_date(date)+dhours(24)]
  }
  # Read-in Event selection
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
fourDRCalc_zeroSum_ORIGINAL<-function(){
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
fourDRCalc_zeroSum<-function(){
  # Create copy of rank table for function:
  fx_rank_table<-rank_table
  # Create table to collect sequential 4DR ranks:
  sequential_ranks<-data.frame(ID=character(),rank4dr=numeric(),date_time=ymd_hms(),
                               stringsAsFactors=FALSE)
  for (i in 1:game_max){
    # Create tmp copy of rank table for function:
    tmp_rank_table<-fx_rank_table
    
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
        fx_rank_table[fx_rank_table$ID==game_table$ID[row_num],2]<-new_rank
        
        ## Add ID, rank and date to sequential ranks table:
        sequential_ranks<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
                                                        rank4dr = fx_rank_table[fx_rank_table$ID==game_table$ID[row_num],2],
                                                        date_time=ymd_hms(game_table$date_time[row_num])) # added ymd_hms 20032026
        
      } else if (game_table$score_side[row_num]<game_table$score_opp[row_num]) { # i.e. player lost
        prob<-exp((own_rank+partner_rank)/scalar_adj)/
          (exp((own_rank+partner_rank)/scalar_adj)+exp((opponent_1_rank+opponent_2_rank)/scalar_adj))
        points_diff<-(game_table$score_side[row_num]/
                        game_table$score_opp[row_num])*11 #alternative points diff, normalised to 11-pt game [8/5/25]
        points_awarded<-(-0.1*prob)+(0.001*points_diff)
        ## Set floor of 1.000 for ranks [Update 25022026)]:
        new_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        if(new_rank < 1) new_rank<-1
        fx_rank_table[fx_rank_table$ID==game_table$ID[row_num],2]<-new_rank

        # Add ID, rank and date to sequential ranks table:
        sequential_ranks<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
                                                        rank4dr = fx_rank_table[fx_rank_table$ID==game_table$ID[row_num],2],
                                                        date_time=ymd_hms(game_table$date_time[row_num])) # added ymd_hms 20032026
      } else { print("No-difference in score")} # therefore 4dr rank not updated
    }
  }
  returns <- list(ranks=fx_rank_table,seqRanks=sequential_ranks)
  return(returns)
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
# DB connection function:
connectDB <- function(){
  
  # Set Supabase connection env. parameters:
  Sys.setenv(SUPABASE_DB_HOST = "aws-1-eu-west-1.pooler.supabase.com")
  Sys.setenv(SUPABASE_DB_PORT = "5432")
  Sys.setenv(SUPABASE_DB_NAME = "postgres")
  Sys.setenv(SUPABASE_DB_USER = "postgres.bnnisnnqvsghpyktijal")
  
  Sys.setenv(SUPABASE_DB_PASS = Sys.getenv("SUPABASE_PW"))   # Password stored on Connect Cloud: Admin/Settings > Variables
  
  # Store environment parameters in variables:
  host <- Sys.getenv("SUPABASE_DB_HOST", "db.bnnisnnqvsghpyktijal.supabase.co") # Not sure if second argument is needed? Same for next few lines
  port <- as.integer(Sys.getenv("SUPABASE_DB_PORT", "5432"))
  dbname <- Sys.getenv("SUPABASE_DB_NAME", "postgres")
  user <- Sys.getenv("SUPABASE_DB_USER", "postgres")
  password <- Sys.getenv("SUPABASE_DB_PASS")
  if (!nzchar(password)) stop("Set SUPABASE_DB_PASS environment variable")
  
  # Establish DB connection:
  con <- dbConnect(
    RPostgres::Postgres(),
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password,
    sslmode = "require"
  )
  return(con)
}

#Call DB connection function
con<-connectDB()

## Some useful DB debug commands:
#----
#print(dbGetQuery(con, "SELECT current_database() AS db, current_user AS user, inet_server_addr() AS server_ip;"))
#print(dbListTables(con))           # lists tables in the search_path
#----

loadDataDB<-function(){
  ## Load data from database
  match_table <- dbReadTable(con, "mastersheet")   # equivalent to SELECT * FROM "mastersheet"
  init_4dr_table<-dbReadTable(con, "4DR_initialiser")
  rank_table<-dbReadTable(con, "4DR_current")
  sequential_ranks<-dbReadTable(con, "sequential_ranks")
  match_table_long <- dbReadTable(con, "match_table_long")
  
  ## Re-cast some columns (this can be tidied in the future)
  rank_table$ID<-rank_table$name
  sequential_ranks$ID<-sequential_ranks$name
  sequential_ranks$rank4dr<-sequential_ranks$rank
  
  # Add Days of the Week:
  match_table$dow<-as.character(wday(match_table$date_time, label=TRUE))
  
  # Format data and sort-by date
  match_table$date_time <- ymd_hms(match_table$date_time) #Convert to lubridate date/time format
  match_table <- match_table %>% arrange(date_time) #Sort table by date_time
  
  return_list<-list("mt"=match_table,"init4dr"=init_4dr_table,"current4dr"=rank_table,"seq"=sequential_ranks,"mtl"=match_table_long)
}

all_data<-loadDataDB()
match_table <- all_data$mt
init_4dr_table<-all_data$init4dr
rank_table<-all_data$current4dr
sequential_ranks<-all_data$seq
match_table_long <- all_data$mtl


## app.R ##
server <- function(input, output) {
  # Data update code:
   run_update <- reactive({
     all_data<-loadDataDB() # re-loads data from DB
     match_table <- all_data$mt
     init_4dr_table<-all_data$init4dr
     rank_table<-all_data$current4dr
     sequential_ranks<-all_data$seq
     match_table_long <- all_data$mtl
     print(dbListTables(con))
   }) %>% bindEvent(input$update)
   
  # output$time_string <- renderText({
  #   paste("Data re-loaded, ", as.character(floor_date(ymd_hms(Sys.time()))),run_update())
  # })
  
  
  #Ladder leaderboards:
  output$thu_ladder <- renderFormattable({
    thu_stats<-match_table_long %>% filter(dow %in% "Thu" & event_type %in% "ladder")
    if (nrow(thu_stats) != 0){
      thu_this<-makeStatTable(thu_stats)
      last_session<-max(as_date(thu_stats$date_time))
      if (nrow(thu_stats[thu_stats$date_time < last_session,])==0){
        print("No previous dates for Thu leaderboard comparison")
        thu_prev <- 0
      } else {
        thu_prev<-makeStatTable(thu_stats[thu_stats$date_time < last_session,])
      }
      createLeaderBoard(thu_this,thu_prev,10)
    } else {
      formattable(as.data.frame("No Data"))
    }
  })
  
  output$lhs_ladder <- renderFormattable({
    lhs_stats<-match_table_long %>% filter(location %in% "Llanishen HS" & event_type %in% "ladder")
    if (nrow(lhs_stats) != 0){
      lhs_this<-makeStatTable(lhs_stats)
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
                                          date_time %in% choice_date)
                                          #date %in% as.POSIXct(choice_date,tz='UTC')) # Changed this for postgres date format compatibility - not sure what the precise format difference was
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
      # Filter data (with 'filtered_rows') and generate stats table with custom makeStatTable function
      data_instance<-makeStatTable(filtered_rows()) # NB this will exclude non-ladder players (i.e. if someone only played in a tournament) but this is critical because it has to duplicate the content of the table created above, so that the row numbers accord.
      
      rows_index<-input$table_rows_selected
      plot_data<-sequential_ranks_calc(c(data_instance$ID[rows_index]))
      g<-ggplot(plot_data, aes(x=date_time, y=rank4dr)) +
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
  div(img(src='logo.png', width="20%",style="max-width:150px; height:auto; padding:15px;"), style="text-align: center;"),
  titlePanel(h1("Clwb Picl Stats", align="center")),
  div(tags$a("Return to Main site",href="https://cardiffpickleballclub.wordpress.com/"), style="text-align: center;"),
  fluidRow(column(4),column(4,actionButton("update","Re-load Data"),
                            textOutput("time_string"),
                            align="center"),column(4)),
  
  # Leaderboards:
  card(card_header("Ladder Leaderboards"),
       div(p(paste0("(Last updated: ",as_date(ymd_hms(max(match_table$date_time))),")"),style="font-size: 12px;")),
  layout_columns(
    card(card_header("LLC Thursday"),
         formattableOutput("thu_ladder")),
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
                                     unique(as.character(as_date(match_table$date_time))))))
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