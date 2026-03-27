# This will eventually refresh DB 4DR
# Plan is to Output 'match_table_long', 'rank_4DR', and 'sequential_ranks' to DB

library(DBI)
library(RPostgres)

# Set Supabase connection env. parameters:
Sys.setenv(SUPABASE_DB_HOST = "aws-1-eu-west-1.pooler.supabase.com")
Sys.setenv(SUPABASE_DB_PORT = "5432")
Sys.setenv(SUPABASE_DB_NAME = "postgres")
Sys.setenv(SUPABASE_DB_USER = "postgres.bnnisnnqvsghpyktijal")

Sys.setenv(SUPABASE_DB_PASS = Sys.getenv("DB_PASS"))   # Password stored in .Renviron file

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


# Quick test
print(dbGetQuery(con, "SELECT current_database() AS db, current_user AS user, inet_server_addr() AS server_ip;"))
# 1) List available tables (schema-qualified)
print(dbListTables(con))           # lists tables in the search_path
# 
# ## Load data from database
# match_table <- dbReadTable(con, "mastersheet")   # equivalent to SELECT * FROM "mastersheet"
# init_4dr_table<-dbReadTable(con, "4DR_initialiser")
# 
# # Format and sort-by date
# match_table$date_time <- ymd_hms(match_table$date_time) #Convert to lubridate date/time format
# match_table <- match_table %>% arrange(date_time) #Sort table by date_time
# 
# # FUNCTIONS:
# makeStatTable<-function(stat_data){
#   if(nrow(stat_data)==0){
#     empty_table<-data.frame(c("No data to show - Please try other combinations."))
#     colnames(empty_table)<-"Empty table!"
#     return(empty_table)
#   } else {
#     tmp_player_list<-unique(stat_data$ID)
#     tmp_stats_table<-data.frame(ID=tmp_player_list)
#     tmp_stats_table$gp<-NA
#     tmp_stats_table$gw<-NA
#     tmp_stats_table$ps<-NA
#     tmp_stats_table$adj_ps<-NA
#     tmp_stats_table$pp<-NA
#     tmp_stats_table$gw_div_gp<-NA
#     tmp_stats_table$ps_div_pp<-NA
#     tmp_stats_table$adj_ps_div_pp<-NA
#     tmp_stats_table$beta<-NA # New stat, to be calculated with adj_ps_div_pp*(1+((gw_div_gp-0.5)/2)) ... this centres gw/gp around zero, with min/max -0.25,0.25 (would be -0.5,0.5, but divided by 2)
#     tmp_stats_table$rank_4dr<-NA
#     tmp_stats_table$sp<-NA
#     tmp_stats_table$sa<-NA
#     tmp_stats_table$sp_div_sa<-NA
#     # Look for calcs:
#     for (i in tmp_player_list){
#       tmp_stats_table[tmp_stats_table$ID==i,]$gp<-
#         length(stat_data[stat_data$ID==i,1])
#       tmp_stats_table[tmp_stats_table$ID==i,]$gw<-
#         length(stat_data[stat_data$ID==i &
#                            stat_data$score_side>stat_data$score_opp,1])
#       tmp_stats_table[tmp_stats_table$ID==i,]$ps<-
#         sum(stat_data[stat_data$ID==i,]$score_side)
#       tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps<-
#         round(sum(stat_data[stat_data$ID==i,]$score_side_adj),2) # Rounded to 2dp ... probably should be un-rounded
#       tmp_stats_table[tmp_stats_table$ID==i,]$pp<-
#         sum(stat_data[stat_data$ID==i,]$score_side)+
#         sum(stat_data[stat_data$ID==i,]$score_opp)
#       tmp_stats_table[tmp_stats_table$ID==i,]$gw_div_gp<-
#         round(tmp_stats_table[tmp_stats_table$ID==i,]$gw/
#                 tmp_stats_table[tmp_stats_table$ID==i,]$gp,3)
#       tmp_stats_table[tmp_stats_table$ID==i,]$ps_div_pp<-
#         round(tmp_stats_table[tmp_stats_table$ID==i,]$ps/
#                 tmp_stats_table[tmp_stats_table$ID==i,]$pp,3)
#       tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps_div_pp<-
#         round(tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps/
#                 tmp_stats_table[tmp_stats_table$ID==i,]$pp,3) # Rounded to 3dp ... (see above)
#       tmp_stats_table[tmp_stats_table$ID==i,]$beta<-
#         round(tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps_div_pp *
#                 (1+((tmp_stats_table[tmp_stats_table$ID==i,]$gw_div_gp-0.5)/2)),3) # New stat (see rationale above)
#       tmp_stats_table[tmp_stats_table$ID==i,]$rank_4dr<-
#         round(rank_table[rank_table$ID==i,]$rank,4)
#       #NB the following assumes a maximum of ONE 'session' per DATE.
#       tmp_stats_table[tmp_stats_table$ID==i,]$sp<-
#         length(unique(as_date(stat_data[stat_data$ID==i,]$date_time)))
#       tmp_stats_table[tmp_stats_table$ID==i,]$sa<-
#         length(unique(as_date(stat_data$date_time)))
#       tmp_stats_table[tmp_stats_table$ID==i,]$sp_div_sa<-
#         round(tmp_stats_table[tmp_stats_table$ID==i,]$sp/
#                 tmp_stats_table[tmp_stats_table$ID==i,]$sa*100,0)
#       
#     }
#     return(tmp_stats_table)
#   }
# }
# sequential_ranks_calc<-function(ID){
#   id_count=1
#   data_output<-sequential_ranks[0,] # Create empty table with same columns
#   print(sequential_ranks[1:5,])
#   print(summary(sequential_ranks))
#   while(id_count<=length(ID)){
#     #dates<-unique(sequential_ranks[sequential_ranks$ID==ID[id_count],]$date_time)
#     dates<-unique(as_date(sequential_ranks[sequential_ranks$ID==ID[id_count],]$date_time)) # Isolates unique dates from date-times for each participant (ID)
#     for(i in dates){
#       one_row<-sequential_ranks[sequential_ranks$ID==ID[id_count],]
#       one_row<-one_row[one_row$date_time==
#                          max(one_row$date_time[one_row$date_time<(as_date(i)+dhours(24))]),]
#       one_row<- one_row %>% arrange(ymd_hms(date_time)) %>% slice(n())
#       data_output<-data_output %>% add_row(one_row)
#     }
#     id_count<-id_count+1
#   }
#   return(data_output)
# }
# fourDRCalc_zeroSum<-function(){
#   for (i in 1:game_max){
#     tmp_rank_table<-rank_table # after each game, current ranks stored in tmp_table
#     
#     # Create table for single game, i
#     game_table<-match_table_long[match_table_long$game==i,]
#     
#     # Store four players' starting ranks:
#     player_one_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[1],2]
#     player_two_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[2],2]
#     player_three_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[3],2]
#     player_four_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[4],2]
#     
#     for (row_num in 1:4){
#       ## Assign pre-game ranks to players for each row (player):
#       if (row_num == 1) {
#         own_rank<-player_one_rank
#         partner_rank<-player_two_rank
#         opponent_1_rank<-player_three_rank
#         opponent_2_rank<-player_four_rank
#       } else if (row_num == 2) {
#         own_rank<-player_two_rank
#         partner_rank<-player_one_rank
#         opponent_1_rank<-player_three_rank
#         opponent_2_rank<-player_four_rank
#       } else if (row_num == 3) {
#         own_rank<-player_three_rank
#         partner_rank<-player_four_rank
#         opponent_1_rank<-player_one_rank
#         opponent_2_rank<-player_two_rank
#       } else {
#         own_rank<-player_four_rank
#         partner_rank<-player_three_rank
#         opponent_1_rank<-player_one_rank
#         opponent_2_rank<-player_two_rank
#       }
#       
#       scalar_adj<-1 # Number to divide side ranks by to balance probability of a win
#       
#       if (game_table$score_side[row_num]>game_table$score_opp[row_num]) { # i.e. player won
#         prob<-exp((opponent_1_rank+opponent_2_rank)/scalar_adj)/
#           (exp((own_rank+partner_rank)/scalar_adj)+exp((opponent_1_rank+opponent_2_rank)/scalar_adj))
#         points_diff<-(game_table$score_opp[row_num]/
#                         game_table$score_side[row_num])*11 #alternative points diff, normalised to 11-pt game [8/5/25]
#         points_awarded<-(0.1*prob)-(0.001*points_diff)
#         ## Calculate new rank
#         new_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
#         ## Set floor of 1.000 for ranks [Update 25022026)]:
#         if(new_rank < 1) new_rank<-1
#         ## Assign new rank to rank_table
#         rank_table[rank_table$ID==game_table$ID[row_num],2]<<-new_rank
#         #rank_table[rank_table$ID==game_table$ID[row_num],2]<<-
#         #  tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
#         
#         ## Add ID, rank and date to sequential ranks table:
#         sequential_ranks<<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
#                                                         rank4dr = rank_table[rank_table$ID==game_table$ID[row_num],2],
#                                                         date_time=ymd_hms(game_table$date_time[row_num])) # added ymd_hms 20032026
#         
#       } else if (game_table$score_side[row_num]<game_table$score_opp[row_num]) { # i.e. player lost
#         prob<-exp((own_rank+partner_rank)/scalar_adj)/
#           (exp((own_rank+partner_rank)/scalar_adj)+exp((opponent_1_rank+opponent_2_rank)/scalar_adj))
#         points_diff<-(game_table$score_side[row_num]/
#                         game_table$score_opp[row_num])*11 #alternative points diff, normalised to 11-pt game [8/5/25]
#         points_awarded<-(-0.1*prob)+(0.001*points_diff)
#         ## Set floor of 1.000 for ranks [Update 25022026)]:
#         new_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
#         if(new_rank < 1) new_rank<-1
#         rank_table[rank_table$ID==game_table$ID[row_num],2]<<-new_rank
#         #rank_table[rank_table$ID==game_table$ID[row_num],2]<<-
#         #  tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
#         # Add ID, rank and date to sequential ranks table:
#         sequential_ranks<<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
#                                                         rank4dr = rank_table[rank_table$ID==game_table$ID[row_num],2],
#                                                         date_time=ymd_hms(game_table$date_time[row_num])) # added ymd_hms 20032026
#       } else { print("No-difference in score")} # therefore 4dr rank not updated
#     }
#   }
# }
# 
# ## Data re-formatting and calculation of ratios and 4DR
# #----
# # Add Days of the Week:
# match_table$dow<-as.character(wday(match_table$date_time, label=TRUE))
# 
# # Find number of games(=rows) in match_table:
# game_max<-nrow(match_table)
# # Create empty data.frame to store results in 'long' format (i.e. one row per player per game)
# match_table_long <- data.frame(ID=character(),
#                                date_time=as.Date(character()), #update to 'date_time' 19032026
#                                dow=character(),
#                                #time=character(), # removed 19032026
#                                game=integer(),
#                                partner=character(), 
#                                opp1=character(), 
#                                opp2=character(),
#                                score_side=integer(),
#                                score_opp=integer(),
#                                score_method=character(),
#                                location=character(),
#                                indoor=integer(),
#                                no_of_courts=integer(),
#                                court_rank=integer(),
#                                event_type=character(),
#                                stringsAsFactors = FALSE)
# 
# 
# # Reformat to long format (see above) and store in match_table_long:
# # Might at some point be safer to check this with game id?
# for (i in 1:game_max){
#   row1<-match_table[i,] %>%# mutate(date=as.Date(date,format = "%d/%m/%y")) %>%  // match_table$game_no==i
#     select(ID=p1,partner=p2,opp1=p3,opp2=p4,score_side=p1p2_score,score_opp=p3p4_score,score_method,
#            location,date_time=date_time,dow=dow,indoor,no_of_courts,court_rank,event_type)
#   row1$game<-i
#   
#   row2<-match_table[i,] %>%# mutate(date=as.Date(date,format = "%d/%m/%y")) %>%
#     select(ID=p2,partner=p1,opp1=p3,opp2=p4,score_side=p1p2_score,score_opp=p3p4_score,score_method,
#            location,date_time=date_time,dow=dow,indoor,no_of_courts,court_rank,event_type)
#   row2$game<-i
#   
#   row3<-match_table[i,] %>%# mutate(date=as.Date(date,format = "%d/%m/%y")) %>%
#     select(ID=p3,partner=p4,opp1=p1,opp2=p2,score_side=p3p4_score,score_opp=p1p2_score,score_method,
#            location,date_time=date_time,dow=dow,indoor,no_of_courts,court_rank,event_type)
#   row3$game<-i
#   
#   row4<-match_table[i,] %>%# mutate(date=as.Date(date,format = "%d/%m/%y")) %>%
#     select(ID=p4,partner=p3,opp1=p1,opp2=p2,score_side=p3p4_score,score_opp=p1p2_score,score_method,
#            location,date_time=date_time,dow=dow,indoor,no_of_courts,court_rank,event_type)
#   row4$game<-i
#   
#   match_table_long<-bind_rows(match_table_long,row1,row2,row3,row4)
# }
# 
# 
# print("4DR table summary") # DEBUG
# print(summary(init_4dr_table)) # DEBUG
# 
# 
# # All players:
# player_list<-unique(match_table_long$ID) # I think this should now be pullede straight from member list??
# 
# ## Rank tables
# # Create table of ranks with all players as 3.000:
# rank_table_all<-data.frame(ID=player_list,rank=3)
# # Merge with historical ranks to replace '3.000's where known ('init_4dr_table')
# for(id in 1:nrow(init_4dr_table)){
#   rank_table_all$rank[rank_table_all$ID %in% init_4dr_table$name[id]] <- init_4dr_table$rank[id]
#   #rank_table_all$rank[rank_table_all$ID %in% init_4dr_table$ID[id]] <- init_4dr_table$rank[id] # For google 4DR name<->ID
# }
# # Store ranks in rank_table
# rank_table<-rank_table_all
# rank_table$rank<-as.numeric(rank_table$rank)
# print("Rank table summary")
# print(rank_table)
# 
# # maximum (i.e. smallest!) fraction by which points are down-adjusted
# max_adj_factor<-0.8
# # add adjusted score column
# match_table_long$score_side_adj<-NA
# # calculate adjusted scores from court 'levels' (0=courts not assigned levels)
# for (i in 1:game_max){
#   if (match_table_long[match_table_long$game==i,]$court_rank[1] == 0){
#     match_table_long[match_table_long$game==i,]$score_side_adj<-
#       match_table_long[match_table_long$game==i,]$score_side
#   } else {
#     match_table_long[match_table_long$game==i,]$score_side_adj<-
#       match_table_long[match_table_long$game==i,]$score_side *
#       (1-((match_table_long[match_table_long$game==i,]$court_rank-1)*
#             ((1-max_adj_factor)/(match_table_long[match_table_long$game==i,]$no_of_courts-1))))
#   }
# }
# 
# 
# # Create table to collect sequential 4DR ranks:
# sequential_ranks<-data.frame(ID=character(),rank4dr=numeric(),date_time=ymd_hms(),
#                              stringsAsFactors=FALSE)
# 
# ## Decide on 4DR system ...
# fourDRCalc_zeroSum()