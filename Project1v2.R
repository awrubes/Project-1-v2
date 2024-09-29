library(stringr)
library(dplyr)

#import chess data as txt file
chess_data <- suppressWarnings(readLines("https://raw.githubusercontent.com/awrubes/DATA607_Project1/main/tournamentinfo.txt"
))
head(chess_data)
chess_df <-tibble(raw=chess_data)

head(chess_df)

chess_df_clean <- chess_df %>%
  filter(!grepl("^-+$", raw),
          !grepl("^\\s*(Pair|Num)", raw))


head(chess_df_clean)

chess_df_clean <-chess_df_clean %>%
  mutate(row_num = row_number()) %>%
  mutate(info_type = ifelse(row_num %%2 == 1, "player_info", "player_rating_info")) %>%
  group_by(info_type) %>%
    mutate(group = row_number())%>%
    pivot_wider(names_from = info_type, values_from = raw) %>%
    select(-group)
  
head(chess_df_clean)

chess_df_clean <- chess_df_clean %>%
  mutate(
    player_rating_info = ifelse(is.na(player_rating_info), lead(player_rating_info), player_info)
       )%>%
  filter(!is.na(player_info), !is.na(player_rating_info))
  
View(chess_df_clean)

chess_df_clean <- chess_df_clean %>%
  mutate(player_name = str_extract(player_info, "(?<=\\|)\\s*[A-Z\\s]+(?=\\s*\\|)"))%>%
  mutate(state = trimws(str_extract(player_rating_info, "^\\s*[A-Z]{2}\\b")))%>%
  mutate(total_points = as.numeric(str_extract(player_info, "(?<=|)\\d\\.\\d")))%>%
  mutate(pre_rating = as.numeric(str_extract(player_rating_info, "(?<=R:)\\s*(\\d+)(?=\\s*->|\\w)")))%>%
  mutate(games_played = as.numeric(str_count(player_info, "\\b(W|L|D){1}\\s*\\d+")))%>%
  mutate(
    opponent_ids = lapply(player_info, function(info){
      as.numeric(trimws(unlist(str_extract_all(info, "(?<=\\b[WLD])\\s*\\d+"))))
    })
  )%>%
  mutate(
    avg_op_rating=sapply(opponent_ids, function(ids){
      
      opponent_ratings <- .data$pre_rating[ids]
      
      total_opponent_ratings <- sum(opponent_ratings, na.rm = TRUE)
      
      avg_op_rating <- total_opponent_ratings / length(ids)
      return(avg_op_rating)
      
    })
  )

head(chess_df_clean)

selected_columns <- chess_df_clean[, c("player_name", "state", "total_points", "pre_rating", "avg_op_rating")]
head(selected_columns)

write.csv(selected_columns, file = "chess_tournament_selected_columns.csv", row.names = FALSE)  
