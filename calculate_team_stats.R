library(data.table)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(viridis)
library(grid)

table <- fread("F:/NBA_20190720/gamelog_func/PAGL.csv")
table1 <- fread("F:/NBA_20190720/gamelog_func/PTGL.csv")

xy <- copy(table1)
calculate_team_stats <- function(table, 
                                 team, 
                                 value, 
                                 number, 
                                 method = "greater or equal",
                                 first_season = 2018,
                                 last_season = 2018,
                                 width = 24,
                                 ncol = 5) {
  table1 <- table[, Season := as.numeric(str_sub(SEASON_YEAR, 1, 4))][
    Season >= first_season & Season <= last_season][order(Season)]
  
  
  team_id_table <- data.table(TEAM_ID = c(1610612766, 1610612757, 1610612762, 1610612753, 1610612763, 1610612749, 1610612765,
                                          1610612744, 1610612742, 1610612748, 1610612750, 1610612755, 1610612760, 1610612751,
                                          1610612737, 1610612759, 1610612754, 1610612758, 1610612746, 1610612752, 1610612743,
                                          1610612741, 1610612739, 1610612761, 1610612764, 1610612738, 1610612745, 1610612747,
                                          1610612756, 1610612740),
                              TEAM_ABBREVIATION = c("CHA", "POR", "UTA", "ORL", "MEM", "MIL", "DET", "GSW", "DAL", "MIA", "MIN", "PHI", "OKC",
                                                    "BKN", "ATL", "SAS", "IND", "SAC", "LAC", "NYK", "DEN", "CHI", "CLE", "TOR", "WAS", "BOS",
                                                    "HOU", "LAL", "PHX", "NOP"))
  
  team_id <- team_id_table[TEAM_ABBREVIATION == team, .(TEAM_ID)][[1]]
  
  table1 <- table1[TEAM_ID == team_id]
  
  select_cols <- c("PLAYER_NAME",
                   "TEAM_ABBREVIATION",
                   "GAME_ID",
                   "SEASON_YEAR",
                   "PLAYER_ID",
                   value)
  table1 <- table1[, ..select_cols]
  
  pct_list <- c("AST_PCT", "OREB_PCT", "DREB_PCT", "REB_PCT", "EFG_PCT", 
                "TS_PCT", "USG_PCT", "PIE", "FG_PCT", "FG3_PCT", "FT_PCT")
  table2 <- if (method == "greater or equal"){
    table1 <- setDF(table1)
    table1 <- if (value %in% pct_list) {table1[table1[,6] >= number/100,]
    } else {
      table1[table1[,6] >= number,]
    }
    table1 <- setDT(table1)
    table1[, .(.N), by = .(TEAM_ABBREVIATION, GAME_ID, SEASON_YEAR)][, count := N][
      , N := NULL][
        , .(.N), by = .(count, TEAM_ABBREVIATION, SEASON_YEAR)]
  } else if (method == "less"){
    table1 <- setDF(table1)
    table1 <- if (value %in% pct_list) {table1[table1[,6] < number/100,]
    } else {
      table1[table1[,6] < number,]
    }
    table1 <- setDT(table1)
    table1[, .(.N), by = .(TEAM_ABBREVIATION, GAME_ID, SEASON_YEAR)][, count := N][
      , N := NULL][
        , .(.N), by = .(count, TEAM_ABBREVIATION, SEASON_YEAR)]
  } 
  
  table3 <- if(method == "greater or equal"){
    table1 <- setDF(table1)
    table1 <- if (value %in% pct_list) {table1[table1[,6] >= number/100,]
    } else {
      table1[table1[,6] >= number,]
    }
    table1 <- setDT(table1)
    table1[
      , .(.N), by = .(PLAYER_ID, PLAYER_NAME, TEAM_ABBREVIATION, SEASON_YEAR)][
        order(TEAM_ABBREVIATION, -N)][ 
          , PLAYER_NAME := str_replace(PLAYER_NAME, "^[:alpha:]{2,}|^[A-Z].[A-Z].", 
                                       str_extract(PLAYER_NAME, "^[A-Z][:graph:]"))]
  } else if(method == "less"){
    table1 <- setDF(table1)
    table1 <- if (value %in% pct_list) {table1[table1[,6] < number/100,]
    } else {
      table1[table1[,6] < number,]
    }
    table1 <- setDT(table1)
    table1[
      , .(.N), by = .(PLAYER_ID, PLAYER_NAME, TEAM_ABBREVIATION, SEASON_YEAR)][
        order(TEAM_ABBREVIATION, -N)][
          , PLAYER_NAME := str_replace(PLAYER_NAME, "^[:alpha:]{2,}|^[A-Z].[A-Z].", 
                                       str_extract(PLAYER_NAME, "^[A-Z][:graph:]"))]
  }
  table4 <- table3 %>%
    group_by(SEASON_YEAR) %>%
    arrange(desc(N), .by_group = TRUE)
  
  table4 <- setDT(table4)
  
cclist <- lapply(unique(table2$SEASON_YEAR), function(year) {
    gg <- ggplot(filter(table2, SEASON_YEAR == year),
                 aes(x = count, y = N, fill = count, frame = SEASON_YEAR))+
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = seq(0, 15, 1)) +
      scale_fill_viridis_c() +
      theme_tufte() +
      ggtitle(year) +
      geom_text(aes(label = N), color = "white", nudge_y = 3, size = rel(1.4)) +
      theme(plot.title = element_text(color="white",hjust=0.5,vjust=1, size=rel(2)),
            plot.caption = element_text(color = "white", size=rel(1.5)),
            plot.background = element_rect(fill="gray20"),
            panel.background = element_rect(fill="gray20"),
            panel.border = element_rect(fill=NA,color="gray20", size=0.5, linetype="solid"),
            axis.text = element_text(color="white"),
            axis.text.y  = element_text(hjust=1),
            legend.position = "none",
            strip.text.x = element_text(color = "white"),
            axis.title = element_blank())
    
    gg1 <- ggplot(filter(table3, SEASON_YEAR == year),
                  aes(x = reorder(PLAYER_ID, -N), y = N, fill = N, frame = SEASON_YEAR))+
      geom_bar(stat = "identity") +
      scale_x_discrete(labels = table4[SEASON_YEAR == year, PLAYER_NAME]) +
      scale_fill_viridis_c(option = "plasma") +
      theme_tufte() +
      ggtitle(year) +
      geom_text(aes(label = N), color = "white", nudge_y = 3, size = rel(1.4)) +
      theme(plot.title = element_text(color="white",hjust=0.5,vjust=1, size=rel(2)),
            plot.caption = element_text(color = "white", size=rel(1.5)),
            plot.background = element_rect(fill="gray20"),
            panel.background = element_rect(fill="gray20"),
            panel.border = element_rect(fill=NA,color="gray20", size=0.5, linetype="solid"),
            axis.text = element_text(color="white"),
            axis.text.y  = element_text(hjust=1),
            axis.text.x = element_text(angle = 90, size = rel(0.8)),
            legend.position = "none",
            strip.text.x = element_text(color = "white"),
            axis.title = element_blank())
    grid.arrange(gg, gg1, ncol = 2)
  })
  
 cclist[["ncol"]] <- ncol
  
  output <- do.call(grid.arrange, cclist)
  title <- textGrob(paste("Number of games", team, "with", value, method, number, "by number of players &\n",
                          "number of games", team, "with", value, method, number, "by players from", 
                          head(table2$SEASON_YEAR, 1), "to", tail(table2$SEASON_YEAR, 1), "seasons", sep = " "), 
                    gp=gpar(fontsize= 12 + 0.97*(width - 5.46), fontface = "italic", col = "grey20"))
  caption <- textGrob("Data sourse: stats.nba.com. Telegram: @NBAatlantic, Twitter: @vshufinskiy", 
                      gp=gpar(fontsize= 12 + 0.81*(width - 5.46), fontface = "italic", col = "grey20"))
  output <- grid.arrange(top = title, bottom = caption, output)
}

Team <- calculate_team_stats(xy, team = "CLE", value = "PTS", number = 10, first_season = 1996, width = 24, ncol = 5)
ggsave("CLE.jpeg", Team, height = 14, width = 24, units = "in")
