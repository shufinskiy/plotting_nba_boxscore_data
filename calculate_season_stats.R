library(data.table)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(viridis)
library(grid)

table <- fread("F:/NBA_20190720/gamelog_func/PAGL.csv")
table1 <- fread("F:/NBA_20190720/gamelog_func/PTGL.csv")


xy <- copy(table1)

calculate_season_stats <- function(table, 
                                 value, 
                                 number = 10,
                                 method = "greater or equal",
                                 season) {
  table1 <- table[, Season := as.numeric(str_sub(SEASON_YEAR, 1, 4))][
   Season == season]
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
          , .(.N), by = .(count, TEAM_ABBREVIATION, SEASON_YEAR)][order(TEAM_ABBREVIATION)]
  } else if (method == "less"){
    table1 <- setDF(table1)
    table1 <- if (value %in% pct_list) {table1[table1[,6] < number/100,]
    } else {
      table1[table1[,6] < number,]
    }
    table1 <- setDT(table1)
    table1[, .(.N), by = .(TEAM_ABBREVIATION, GAME_ID, SEASON_YEAR)][, count := N][
      , N := NULL][
        , .(.N), by = .(count, TEAM_ABBREVIATION, SEASON_YEAR)][order(TEAM_ABBREVIATION)]
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
    if (value %in% pct_list) {table1[table1[,6] < number/100,]
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
    group_by(TEAM_ABBREVIATION) %>%
    arrange(desc(N), .by_group = TRUE)
  
  table4 <- setDT(table4)
  
  cclist <- lapply(unique(table2$TEAM_ABBREVIATION), function(team) {
    gg <- ggplot(filter(table2, TEAM_ABBREVIATION == team),
                 aes(x = count, y = N, fill = count, frame = TEAM_ABBREVIATION))+
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = seq(0, 15, 1)) +
      scale_fill_viridis_c() +
      theme_tufte() +
      ggtitle(team) +
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
    
    gg1 <- ggplot(filter(table3, TEAM_ABBREVIATION == team),
                  aes(x = reorder(PLAYER_NAME, -N), y = N, fill = N, frame = TEAM_ABBREVIATION))+
      geom_bar(stat = "identity") +
      scale_x_discrete(labels = table4[TEAM_ABBREVIATION == team, PLAYER_NAME]) +
      scale_fill_viridis_c(option = "plasma") +
      theme_tufte() +
      ggtitle(team) +
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
  
  cclist[["ncol"]] <- 5
  
  output <- do.call(grid.arrange, cclist)
  title <- textGrob(paste("Number of games with", value, method, number, "by team and number of players &\n",
                          "number of games", value, method, number, "by team and players in", unique(table2$SEASON_YEAR), "seasons", sep = " "), 
                    gp=gpar(fontsize=30, fontface = "italic", col = "grey20"))
  caption <- textGrob("Data sourse: stats.nba.com. Telegram: @NBAatlantic, Twitter: @vshufinskiy", 
                      gp=gpar(fontsize=25, fontface = "italic", col = "grey20"))
  output <- grid.arrange(top = title, bottom = caption, output)
}

r <- calculate_season_stats(xy, "PTS", number = 10, method = "greater or equal", season = 2018)
ggsave("PST.jpeg", r, height = 17, width = 24, units = "in")
