library(data.table)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(viridis)
library(grid)

table <- fread("F:/NBA_20190720/gamelog_func/PAGL.csv")
table1 <- fread("F:/NBA_20190720/gamelog_func/PTGL.csv")

xy <- copy(table1)

calculate_season_stats_facet <- function(table, 
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
  
  gg <- ggplot(table2, aes(x = count, y = N, fill = count))+
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = seq(1, 15, 1)) +
    scale_fill_viridis_c() +
    facet_wrap(~TEAM_ABBREVIATION, ncol = 6, scales = "fixed") +
    theme_tufte() +
    geom_text(aes(label = N), color = "white", nudge_y = 3, size = rel(2.5)) +
    theme(plot.title = element_text(color="white",hjust=0.5,vjust=1, size=rel(2.5)),
          plot.caption = element_text(color = "white", size=rel(1.5)),
          plot.background = element_rect(fill="gray20"),
          panel.background = element_rect(fill="gray20"),
          panel.border = element_rect(fill=NA,color="gray20", size=0.5, linetype="solid"),
          axis.text = element_text(color="white"),
          axis.text.y  = element_text(hjust=1),
          legend.position = "bottom",
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white"),
          strip.text.x = element_text(color = "white", size = rel(2.5)),
          axis.title = element_blank())
  
  title <- textGrob(paste("Number of games with", value, method, number, "by team and number of players", unique(table2$SEASON_YEAR), "seasons", sep = " "), 
                    gp=gpar(fontsize=15, fontface = "italic", col = "grey20"))
  caption <- textGrob("Data sourse: stats.nba.com. Telegram: @NBAatlantic, Twitter: @vshufinskiy", 
                      gp=gpar(fontsize=12, fontface = "italic", col = "grey20"))
  output <- grid.arrange(top = title, bottom = caption, gg)
}

r <- calculate_season_stats_facet(xy, "PTS", number = 10, method = "greater or equal", season = 2012)
ggsave("facet_wrap.jpeg", r, height = 10, width = 11, units = "in")
