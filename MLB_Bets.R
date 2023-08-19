library(xml2)
library(rvest)
library(dplyr)
library(tidytext)
library(tm)
library(ggplot2)
library(forcats)
library(stringr)
library(readxl)
library(knitr)

# Full Season Tracker ---------

MLB_excel_season <- read_excel("Desktop/MLB Betting Tracker.xlsx", 3) %>%
  select(Scenario_Number,Game_Number,	Date, Away_Team, Home_Team,
         Type_of_Bet, Type_More_Info, Spread_Direction,
         Opening_Line,	Final_Line, Public_Bet_Percent,
         Opening_Line_Percent, Final_Line_Percent, Line_Percent_Difference,
         Unit_Bet, Win_Loss, One_Unit_Return) %>%
  filter(Type_of_Bet != "Total") %>%
  mutate(Public_Dawg = ifelse(Public_Bet_Percent < 0.5, "Public Dawg", "Public Fav")) %>%
  mutate(Line_Improved = ifelse(Line_Percent_Difference > 0, "Better Odds", "Worse Odds")) %>%
  mutate(PD_and_LI = ifelse(Public_Dawg == "Public Dawg" & Line_Improved == "Better Odds",
                            "Both", "Not Both"))

PD <- summarise(group_by(MLB_excel_season, Public_Dawg),
                the_sum = sum(One_Unit_Return),
                percent = sum(One_Unit_Return) / sum(Unit_Bet))

LI <- summarise(group_by(MLB_excel_season, Line_Improved),
                the_sum = sum(One_Unit_Return),
                percent = sum(One_Unit_Return) / sum(Unit_Bet))

PD_and_LI <- summarise(group_by(MLB_excel_season, PD_and_LI),
                       the_sum = sum(One_Unit_Return),
                       percent = sum(One_Unit_Return) / sum(Unit_Bet))

PD
LI
PD_and_LI

# Running Tracker July/August ---------------

MLB_excel <- read_excel("Desktop/MLB Betting Tracker.xlsx", 2) %>%
  select(Date, `Home Team`, `Away Team`, Bet, Odds, `Amount Bet`,
         `Win/Loss`, Return)

Moneyline_excel <- MLB_excel %>% filter(grepl("Moneyline", Bet))
Spread_excel <- MLB_excel %>% filter(grepl("+1.5", Bet) | grepl("-1.5", Bet))
Spread_excel_plus <- MLB_excel %>% filter(grepl("\\+1.5", Bet))
Spread_excel_minus <- MLB_excel %>% filter(grepl("-1.5", Bet))
Total_excel <- MLB_excel %>% filter(grepl("Over", Bet) | grepl("Under", Bet))

sum(Moneyline_excel$Return)
sum(Spread_excel$Return)
sum(Spread_excel_plus$Return)
sum(Spread_excel_minus$Return)
sum(Total_excel$Return)

sum(Moneyline_excel$Return) / sum(Moneyline_excel$`Amount Bet`)
sum(Spread_excel$Return) / sum(Spread_excel$`Amount Bet`)
sum(Total_excel$Return) / sum(Total_excel$`Amount Bet`)
# Historical Archives Database ----------

MLB_archive_2021 <- read_excel("Desktop/mlb-odds-2021.xlsx") %>%
  select(Year, Date, VH, Team, Final, Open, Close)
MLB_archive_2019 <- read_excel("Desktop/mlb-odds-2019.xlsx") %>%
  select(Year, Date, VH, Team, Final, Open, Close)
MLB_archive_2018 <- read_excel("Desktop/mlb-odds-2018.xlsx") %>%
  select(Year, Date, VH, Team, Final, Open, Close)

MLB_archive_2021 <- rbind(MLB_archive_2021, MLB_archive_2019, MLB_archive_2018)

MLB_archive_2021 <- cbind(MLB_archive_2021, 1:nrow(MLB_archive_2021)) %>%
  mutate(Open = as.numeric(Open)) %>%
  mutate(Scenario_Num = `1:nrow(MLB_archive_2021)`) %>%
  mutate(Game_Num = ifelse(VH == "V", (Scenario_Num + 1) / 2, Scenario_Num / 2)) %>%
  mutate(Win_Loss = NA) %>%
  mutate(Unit_Bet = 1.0) %>%
  mutate(Open_Percent = ifelse(Open < 0, abs(Open) / (abs(Open) + 100), 100 / (Open + 100)),
         Close_Percent = ifelse(Close < 0, abs(Close) / (abs(Close) + 100), 100 / (Close + 100))) %>%
  mutate(Percent_Diff = Open_Percent - Close_Percent) %>%
  mutate(Line_Improved = ifelse(Percent_Diff > 0, 
                                ifelse(Percent_Diff > 0.024, ">0.024", "0.024 > %Diff > 0"),
                                "Worse")) %>%
  mutate(Return = NA) %>%
  filter(!is.na(Open)) %>%
  select(Year, Scenario_Num, Game_Num, Date, VH, Team, Final, Win_Loss, Open, Close, 
         Open_Percent, Close_Percent, Percent_Diff, Line_Improved, Unit_Bet, Return)

for (i in 1:nrow(MLB_archive_2021)) {
  if(MLB_archive_2021$VH[i] == "V") {
    ifelse(MLB_archive_2021$Final[i] > MLB_archive_2021$Final[i+1],
           MLB_archive_2021$Win_Loss[i] <- 1,
           MLB_archive_2021$Win_Loss[i] <- 0)
  } else if (MLB_archive_2021$VH[i] == "H") {
    ifelse(MLB_archive_2021$Final[i] > MLB_archive_2021$Final[i-1],
           MLB_archive_2021$Win_Loss[i] <- 1,
           MLB_archive_2021$Win_Loss[i] <- 0)
  }
}

MLB_archive_2021 <- MLB_archive_2021 %>%
  filter(Game_Num != 1361) %>%
  filter(VH != "N")

MLB_archive_2021 <- MLB_archive_2021 %>%
  mutate(Return = ifelse(Win_Loss == 1, 
                         (ifelse(Close < 0, 
                                 Unit_Bet * 100 / abs(Close),
                                 Unit_Bet * Close / 100)),
                         -1.00))

MLB_archive_2021 %>% group_by(Year, Line_Improved) %>% summarize(the_sum = sum(Return), count = sum(Unit_Bet))
MLB_archive_2021 %>% group_by(Line_Improved) %>% summarize(the_sum = sum(Return), count = sum(Unit_Bet))
