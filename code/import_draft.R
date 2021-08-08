# ---------------------------------------------------------
# Prelims
# ---------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(magrittr)
library(rio)
library(openxlsx)

'%!in%' <- function(x,y)!('%in%'(x,y))
Sys.setenv(LANG = "en")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

setwd("/Users/jeppeviero/Dropbox/02 Fantasy PL/FPLdraft2021")

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
draft <- rio::import("data/2021_FPLdraft.xlsx")

# ---------------------------------------------------------
# Prep data
# ---------------------------------------------------------
draft <- draft %>%
  mutate(sequence = rep(seq(1, 3, 1),
                        90)) %>% 
  mutate(player = lag(col1, 1)) %>% 
  mutate(club = lag(col1, 2)) %>% 
  mutate(rank = lag(col2, 2)) %>% 
  mutate(round = lag(col3, 2)) %>% 
  mutate(pick_round = lag(col4, 2)) %>% 
  mutate(drafter = lag(col5, 2)) %>% 
  filter(sequence == 3) %>% 
  mutate(pick_overall = row_number())

draft <- draft %>% 
  mutate(position = substrRight(col1, 3))

head(draft)

draft <- draft %>% 
  select(c(player, club, position,
           rank, round, drafter,
           pick_round, pick_overall))

draft <- draft %>% 
  group_by(drafter) %>% 
  mutate(drafter_pickno = row_number()) %>% 
  ungroup()

# ---------------------------------------------------------
# Export data
# ---------------------------------------------------------
# ----- R
save(draft,
     file = "data/fpl_2021.Rdata")

# ----- Excel
write.xlsx(draft,
           file = "data/fpl_2021.xlsx")



