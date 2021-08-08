# ---------------------------------------------------------
# Prelims
# ---------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(magrittr)
library(ggplot2)
library(rio)
library(hrbrthemes)
library(Manu)
library(gganimate)

'%!in%' <- function(x,y)!('%in%'(x,y))
Sys.setenv(LANG = "en")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

setwd("/Users/jeppeviero/Dropbox/02 Fantasy PL/FPLdraft2021")

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
draft <- rio::import("data/fpl_2021.Rdata")

# ---------------------------------------------------------
# Plot
# ---------------------------------------------------------
ggplot(draft) +
  geom_point(aes(x = rank, y = pick_overall,
                 color = drafter,
                 fill = drafter,
                 size = rank),
             shape = 21,
             alpha = 0.5) +
  geom_point(aes(x = rank, y = pick_overall,
                 color = drafter,
                 fill = NA,
                 size = rank),
             shape = 21,
             fill = NA,
             alpha = 1.0) +
  scale_colour_manual(values = get_pal("Kotare")) +
  scale_fill_manual(values = get_pal("Kotare")) +
  scale_size(range = c(2, 8)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) +
  scale_y_continuous(breaks = seq(0, 90, 10),
                     labels = seq(0, 90, 10)) +
  theme_ipsum() +
  labs(x = "Draft Rank",
       y = "Draft Number") +
  theme(axis.title.x = element_text(size = 12,
                                    vjust = -7)) +
  theme(axis.title.y = element_text(size = 12,
                                    vjust = 7))


ggsave(plot = last_plot(),
       file = "plots/corrplot.jpg")
# knitr::plot_crop("corrplot.jpg",
#                  quiet = T)


ggplot(draft) +
  geom_point(aes(x = rank, y = pick_overall,
                 color = drafter,
                 fill = drafter,
                 size = rank),
             shape = 21,
             alpha = 0.5) +
  geom_point(aes(x = rank, y = pick_overall,
                 color = drafter,
                 fill = NA,
                 size = rank),
             shape = 21,
             fill = NA,
             alpha = 1.0) +
  scale_colour_manual(values = get_pal("Kotare")) +
  scale_fill_manual(values = get_pal("Kotare")) +
  scale_size(range = c(2, 8)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) +
  scale_y_continuous(breaks = seq(0, 90, 10),
                     labels = seq(0, 90, 10)) +
  theme_ipsum() +
  labs(x = "Draft Rank",
       y = "Draft Number") +
  theme(axis.title.x = element_text(size = 12,
                                    vjust = -7)) +
  theme(axis.title.y = element_text(size = 12,
                                    vjust = 7)) +
  facet_wrap(~ drafter)

ggsave(plot = last_plot(),
       file = "plots/corrplot_facet.jpg")




# ---------------------------------------------------------
# Plot GIF
# ---------------------------------------------------------
ggplot(draft) +
  geom_point(aes(x = rank, y = pick_overall,
                 color = drafter,
                 fill = drafter,
                 size = rank),
             shape = 21,
             alpha = 0.5) +
  # geom_point(aes(x = rank, y = pick_overall,
  #                color = drafter,
  #                fill = NA,
  #                size = rank),
  #            shape = 21,
  #            fill = NA,
  #            alpha = 1.0) +
  scale_colour_manual(values = get_pal("Kotare")) +
  scale_fill_manual(values = get_pal("Kotare")) +
  scale_size(range = c(2, 8)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) +
  scale_y_continuous(breaks = seq(0, 90, 10),
                     labels = seq(0, 90, 10)) +
  theme_ipsum() +
  labs(x = "Draft Rank",
       y = "Draft number") +
  theme(axis.title.x = element_text(size = 12,
                                    vjust = -7)) +
  theme(axis.title.y = element_text(size = 12,
                                    vjust = 7)) +
  transition_reveal(round) +
  #transition_time(round) +
  ease_aes('linear')


gif <- animate(plot = last_plot(), 
               end_pause = 30, 
               nframes = 150)

anim_save(gif, file = "plots/corr_plot.gif")



# draft <- draft %>% 
#   mutate(rev_pick_overall = (max(pick_overall) + 1) - pick_overall) %>% 
#   mutate(rev_rank = (max(rank) + 1) - rank)
# 
# max(draft$rank)
# min(draft$rank)
# 
# ggplot(draft) +
#   geom_point(aes(x = rank, y = rev_pick_overall,
#                  color = drafter,
#                  fill = drafter,
#                  size = rank),
#              shape = 21,
#              alpha = 0.5) +
#   geom_point(aes(x = rank, y = rev_pick_overall,
#                  color = drafter,
#                  fill = NA,
#                  size = rank),
#              shape = 21,
#              fill = NA,
#              alpha = 1.0) +
#   scale_colour_manual(values = get_pal("Kotare")) +
#   scale_fill_manual(values = get_pal("Kotare")) +
#   scale_size(range = c(2, 8)) +
#   guides(colour = guide_legend(override.aes = list(size = 7))) +
#   # scale_y_continuous(breaks = seq(0, 90, 10),
#   #                    labels = seq(0, 90, 10)) +
#   theme_ipsum() +
#   labs(x = "Draft Rank",
#        y = "Draft number") +
#   theme(axis.title.x = element_text(size = 12,
#                                     vjust = -7)) +
#   theme(axis.title.y = element_text(size = 12,
#                                     vjust = 7))
