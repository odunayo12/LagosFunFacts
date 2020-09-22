library(tidyverse)
library(gganimate)
library(lubridate)
library(gifski)
# SFWatch <- read_csv("C:/Users/rotim/OneDrive/Laptop/Data Project/R Projects/SFWatch/SFWatch/Data/SelectedFoodWatch.csv")
# 
# SFWatch_tidy  <-
#   SFWatch %>% filter(State == "Lagos", Classification == "Food Item")
# 
# write_csv(SFWatch_tidy, "Data_/SFWatch_tidy.csv")


SFWatch_tidy <- read_csv("./Data_/SFWatch_tidy.csv")
#head(SFWatch_tidy)
SFWatch_tidy$Date <- dmy(SFWatch_tidy$Date)
SFWatch_tidy$Date <- format(as.Date(SFWatch_tidy$Date), "%Y-%m")
#SFWatch_tidy$Date <- month.abb[SFWatch_tidy$Date]

SFWatch_formatted <- SFWatch_tidy %>%
  group_by(Date) %>%
  mutate(
    rank = rank(-Amount),
    Value_rel = Amount / Amount[rank == 1],
    Value_lbl = paste0("NGN ", format(round(Amount * 1), big.mark = ","))
  ) %>%
  group_by(GenItemLabel) %>%
  filter(rank <= 10) #%>%
#ungroup() #%>%
#arrange(asc(Date))
#Value_lbl = paste0("NGN ",format(round(Amount*1.0), nsmall = 1, big.mark = ","))) %>% group_by(GenItemLabel)
# Animation


anim2 <- ggplot(
  SFWatch_formatted,
  aes(
    rank,
    group = GenItemLabel,
    fill = as.factor(GenItemLabel),
    color = as.factor(GenItemLabel)
  )
) +
  geom_tile(aes(
    y = Amount / 2,
    height = Amount,
    width = 0.9
  ),
  alpha = 0.8,
  color = NA) +
  geom_text(aes(y = 0, label = paste(GenItemLabel, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = Amount, label = Value_lbl, hjust = 0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(size = .1, color = "grey"),
    panel.grid.minor.x = element_line(size = .1, color = "grey"),
    plot.title = element_text(
      size = 20,
      hjust = 0.5,
      face = "bold",
      vjust = -1
    ),
    plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
    plot.caption = element_text(size = 8),
    plot.background = element_blank(),
    plot.margin = margin(2, 2, 2, 4, "cm")
  ) +
  transition_states(Date, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(
    title = 'Price in {closest_state}',
    subtitle  =  "Top 10 most Expensive Food Items (per KG) in Lagos State, Nigeria",
    caption  = "Data Source: NATIONAL BUREAU OF STATISTICS |Plot by: Odunayo Rotimi",
    y = "",
    x = ""
  )

# For GIF

animate(
  anim2,
  nframes = 300,
  fps = 5,
  width = 600,
  height = 400,
  renderer = gifski_renderer("Animation/gganim2.gif")
)

# For MP4  width = 600, height = 500,

animate(
  anim2,
  nframes = 300,
  fps = 5,
  width = 600,
  height = 400,
  renderer = ffmpeg_renderer()
) -> for_mp4

anim_save("Animation/animation2.mp4", animation = for_mp4)

#,hjust=0.5, face="italic"
