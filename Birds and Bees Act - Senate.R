library(tidyverse)
library(sf)
library(jsonlite)
rm(list=ls())

sen <- 
  read_sf('https://services6.arcgis.com/EbVsqZ18sv1kVJ3k/arcgis/rest/services/NYS_Senate_Districts/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') %>% 
  st_transform(3857) %>%
  rmapshaper::ms_simplify()

# replace with your key
votes <- fromJSON('https://legislation.nysenate.gov/api/3/bills/2023/s1856a?key=49fCHARAWjylZZT7Oavh1qvpJbmbJxKK')

votes <- tibble(
  DISTRICT = votes[["result"]][["votes"]][["items"]][["memberVotes"]][1][['items']][['AYE']][1][1][[1]][[2]][,'districtCode'],
  Vote = 'AYE'
) %>%
  rbind(
    tibble(
      DISTRICT = votes[["result"]][["votes"]][["items"]][["memberVotes"]][1][['items']][['NAY']][1][1][[1]][[2]][,'districtCode'],
    Vote = 'NAY'
    )   
  ) %>%
  rbind(
    tibble(
      DISTRICT = votes[["result"]][["votes"]][["items"]][["memberVotes"]][1][['items']][['EXC']][1][1][[1]][[2]][,'districtCode'],
      Vote = 'EXC'
    )   
  )

sen <- sen %>% inner_join(votes, join_by(DISTRICT)) %>%
  mutate(Party.Vote = str_c(Party, Vote, sep = ' - '))

library(ggpattern)
library(tigris)
ups <- counties('ny') %>% filter(NAME %in% c('Chautauqua', 'Niagara', 'Clinton','Putnam')) %>% st_transform(3857) %>% st_bbox()
ds <- counties('ny') %>% filter(NAME %in% c('Rockland','Richmond','Suffolk')) %>% st_transform(3857) %>% st_bbox()

ggplot(sen) + geom_sf_pattern(aes(fill=Vote, pattern=Party), linewidth=0.1, pattern_size=0.05, 
                               pattern_alpha=0.6, pattern_density=0.4, color='white') +
  scale_fill_manual(values = c('#1b9e77', '#7570b3','#d95f02')) +
  geom_sf_text(aes(label=DISTRICT), color='white', size=2.2, check_overlap = T, fontface='bold') + 
  scale_pattern_manual(values = c('none','wave')) +
  coord_sf(crs=3857, xlim = c(ups$xmin, ups$xmax), ylim=c(ups$ymin, ups$ymax), expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="font-size: 22pt;"><b>Birds and Bees Act</b> <br />Floor Vote S.1856a<br />(Bans Seeds Treated with Neonicotinoids)<br /><span style="font-size: 18pt">June 7, 2023</span>'),
       tag=str_c(
         '<b>Data Source:</b> NYS Senate API<br />',
         'Andy Arthur - ', format(Sys.Date(), format="%-m.%-d.%y"),
         '')
  )  +
  theme(
    text= element_text(family='Roboto Condensed',size=18),
    plot.title=ggtext::element_textbox_simple(hjust=0, halign=0, size=28, margin=margin(5,0,20,0), maxheight = 0, maxwidth=0.43),
    plot.background = element_rect(fill = "white", linewidth=0),
   legend.position = 'None',
   plot.tag=ggtext::element_textbox(size=14,hjust=0, color='#555555', maxheight=0, halign = 0, valign=0, margin = margin(b=10)),
   plot.tag.position = c(0,0)
  ) -> upmap


ggplot(sen) + geom_sf_pattern(aes(fill=Vote, pattern=Party), linewidth=0.1, pattern_size=0.05, 
                               pattern_alpha=0.6, pattern_density=0.4, color='white') +
  scale_fill_manual(values = c('#1b9e77', '#7570b3','#d95f02')) +
  geom_sf_text(aes(label=DISTRICT), color='white', size=2.2, check_overlap = T, fontface='bold') + 
  scale_pattern_manual(values = c('none','wave')) +
  coord_sf(crs=3857, xlim = c(ds$xmin, ds$xmax), ylim=c(ds$ymin, ds$ymax), expand=F) +
  theme_void() + 
  theme(
    legend.key.height = unit(1,'cm'),
    legend.direction = 'horizontal',
    legend.key.width = unit(1,'cm'),
    legend.title = element_text(face = 'bold'),
    legend.text = element_text(margin = margin(t = 18, unit = "pt")),
    legend.position = c(1,0),
    legend.justification = c(1,0),
    text= element_text(family='Roboto Condensed',size=18),
    plot.background = element_rect(fill = "white", linewidth=0),
  ) +
  guides(fill = guide_legend(override.aes = list(pattern='none'), direction = 'horizontal')) -> dsmap

library(patchwork)
upmap / dsmap

fn <- str_c('birds-bees-senate')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1080, height=1400, units='px', dpi=110)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1080, height=1400, units='px', dpi=110, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

source('upload-svg.R', local = T)
