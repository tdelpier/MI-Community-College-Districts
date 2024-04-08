
library(sf)
library(tidyverse)
library(TannersTools)
library(ggrepel)

# private functions that bring in public geographic data
tt_dir_rd()
geo_dist <- tt_import_geo_district() %>% mutate(geo.id = as.numeric(dcode)) %>% st_make_valid()
geo_co <- tt_import_geo_county() %>% mutate(geo.id = as.numeric(fipsnum))%>% st_make_valid()
geo_isd <- tt_import_geo_isd() %>% mutate(geo.id = as.numeric(dcode)) %>% st_make_valid()
geo_town <- tt_import_geo_township() %>% mutate(geo.id = as.numeric(FIPSNUM))%>% st_make_valid()

names_cc <- 
  readxl::read_excel("Maps/MI-Community-College-Districts/240404_community-college-district-description.xlsx") %>% 
  rename(opeid = OPEID,
         names = College)


# a function to easily choose multiple geographies of the same type
choose_geo <- function(geo.data, ...) {
  
  return.geo <- 
    geo.data %>% 
    dplyr::filter(geo.id %in% c( ... )) 
  
  if(return.geo %>% nrow() > 1) {
    
    return.geo <- 
      return.geo %>% 
      sf::st_union() %>% 
      st_as_sf() %>% 
      ungroup() 
    }
  
  return(return.geo)
  

}


# a function to clean a couple things and add an opeid number for merging
clean_geo <- function(geo.data, opeid){
  
  return.geo <- 
    geo.data %>% 
    mutate(opeid = {{ opeid }}) %>% 
    dplyr::select(opeid, geom) 
  
}



# Defining CC districts based on other geographies 
cc_alpena <- 
  choose_geo(geo_co, 7) %>% 
  st_difference(choose_geo(geo_dist, 60020)) %>% 
  clean_geo(223700)

cc_baydenoc <- 
  choose_geo(geo_co, 41) %>% 
  clean_geo(224000)

cc_delta <- 
  choose_geo(geo_co, 17, 111, 145) %>% 
  rename(geom = x) %>% 
  clean_geo(225100)

cc_glen <- 
  choose_geo(geo_isd, 75000) %>% 
  st_difference(choose_geo(geo_dist, 14050)) %>% 
  st_difference(choose_geo(geo_co, 27)) %>% 
  clean_geo(226300)

cc_gogebic <- 
  choose_geo(geo_co, 53) %>% 
  clean_geo(226400)

cc_grandrapids <- 
  choose_geo(geo_isd, 41000) %>% 
  clean_geo(226700)

cc_henryford <- 
  choose_geo(geo_dist, 82030) %>% 
  clean_geo(227000)

cc_jackson <- 
  choose_geo(geo_co, 75) %>% 
  st_difference(choose_geo(geo_co, 65)) %>% 
  clean_geo(227400)

cc_kzoo <- 
  choose_geo(geo_isd, 39000) %>% 
  st_union(choose_geo(geo_dist, 80150)) %>% 
  clean_geo(694900)

cc_kellogg <- 
  choose_geo(geo_isd, 13000) %>% 
  st_difference(choose_geo(geo_dist, 23010, 23080) %>% st_make_valid()) %>% 
  clean_geo(227600)

cc_kirtland <- 
  choose_geo(geo_isd, 72000) %>% 
  clean_geo(717100)

lakemi_add1 <- 
  choose_geo(geo_dist, 80010) %>% 
  st_intersection(choose_geo(geo_co, 5, 159)) %>% 
  st_make_valid()

lakemi_add2 <- 
  choose_geo(geo_town, 18560) %>% 
  st_intersection(choose_geo(geo_co,  159)) %>% 
  st_make_valid()

cc_lakemi <- 
  choose_geo(geo_co, 21) %>%   st_make_valid() %>% 
  st_union(lakemi_add1) %>% 
  st_union(lakemi_add2) %>% 
  clean_geo(227700)

rm(lakemi_add1, lakemi_add2)

cc_lansing <- 
  choose_geo(geo_dist, 19100, 33040, 19010, 33010, 33020, 23060, 33060, 33070, 
             33100, 33130, 33170, 33200, 33215, 33220, 33230) %>% 
  rename(geom = x) %>% 
  clean_geo(227800)

cc_macomb <- 
  choose_geo(geo_co, 99) %>% 
  clean_geo(890600)
  
cc_midmi <- 
  choose_geo(geo_isd, 18000) %>% 
  clean_geo(676800)

cc_monroe <- 
  choose_geo(geo_co, 115) %>% 
  clean_geo(229400)

cc_montcalm <- 
  choose_geo(geo_isd, 59000) %>% 
  clean_geo(229500)

cc_mott <- 
  choose_geo(geo_isd, 25000) %>% 
  clean_geo(226100)

cc_muskegon <- 
  choose_geo(geo_co, 121) %>% 
  st_difference(choose_geo(geo_isd, 41000)) %>% 
  clean_geo(229700)

cc_northcentral <- 
  choose_geo(geo_co, 47) %>% 
  clean_geo(229900)

cc_northwest <- 
  choose_geo(geo_co, 55) %>% 
  clean_geo(230200)

cc_oakland <- 
  choose_geo(geo_isd, 63000) %>% 
  st_difference(choose_geo(geo_dist, 63100)) %>% 
  clean_geo(230300)

cc_schoolcraft <- 
  choose_geo(geo_dist, 82095, 82050, 82100, 82390, 63090, 63100) %>% 
  rename(geom = x) %>% 
  clean_geo(231500)

southwest_add1 <- 
  choose_geo(geo_town, 42500, 36160) %>% 
  st_intersection(choose_geo(geo_co, 159)) %>% 
  st_make_valid()

cc_southwest <- 
  choose_geo(geo_co, 27) %>% 
  st_union(southwest_add1) %>% 
  clean_geo(231700)

rm(southwest_add1)

cc_stclair <- 
  choose_geo(geo_isd, 74000) %>% 
  st_difference(choose_geo(geo_isd, 50000)) %>% 
  clean_geo(231000)

cc_washtenaw <- 
  choose_geo(geo_co, 161) %>% 
  st_difference(choose_geo(geo_isd, 33000, 82000, 63000)) %>% 
  clean_geo(232800)

cc_wayne <- 
  choose_geo(geo_co, 163) %>% 
  st_difference(choose_geo(geo_dist, 82070, 82030, 82095, 82050, 82100, 82390, 63090, 81070, 58020) %>% st_make_valid()) %>% 
  clean_geo(232900)


cc_westshore <- 
  choose_geo(geo_dist, 51020, 51045, 53040, 51070, 53010, 53020, 51060, 64090) %>% 
  st_union(choose_geo(geo_town, 19100, 25200, 85040)) %>% 
  rename(geom = x) %>% 
  clean_geo(795000)
# crystal township is two numbers: 19100 and 19080






# Combining all the CC districts together 

cc_districts <- 
  bind_rows(cc_alpena,  cc_baydenoc, cc_delta, cc_glen, cc_gogebic, cc_grandrapids,
            cc_henryford, cc_jackson, cc_kzoo, cc_kellogg, cc_kirtland, cc_lakemi,
            cc_lansing, cc_macomb, cc_midmi, cc_monroe, cc_montcalm, cc_mott, 
            cc_muskegon, cc_northcentral, cc_northwest, cc_oakland, cc_schoolcraft,
            cc_southwest, cc_stclair, cc_washtenaw, cc_wayne, cc_westshore) %>% 
  left_join(names_cc) %>% 
  select(opeid, names, Label, geom) %>% 
  st_make_valid() %>% 
  cbind(., st_coordinates(st_centroid(.)))


# write data to a geo package 
st_write(cc_districts, "Maps/MI-Community-College-Districts/cc_map.gpkg")


# create SVG map
map_svg <- 
  ggplot() + 
  geom_sf(data = geo_co) +
  geom_sf(data = cc_districts , 
          aes(fill = Label, label = Label, color = names),
          color = "white",
          size = 2) +
  geom_label_repel(data = cc_districts , 
                aes(X, Y,fill = Label, label = Label, color = names), fill = "white",
               size = 1.5,
               label.padding = unit(0.1, "lines"),
               position = position_jitter()
               ) +
  scale_y_continuous(limits = c(41.9, 47.3)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Michigan's Community College Districts",
       caption = "Unofficial map created by Tanner Delpier based on district descriptions")


ggsave(filename = "MI-Community-College-Districts.svg",
       plot = map_svg,
       path = "Maps/MI-Community-College-Districts")

