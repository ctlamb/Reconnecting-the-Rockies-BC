Reconnecting the Rockies:BC early results
================
Clayton Lamb
24 April, 2021

## Load Packages & Data

``` r
library(here)
library(raster)
library(sf)
library(ggmap)
library(mapview)
library(rgdal)
library(hrbrthemes)
library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(tidylog)
options(scipen=999)
##################  
##LOAD DATA
################## 

##camera deployments
cams <- read_csv(here::here("data","Camera Deployment.csv"))

##scored photos
df <- read_csv(here::here("data","Highway_3_Wildlife_Collision_Reduction_2020_WILDTRAX_REPORT.csv"))
```

## Map of cams

``` r
##make spatial
cams <- st_as_sf(cams,
               coords=c("Location (longitude)", "Location (latitude)"),
               crs=4326)
##map
# register_google("ADD OWN TOKEN HERE")
# basemap <- ggmap::get_map(location = c(lat = 49.686547, lon =-114.799671),zoom=12)
# saveRDS(basemap,file=here::here("data","basemap.rds"))
 basemap <- readRDS(here::here("data","basemap.rds"))

##kmz
st_write(cams%>%select(Name=`Camera Name`), here::here("data", "hwycams_all.kml"), driver = "kml", delete_dsn = TRUE)


##drop cams not in use
cams_active <- cams%>%
  filter(is.na(`Date Removed`))

##stats
stats <-paste0(n_distinct(cams_active%>%pull(`Camera Name`)), " Cameras, (",
               n_distinct(cams_active%>%filter(Type%in%"Control")%>%pull(`Camera Name`))," Control & ",
               n_distinct(cams_active%>%filter(Type%in%"Treatment")%>%pull(`Camera Name`))," Treatment)")


cols <- brewer.pal(n = 8, name = "Dark2")[c(3,6)]
map <- ggmap(basemap)+
  geom_sf(data=cams_active, aes(color=Type),size=5, alpha=0.6,inherit.aes = FALSE)+
  labs(subtitle=stats)+
  theme_ipsum()+
  scale_color_manual(values=cols)+
  ggtitle("Connectivity Monitoring for Hwy 3 Hosmer-Alberta Project")

ggsave(here::here("plots","map_active.png"), height=7, width=7, unit="in")

##kmz
st_write(cams_active%>%select(Name=`Camera Name`), here::here("data", "hwycams_active.kml"), driver = "kml", delete_dsn = TRUE)

##plot map
map
```

![](README_files/figure-gfm/map-1.png)<!-- -->

## Prep camera data

``` r
##join with cam data
df<- df%>%
  left_join(cams%>%
  tibble%>%
  select(structure=`Structure Name`,location=`Camera Name`,Type, deployed=`Date Deployed`,removed=`Date Removed`)%>%
      mutate(location=str_to_upper(location),
             deployed=mdy_hm(deployed),
             removed=mdy_hm(removed)),
  by="location")%>%
  group_by(location)%>%
  mutate(removed=case_when(is.na(removed)~max(date_detected),
                           TRUE~removed))



df%>%
  count(common_name)

df_trim <- df%>%
  filter(common_name %in% c("Bighorn sheep", "Black Bear", "Canada Lynx", "Cougar", "Coyote","Domestic Dog","Elk (wapiti)", "Grizzly bear","Human","Moose","Red fox","River Otter", "mink","Striped Skunk","Vehicle","White-tailed Deer","Mule deer"))


##identify distinct events
df_trim <- df_trim%>%
  group_by(location,common_name)%>%
  arrange(date_detected)%>%
  mutate(timedif=c(0.1,diff(date_detected))/60,
         start = timedif > 20,
         event.id=paste(cumsum(start),common_name,location, sep="_"))
```

\#\#plot

``` r
df_trim%>%
  group_by(common_name,location)%>%
  count%>%
  ggplot(aes(x=location,y=n,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Pic count",
       title="Total Picture Counts")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/plot%20results-1.png)<!-- -->

``` r
df_trim%>%
  group_by(common_name,location)%>%
  count%>%
  mutate(n=1)%>%
  ggplot(aes(x=location,y=n,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Species count",
       title="Total Species Counts")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/plot%20results-2.png)<!-- -->

``` r
df_trim%>%
  group_by(common_name,location)%>%
  summarise(n=n_distinct(event.id))%>%
  ggplot(aes(x=location,y=n,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Event count",
       title="Unique Events")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/plot%20results-3.png)<!-- -->

``` r
df_trim%>%
  filter(!common_name%in%c("Domestic Dog","Human"))%>%
  group_by(location,event.id,common_name)%>%
  summarise(n=mean(number_individuals%>%as.numeric(), na.rm=TRUE))%>%
  group_by(location,common_name)%>%
  summarise(n=sum(n, na.rm=TRUE))%>%
  ggplot(aes(x=location,y=n,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Total detections",
       title="Unique Events, with abundance (n animals seen) added in")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/plot%20results-4.png)<!-- -->

``` r
df_trim%>%
  filter(!common_name%in%c("Domestic Dog","Human"))%>%
  mutate(dur=((removed-deployed)/30)%>%as.numeric)%>%
  group_by(location,event.id,common_name)%>%
  summarise(n=mean(number_individuals%>%as.numeric(), na.rm=TRUE),
            dur=mean(dur))%>%
  group_by(location,common_name)%>%
  summarise(n=sum(n, na.rm=TRUE),
            dur=mean(dur),
            hitrate=n/dur)%>%
  ggplot(aes(x=location,y=hitrate,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Monthly detections",
       title="Monthly Hit rates")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/plot%20results-5.png)<!-- -->

## Compare treatment vs control

``` r
df_trim%>%
filter(common_name %in% c("Bighorn sheep", "Black Bear", "Canada Lynx", "Cougar", "Coyote","Elk (wapiti)", "Grizzly bear","Moose","Red fox","White-tailed Deer","Mule deer"))%>%
  mutate(dur=((removed-deployed)/30)%>%as.numeric)%>%
  group_by(location,event.id,common_name,Type)%>%
  summarise(n=mean(number_individuals%>%as.numeric(), na.rm=TRUE),
            dur=mean(dur))%>%
  group_by(Type,common_name)%>%
  summarise(n=sum(n, na.rm=TRUE),
            dur=mean(dur),
            hitrate=n/dur)%>%
  ggplot(aes(x=Type,y=hitrate,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Monthly detections",
       title="Monthly detections between control and treatment sites")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))+
  scale_fill_brewer(palette = "Paired")
```

![](README_files/figure-gfm/plot%20results2-1.png)<!-- -->

``` r
df_trim%>%
filter(common_name %in% c("Bighorn sheep", "Black Bear", "Canada Lynx", "Cougar", "Coyote","Elk (wapiti)", "Grizzly bear","Moose","Red fox","White-tailed Deer","Mule deer"))%>%
  mutate(dur=((removed-deployed)/30)%>%as.numeric)%>%
  group_by(location,structure,event.id,common_name,Type)%>%
  summarise(n=mean(number_individuals%>%as.numeric(), na.rm=TRUE),
            dur=mean(dur))%>%
  group_by(Type,structure, common_name)%>%
  summarise(n=sum(n, na.rm=TRUE),
            dur=mean(dur),
            hitrate=n/dur)%>%
  ggplot(aes(x=Type,y=hitrate,fill=common_name))+
  facet_wrap(vars(structure), scales="free_y")+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Monthly detections",
       title="Monthly detections between control and treatment sites")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))+
  scale_fill_brewer(palette = "Paired")
```

![](README_files/figure-gfm/plot%20results2-2.png)<!-- -->
