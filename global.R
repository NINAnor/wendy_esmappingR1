## global

#### global file

library(shiny)
library(shinyjs)
library(bigrquery)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(leafem)
library(leafpop)
library(mapview)
library(dplyr)
library(mapedit)
library(sf)
library(SSDM)
library(stringi)
library(dbplyr)
library(DT)
library(shinycssloaders)
library(tibble)
library(shinyWidgets)
library(tidyverse)
library(shinyBS)
library(shinybusy)
library(googleCloudStorageR)
library(terra)
library(elevatr)



source("mod_questionnaire.R")
source("mod_mapping1.R")
source("mod_instructions.R")

### BQ connection to store rectangles
bq_auth(
  path = "bq_wendy.json"
)

###GCS auth
gcs_auth("bq_wendy.json")
#
# gcs_get_bucket("ind_es") #name of the bucket that you have created
# bucket_name <- "ind_es"
# gcs_global_bucket("ind_es") #set it as global bucket
# gcs_get_global_bucket() #check if your bucket is set as global,you should get your bucket name


env<-"dev"
project<-"wendy"
var_lang<-"en"
dataset <- paste0(project,"_",env)
# dataset <- "admin_data"

con_admin<-data.frame(
  project = "eu-wendy",
  dataset = dataset,
  billing ="eu-wendy"
)


con_admin <- dbConnect(
  bigrquery::bigquery(),
  project = con_admin$project,
  dataset = con_admin$dataset,
  billing = con_admin$billing
)

studies<-tbl(con_admin, "studSITE")
studies<-studies%>%collect()
#just for testing since this data is prepared
studies<-studies%>%filter((siteID == "ITA-BGL" | siteID == "ITA-MSF") & siteSTATUS == 1 & projID == project)


es_study<-tbl(con_admin, "es_descr")
stud_es<-es_study%>%collect()



#how many es should be mapped by each participant from all ES?
num_tabs <- 4

#load env var for random forest
# pred<-"env_var/"
# pred<- load_var(path=pred)

#
# site_id <-"YTmB1fO1cN"
# site_type<-"onshore"

## study area
# sf_stud_geom<-st_read("dev/base_dat/stud_area.shp")%>%st_transform(crs = as.character(st_crs(4326)$proj4string))

# sf_stud_geom<-tbl(con_admin,"studSite")
# sf_stud_geom<-sf_stud_geom%>%collect()%>%filter(siteID == site_id & projID == project)
# area_name<-sf_stud_geom$area_descr
# sf_stud_geom <- sf::st_as_sf(sf_stud_geom, wkt = "geometry" )%>%st_set_crs(4326)


# sf_stud_geom$siteID <- site_id
# sf_stud_geom$projID <- project
# # sf_stud_geom$cntrID <- "NOR"
# sf_stud_geom$siteLANG <- as.character("NORWEGIAN")
# sf_stud_geom$siteN_es <- as.integer(num_tabs)
# sf_stud_geom$siteNAME<-as.character("Trondheim")
# sf_stud_geom$siteDESCR<-as.character("Trondheim test area")
# sf_stud_geom$siteSTATUS<-as.integer(1)
# sf_stud_geom$siteTYPE <-as.character(site_type)
# sf_stud_geom$siteAREAkm2<-as.integer(round(as.numeric(st_area(sf_stud_geom))/1000000,0))
# sf_stud_geom$siteCREATETIME<-Sys.time()
# sf_stud_geom$siteCREATOR <-Sys.getenv("USERNAME")
# polygons<-sf_stud_geom%>%st_drop_geometry()
# polygons$geometry<-st_as_text(sf_stud_geom$geometry)
# # #
# # # #save it on bq
# poly_table = bq_table(project = project, dataset = dataset, table = 'studSite')
# bq_table_upload(x = poly_table, values = polygons, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')



## a grid for the questionnaire
# grd<-st_make_grid(sf_stud_geom, cellsize = 0.05,
#                   offset = st_bbox(sf_stud_geom)[1:2],  what = "polygons")
