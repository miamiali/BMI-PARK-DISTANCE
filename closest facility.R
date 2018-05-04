# Date: Tuesday, April 17th, 2018
# By: Mia Li
# Description: BMI&Park Distance Project
# Version of R Studio used: 1.0.136

#install related packages####
#data cleaning packages
install.packages(c("ggplot2","dplyr","stringr","scales","xlsx","data.table","tidyr","ggpubr"))
r1<-c("ggplot2","dplyr","stringr","scales","xlsx","data.table","tidyr","ggpubr")
#geospatial data processing packages
install.packages(c("sf","raster","sp","rgdal","rgeos"))
r2<-c("sf","raster","sp","rgdal","rgeos")

lapply(r1,require,character.only=TRUE)
lapply(r2,require,character.only=TRUE)

#[obsolete]
#step 1 filter the geocoded dataset to only show parks
#step 2 pull out duplicated coordinates from the geocoded dataset to see if duplicates have the same park name. If not same, go to step 3; if same, go to step 4
#step 3 match with the parks dataset to decide which one of the duplicates to keep
#step 4 merge two datasets by park name
#step 5 identify the parks from the parks dataset that don't have a match in the geocoded dataset, conduct second round of approximate match
#step 6 use arcgis to do closest facility calculation

#steps of cleaning the datasets####
#[New Steps]
#step 1 clean the format of two datasets 
#step 2 merge geocoded and ungeocoded datasets to get the intersection set
#step 3 filter the geocoded dataset to keep parks larger than 0.5 acre and parks with no areage information but in the intersection set 
#step 4 export the dataset to argis and calculate the walk distance
#step 5 import the results from arcgis and clean the results
#step 6 import individual dataset and clean it
#step 7 merge distance file with individual address file
#step 8 sample 100 census tracts and plot


#Step 1####
#import raw datasets from computer###
#ungeocoded dataset##
parks<-read.xlsx("H:/EHR project/Raw Dataset/dpr_park_list.xls",sheetName = "working file",startRow=4,endRow = 1072,colIndex = c(1:5)) #sheetName or sheetIndex to indicate the sheet to import
#geocoded dataset##
parks_geocoded<-fread("H:/EHR project/Raw Dataset/facilities.csv") #use fread from "data.table" package to handle raw dataset with quotes (other package will ignore observations with quotes while importing data)

#see the summary of the dataset###
glimpse(parks_geocoded) #glimpse from dplyr package
levels(parks_geocoded$facdomain)

#clean un-geocoded dataset### 
#clean the park name variable##
parks_ungeo_valid<-parks
parks_ungeo_valid$Park.Name<-parks_ungeo_valid$Park.Name %>%
  {gsub("plgd","playground",.,ignore.case = TRUE)} %>% #replace all plgd with playground # pipe will pass the data to the first argument of a function, to use the data eslewhere, have to use {} around the function and . to indicate data
  tolower()#lower case for the entire name 
#remove parks whose name is either just "park" or "park strip" because it's hard to match accurately to the parks whose name is also "park"/"park strip" in the other dataset
parks_ungeo_valid<-parks_ungeo_valid %>%
  filter(! (Park.Name %in% c("park","park strip"))) #N=1050

#clean geocoded dataset###
#clean the park name variable##
parks_geocoded$facname<-parks_geocoded$facname%>% 
  {gsub("plgd","playground",.,ignore.case = TRUE)} %>%
  tolower()

#step 2 and 3####
###[think process]
#structure of the geocoded park dataset: facdomain > facgroup > facsubgrp > factype
#the second smallest category (facsubgrp) includes "streetscapes,plazas, and malls" "Recreation and waterfront sites", etc.
#within these categories there are parks and non parks, e.g. malls and pools etc. therefore, have to use the smallest category to filter for parks
#however, some unselected factype includes both parks and nonparks as well, e.g. Triangle and plaza (Joan of arc park)
#first merge ungeocoded park dataset with full dataset to get the intersection set, and then merge the ungeocoded park dataset with subset of geocoded dataset filtered by factype
#then compare the difference between the two
#final reorganized geocoded park dataset = parks with acreage info and is larger than 0.5 (df1=947) + parks with no acreage info but are matches in the ungeocoded dataset (df2=184)  + "non-parks" that are matches in the ungeocoded dataset (df3=170) + schools with jop playground (df4=175) + schools with no jop playground but are accessible to the public (df5=10)- school playground not open to the public (df5=3)

#create df1###
#create a subset of geocoded dataset by filtering out non park facilities##
parks_type <- parks_geocoded %>%
filter(factype %in% c("Natural Area/Wetland/Wildlife Refuge",
                      "Playground",
                      "Park",
                      "Tidal Wetland",
                      "Garden",
                      "Neighborhood Park",
                      "Community Park",
                      "Playground/Sports Area",
                      "Jointly Operated Playground",
                      "Playing Field",
                      "Nature Area",
                      "Trailway",
                      "Waterfront Facility",
                      "City-State Park",
                      "Regional Park",
                      "Beach",
                      "Flagship Park",
                      "Community Garden",
                      "Unique Area",
                      "Natural Resource Area",
                      "Freshwater Wetland",
                      "State Forest",
                      "State Park Preserve",
                      "State Park"
))

#clean the park area variable##
#seperate column area by ":"  # [find a easier way to directly remove the string before and after the number]
parks_type<-separate(parks_type,col=area,into=c("Area_source","Area_num"),sep=": ")
# clean the two values with ; in the area column 
parks_type$Area_num<-gsub(";.*","",parks_type$Area_num)
#convert the area_num from character to numeric
parks_type$Area_num<-as.numeric(parks_type$Area_num)
#filter to show parks bigger than 0.5 acre
parks_df1<-parks_type %>%
  filter(round(Area_num,digits=1)>=0.5)   #N=947
#filter to show parks with no acreage info
parks_na<-parks_type %>%
  filter(is.na(Area_num))

#create df2###
#matches of filtered geocoded park dataset and ungeocoded dataset##
matches_sub<-semi_join(parks_type,parks_ungeo_valid,by=c("facname"="Park.Name",
                                                          "boro"="Borough")) #redundant step, could be just parks_df2<-semi_join(parks_na,parks_ungeo_valid,...)
parks_df2<-semi_join(parks_na,matches_sub,by="uid")


#create df3###
#matches of raw geocoded park dataset and the ungeocoded dataset##
matches_all<-semi_join(parks_geocoded,parks_ungeo_valid,by=c("facname"="Park.Name",
                                                          "boro"="Borough"))
#difference between matches_all and matches_sub
parks_df3<-anti_join(matches_all,matches_sub,by="uid") #N=170

#combine df1-3
parks_df123<-full_join(parks_df1,parks_df2)
parks_df123<-full_join(parks_df123,parks_df3)


#create df4###
parks_jop_all<-parks_geocoded %>%
  mutate(jop=str_extract(facname,"jop")) %>%
  filter(!is.na(jop))
parks_jop_sub<-parks_type %>%
  mutate(jop=str_extract(facname,"jop")) %>%
  filter(!is.na(jop))
difference_jop<-anti_join(parks_jop,parks_jop_sub,by="uid")

parks_df4<-parks_jop_all

parks1234<-bind_rows(parks_df123,parks_df4)

#create df5###
#Manually filtering out public school playgrounds that are open to public, match with geocoded dataset and remove the others##
#filter to keep parks with "PS ##" in the name in the ungeocoded dataset
parks_ps<-parks_ungeo_valid %>%
  mutate(ps=str_extract(Park.Name,"ps\\s+\\d+")) %>%
  filter(!is.na(ps))
#filter to keep parks in all valid parks combined together that have "ps ##" in names but don't have "jop" in names
parks1234_ps<-parks1234 %>% 
  mutate(ps=str_extract(facname,"ps\\s+\\d+")) %>%
  filter(!is.na(ps) & !grepl("jop",facname))
#filter to keep parks in all parks combined together that have "ps ##" in names but don't have "jop" in names
parks_ps_all<-parks_geocoded %>%
  mutate(ps=str_extract(facname,"ps\\s+\\d+")) %>%
  filter(!is.na(ps) & !grepl("jop",facname))
parks_ps_sub<-parks_type %>% #use this dataset for the next step
  mutate(ps=str_extract(facname,"ps\\s+\\d+")) %>%
  filter(!is.na(ps) & !grepl("jop",facname))
difference_ps<-anti_join(parks_ps_all,parks_ps_sub,by="uid")
table(difference_ps$factype) #after checking the frequency table, most facilities containing "PS" in the complete dataset are not parks, so use parks_type dataset to filter for ps schools

#match school parks with no JOP in geocoded dataset with school parks in ungeocoded dataset and filter for public school parks
public_ps<-parks_ps_sub %>%
  inner_join(parks_ps,by=c("ps"="ps","boro"="Borough"))
  
tolower(public_ps$Public)
parks_df5<-public_ps %>%  
  filter(Public=="public")  #N=10
parks_private<-public_ps %>%
  filter(Public!="public")

parks12345<-bind_rows(parks1234,parks_df5)

parks_final<-parks12345[!parks12345$facname %in% parks_private$facname,]

#step 4 export to folder ####
write.xlsx(parks_final,"H:/EHR project/R output/parks_valid_5.3.18.xlsx")

#step 5 import walk distance file and clean the dataset####
#import file with walk distance calculated using arcgis
walk_distance<-read.xlsx2("H:/EHR project/arcgis files/walk_distance_5.3.18.xls",sheetIndex = 1) #xlsx2 is faster than xlsx when processing large datasets

#seperate name column into park id and individual id
walk_distance2<-walk_distance %>%
  separate(col=Name,into=c("ID #","uid"),by=" - ") %>%
  select(c(5,6,10))

#step 6 Read individual dataset and clean it (Valid N=25901)####

#read individual addresses file###
ind<-read.xlsx2("H:/EHR project/Raw Dataset/bmi_changes_nobariatric_withFIPS_sorted_withlatlong.xlsx",sheetIndex = 1)
#ind<-fread("H:/EHR project/Raw Dataset/bmi_changes_nobariatric_withFIPS_sorted_withlatlong.csv") #manually converted xlsx file to csv then import using data.table because it's faster, but will info be lost? xlsx2 can fast import

#clean coordinates column by seperating longitude and latitude###
ind_valid<-ind %>%
  separate(coordinates,sep=",",c("lon","lat"))

#convert lat and lon to numeric remove observations if either the coordinates is NA###
ind_valid$lon<-as.numeric(as.character(ind_valid$lon))
ind_valid$lat<-as.numeric(as.character(ind_valid$lat))
ind_valid2<-ind_valid %>%
  filter(!is.na(lon) & !is.na(lat))  

#step 7 merge individual dataset and walk_distance dataset####

#step 8 combine ind and walk distance file and do the calculation####

#merge walk_distance and ind_test###

ind_test<-left_join(ind_valid2,walk_distance2,by=c("ID.."="ID #"))


#randomly sampling 100 census tracts with more than 20 people###
#find unique census tract # FIPS 01(state)234(county)567890(tract)##
unique<-unique(ind_valid2$FIPS)
#convert FIPS to numeric##
ind_valid2$FIPS<-as.numeric(as.character(ind_valid2$FIPS)) #use as.character first can keep the original format of the string after converting to number
#calculate the number of patients in each FIPS code area##
count<-ind_valid2 %>%
  group_by(FIPS) %>%
  summarize(count=n()) 

#see the range and mean and median of the counts##       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#                                                       1.00    4.00    9.00   12.36   15.00  133.00 
summary(count$count)

#select FIPS codes that have at least 20 people## 
unique_sample<-count %>%
  filter(count>=20) %>%
  sample_n(100)


#create a subset of individual whose FIPS code matches with the sampled FIPS code
ind_test2<-ind_test[ind_test$FIPS %in% unique_sample$FIPS,]
ind_test2$Total_Leng <-as.numeric(as.character(ind_test2$Total_Leng))
ind_test2$Total_Leng<- round(ind_test2$Total_Leng,1)
ind_test2$Change.in.BMI<-as.numeric(as.character(ind_test2$Change.in.BMI))
  

#step 9 plotting####
#all<-
  ind_test2 %>%
  ggplot(aes(x=Change.in.BMI,y=Total_Leng))+
  geom_point()+
  facet_wrap(~FIPS)+
  geom_smooth(method="lm",se=FALSE)+
  labs(y="Distance to the closest park /m")+
  theme(panel.grid = element_blank())

#no_extreme<-
  ind_test2 %>%
  filter(Total_Leng<=2000) %>%
  ggplot(aes(x=Change.in.BMI,y=Total_Leng))+
  geom_point()+
  facet_wrap(~FIPS)+
  geom_smooth(method="lm",se=FALSE)+
  scale_y_continuous(limits=c(0,2000),breaks = seq(0,2000,by=400))+
  labs(y="Distance to the closest park /m")+
    theme(panel.grid = element_blank())


ind_test2 %>%
  filter(FIPS==36081066501) %>%
ggplot(aes(x=Change.in.BMI,y=Total_Leng))+
  geom_point()+
  geom_smooth(method="lm",color="red")+
  labs(y="Distance to the closest park /m")

#save the plot
ggsave("all.pdf")

#[obsolete section]####

##find the duplicates [don't have to find the duplicate since the final goal is to find the closest one not two or more]
duplicates<-parks_size$facname[duplicated(parks_size$facname)]

#approximate match
unmatched_list<-unmatched$Park.Name
unmatched_geocoded_list<-unmatched_geocoded$facname
matched_fuzzy_list<-unmatched_geocoded_list[agrep(unmatched_list[1],unmatched_geocoded_list,max.distance = 3,ignore.case = T)]

matched_fuzzy_list<-list()

#Reading vector and raster data ##

#raster source:http://library.columbia.edu/locations/dssc/data/nyc.html

#gis source: https://github.com/CityOfNewYork/nyc-geo-metadata
streets<-st_read("H:/EHR project/NYC/Centerline/Centerline.shp")
streets2<-readOGR(dsn="H:/EHR project/NYC/Centerline",layer="Centerline")
streets2_utm<-spTransform(streets2,CRS("+init=epsg:3857"))
raster_nyc<-raster("H:/EHR project/NYC/hammondnyc/HammondNYC.tif")

#assign crs since the dataset doesn't have crs
st_crs(streets)<-3857
st_crs(ind)<-3857
st_crs(parks_valid)<-3857
proj4string(raster_nyc)<-CRS("+init=epsg:3857")
proj4string(ind.sp)<-CRS("+init=epsg:3857")
proj4string(parks.sp)<-CRS("+init=epsg:3857")

#convert sf to sp##
#convert linestrings to Spatiallines
streets.sp<-as(streets$geometry,"Spatial")
#convert points to spatialpoints
parks_valid$coords<-cbind(parks_valid$longitude,parks_valid$latitude)   #longitude in front of latitude
parks.sp<-SpatialPoints(parks_valid$coords)

#remove observations if either the coordinates is NA
ind_valid<-ind %>%
  separate(coordinates,sep=",",c("lon","lat"))
    
#convert lat and lon to numeric
ind_valid$lon<-as.numeric(as.character(ind_valid$lon))
ind_valid$lat<-as.numeric(as.character(ind_valid$lat))
ind_valid2<-ind_valid %>%
  filter(!is.na(lon) & !is.na(lat))
ind_valid2$coords<-cbind(ind_valid2$lon,ind_valid2$lat)
ind.sp<-SpatialPoints(ind_valid2$coords)

#calculate closest space

#convert shapefile to raster##
#create a raster template
streets2_utm_rst<-raster(extent(streets2_utm),crs=projection(streets2_utm))

lengths<-sapply(1:ncell(streets2_utm_rst),function(i){
  tmp_rst<-streets2_utm_rst
  tmp_rst[i]<-1
  tmp_shp<-rasterToPolygons(tmp_rst)
  
  if(gIntersects(streets2_utm,tmp_shp)){
    streets2_utm_crp<-crop(streets2_utm,tmp_shp)
    streets2_utm_crp_length<-gLength(streets2_utm_crp)
    return(streets2_utm_crp_length)
  } else{
      return(0)
  }
})
    
streets2_utm_rst[]<-lengths/1000

library(RColorBrewer)
spplot(streets2_utm_rst,scale=list(draw=TRUE),xlab="x",ylab="y",
       col.regions=colorRampPalette(brewer.pal(9,"YlOrRd")),
       sp.layout=list("sp.lines",streets2_utm),
       par.settings=list(fontsize=list(text=15)),at=seq(0,1800,200))

#export ind dataset and parks dataset##
write.xlsx(ind_valid2,"H:/EHR project/R output/ind_valid.xlsx")
write.xlsx(parks_valid,"H:/EHR project/R output/parks_valid2.xlsx")

#import closest distance file from arcgis
park_dis<-readOGR(dsn="H:/Personal/ArcGIS/Web Maps/New York City",layer="Export_Output_3")
park_dis_df<-as(park_dis,"data.frame")

streets2_utm_rst<-setValues(streets2_utm_rst,runif(ncell(streets2_utm_rst)))

tr<-transition(streets2_utm_rst,transitionFunction=mean,directions=4)
tr<-geoCorrection(tr)

path<-shortestPath(tr,test.sp,parks.sp,output="SpatialLines")




#compare the difference between straightline park distance and walk distance##

ind_test<-ind_valid2
ind_test$nearest_park<-apply(distm(ind_test[,11],parks_valid[,52],fun=distGeo),1,min)
ind_test$nearest_park_index<-apply(distm(ind_test[,11],parks_valid[,52],fun=distGeo),1,which.min)

#calculate the difference between walk distance and straight line distance
t.test(ind_test2$nearest_park,ind_test2$dis)
ind_test2$Total_Leng<-as.numeric(ind_test2$Total_Leng) #number completely changed after coercing into numeric, why?
#turned out the average difference is around 100 meters