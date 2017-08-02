library(RTaiwan)
library(maptools)
library(data.table)
library(dplyr)
library(dtplyr)

# download shapefiles from http://data.gov.tw/node/25128
taiwan <- readShapePoly("generation/shapefiles/taiwan.shp", proj4string = CRS("+init=epsg:3826"))
for(col in c("VILLAGE", "TOWN", "COUNTY")) {
  taiwan[[col]] <- iconv(taiwan[[col]], "BIG5", "UTF-8")
  Encoding(taiwan[[col]]) <- "UTF-8"
}
TaiwanBasicStatisticalArea <- taiwan
save(TaiwanBasicStatisticalArea, file = "data/TaiwanBasicStatisticalArea.rda", compress = "xz", compression_level = 9)

Taiwan1stArea <- local({
  .poly <- unionSpatialPolygons(taiwan, taiwan$CODE1)

  # lu <- function(x) length(unique(x))
  .data <- data.table(taiwan@data)
  setkey(.data, "CODE1")
  .dt <- tbl_dt(.data)
  .data.lv1 <- group_by(.dt, CODE1) %>%
    summarise(CODE2 = head(CODE2, 1), TOWN_ID = head(TOWN_ID, 1), TOWN = head(TOWN, 1), COUNTY_ID = head(COUNTY_ID, 1), COUNTY = head(COUNTY, 1)) %>%
    as.data.frame()
  rownames(.data.lv1) <- .data.lv1$CODE1
  SpatialPolygonsDataFrame(
    .poly,
    .data.lv1,
    match.ID = TRUE
  )
})
save(Taiwan1stArea, file = "data/Taiwan1stArea.rda", compress = "xz", compression_level = 9)

Taiwan2ndArea <- local({
  .poly <- unionSpatialPolygons(Taiwan1stArea, Taiwan1stArea$CODE2)
  .data <- data.table(Taiwan1stArea@data)
  setkey(.data, "CODE2")
  .dt <- tbl_dt(.data)
  .data.lv2 <- group_by(.dt, CODE2) %>%
    # summarise(TOWN_ID = lu(TOWN_ID), TOWN = lu(TOWN), COUNTY_ID = lu(COUNTY_ID), COUNTY = lu(COUNTY))
    summarise(TOWN_ID = head(TOWN_ID, 1), TOWN = head(TOWN, 1), COUNTY_ID = head(COUNTY_ID, 1), COUNTY = head(COUNTY, 1)) %>%
    as.data.frame()
  rownames(.data.lv2) <- .data.lv2$CODE2
  SpatialPolygonsDataFrame(
    .poly,
    .data.lv2,
    match.ID = TRUE
  )
})
save(Taiwan2ndArea, file = "data/Taiwan2ndArea.rda", compress = "xz", compression_level = 9)

TaiwanTownArea <- local({
  .poly <- unionSpatialPolygons(Taiwan2ndArea, Taiwan2ndArea$TOWN_ID)
  .data <- data.table(Taiwan2ndArea@data)
  setkey(.data, "TOWN_ID")
  .dt <- tbl_dt(.data)
  .data.town <- group_by(.dt, TOWN_ID) %>%
    summarise(TOWN = head(TOWN, 1), COUNTY_ID = head(COUNTY_ID, 1), COUNTY = head(COUNTY, 1)) %>%
    as.data.frame()
  rownames(.data.town) <- .data.town$TOWN_ID
  SpatialPolygonsDataFrame(
    .poly,
    .data.town,
    match.ID = TRUE
  )
})
save(TaiwanTownArea, file = "data/TaiwanTownArea.rda", compress = "xz", compression_level = 9)

TaiwanCountyArea <- local({
  .poly <- unionSpatialPolygons(TaiwanTownArea, TaiwanTownArea$COUNTY_ID)
  .data <- data.table(TaiwanTownArea@data)
  setkey(.data, "COUNTY_ID")
  .dt <- tbl_dt(.data)
  .data.county <- group_by(.dt, COUNTY_ID) %>%
    summarise(COUNTY = head(COUNTY, 1)) %>%
    as.data.frame()
  rownames(.data.county) <- .data.county$COUNTY_ID
  SpatialPolygonsDataFrame(
    .poly,
    .data.county,
    match.ID = TRUE
  )
})
save(TaiwanCountyArea, file = "data/TaiwanCountyArea.rda", compress = "xz", compression_level = 9)

