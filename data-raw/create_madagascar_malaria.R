
#######
# Example predicting malaria in Madagascar
#######

library(disaggregation)
library(raster)
library(dplyr)

shapes <- shapefile('data-raw/shapes/mdg_shapes.shp')
population_raster <- raster('data-raw/population.tif')
covariate_stack <- getCovariateRasters('data-raw/covariates', 
                                       shape = population_raster)

dis_data <- prepare_data(polygon_shapefile = shapes, 
                         covariate_rasters = covariate_stack, 
                         aggregation_raster = population_raster, 
                         mesh.args = list(max.edge = c(0.7, 8), 
                                          cut = 0.05, 
                                          offset = c(1, 2)),
                         id_var = 'ID_2',
                         response_var = 'inc',
                         na.action = TRUE,
                         ncores = 8)


full_data <- left_join(dis_data$covariate_data, dis_data$polygon_data, by = c('ID_2' = 'area_id'))

full_data$N <- dis_data$aggregation_pixels

coords <- data.frame(dis_data$coordsForFit)
names(coords) <- c('longitude', 'latitude')

full_data <- bind_cols(full_data, coords)

madagascar_malaria <-
    full_data %>% 
        group_by(ID_2) %>% 
        mutate(N_agg = sum(N),
               response_rate = response / N_agg) %>% 
        ungroup

madagascar_malaria <-
    madagascar_malaria %>% 
        rename(ID = ID_2,
               cases = response,
               case_rate = response_rate,
               pop = N,
               agg_pop = N_agg) %>% 
        dplyr::select(ID, cellid, cases, case_rate, pop, agg_pop, Elevation, EVI, LSTmean, longitude, latitude)

usethis::use_data(madagascar_malaria)
# usethis::use_data(madagascar_malaria, overwrite = TRUE)





