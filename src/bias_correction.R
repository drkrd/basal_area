








coeff_gf <- data.frame("Intercept"=c(models[[1]]$coefficients[1], 
                                     models[[2]]$coefficients[1], 
                                     models[[3]]$coefficients[1]),
                       "Slope"=c(models[[1]]$coefficients[2],
                                 models[[2]]$coefficients[2],
                                 models[[3]]$coefficients[2]))


rst_stack <- stack(met_gapfraction, p95)
rst_df <- extract(rst_stack, shp, method='simple', cellnumbers=TRUE)
rst_df <- as.data.frame(rbind(rst_df[[1]], rst_df[[2]], rst_df[[3]], rst_df[[4]], rst_df[[5]]))
rst_df_7m <- rst_df[rst_df$zq95>7,]
rst_df_7m <- rst_df_7m[,c(2:5)]
rst_vals <- rst_df_7m[complete.cases(rst_df_7m),]


#Simple correction of the linear model
#Subtract the intercept and divid slope
#y = mx + c  =>  (y-c)/m = x


area_calc = function(ls)
{
  #print(length(dfr$x))
  dfr <- data.frame("x"=ls@data$X, "y"=ls@data$Y)
  ch_pts <- chull(dfr$x,dfr$y)
  ch_pts <- c(ch_pts, ch_pts[1])
  dfr <- dfr[ch_pts,]
  dfr <- dfr %>% 
    select(1:2) 
  ch_poly <- Polygon(dfr, hole=F)
  return(ch_poly@area)
}


func_pf <- function(x)
{
  if (is.null(x))
  {
    return(NULL)
  }
  else
  {
    x <- lasfilterfirst(x)
    num <- lasfilter(x, Z <= 2)
    return(npoints(num)/npoints(x))
  }
}


scan_corr <- function(x)
{
  x <-lasflightline(x)
  unique_flines <- unique(x@data$flightlineID)
  areas <- NULL
  pfs_w <- NULL
  for (fline_id in unique_flines)
  {
    las_sub <- lasfilter(x, flightlineID == fline_id)
    mean_scan <- abs(mean(las_sub@data$ScanAngleRank))
    las_sub_area <- area_calc(las_sub)
    pf_sub <- func_pf(las_sub)
    pf_sub_corr <- ifelse(mean_scan<20, ((pf_sub-coeff_gf$Intercept[1])/coeff_gf$Slope[1]),
                          ifelse(mean_scan<30, ((pf_sub-coeff_gf$Intercept[2])/coeff_gf$Slope[2]),
                                 pf_sub-coeff_gf$Intercept[3])/coeff_gf$Slope[3])
    
    pfs_w <- c(pfs_w,(pf_sub_corr*las_sub_area))
    
    
    areas <- c(areas, las_sub_area)
  }
  
  mean_pf <- sum(pfs_w, na.rm = TRUE)/sum(areas, na.rm = TRUE)
  return(mean_pf)
}


gf_corr <- lapply(aois, scan_corr)
gf <- lapply(aois, func_pf)


rst_vals <- rst_vals %>% 
  mutate(cl2a = (cl2-coeff_gf$Intercept[1])/coeff_gf$Slope[1],
         cl3a = (cl3-coeff_gf$Intercept[2])/coeff_gf$Slope[2],
         cl4a = (cl4-coeff_gf$Intercept[3])/coeff_gf$Slope[3])