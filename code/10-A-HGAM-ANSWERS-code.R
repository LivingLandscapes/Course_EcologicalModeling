fia$coverBeta <- 
  ifelse(fia$cover == 0, 0.0001, 
         ifelse(fia$cover == 1, 0.9999, 
                fia$cover))

# Create model GS using bam()
growth_modGS <-
  bam(coverBeta ~
        te(Longitude_scaled, Latitude_scaled) + # tensor smooth lets latitude/longitude 'interact'
        s(Year_scaled) +
        s(Year_scaled, GrowthHabit1, bs = "fs"),
      family = betar,
      data = fia,
      discrete = TRUE,
      method = "fREML") # need to use fREML for fast fitting.
gratia::appraise(growth_modGS)

# (One) solution: get hexagonal centroids!
honeycomb_centroids <- st_centroid(honeycomb_grid)

# Scale prediction covariates--on the same scale as the covariates used for fitting the model!
nd_0 <- data.frame(Latitude_scaled = rep((st_coordinates(honeycomb_centroids)[,2] - mean(fia$Latitude)) / sd(fia$Latitude), 2),
                   Longitude_scaled = rep((st_coordinates(honeycomb_centroids)[,1] - mean(fia$Longitude)) / sd(fia$Longitude), 2),
                   Year_scaled = rep(c(min(fia$Year_scaled),
                                       max(fia$Year_scaled)),
                                     each = nrow(honeycomb_centroids)), # Since we're not interested in time, pick most recent year.
                   hex_id = rep(honeycomb_centroids$hex_id, 2))

# Make a data.frame for prediction
nd <-
  do.call(rbind,
          lapply(unique(fia$Order),
                 function(X) {
                   # We need to get latitudes and longitudes for each grid cell
                   # for each growth habit. We'll use a lapply function to do this
                   # quickly.
                   df <- data.frame(Order = X,
                                    nd_0)
                   return(df)
                 }))

# Final step: make the prediction and the map! You should be familiar with all this from previous labs. The only new parts are the color ramp and the "geom_sf" code. Thankfully, the folks who created the "sf" package made it compatable with ggplot, so all you need is an sf object (e.g., our "fia_sf" object). The rest takes care of itself!

# Prediction. We won't worry about the standard error/confidence here. However, it's best practice to create maps of uncertainty whenever you create a predictive map.
spFit_pred <-
  predict(order_modGS,
          newdata = nd,
          type = "response")

# Create prediction data.frame
spFit_pred_df <- data.frame(fit = spFit_pred, nd)

# Join prediction with the honeycomb grid
pred_sf <-
  left_join(honeycomb_grid,
            spFit_pred_df)

# Create a custom color ramp. We'll need this because the predictions are CRAZY skewed. So if we allowed R to generate a color ramp for us, we wouldn't be able to see the nuance in the predictions.
colorBreaks <- seq(min(pred_sf$fit),
                   max(pred_sf$fit),
                   0.01)
colorRamp <- c(colorRampPalette(rev(c("#fde725", # these are viridis color-blind friendly colors
                                      "#5ec962",
                                      "#21918c",
                                      "#3b528b",
                                      "#440154")),
                                bias = 1.5)(length(colorBreaks[colorBreaks < quantile(pred_sf$fit, 0.99)])), # Cutoff at 99th quantile
               rep("#fde725", length(colorBreaks) - length(colorBreaks[colorBreaks < quantile(pred_sf$fit, 0.99)])))

# Make the map! Let's just use the first year (2001)
ggplot(pred_sf %>% filter(Year_scaled == min(Year_scaled)),
       aes(fill = fit)) +
  facet_wrap(~ Order, ncol = 6) +
  scale_fill_gradientn(colors = colorRamp,
                       name = "Cover") +
  geom_sf()