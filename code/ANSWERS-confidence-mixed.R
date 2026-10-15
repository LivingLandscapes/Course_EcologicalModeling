# boot::inv.logit() does the job
nd$lower.point <- inv.logit(envPred$point[1,])
nd$upper.point <- inv.logit(envPred$point[2,])

# Make the bootstrap object into a nice data.frame for ggplot
bigboot2_df <-
  as.data.frame(apply(bigBoot2$t, 1, inv.logit)) %>%
  cbind(nd %>% dplyr::select(c("csLength", "fSex"))) %>%
  pivot_longer(cols = starts_with("V"),
               names_to = "Sample",
               values_to = "Estimate")

# Create a prediction with all bootstrap samples similar to Shipley et al. (2022)
ggplot() +
  # and don't forget to change alpha
  geom_line(aes(x=csLength, y=lower.point, color=fSex),
            data=nd,
            linetype = 2,
            linewidth = 1,
            inherit.aes=FALSE,
            alpha=0.75) +
  geom_line(aes(x=csLength, y=upper.point, color=fSex),
            data=nd,
            linetype = 2,
            linewidth = 1,
            inherit.aes=FALSE,
            alpha=0.75) +
  geom_line(data = bigboot2_df,
            aes(x = csLength, y = Estimate, group = Sample),
            color = "grey70",
            alpha = 0.5) +
  geom_line(aes(x=csLength, y=pp, col=fSex),
            data = nd,
            alpha=0.5) +
  ylab("Probability of E. cervi infection") +
  geom_line(aes(x=csLength, y=pp, col=fSex),
            data = nd,
            inherit.aes = FALSE,
            linewidth = 1.5) +
  facet_wrap(~fSex) +
  guides(color=FALSE, fill=FALSE)