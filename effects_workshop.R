# Effect Displays
# Clay Ford
# UVa StatLab 
# Fall 2016

# packages used in this workshop
install.packages("effects")
install.packages("magrittr")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("visreg")
install.packages("coefplot")
install.packages("stargazer")


# Getting Started ---------------------------------------------------------


# Logistic regression example 

# Data from a study of the personality determinants of volunteering for
# psychological research. This data come with the effects package.
library(effects)
str(Cowles)
?Cowles
data(Cowles)

# How does neuroticism, extraversion and sex affect the probability of 
# volunteering for psychological research? (neuroticism and extraversion are
# scale measures from the Eysenck personality inventory.)


# Fit a logistic model
cowles.mod <- glm(volunteer ~ sex + neuroticism + extraversion + neuroticism:extraversion, 
                  data=Cowles, family=binomial)
summary(cowles.mod)

# basic effect display using all effects
plot(allEffects(cowles.mod))

# We can also "pipe" these commands using the %>% operator in the magrittr
# package
library(magrittr)
allEffects(cowles.mod) %>% plot()

# what is being plotted? 
allEffects(cowles.mod)

# We can this as an effect object:
e.out <- allEffects(cowles.mod)
e.out$sex
e.out$`neuroticism:extraversion`

# What predictors were used to get these values? Look at the "model.matrix"
# element of the effect object:

# sex effect predictors
e.out$sex$model.matrix

# neuroticism and extraversion set to means 
mean(Cowles$neuroticism)
mean(Cowles$extraversion)
mean(Cowles$neuroticism) * mean(Cowles$extraversion)

# another longer way to get the predictions for sex
predict(cowles.mod, newdata = data.frame(sex = factor(c("female","male")),
                                         neuroticism = mean(Cowles$neuroticism),
                                         extraversion = mean(Cowles$extraversion)),
        type = "response")

# neuroticism*extraversion effect
e.out$`neuroticism:extraversion`$model.matrix

# neuroticism and extraversion set to "pretty" values (0,5,10,15,20);
# sex set 0.4510908: percent of males
prop.table(table(Cowles$sex))

# 45% male?


# YOUR TURN!

# Read in babies.csv which contains data on mothers and their babies (courtesy
# of the UsingR package)

# VARIABLES:
# wt - birth weight in ounces
# gestation - length of gestation in days
# age - mother's age
# ht - mother's height in inches
# wt1 - mother prepregnancy wt in pounds
# smoke - does mother smoke? (0=never, 1=smokes now, 2=until current pregnancy, 3=once did, not now)

babies <- read.csv("http://people.virginia.edu/~jcf2d/workshops/effects/babies.csv")
# set smoke as a factor
babies$smoke <- factor(babies$smoke,
                       labels =  c("never","still smokes", "until pregnant","once did"))

# Model baby birth weight as a function of gestation, age, gestation:age
# interaction and smoke.
babies.mod <- lm(wt ~ smoke + gestation + age + gestation:age, data=babies)
summary(babies.mod)

# (1) visualize the model using allEffects() with plot()


# (2) save the result of allEffects(babies.mod) and view the model matrix to see
# what values were used to create plots:


# back to presentation.

# Modifying effect calculations -------------------------------------------

# using effect and Effect for specific terms or focal predictors
effect(term = "neuroticism*extraversion", cowles.mod)
Effect(focal.predictors = c("neuroticism","extraversion"), cowles.mod)

# They return the same thing
all.equal(
  effect(term = "neuroticism*extraversion", cowles.mod),
  Effect(focal.predictors = c("neuroticism","extraversion"), cowles.mod)
)

# working with a factor
effect(term = "sex", cowles.mod)
Effect(focal.predictors = "sex", cowles.mod)

# and of course we can plot these as well:
plot(Effect(focal.predictors = c("neuroticism","extraversion"), cowles.mod))
plot(Effect(focal.predictors = "sex", cowles.mod))


# Using xlevels to set number of levels of the focal predictors

# 3 values for each predictor
Effect(focal.predictors = c("neuroticism","extraversion"), cowles.mod, xlevels = 3)

# same values for neuroticism and extraversion
Effect(focal.predictors = c("neuroticism","extraversion"), cowles.mod, 
       xlevels = list(neuroticism=seq(5,20,5), extraversion=seq(5,20,5)))

# specific values for neuroticism and 3 values for extraversion
Effect(focal.predictors = c("neuroticism","extraversion"), cowles.mod, 
       xlevels = list(neuroticism=seq(0,24,2), extraversion=3))


e.out2 <- Effect(focal.predictors = c("neuroticism","extraversion"), cowles.mod, 
       xlevels = list(neuroticism=seq(0,24,2), extraversion=3))
e.out2$model.matrix

# Using given.values to set levels of predictors that are not focal predictors.

# In previous plot, sex was not a focal predictor. By default it was set to
# 0.45, the proportion of males. 

# Let's set sex to 1 instead of 0.45
Effect(focal.predictors = c("neuroticism","extraversion"), cowles.mod, 
       xlevels = list(neuroticism=seq(0,24,2), extraversion=3),
       given.values = c(sexmale = 1))

# If we want predictions for both males and females, we could include sex as a
# focal predictor:
Effect(focal.predictors = c("neuroticism","extraversion", "sex"), cowles.mod, 
            xlevels = list(neuroticism=seq(0,24,2), extraversion=3))
# and plot
plot(Effect(focal.predictors = c("neuroticism","extraversion", "sex"), cowles.mod, 
            xlevels = list(neuroticism=seq(0,24,2), extraversion=3)))


# YOUR TURN!

# 1) plot just the gestation:age interaction from the babie.mod object using 10 
# values for age, 6 values for gestation, and assuming the mother still smokes.

# Tip: to calculate effects assuming mother still smokes, set the given.values 
# argument as follows. Notice the names must match what's in the model summary:
given.values = c("smokestill smokes" = 1, "smokeuntil pregnant" = 0, 
                 "smokeonce did" = 0)



# back to the presentation.

# Modifying plots ---------------------------------------------------------


# type examples
e.out3 <- Effect(focal.predictors = c("neuroticism","extraversion"), cowles.mod, 
       xlevels = list(neuroticism=seq(0,20,4), extraversion=seq(0,20,5)),
       given.values = c(sexmale = 1))

plot(e.out3, type = "rescale") # default
plot(e.out3, type = "link")
plot(e.out3, type = "response")


# x.var examples
# switch "extraversion" to x-axis
plot(e.out3, x.var = "extraversion") #

# multiline
plot(e.out3, multiline = TRUE)

# adding sex as a focal predictor to produce multiline plots in each panel:
plot(Effect(focal.predictors = c("neuroticism","extraversion","sex"),
            mod = cowles.mod),
     multiline = TRUE)

# The lines are parallel because we didn't fit a 3-way interaction

# We can try it and see how the plot changes
cowles.mod3 <- glm(volunteer ~ neuroticism * extraversion * sex, data = Cowles,
                   family = binomial)
summary(cowles.mod3)
plot(Effect(focal.predictors = c("neuroticism","extraversion","sex"),
            mod = cowles.mod3),
     multiline = TRUE)



# changing line colors and line types

plot(e.out3, multiline = TRUE, colors = "black")
# what happend? 
# Default: lines = 1:length(colors)
# If only one color defined, then we only get one line type!

plot(e.out3, multiline = TRUE, colors = "black", lines = 1:5)

# colors using the RColorBrewer package; Provides color schemes designed by
# Cynthia Brewer as described at http://colorbrewer2.org
library(RColorBrewer)

# sequential palettes names: Blues BuGn BuPu GnBu Greens Greys Oranges OrRd
# PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd

# Try a sequence of 5 Reds
display.brewer.pal(5,"Reds")

# define a palette using brewer.pal
myPalette <- brewer.pal(5,"Reds")

# Now use the palette in the effect display
plot(e.out3, multiline = TRUE, colors = myPalette, lines = 1:5)

# see ?brewer for more information; it has excellent, easy-to-follow examples


# changing ci.style
plot(e.out3, ci.style = "bands") # default
plot(e.out3, ci.style = "lines") 
plot(e.out3, ci.style = "bars") 

# changing ci.style with multiline = TRUE
plot(e.out3, multiline = TRUE, ci.style = "bands") 
plot(e.out3, multiline = TRUE, ci.style = "bars") 
plot(e.out3, multiline = TRUE, ci.style = "lines") # does not actually produce lines

# our model with all interactions:
plot(Effect(focal.predictors = c("neuroticism","extraversion","sex"),
            mod = cowles.mod3),
     multiline = TRUE,
     ci.style = "bands")


# changing band colors
plot(e.out3)
plot(e.out3, band.colors = "red")
plot(e.out3, band.colors = "blue")

# changing band transparency
plot(e.out3, band.colors = "blue", band.transparency=.15) # default
plot(e.out3, band.colors = "blue", band.transparency=.3)
plot(e.out3, band.colors = "blue", band.transparency=0) # disappears
plot(e.out3, band.colors = "blue", band.transparency=1) # covers up line


# changing color of the strips to monochrome
.save.strip <- setStrip() 
plot(e.out3)
restoreStrip(.save.strip) # restore

# changing tick marks on vertical axis

# Recall: if at is non-NULL, then the value of n is ignored; also n is only a
# suggestion

# add ticks for 0, 0.1, 0.2, ... , 0.9, 1.0
plot(e.out3, ticks=list(at=seq(0, 1, 0.1)))

# Notice 0.9 and 1.0 is not visible; need increase the limits of the y-axis,
# which we can do with the ylim argument. ylim = (smallest value, largest value)
plot(e.out3, ticks=list(at=seq(0,1,0.1)), ylim = c(0,1)) # look out!

# what happened? For logistic regression models, the default behavior is to plot
# the y-axis on the logit, or log-odds, scale but to label it on the scale of
# the response.

# If we want to increase the limits of the y-axis, we have to define the limits 
# on the log-odds scale. c(-3,3) on the logit scale is about the same as c(0,1)
# on the response scale.
plot(e.out3, ticks=list(at=seq(0,1,0.1)), ylim = c(-3,3))

# Probably better to change the plot type to "response and use ylim = c(0,1)
plot(e.out3, ticks=list(at=seq(0,1,0.1)), type = "response", ylim = c(0,1))

# changing tick marks on horizontal axis
plot(e.out3, ticks.x=list(neuroticism = list(at=seq(0,20,2))))

# setting the rug and grid values
plot(e.out3, rug = FALSE, grid = TRUE)

# YOUR TURN!

# Plot the gestation*age effect display with monochrome strips and no rug.


# Effect displays with ggplot2 --------------------------------------------

# recall this plot
plot(e.out3)

# Let's reproduce it using ggplot.

# First convert e.out3 object to a data frame
eDF <- as.data.frame(e.out3)
head(eDF)
# se is "standard error" on the scale of the linear predictor

# plots with ggplot2
library(ggplot2)

# "rescale" version - plotted on link scale, labeled on response scale
ggplot(eDF, aes(y=fit, x=neuroticism)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") + 
  scale_y_continuous(trans = "logit") +
  geom_line() +
  facet_wrap(~extraversion, labeller = label_both) +
  labs(title = "neuroticism:extraversion effect plot",
          y = "volunteer")

# "response" version - curved lines; delete scale_y_continuous(trans = "logit")
ggplot(eDF, aes(y=fit, x=neuroticism)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") + 
  geom_line() +
  facet_wrap(~extraversion, labeller = label_both) +
  labs(title = "neuroticism:extraversion effect plot",
       y = "volunteer")


# Recall the allEffects() object we created
e.out
# access the sex effect
e.out$sex

eDF2 <- as.data.frame(e.out$sex)
eDF2
ggplot(eDF2, aes(x=sex, y=fit, group=1)) + geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, color="red") +
  labs(title="sex effect plot", y="volunteer")


## ggplot2 effect displays using the visreg package
library(visreg)

# lattice version, includes partial residuals
visreg(cowles.mod, xvar = "neuroticism", by = "extraversion")

# using ggplot2
visreg(cowles.mod, xvar = "neuroticism", by = "extraversion", gg = TRUE)

# change the scale and turn off partial residuals
visreg(cowles.mod, xvar = "neuroticism", by = "extraversion", gg = TRUE, 
       scale = "response", partial = FALSE)

# create five panels
visreg(cowles.mod, xvar = "neuroticism", by = "extraversion", gg = TRUE, 
       scale = "response", partial = FALSE, breaks = 5)

# for sex = Male
visreg(cowles.mod, xvar = "neuroticism", by = "extraversion", gg = TRUE, 
       scale = "response", partial = FALSE, breaks = 5, 
       cond = list(sex="male"))

# change color of line and change y-axis
visreg(cowles.mod, xvar = "neuroticism", by = "extraversion", gg = TRUE, 
       scale = "response", partial = FALSE, breaks = 5, 
       cond = list(sex="male"),
       line.par = list(col = "black"), ylab = "Volunteer")


# save visreg
v.out <- visreg(cowles.mod, xvar = "neuroticism", by = "extraversion", gg = TRUE, 
       scale = "response", partial = FALSE, breaks = 5, 
       cond = list(sex="male"), plot = FALSE)

# visreg objects do not contain as much information as effects objects
str(v.out)


# Coefficient plot --------------------------------------------------------

library(coefplot)

coefplot(cowles.mod)
coefplot(cowles.mod, sort = "magnitude")

# sort alphabetical (from bottom)
coefplot(cowles.mod, sort = "alphabetical")

# supress the intercept
coefplot(cowles.mod, intercept = FALSE)

# view specific predictors
coefplot(cowles.mod, predictors = c("neuroticism","extraversion"), 
         intercept = FALSE)

# The arm package also has a coefplot() function that uses base R graphics and
# supports ordered logit models and mixed-effect models.
arm::coefplot(cowles.mod)

# Multiple Coefficient Plots ----------------------------------------------

# Fit three models

# main effects model
cowles.mod1 <- glm(volunteer ~ sex + neuroticism + extraversion, 
                  data=Cowles, family=binomial)
# all two-way interactions
cowles.mod2 <- glm(volunteer ~ (sex + neuroticism + extraversion)^2, 
                  data=Cowles, family=binomial)
# all interactions
cowles.mod3 <- glm(volunteer ~ sex * neuroticism * extraversion, 
                  data=Cowles, family=binomial)

# summarize coefficients in a table using stargazer package
library(stargazer)
stargazer(cowles.mod1, cowles.mod2, cowles.mod3, type="text", 
          title = "Linear Model Results")
# stargazer package will also produce code for HTML and LaTeX

# A coefficient plot can complement such a table
multiplot(cowles.mod1, cowles.mod2, cowles.mod3, intercept = FALSE)

# produce separate graphs
multiplot(cowles.mod1, cowles.mod2, cowles.mod3, intercept = FALSE, 
          single = FALSE)

# rotate numbers
multiplot(cowles.mod1, cowles.mod2, cowles.mod3, intercept = FALSE,
          single = FALSE, 
          numberAngle = 0)

# change model names in legend
multiplot(cowles.mod1, cowles.mod2, cowles.mod3, intercept = FALSE,
          single = FALSE, 
          numberAngle = 0, 
          names = c("model 1","model 2","model 3"))

# rotate plot
multiplot(cowles.mod1, cowles.mod2, cowles.mod3, intercept = FALSE, 
          single = FALSE, 
          horizontal = TRUE)

# move legend to bottom
multiplot(cowles.mod1, cowles.mod2, cowles.mod3, intercept = FALSE, 
          single = FALSE, numberAngle = 0,
          legend.position="bottom",
          names = c("model 1","model 2","model 3"))

# only view certain predictors
multiplot(cowles.mod1, cowles.mod2, cowles.mod3, 
          predictors = c("neuroticism","extraversion"),
          names = c("model 1","model 2","model 3"),
          numberAngle = 0)


## END
