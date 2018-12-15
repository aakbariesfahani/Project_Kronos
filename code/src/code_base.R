
# Libraries ---------------------------------------------------------------

library(DBI)
library(RSQLite)
library(tidyverse)
library(forecast)
library(splines)
library(quantreg)
library(RColorBrewer)

# Data Prep -------------------------------------------------------------------------------------------------------

db <- dbConnect(RSQLite::SQLite(), dbname = "data/prod/climate.sqlite")

clim_parm <- "temprature" # "temprature" "pdsi" "precipitation"
state <- "CA"
step_h <- 50
rhg_cols <- c("#771C19","#AA3929","#E25033","#F27314","#F8A31B",
              "#E2C59F","#B6C5CC","#8E9CA3","#556670","#000000")

# Data --------------------------------------------------------------------

data_mod = dbReadTable(db, clim_parm) %>%
        select(year, contains(state))


# First round analysis ----------------------------------------------------

ggplot(data_mod, aes(CA__T)) +
        geom_histogram(bins = 50)
hist(data_mod$CA__T, main=paste0(state," Temp"),xlab="Temp in Bins of 0.5 degrees of Celcius")

fds = nnetar(data_mod$CA__T)
summary(fds)


ff = forecast(fds,h=step_h)

accuracy(ff)

plot(ff)



# Quantile ----------------------------------------------------------------

cli_typ <- data_mod %>%
        select(-year) %>%
        names()

qst <-rq(data_mod[,cli_typ] ~ bs(data_mod[,"year"], degree = 3,df=200), 
         tau = c(0.1, 0.25,0.5,0.75,0.9))


ggplot() + 
        geom_line(aes(y = data_mod[,cli_typ], x = data_mod$year)) +
        geom_line(aes(y = predict(qst)[,1], x = data_mod$year, color = "a")) +
        geom_line(aes(y = predict(qst)[,2], x = data_mod$year, color = "b")) +
        geom_line(aes(y = predict(qst)[,3], x = data_mod$year, color = "c")) +
        geom_line(aes(y = predict(qst)[,4], x = data_mod$year, color = "d")) +
        geom_line(aes(y = predict(qst)[,5], x = data_mod$year, color = "e")) +
        scale_color_manual(name = "Probability Distribution",
                            values = c("a"=rhg_cols[1],
                                       "b"=rhg_cols[2],
                                       "c"=rhg_cols[3],
                                       "d"=rhg_cols[4],
                                       "e"=rhg_cols[5]), 
                            labels = c("10% Probability",
                                       "25% Probability",
                                       "50% Probability",
                                       "75% Probability",
                                       "90% Probability")) +
        xlab("Year") +
        ylab("Temperature") +
        ggtitle(paste("Probability distribution of",clim_parm, "in", state, 
                      "from 10 AC to 2010 AC"))


qoh <- function(qst) {
        qohn <- c()
        qohn[[1]] <- ff
        for(i in 2:6){
                qohn[[i]] <- predict(qst)[,(i-1)] %>%
                        nnetar() %>%
                        forecast(h=step_h)
        }
        
        return(qohn)
}

qoh_out <- qoh(qst) 
results_together <- do.call(rbind,lapply(names(qoh_out),function(x){
        transform(as.data.frame(qoh_out[[x]]), Name = x)
}))
ggplot() +
        geom_line(data = as.data.frame(ff), aes(x = `Point Forecast`, y = 1:50)) +
        geom_line(data = as.data.frame(qoh_out[[1]]), 
                  aes(x = `Point Forecast`, y = 1:50, color = "a")) +
        geom_line(data = as.data.frame(qoh_out[[2]]), 
                  aes(x = `Point Forecast`, y = 1:50, color = "b")) +
        geom_line(data = as.data.frame(qoh_out[[3]]), 
                  aes(x = `Point Forecast`, y = 1:50, color = "c")) +
        geom_line(data = as.data.frame(qoh_out[[4]]), 
                  aes(x = `Point Forecast`, y = 1:50, color = "d")) +
        geom_line(data = as.data.frame(qoh_out[[5]]), 
                  aes(x = `Point Forecast`, y = 1:50, color = "e")) +
        coord_flip() +
        xlab('Dates') +
        ylab('percent.change') +
        scale_color_manual(name = "Probability Distribution",
                           values = c("a"=rhg_cols[1],
                                      "b"=rhg_cols[2],
                                      "c"=rhg_cols[3],
                                      "d"=rhg_cols[4],
                                      "e"=rhg_cols[5]), 
                           labels = c("10% Probability",
                                      "25% Probability",
                                      "50% Probability",
                                      "75% Probability",
                                      "90% Probability"))



y = c(.1,.25,.5,.75,.9)
x = t(as.data.frame(qoh_out)[1,2:6])
df = data.frame(y, x)

names(df) <- c("scale", "unc_window")
ggplot(df, aes(unc_window)) + stat_ecdf(geom = "step")

plot(ecdf(x),do.points=FALSE,verticals=TRUE,main="",ylab="",xlab="")
title(main = "NM Temperature Empirical Cumulative Distribution Function at year 2020",
      ylab="quantiles",xlab="Temperature in Celsius")