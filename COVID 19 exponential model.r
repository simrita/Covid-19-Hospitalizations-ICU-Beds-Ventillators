rm(list = ls())

# install packages
list.of.packages <- c("ggplot2", "gridExtra","grid","lattice","chron","zoo","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)
library(chron)
library(zoo)
library(scales)

# set L_stay (length of stay to be checked, should have length = 3)
L_stay = c(10,15,20)

# set ICU capacity
capacity = 1830

# occupied beds in ICU (data from 13/3/2020 to 30/3/2020 for Belgium)
beds_taken = c(5,24,33,53,79,100,130,164,238,290,322,381,474,605,690,789,867,927) 

# start date of beds_taken
start_date = '03/13/2020'

# end date of beds_taken
end_date = '03/30/2020'

time = seq(length(beds_taken))
beds_time_real = data.frame(cbind(time,beds_taken))
beds_time_real$inflow = c(beds_taken[1],diff(beds_taken))
beds_time_real$dates = seq.dates(from = start_date, to = end_date, by = 'days') 
beds_time_real$dates  = as.Date(beds_time_real$dates)

time = seq(L_stay[1] + length(beds_time_real$inflow) )
outflow = c(rep(0,times = L_stay[1]), beds_time_real$inflow)
data.outflow1 = data.frame(cbind(time,outflow))
data.outflow1$cum = cumsum(data.outflow1$outflow)
end_date1 = as.character(format(as.Date(start_date, format = '%m/%d/%Y') + L_stay[1] + length(beds_time_real$inflow) - 1,'%m/%d/%Y'))
data.outflow1$dates = seq.dates(from = start_date, to = end_date1, by = 'days') 
data.outflow1$dates  = as.Date(data.outflow1$dates)

time = seq(L_stay[2] + length(beds_time_real$inflow) )
outflow = c(rep(0,times = L_stay[2]), beds_time_real$inflow)
data.outflow2 = data.frame(cbind(time,outflow))
data.outflow2$cum = cumsum(data.outflow2$outflow)
end_date2 = as.character(format(as.Date(start_date, format = '%m/%d/%Y') + L_stay[2] + length(beds_time_real$inflow) - 1,'%m/%d/%Y'))
data.outflow2$dates = seq.dates(from = start_date, to = end_date2, by = 'days') 
data.outflow2$dates  = as.Date(data.outflow2$dates)

time = seq(L_stay[3] + length(beds_time_real$inflow) )
outflow = c(rep(0,times = L_stay[3]), beds_time_real$inflow)
data.outflow3 = data.frame(cbind(time,outflow))
data.outflow3$cum = cumsum(data.outflow3$outflow)
end_date3 = as.character(format(as.Date(start_date, format = '%m/%d/%Y') + L_stay[3] + length(beds_time_real$inflow) - 1,'%m/%d/%Y'))
data.outflow3$dates = seq.dates(from = start_date, to = end_date3, by = 'days') 
data.outflow3$dates  = as.Date(data.outflow3$dates)


# fit exponential model for inflow
mod <- nls(inflow ~ exp(a + b * time), data = beds_time_real, start = list(a = 1, b = 1))
mod = summary(mod)
mod

time_length = 35

time = seq(0,time_length)
inflow = exp(as.numeric(mod$coefficients[1,1]) + as.numeric(mod$coefficients[2,1])*time)
model = data.frame(cbind(time, inflow))

beds_taken = exp(as.numeric(mod$coefficients[1,1]))*exp(as.numeric(mod$coefficients[2,1])*time)/as.numeric(mod$coefficients[2,1]) - exp(as.numeric(mod$coefficients[1,1]))*exp(as.numeric(mod$coefficients[2,1])*0)/as.numeric(mod$coefficients[2,1]) 
model_total = data.frame(cbind(time, beds_taken))

end_date_total =  as.character(format(as.Date(start_date, format = '%m/%d/%Y') + time_length,'%m/%d/%Y'))
model_total$dates = seq.dates(from = start_date, to = end_date_total, by = 'days') 
model_total$dates  = as.Date(model_total$dates)

model_total$cum_outflow1 = exp(as.numeric(mod$coefficients[1,1]))*exp(as.numeric(mod$coefficients[2,1])*(time-L_stay[1]))/as.numeric(mod$coefficients[2,1]) - exp(as.numeric(mod$coefficients[1,1]))*exp(as.numeric(mod$coefficients[2,1])*0)/as.numeric(mod$coefficients[2,1]) 
model_total$cum_outflow1[1:L_stay[1]] = 0

model_total$cum_outflow2 = exp(as.numeric(mod$coefficients[1,1]))*exp(as.numeric(mod$coefficients[2,1])*(time-L_stay[2]))/as.numeric(mod$coefficients[2,1]) - exp(as.numeric(mod$coefficients[1,1]))*exp(as.numeric(mod$coefficients[2,1])*0)/as.numeric(mod$coefficients[2,1]) 
model_total$cum_outflow2[1:L_stay[2]] = 0

model_total$cum_outflow3 = exp(as.numeric(mod$coefficients[1,1]))*exp(as.numeric(mod$coefficients[2,1])*(time-L_stay[3]))/as.numeric(mod$coefficients[2,1]) - exp(as.numeric(mod$coefficients[1,1]))*exp(as.numeric(mod$coefficients[2,1])*0)/as.numeric(mod$coefficients[2,1]) 
model_total$cum_outflow3[1:L_stay[3]] = 0

model_total$inventory1 = model_total$beds_taken - model_total$cum_outflow1
model_total$inventory2 = model_total$beds_taken - model_total$cum_outflow2
model_total$inventory3= model_total$beds_taken - model_total$cum_outflow3

p1 <- ggplot() + geom_point(data = beds_time_real, aes(x = time, y = inflow)) + geom_line(data = model, aes(x = time, y = inflow), col = 'blue') +
        xlab('time (days)') + ylab('inflow') +
        theme_bw() 
p2 <- ggplot() + geom_point(data = beds_time_real, aes(x = time, y = beds_taken)) + geom_line(data = model_total, aes(x = time, y = beds_taken), col = 'blue') +
        xlab('time (days)') + ylab('cum. inflow') +
        theme_bw() 
grid.arrange(p1, p2, ncol = 2)

date.1 = format(tail(model_total$dates[model_total$inventory1<=capacity],1), "%d/%m")
date.2 = format(tail(model_total$dates[model_total$inventory2<=capacity],1), "%d/%m")
date.3 = format(tail(model_total$dates[model_total$inventory3<=capacity],1), "%d/%m")

date.1.ext = as.Date(format(tail(model_total$dates[model_total$inventory1<=capacity],1), "%Y-%m-%d"))
date.2.ext = as.Date(format(tail(model_total$dates[model_total$inventory2<=capacity],1), "%Y-%m-%d"))
date.3.ext = as.Date(format(tail(model_total$dates[model_total$inventory3<=capacity],1), "%Y-%m-%d"))

title = paste('Assuming continued exponential inflow, ICU capacity is reached on',date.1,'for', L_stay[1], 'days length of stay in ICU, on', 
              date.2, 'for', L_stay[2], 'days in ICU, or',date.3,
              'for', L_stay[3],'days in ICU', sep = ' ')
p1 <- ggplot() + 
        geom_point(data = beds_time_real, aes(x = dates, y = beds_taken, col = 'data cum. inflow')) + 
        geom_line(data = model_total, aes(x = dates, y = beds_taken, col = 'cum. inflow'), ) +
        geom_point(data = data.outflow1, aes(x = dates, y = cum, col = 'data cum. outflow')) +
        geom_line(data = model_total, aes(x = dates, y = cum_outflow1, col = 'cum. outflow')) +
        geom_line(data = model_total, aes(x = dates, y = inventory1, col = 'number of occupied beds (inventory)')) +
        xlab('') + ylab('intensive care beds') +
        geom_hline(aes(yintercept = capacity, col = 'ICU capacity')) +
        theme_bw() + theme(legend.title = element_blank(), legend.position="top",axis.text.x = element_text(angle = 30, hjust = 1)) + 
        ggtitle(paste('Length of stay =', L_stay[1], sep = ' ') ) + 
        scale_x_date(labels = date_format("%d-%m-%Y")) + 
        annotate("rect",xmin = date.1.ext, xmax = date.1.ext + 0.5,ymin = capacity-30,ymax = capacity+30, color = 'red', fill = NA)
p2 <- ggplot() + 
        geom_point(data = beds_time_real, aes(x = dates, y = beds_taken, col = 'data cum. inflow')) + 
        geom_line(data = model_total, aes(x = dates, y = beds_taken, col = 'cum. inflow'), ) +
        geom_point(data = data.outflow2, aes(x = dates, y = cum, col = 'data cum. outflow')) +
        geom_line(data = model_total, aes(x = dates, y = cum_outflow2, col = 'cum. outflow')) +
        geom_line(data = model_total, aes(x = dates, y = inventory2, col = 'number of occupied beds (inventory)')) +
        ylab('') + xlab('time') + geom_hline(aes(yintercept = capacity, col = 'ICU capacity')) +
        theme_bw() + theme(legend.title = element_blank(), legend.position="none",axis.text.x = element_text(angle = 30, hjust = 1)) + 
        ggtitle(paste('Length of stay =', L_stay[2], sep = ' ') ) + 
        scale_x_date(labels = date_format("%d-%m-%Y")) +
        annotate("rect",xmin = date.2.ext+0.5, xmax = date.2.ext + 1,ymin = capacity-30,ymax = capacity+30, color = 'red', fill = NA)
p3 <- ggplot() + 
        geom_point(data = beds_time_real, aes(x = dates, y = beds_taken, col = 'data cum. inflow')) + 
        geom_line(data = model_total, aes(x = dates, y = beds_taken, col = 'cum. inflow'), ) +
        geom_point(data = data.outflow3, aes(x = dates, y = cum, col = 'data cum. outflow')) +
        geom_line(data = model_total, aes(x = dates, y = cum_outflow3, col = 'cum. outflow')) +
        geom_line(data = model_total, aes(x = dates, y = inventory3, col = 'number of occupied beds (inventory)')) +
        xlab('') + ylab('') +geom_hline(aes(yintercept = capacity, col = 'ICU capacity')) +
        theme_bw() + theme(legend.title = element_blank(), legend.position='none',axis.text.x = element_text(angle = 30, hjust = 1)) + 
        ggtitle(paste('Length of stay =', L_stay[3], sep = ' ') ) + 
        scale_x_date(labels = date_format("%d-%m-%Y")) +
        annotate("rect",xmin = date.3.ext+0.25, xmax = date.3.ext + 0.75,ymin = capacity-30,ymax = capacity+30, color = 'red', fill = NA)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)

p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         p2 + theme(legend.position="none"),p3 + theme(legend.position="none"),
                         nrow=1), mylegend, nrow=2, heights=c(10, 1), bottom = title )

ggsave('inventory.pdf', p3, width = 35, height = 15, units = "cm") 
dev.off()
plot(p3)





