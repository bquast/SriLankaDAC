# Plotting Western Aid vs. DAC
# Bastiaan Quast
# bquast@gmail.com

# load packages
library(plm)
library(plyr)
library(ggplot2)
library(scales)

# load the data
western.aid <- read.csv('WesternAid.csv')

# inspect the data
View(western.aid)

# keep only value of Aid.type: Grants, Total
oda <- subset(western.aid, Aid.type == 'ODA: Total Net' & Amount.type == 'Constant Prices (2012 USD millions)')
names(oda) <- tolower(names(oda))

# create a coalition-of-like-minded dummy variable
clm.list <- c('Denmark', 'Finland', 'Germany', 'Luxembourg', 'Netherlands', 'Norway', 'Sweden', 'Switzerland', 'Iceland')
oda$clm <- ifelse(oda$donor %in% clm.list, TRUE, FALSE)
save(oda, file = 'oda.RData' )

# summarise
oda.sum <- ddply(oda, .(clm, year), summarize, total.aid=sum(value))
oda.sum

# plot data
plot <- ggplot(oda.sum, aes(year, total.aid, colour=clm))
plot <- plot + geom_line() + scale_x_continuous(breaks= pretty_breaks())
plot <- plot + labs( x ='Year',  y = 'Total Aid Western (thousand USD)')
plot <- plot + scale_color_discrete(name = "Coalition of \nlike minded states", labels = c('Non-members', 'Members') )
print(plot)

# create image file
png('clm.png')
print(plot)
dev.off()
