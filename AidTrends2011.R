# Aid Trends
# Bastiaan Quast
# bquast@gmail.com

# load packages
library(plm)
library(plyr)
library(ggplot2)
library(scales)

# load the data
ocha <- read.csv('ocha_R4_A924___1406131346.csv', sep=';')

# inspect the data
View(ocha)
str(ocha)

# rename to lowercase
names(ocha) <- tolower(names(ocha))
ocha$funding...usd

# numeric donation
amount <- ocha$funding...usd
amount <- gsub(',', '', amount)
ocha$amount <- as.numeric(amount)
rm(amount)

# add clm variable
clm.list <- c('Czech Republic', 'Denmark', 'Finland', 'Germany', 'Luxembourg', 'Netherlands', 'Norway', 'Sweden', 'Switzerland', 'Iceland')
pol.strat.list <- c('Canada', 'European Commission Humanitarian Aid Office', 'United States of America', 'UNICEF National Committee/United Kingdom', 'UNICEF National Committee/Ireland')
ocha$donor.type <- ifelse(ocha$donor %in% clm.list, 'clm',
                          ifelse(ocha$donor %in% pol.strat.list, 'polstrat', 'other'
                            )
                          ) 

# summarise data
aggregate <- ddply(ocha, .(donor.type), summarize, total.aid=sum(amount))
aggregate

# plot data
plot <- ggplot(aggregate, aes(donor.type, weight=total.aid) )
plot <- plot + geom_bar()
plot <- plot + scale_y_continuous(name='Other Humanitarian Funding' , labels=comma) + xlab('Donor Type')
plot

png('ocha2011.png')
print(plot)
dev.off()


# even better
breakdown <- ddply(ocha, .(donor.type, channel), summarize, total.aid=sum(amount))
breakdown

bd.plot <- ggplot(breakdown, aes(donor.type, weight=total.aid, color=channel))
bd.plot <- bd.plot + geom_bar()
bd.plot <- bd.plot + scale_y_continuous(name='Other Humanitarian Funding' , labels=comma) + xlab('Donor Type')
bd.plot

png('bd.ocha2011png')
print(bd.plot)
dev.off()