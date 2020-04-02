### Supplementary analyses

source("analyses2.R")

library(ggalt)

# Document data -----------------------------------------------------------

## Histogram of document publication date

documents$d_publication_date <- as.numeric(documents$`d_publication date`)

ggplot(documents, aes(d_publication_date)) + 
  geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks = seq(1860,2000,10)) +
  geom_vline(xintercept = mean(documents$d_publication_date, na.rm=T),
             linetype="dotted", 
             color = "grey", size=.5)+
  labs(x="\nDocument publication years (mean year 1960)", y="Count\n")

## Dumbbell plot of fieldwork time frames

documents$d_field_date_start <- as.numeric(documents$d_field_date_start)
documents$d_field_date_end <- as.numeric(documents$d_field_date_end)

documents2<-left_join(documents, leader_cult)

plot_field<- ggplot(documents2[!is.na(documents$d_field_date_start)==T,], 
       aes(x=d_field_date_start, xend=d_field_date_end, y=reorder(d_ID, d_field_date_start), colour=region)) + 
  geom_dumbbell() +
  facet_grid(region~., scales = "free_y", space = "free_y")+ 
  scale_x_continuous(breaks = seq(1790,2000,10), minor_breaks = seq(1790,2000,10)) +
  labs(x="\nDocument field work start and end dates", y="")+
  theme_bw(15) +
  theme(axis.text.y=element_blank(),
        strip.text.y = element_text(angle=0),
        legend.position = "none",
        strip.text = element_text(colour = 'black'))
plot_field
