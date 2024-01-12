milestoneplot<- function(riskitem, milestonedf){
projphases<- milestonedf |>
  filter(PHASEID == riskitem$PROJECTPHASEID)|>
  mutate('yval' = rep(0.15))|>
  arrange(ORDERBY)
  
projphases$xwrap = str_wrap(projphases$MILESTONE, width=15)



ggplot(projphases, aes(x=factor(MILESTONE), level=ORDERBY, y= yval))+
  geom_segment(x=1, y = 0.15, xend = length(projphases$ORDERBY), yend = 0.15, size=1.5, color = "grey")+
  geom_point(size=8,color="grey", fill="snow1", shape=21, stroke=1.5)+
  geom_point(aes(x=riskitem$MILESTONE, y=0.15), colour="#1F78B4", fill="#1F78B4", shape = 21, size =8, stroke=1.5)+
  geom_text(aes(label=xwrap), position="nudge", vjust=1.7)+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim=c(0.15,0.15))+
  aes(x = fct_inorder(MILESTONE))+
  theme(line = element_blank(),
        title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        panel.background = element_blank(),
  )+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))
}
