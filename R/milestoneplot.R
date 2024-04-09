#' @title Milestone Plot
#'
#' @description Creates a milestone chart
#'
#' @param riskitem   data frame; A data frame of a filtered risk item.
#' @param milestonedf data frame; A data frame of phase-milestone pairings
#'
#' @return A ggplot of current milestone on timeline of all project milestones.
#'
#' @examples
#' #Get milestone data and risk item
#' milestonedf<-read.csv("inst/app/data/PHASEMILESTONE.csv")
#' milestone_df<-data.frame(milestonedf)
#'
#' riskitem <- risk_item
#'
#' #example
#' milestone_plot<-milestoneplot(riskitem=risk_item, milestonedf = milestone_df)
#'
#' @importFrom dplyr mutate select arrange
#' @importFrom rlang .data
#' @importFrom scales label_wrap
#' @export
#'



milestoneplot<- function(riskitem, milestonedf){
projphases<- milestonedf |>
  dplyr::filter(PHASEID == riskitem$PROJECTPHASEID)|>
  dplyr::mutate('yval' = rep(0.1))|>
  dplyr::arrange(ORDERBY)

#, vjust=1.4

mileplot <- ggplot(projphases, aes(x=factor(MILESTONE), level=ORDERBY, y= yval))+
  geom_segment(x=1, y = 0.1, xend = length(projphases$ORDERBY), yend = 0.1, linewidth=1.5, color = "grey")+
  geom_point(size=8,color="grey", fill="snow1", shape=21, stroke=1.5)+
  geom_point(aes(x=riskitem$MILESTONE, y=0.1), colour="#1F78B4", fill="#1F78B4", shape = 16, size =7.75)+
  geom_text(aes(label=str_wrap(MILESTONE, width=14)), 
            position = position_nudge(y = -0.5))+
  scale_y_continuous(limits=c(-1,1),expand=c(0,0))+
  coord_cartesian(clip="off")+
  aes(x = fct_inorder(MILESTONE))+
  theme(line = element_blank(),
        title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        panel.background = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm")
        )
mileplot<-ggsave("mileplot.png", dpi=600)
return(mileplot)
}
