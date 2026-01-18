library(HH)
library(readODS)

survey <- read_ods("D:/survey-sime-2026.ods",
                   sheet="sanitized", range="A1:H60")

# View(survey)

printf_pdf <- function(plot, filename, width, height, custom_fn=NULL) {
    pdf(filename, width=width, height=height)
    print(plot)

    if (!is.null(custom_fn)) {
        custom_fn()
    }

    dev.off()
}

do_column_plots <- function(question, title, reduced=FALSE) {
    FilteredRows <- survey$Question == question

    PercentPlot <-
        likert(Group ~ . | Subtable, data=survey[FilteredRows,],
               as.percent=TRUE,
               ylab=NULL,
               main=NULL,
               strip=FALSE,
               par.strip.text=list(cex=.6, lines=5),
               layout=c(1,2),
               scales=list(y=list(relation="free")),
               auto.key=FALSE,
               aspect=if (reduced) 0.1 else "fill")

    CountPlot <-
        likert(Group ~ . | Subtable, data=survey[FilteredRows,],
               ylab=NULL,
               rightAxis=TRUE,
               main=NULL,
               strip=FALSE,
               par.strip.text=list(cex=.6, lines=5),
               layout=c(1,2),
               scales=list(y=list(relation="free")),
               auto.key=FALSE,
               aspect=if (reduced) 0.27 else "fill")

    PercentCountPlot <-
        as.TwoTrellisColumns5(update(PercentPlot,
                                     rightAxis=FALSE),
                              update(CountPlot,
                                     ylab.right="Totals"),
                              pw=c(.09, .59, .01, .22, .09))

    printf_pdf(PercentCountPlot, sprintf("%s.pdf", question),
               width=9,
               height=if (reduced) 3 else 4.5,
               custom_fn=function() {
                   grid.text(title,
                             y=unit(0.97, "npc"),
                             gp=gpar(fontsize=14))
                   })
}

do_single_plot <- function(question, title) {
    PercentPlot <-
        likert(Group ~ . | Subtable, data=survey[survey$Question == question,],
               as.percent=TRUE,
               ylab=NULL,
               ylab.right="Totals",
               main=list(title, fontface=1),
               strip=FALSE,
               par.strip.text=list(cex=.6, lines=5),
               layout=c(1,1),
               scales=list(y=list(relation="free")),
               auto.key=FALSE,
               aspect=0.1)

    printf_pdf(PercentPlot, sprintf("%s.pdf", question),
               width=9,
               height=2.4)
}

do_column_plots("drawing-1", "I found the drawing task entertaining")
do_column_plots("drawing-2", "I managed to successfully complete the drawing task")
do_column_plots("drawing-3", "Our group managed to complete the drawing task with little or no assistance from the instructor")
do_column_plots("drawing-4", "Code transfer between simulation (RobotStudio) and the real controller improved my productivity")
do_column_plots("drawing-5", "I had no issues while running the simulation in RobotStudio")
do_column_plots("drawing-6", "I had no issues while running the same code on the real robot after transferring it from RobotStudio")

do_column_plots("magnet-1", "I found the pick-and-place task entertaining", reduced=TRUE)
do_column_plots("magnet-2", "I managed to successfully complete the pick-and-place task", reduced=TRUE)
do_single_plot("magnet-3", "Our group managed to complete the pick-and-place task with little or no assistance from the instructor")
do_single_plot("magnet-4", "Having a code template helped me complete the pick-and-place task")

do_column_plots("generic-1", "I think that the proposed task(s) helped boost learning")
do_column_plots("generic-2", "I found the tools (robot, FlexPendant, RobotStudio) easy to use")
do_column_plots("generic-3", "The lab session(s) helped me improve my robotics skills")
do_column_plots("generic-4", "The course materials and theory/practice classes helped me prepare for the real lab session(s)")

do_single_plot("misc-1", "The introductory talk on TIAGo++ provided me with valuable insights on assistive robotics")
do_single_plot("misc-2", "My previous knowledge helped me during the execution of the lab sessions")
do_single_plot("misc-3", "Programming on RobotStudio was easier than block programming on the FlexPendant")
