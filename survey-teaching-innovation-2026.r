library(HH)
library(readODS)

survey <- read_ods("D:/survey-teaching-innovation-2026.ods",
                   sheet="sanitized", range="A1:H70")

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

do_column_plots("drawing-1", "La tarea de dibujar me ha parecido entretenida")
do_column_plots("drawing-2", "He conseguido completar la tarea de dibujar exitosamente")
do_column_plots("drawing-3", "Mi grupo ha sido capaz de realizar la tarea de dibujar de forma autónoma")
do_column_plots("drawing-4", "La sincronización de código entre la simulación (RobotStudio) y el controlador real me ha permitido completar la tarea eficazmente")
do_column_plots("drawing-5", "No he tenido problemas durante la simulación en RobotStudio")
do_column_plots("drawing-6", "No he tenido problemas durante la ejecución del código en el robot real después de transferirlo desde RobotStudio")

do_column_plots("magnet-1", "La tarea de pick-and-place me ha parecido entretenida", reduced=TRUE)
do_column_plots("magnet-2", "He conseguido completar la tarea de pick-and-place exitosamente", reduced=TRUE)
do_single_plot("magnet-3", "Mi grupo ha sido capaz de realizar la tarea de pick-and-place de forma autónoma")
do_single_plot("magnet-4", "Disponer de una plantilla de código me ha ayudado a completar la tarea de pick-and-place")

do_column_plots("generic-1", "Creo que la tarea propuesta facilita la asimilación de los conceptos explicados en clase")
do_column_plots("generic-2", "Las herramientas (robot, FlexPendant, RobotStudio) me han parecido fáciles de manejar")
do_column_plots("generic-3", "La(s) sesión/sesiones de laboratorio me ha(n) ayudado a mejorar mis conocimientos de Robótica")
do_column_plots("generic-4", "Las clases previas y el material de teoría/ejercicios disponible en AulaGlobal me han ayudado en la realización de esta(s) sesión/sesiones práctica(s)")

do_single_plot("misc-1", "La charla introductoria sobre TIAGo++ me ha proporcionado conocimientos útiles sobre la robótica asistencial")
do_single_plot("misc-2", "Mis conocimientos previos me han ayudado durante la ejecución de las sesiones de laboratorio")
do_single_plot("misc-3", "Programar en RobotStudio ha sido más fácil que programar por bloques en el FlexPendant")
