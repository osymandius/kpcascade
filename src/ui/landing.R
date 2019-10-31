landing <- function() {
  fluidRow(
    div(style="margin-right:50px",
        splitLayout(align="center",
                    style="display: flex; justify-content: space-around; align-items: center; margin-top:50px",
                    cellWidths = rep("25%", 4),
                    img(style="width:200px", src='SANAC.jpg'),
                    img(style="width:250px", src="UCSF.jpg"),
                    img(style="width:250px", src='imperial.png'),
                    img(style="width:250px", src='pepfar.png')
        )
    ),
    h1(align="center", "KPCascade"),
    br(),
    div(style="margin-left: 10%; margin-right: 10%; font-size:16px",
        p("Cascades are a method that is used to outline the steps of care that people go through when accessing a health service. HIV care & treatment cascades reflect the pathway people living with HIV go through from diagnosis towards achieving viral suppression. The cascade graphs depict the proportion of people engaged at each stage of the care pathway."),
        br(),
        p("This site provides an interactive platform to upload and visualise available South African Key Population (KP) surveillance data. It was developed in collaboration between the South African National AIDS Council (SANAC), the University of California, San Francisco and Imperial College London. Default surveillance data inputs are of Female Sex Workers from the South African Health Monitoring Survey (SAHMS) 2013-14 and 2017-18). These studies have been conducted in an academic partnership between the University of California, San Francisco, the Anova Health Institute, the Aurum Institute, the Wits Reproductive Health and HIV Research Institute and the Human Sciences Research Council.")
    )
  )
}