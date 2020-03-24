aboutUI <- function(id) {
  ns <- NS(id)
  material_row(
    material_column(width = 6,
                    h2("The data"),
                    img(src="./img/enterprise-medicine.logo.small.horizontal.white.581be190.png", width=120, style="background-color:#002d72;padding: 0.8em;
"),
                    p("All data is taken from Johns Hopkins CSSE dataset on github"),
                    a(href="https://github.com/CSSEGISandData/COVID-19", "Johns Hopkins Github Repository")
    ),
    material_column(width = 6,
                    h2("The author"),
                    material_row(
                      material_column(width = 6,
                                      img(src="./img/zappingseb.jfif", width=60),
                                      p(a(href="https://mail-wolf.de/?page_id=1292", alt="Sebastian Engel-Wolf", "Sebastian Engel-Wolf"), " is a freelance scientific software developer developing R-shiny apps in
                            a pharmaceutical environment")
                      ),
                      material_column(width = 6,
                                      img(src="./img/graphcount.jpg", width=60),
                                      p("My Monkey and Graph Count did some work")          
                      )
                    ),
                    p(a(href="https://github.com/zappingseb/coronashiny", "All code for this project"), "can be found on github")
                    
    )
  )
}
about <- function(input, output, session) {
  
}