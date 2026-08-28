library(shiny)
library(lifecontingencies)
library(ggplot2)

## -------------------------------
##  CARGAR TABLA DE MORTALIDAD
## -------------------------------
data <- read.csv("SULT.csv")
table1 <- probs2lifetable(data$qx, type = "qx", radix = 100000, name = "Table1")

i <- 0.05
v <- 1/(1 + i)
retire_age <- 65

## ===============================
##           UI
## ===============================
ui <- fluidPage(
  titlePanel("Plan de Pensión – Prima y Reservas"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "edad",
        "Edad actual:",
        min = 25, max = 45, value = 25, step = 1
      ),
      
      numericInput(
        "pension",
        "Pensión mensual deseada ($):",
        value = 30000,
        min = 1000,
        step = 500
      ),
      
      selectInput(
        "n_primas",
        "Años de pago de primas (anticipadas):",
        choices = c(10, 15, 20, 25),
        selected = 15
      )
    ),
    
    mainPanel(
      h3("Prima anual anticipada"),
      verbatimTextOutput("prima_out"),
      
      h3("Valor del primer pago mensual a la edad 65 (Valor Presente)"),
      verbatimTextOutput("primer_pago_out"),   # ← ESTE ERA EL QUE NO APARECÍA
      
      h3("Tabla de reservas"),
      tableOutput("tabla_out"),
      
      h3("Gráfica de la reserva"),
      plotOutput("plot_out")
    )
  )
)

## ===============================
##          SERVER
## ===============================
server <- function(input, output, session) {
  
  ## --- Cálculos reactivos base (prima y reservas) ---
  calc <- reactive({
    x <- input$edad
    monthly_amount <- input$pension
    n <- as.numeric(input$n_primas)
    
    # Anualidad vitalicia mensual anticipada a los 65
    annuity65 <- axn(table1, x = retire_age, i = i, k = 12, payment = "due")
    
    # Probabilidad de sobrevivir de x a 65
    p_x_65 <- pxt(table1, x = x, t = retire_age - x)
    
    # Valor presente del beneficio a la edad x por 1 peso mensual
    PV_retire <- (v^(retire_age - x)) * p_x_65 * annuity65
    
    # Anualidad de primas (n pagos anticipados)
    denom <- axn(table1, x = x, n = n, i = i, payment = "due")
    
    P_ret <- PV_retire / denom          # prima por 1 peso mensual
    P <- P_ret * monthly_amount         # prima anual total
    
    # Reservas desde edad x hasta 20 años después del retiro
    ages <- x:(retire_age + 20)
    reserva <- numeric(length(ages))
    
    for (j in seq_along(ages)) {
      age_now <- ages[j]
      t <- age_now - x
      
      ## PV de beneficios futuros
      if (age_now < retire_age) {
        t_defer <- retire_age - age_now
        p_def <- pxt(table1, x = age_now, t = t_defer)
        ann_65_now <- axn(table1, x = retire_age, i = i, k = 12, payment = "due")
        pv_benef <- (v^t_defer) * p_def * ann_65_now * monthly_amount
      } else {
        ann_current <- axn(table1, x = age_now, i = i, k = 12, payment = "due")
        pv_benef <- ann_current * monthly_amount
      }
      
      ## PV de primas futuras
      if (t < n) {
        pv_primas <- axn(table1, x = age_now, i = i, n = n - t, payment = "due") * P
      } else {
        pv_primas <- 0
      }
      
      reserva[j] <- pv_benef - pv_primas
    }
    
    list(
      P = P,
      ages = ages,
      reserva = reserva,
      p_x_65 = p_x_65,
      monthly_amount = monthly_amount,
      x = x
    )
  })
  
  ## --- Output: Prima anual anticipada ---
  output$prima_out <- renderPrint({
    res <- calc()
    res$P
  })
  
  ## --- Output: Valor presente del primer pago mensual a los 65 ---
  output$primer_pago_out <- renderPrint({
    res <- calc()
    
    x <- res$x
    monthly_amount <- res$monthly_amount
    p_x_65 <- res$p_x_65
    
    # Valor presente del primer pago mensual
    PV_primer_pago <- monthly_amount * p_x_65 * (v^(retire_age - x))
    
    PV_primer_pago
  })
  
  ## --- Tabla de reservas ---
  output$tabla_out <- renderTable({
    res <- calc()
    data.frame(Edad = res$ages, Reserva = res$reserva)
  })
  
  ## --- Gráfica de reservas ---
  output$plot_out <- renderPlot({
    res <- calc()
    df <- data.frame(edad = res$ages, reserva = res$reserva)
    
    x <- input$edad
    n <- as.numeric(input$n_primas)
    
    ggplot(df, aes(x = edad, y = reserva)) +
      geom_line(color = "#FF69B4", linewidth = 1.2) +   # Rosa cute
      geom_vline(xintercept = retire_age,
                 linetype = "dashed",
                 color = "#FF0000",
                 linewidth = 1) +                      # Rojo (Retiro)
      geom_vline(xintercept = x + n,
                 linetype = "dashed",
                 color = "#00CC66",
                 linewidth = 1) +                      # Verde (Fin de primas)
      labs(
        title = "Reserva actuarial del plan de pensiones",
        subtitle = "Rojo = Retiro (65) | Verde = Fin del pago de primas",
        x = "Edad",
        y = "Reserva"
      ) +
      theme_minimal()
  })
}

## ===============================
##  LANZAR APP
## ===============================
shinyApp(ui = ui, server = server)

