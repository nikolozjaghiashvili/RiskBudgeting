library("Rblpapi")
con <-blpConnect()
library(jrvFinance)
library(neldermead)
library(optimization)
library(lme4)
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(RColorBrewer)
library(openxlsx)
library(timeDate)
library(NMOF)
library(arrangements)
library(lpSolve)
library(readxl)
library(shiny)
options(scipen=999)

ui = fluidPage(
  
  
  fluidRow(
    column(3,
           textInput('sec1','','912828F6     Govt'),
           textInput('sec2','','AS088785     Corp'),
           textInput('sec3','','EDM1 Comdty'),
           textInput('sec4','','TUU9 Comdty')
    ),
    column(3,
           numericInput('pos1','', 0),
           numericInput('pos2','', 0),
           numericInput('pos3','', 0),
           numericInput('pos4','', 0)
    ),
    column(3,
           actionButton("update", "Update View"),
           actionButton("update1", "Strategy 1"),
           actionButton("update2", "Strategy 2"),
           actionButton("update3", "Strategy 3"),
           actionButton("update4", "Strategy New")
           )
    ),
  
  
  
  hr(),
  
  mainPanel(
    tabsetPanel(
      id = 'Charts',
      tabPanel("Key Rate", plotOutput('plot1')),
      tabPanel("Principal Component", plotOutput('plot2')),
      tabPanel("Eigenvectors", plotOutput('plot3'))
    ),
    tabsetPanel(
      id = 'Tables',
      tabPanel("Eigenvector Loadings",  tableOutput("view1")),
      tabPanel("Principal Component Loadings",  tableOutput("view2")),
      tabPanel("S. Eigenvector Loadings", tableOutput("view3")),
      tabPanel("S. Principal Component Loadings", tableOutput("view4"))
    ),
    tabsetPanel(
      id = 'Strategies',
      tabPanel("S1", plotOutput('plot4')),
      tabPanel("S2", plotOutput('plot5')),
      tabPanel("S3", plotOutput('plot6')),
      tabPanel("Sn", plotOutput('plot7'))
    )
  )
)

server = function(input, output) {
  Port = read_excel("Port.xlsx")
  Pos = read_excel("Pos.xlsx")
  
  foo = function(){
    if (exists("PV") == FALSE | exists("PVB") == FALSE) {
      library(readxl)
      Port = read_excel("Port.xlsx")
      Pos = read_excel("Pos.xlsx")
      Port = Port$Port
      Pos = Pos$Position
      Price = bdp(Port,"PX_MID")
      Mval = Pos*Price$PX_MID
      Stype = bdp(Port,"SECURITY_TYP")
      Stype = Stype$SECURITY_TYP
      cpn = bdp(Port,"CPN_TYP")
      cpn = cpn$CPN_TYP
      tenor = c("3MO","6MO","1YR","2YR","3YR","4YR","5YR")
      tenor1 = c("3M","6M","1Y","2Y","3Y","4Y","5Y")
      
      q = cbind(Port,Pos, Stype, cpn)
      w = q[order(q[,3]),]
      
      der = subset(w,w[,3]=="Financial commodity future.")
      frn = subset(w,w[,4]=="FLOATING")
      fxn = subset(w,w[,4]!="FLOATING" & w[,3]!="Financial commodity future." & w[,3]!="SPOT")
      spot = subset(w, w[,3]=="SPOT")
      
      if (all(is.na(der)) == FALSE){
        KRMD = c()
        for (x in tenor1) {
          print(x)
          fld = paste("GRID_PT_DELTA_",x,sep = "")
          k = bdp(der[,1], fld)
          print(k)
          KRMD = cbind(KRMD, k[,1])
        }
      } else {
        KRMD = NULL
      }
      
      
      
      if (all(is.na(frn)) == FALSE){
        KRMfr = c()
        for (x in tenor) {
          print(x)
          if (x == "3MO") {
            fld = paste("DUR_ADJ_OAS_BID")
            k = bdp(frn[,1], fld)
            print(k)
            KRMfr = cbind(KRMfr,k[,1])
          } else {
            KRMfr = cbind(KRMfr, matrix(0,nrow(KRMfr),1))
          }
        }   
      }else {
        KRMfr = NULL
      }
      
      if (all(is.na(fxn)) == FALSE){
        KRMfx = c()
        for (x in tenor) {
          print(x)
          fld = paste("KEY_RATE_DUR_",x,sep = "")
          k = bdp(fxn[,1], fld)
          print(k)
          KRMfx = cbind(KRMfx, k[,1])
        }
      } else {
        KRMfx = NULL
      }
      
      
      KRMsp = c()
      for (x in tenor) {
        print(x)
        k = 0
        KRMsp = cbind(KRMsp, k)
      }
      
      
      
      KRM = rbind(KRMD,KRMfx,KRMfr, KRMsp)
      KRM = as.matrix(KRM)
      KR = colSums(KRM)
      
      
      if (all(is.na(der)) == FALSE){
        posD = c()
        contnum = as.numeric(der[,2])
        contval = bdp(der[,1],"FUT_CONT_SIZE")[,1]
        posD = contnum*contval
      } else {
        posD = NULL
      }
      
      
      
      if (all(is.na(frn)) == FALSE){
        posfr = c()
        contnum = as.numeric(frn[,2])
        contval = bdp(frn[,1],"PX_MID")[,1]
        posfr = contnum*contval*10
        
      } else {
        posfr = NULL
      }
      
      
      if (all(is.na(fxn)) == FALSE){
        posfx = c()
        contnum = as.numeric(fxn[,2])
        contval = bdp(fxn[,1],"PX_MID")[,1]
        posfx = as.matrix(contnum*contval*10)
      } else {
        posfx = NULL
      }
      
      
      
      
      pos = rbind(as.matrix(posD),as.matrix(posfx),as.matrix(posfr),as.matrix(spot[,2]))
      pos = as.numeric(pos)
      PVM = KRM*pos/10000
      
      PV = colSums(PVM)
      
      
      
      PortB = read_excel("PortB.xlsx")
      PosB = read_excel("PosB.xlsx")
      PortB = PortB$Port
      PosB = PosB$Position
      PriceB = bdp(PortB,"PX_MID")
      MvalB = PosB*PriceB$PX_MID
      StypeB = bdp(PortB,"SECURITY_TYP")
      StypeB = StypeB$SECURITY_TYP
      cpnB = bdp(PortB,"CPN_TYP")
      cpnB = cpnB$CPN_TYP
      tenor = c("3MO","6MO","1YR","2YR","3YR","4YR","5YR")
      tenor1 = c("3M","6M","1Y","2Y","3Y","4Y","5Y")
      
      q = cbind(PortB,PosB, StypeB, cpnB)
      w = q[order(q[,3]),]
      
      derB = subset(w,w[,3]=="Financial commodity future.")
      frnB = subset(w,w[,4]=="FLOATING")
      fxnB = subset(w,w[,4]!="FLOATING" & w[,3]!="Financial commodity future." & w[,3]!="SPOT")
      spotB = subset(w, w[,3]=="SPOT")
      
      
      if (all(is.na(derB)) == FALSE){
        KRMDB = c()
        for (x in tenor1) {
          print(x)
          fld = paste("GRID_PT_DELTA_",x,sep = "")
          k = bdp(derB[,1], fld)
          KRMDB = cbind(KRMDB, k[,1])
        }
      } else {
        KRMDB = NULL
      }
      print(KRMDB)
      if (all(is.na(frnB)) == FALSE){
        KRMfrB = c()
        for (x in tenor) {
          print(x)
          if (x == "3MO") {
            fld = paste("DUR_ADJ_OAS_BID")
            k = bdp(frnB[,1], fld)
            KRMfrB = cbind(KRMfrB,k[,1])
          } else {
            KRMfrB = cbind(KRMfrB, matrix(0,nrow(KRMfrB),1))
          }
        }   
      }else {
        KRMfrB = NULL
      }
      print(KRMfrB)
      
      
      if (all(is.na(fxnB)) == FALSE){
        KRMfxB = c()
        for (x in tenor) {
          print(x)
          fld = paste("KEY_RATE_DUR_",x,sep = "")
          k = bdp(fxnB[,1], fld)
          KRMfxB = cbind(KRMfxB, k[,1])
        }
      } else {
        KRMfxB = NULL
      }
      
      print(KRMfxB)
      
      KRMspB = c()
      for (x in tenor) {
        print(x)
        k = 0
        KRMspB = cbind(KRMspB, k)
      }
      
      
      
      KRMB = rbind(KRMDB,KRMfxB,KRMfrB, KRMspB)
      KRMB = as.matrix(KRMB)
      KRB = colSums(KRMB)
      
      if (all(is.na(derB)) == FALSE){
        posDB = c()
        contnum = as.numeric(derB[,2])
        contval = bdp(derB[,1],"FUT_CONT_SIZE")[,1]
        posDB = contnum*contval
      } else {
        posDB = NULL
      }
      
      print(posDB)
      
      
      
      if (all(is.na(frnB)) == FALSE){
        contnum = as.numeric(frnB[,2])
        contval = bdp(frnB[,1],"PX_MID")[,1]
        posfrB = contnum*contval*10
        
      } else {
        posfrB = NULL
      }
      
      print(posfrB)
      
      
      
      
      if (all(is.na(fxnB)) == FALSE){
        contnum = as.numeric(fxnB[,2])
        contval = bdp(fxnB[,1],"PX_MID")[,1]
        posfxB = as.matrix(contnum*contval*10)
      } else {
        posfxB = NULL
      }
      
      print(posfxB)
      
      posB = rbind(posDB,posfxB,posfrB,spotB[,2])
      posB = as.numeric(as.vector(posB))
      print(posB)
      print(KRMB)
      PVMB = KRMB*posB/10000
      
      PVB = colSums(PVMB)
      PV <<- PV
      PVB <<- PVB
      PVM <<- PVM
      KRM <<- KRM
    }
  }
  
  if (exists("PV") == FALSE | exists("PVB") == FALSE) {
    
    foo()
  }
  
  strat = cbind(PVM,Port$Strat)
  for (n in 1:max(Port$Strat)){
    assign(paste("S", n, sep = ""), subset(strat,strat[,ncol(strat)]==n)[,-ncol(strat)])
    assign(paste("PVS", n, sep = ""), colSums(subset(strat,strat[,ncol(strat)]==n)[,-ncol(strat)]))
  }
  
  PortS = eventReactive(input$update, {c(input$sec1,input$sec2,input$sec3,input$sec4)})
  PosS = eventReactive(input$update, {c(input$pos1,input$pos2,input$pos3,input$pos4)})
  PriceS = eventReactive(input$update, {bdp(PortS(),"PX_MID")})
  MvalS = eventReactive(input$update, {PosS()*PriceS()[,1]})
  
  StypeS1 = eventReactive(input$update, {bdp(PortS(),"SECURITY_TYP")})
  StypeS = eventReactive(input$update, {StypeS1()[,1]})
  cpnS1 = eventReactive(input$update, {bdp(PortS(),"CPN_TYP")})
  cpnS = eventReactive(input$update, {cpnS1()[,1]})
  
  tenor = c("3MO","6MO","1YR","2YR","3YR","4YR","5YR")
  tenor1 = c("3M","6M","1Y","2Y","3Y","4Y","5Y")
  
  q = reactive({cbind(PortS(),PosS(), StypeS(), cpnS())})
  w = reactive({q()[order(q()[,3]),]})
  
  derS = reactive({(subset(w(),w()[,3]=="Financial commodity future."))[,1]})
  frnS = reactive({(subset(w(),w()[,4]=="FLOATING"))[,1]})
  fxnS = reactive({(subset(w(),w()[,4]!="FLOATING" & w()[,3]!="Financial commodity future." & w()[,3]!="SPOT"))[,1]})
  spotS = reactive({(subset(w(), w()[,3]=="SPOT"))[,1]})

  KRMDS <- reactive({
    KRMDS = (matrix(0,nrow = length(derS()),ncol = length(tenor)))
    i=0
    for (x in tenor1) {
      print(x)
      i = i + 1
      
      fld = paste("GRID_PT_DELTA_",x,sep = "")
      KRMDS[,i] = bdp(derS(), fld)[,1]
    }
    
    KRMDS
  })
 
  KRMfrS = reactive({(matrix(0,nrow = length(frnS()),ncol = length(tenor)))})
  KRMfrS <- reactive({cbind(bdp(frnS(),"DUR_ADJ_OAS_BID")[,1],matrix(0,nrow = length(frnS()),ncol = 6))})
  
  KRMfxS <- reactive({
    KRMfxS = (matrix(0,nrow = length(fxnS()),ncol = length(tenor)))
    i=0
    for (x in tenor) {
      print(x)
      i = i + 1
      
      fld = paste("KEY_RATE_DUR_",x,sep = "")
      KRMfxS[,i] = bdp(fxnS(), fld)[,1]
    }
    
    KRMfxS
  })
  
  KRMspS = reactive({(matrix(0,nrow = 1,ncol = length(tenor)))})
  KRMS = reactive({rbind(KRMDS(),KRMfxS(),KRMfrS(), KRMspS())})
  KRS = reactive({colSums(KRMS())})

  contnum1 = reactive({(subset(w(),w()[,3]=="Financial commodity future."))[,2]})
  contval1 = reactive({bdp(derS(),"FUT_CONT_SIZE")[,1]})
  posDS   = reactive({as.numeric(contnum1())*as.numeric(contval1())})

  contnum2 = reactive({(subset(w(),w()[,4]=="FLOATING"))[,2]})
  contval2 = reactive({bdp(frnS(),"PX_MID")[,1]})
  posfrS   = reactive({as.numeric(contnum2())*as.numeric(contval2())})
  

  contnum3 = reactive({(subset(w(),w()[,4]!="FLOATING" & w()[,3]!="Financial commodity future." & w()[,3]!="SPOT"))[,2]})
  contval3 = reactive({bdp(fxnS(),"PX_MID")[,1]})
  posfxS   = reactive({as.numeric(contnum3())*as.numeric(contval2())})

  
  
  posS1 = reactive({rbind(as.matrix(posDS()),as.matrix(posfxS()),as.matrix(posfrS()))})

  KRMST = reactive({KRMS()[-5,]})
  PVMS = reactive({KRMST()*as.vector(t(posS1()))/10000})
  
  
  PVS = reactive({colSums(PVMS())})
  
  netexp = PV - PVB
  netexpS = reactive(netexp+PVS())

  
  counts = reactive({cbind(as.matrix(netexp),as.matrix(netexpS()))})
  counts1 = reactive({t(counts())})
  
  names(netexp) = tenor
  reactive({names(netexpS()) = tenor})
  
  yoo = function(){
    ## Set Dataset timeframe Start
    start <- "2019-07-09"
    end <- "2020-07-09"
    opt <- c("periodicitySelection"="DAILY")
    ## Set Dataset timeframe end
    
    ## Extract Data Start
    M3 <- bdh("USGG3M  Index","PX_MID", as.Date(start), as.Date(end),options = opt)
    M6 <- bdh("USGG6M  Index","PX_MID", as.Date(start), as.Date(end),options = opt)
    Y1 <- bdh("USGG12M  Index","PX_MID", as.Date(start), as.Date(end),options = opt)
    Y2 <- bdh("USGG2YR  Index","PX_MID", as.Date(start), as.Date(end),options = opt)
    Y3 <- bdh("USGG3YR  Index","PX_MID", as.Date(start), as.Date(end),options = opt)
    Y5 <- bdh("USGG5YR  Index","PX_MID", as.Date(start), as.Date(end),options = opt)
    
    M3 = M3[nrow(M3):1,]
    M6 = M6[nrow(M6):1,]
    Y1 = Y1[nrow(Y1):1,]
    Y2 = Y2[nrow(Y2):1,]
    Y3 = Y3[nrow(Y3):1,]
    Y5 = Y5[nrow(Y5):1,]
    
    
    data <- cbind(M3[,2],M6[,2],Y1[,2],Y2[,2],Y3[,2],Y5[,2])
    names <- c("M3","M6","Y1","Y2","Y3","Y5")
    colnames(data) <- names
    
    Y4 = NULL
    for (n in 1:nrow(data)) {
      print(n)
      y =  data[n,]
      x <- c(0.25, 0.5, 1, 2, 3, 5)
      spl <- smooth.spline(y ~ x)
      z = predict(spl, 4)$y
      Y4 = rbind(Y4,z)
    }
    
    
    
    ddata = cbind(ave(M3$PX_MID, FUN = function(x) c(NA, diff(x))),ave(M6$PX_MID, FUN = function(x) c(NA, diff(x))),
                  ave(Y1$PX_MID, FUN = function(x) c(NA, diff(x))),ave(Y2$PX_MID, FUN = function(x) c(NA, diff(x))),
                  ave(Y3$PX_MID, FUN = function(x) c(NA, diff(x))),ave(Y4, FUN = function(x) c(NA, diff(x))),
                  ave(Y5$PX_MID, FUN = function(x) c(NA, diff(x))))
    ddata = ddata[-1,]
    
    names = c("M3","M6","Y1","Y2","Y3","Y4","Y5")
    colnames(ddata) = names
    rownames(ddata) = as.character(Y5[-1,1])
    data = ddata
    
    mypr <- prcomp(data[,], scale = FALSE)
    summary(mypr)
    
    vector <- mypr$rotation
    par(mfrow=c(2,2))
    plot(vector[,1], type = "l", col = "red")
    plot(vector[,2], type = "l", col = "green")
    plot(vector[,3], type = "l", col = "blue")
    plot(vector[,1], type = "l", col = "red", ylim = c(-1,1))
    lines(vector[,2], type = "l", col = "green")
    lines(vector[,3], type = "l", col = "blue")
    
    vv <<- vector[,1:3] 
    
  }
  
  yoo()
  
  
  mtx = vv*netexp
  mtxS = reactive({vv*netexpS()})
  csmtx = colSums(mtx)
  csmtxS = reactive({colSums(mtxS())})
  
  counts2 = reactive({cbind(as.matrix(csmtx),as.matrix(csmtxS()))})
  counts3 = reactive({t(counts2())})
  
  
  S1S= PVS1
  S2S= PVS2
  S3S= PVS3
  SnS= 0
  
  S1S = eventReactive(input$update1,{PVS1 + PVS()})
  S2S = eventReactive(input$update2,{PVS2 + PVS()})
  S3S = eventReactive(input$update3,{PVS3 + PVS()})
  SnS = eventReactive(input$update4,{PVS()})
  
  S1counts = reactive({cbind(as.matrix(PVS1),as.matrix(S1S()))})
  S1counts1 = reactive({t(S1counts())})
  S2counts = reactive({cbind(as.matrix(PVS2),as.matrix(S2S()))})
  S2counts1 = reactive({t(S2counts())})
  S3counts = reactive({cbind(as.matrix(PVS3),as.matrix(S3S()))})
  S3counts1 = reactive({t(S3counts())})
  Sncounts = reactive({as.matrix(SnS())})
  Sncounts1 = reactive({t(Sncounts())})
  
  
  
  output$plot1 <- renderPlot({
    barplot(counts1(), main="Simulated KR",
            xlab="Key rates", col=c("darkgreen","green"),  beside=TRUE,axis.lty="solid")})
  
  output$plot2 <- renderPlot({
    barplot(counts3(), main="Simulated PC",
            xlab="Key rates", col=c("darkblue","red"),  beside=TRUE,axis.lty="solid")})
  
  

  output$plot3 <- renderPlot({ 
    plot(vv[,1], type = "l", col = "red", ylim = c(-1,1))
    lines(vv[,2], type = "l", col = "green")
    lines(vv[,3], type = "l", col = "blue")
  })
  
  
  
  output$plot4 <- renderPlot({
    barplot(S1counts1(), main="Simulated S1",
            xlab="Key rates", col=c("darkgreen","green"),  beside=TRUE,axis.lty="solid")})
  output$plot5 <- renderPlot({
    barplot(S2counts1(), main="Simulated S2",
            xlab="Key rates", col=c("darkgreen","green"),  beside=TRUE,axis.lty="solid")})
  output$plot6 <- renderPlot({
    barplot(S3counts1(), main="Simulated S3",
            xlab="Key rates", col=c("darkgreen","green"),  beside=TRUE,axis.lty="solid")})
  output$plot7 <- renderPlot({
    barplot(Sncounts1(), main="Simulated Sn",
            xlab="Key rates", col=c("green"),  beside=TRUE,axis.lty="solid")})

  
  output$view1 <- renderTable({mtx})
  output$view2 <- renderTable({csmtx})
  output$view3 <- renderTable({mtxS()})
  output$view4 <- renderTable({csmtxS()})
}



shinyApp(ui = ui, server = server)

