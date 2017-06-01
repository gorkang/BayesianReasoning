
### TODO ################

# PPV in %
# Automagically save png plot

# BUG: Something odd in last two PPV (1 and 0.95)

### IDEALMENTE ###

#Preparar sistema para q se pueda elegir arbitrariamente q se mete en cada eje, y q se plotea
#Pej: PPV en Y, FP en X, y distintos niveles de PRevalencia en lineas diferentes

########################


# FP = 1-Specificity
# Prevalence = Prevalence_x/Prevalence_y
# 
# PPV = (Sensitivity * Prevalence) / ((Sensitivity * Prevalence) + (1-Prevalence)*(1-Specificity) )


#Variable definitions
Prevalence_x = 1
Prevalence_y = 1000
Sensitivity = 1 # Tasa de aciertos
# Specificity = .99 # 1-False Positives


#FP Loop
  #In %
Min_FP = 0.01
Max_FP = 1
Number_of_Steps_FP = 20
Step_size_FP = (Max_FP-Min_FP)/Number_of_Steps_FP


#PPV Loop
Min_PPV = 0.7
Max_PPV = 1
Number_of_Steps_PPV = 6
Step_size_PPV = (Max_PPV-Min_PPV)/Number_of_Steps_PPV

PPV_Sequence = seq(Min_PPV, Max_PPV, Step_size_PPV)
# PPV_Sequence = seq(0.65, 0.95, 0.1)


# png(paste0("PPV", PPV_Umbral*100, " - Sensitivity", Sensitivity*100, ".png"),
#     width     = 1600,
#     height    = 1200,
#     units     = "px")

#Establece margenes en Plot (6 para que quepa la etiqueta 1 out of ...)
op <- par(mar = c(4,6,2,2) + 0.1)  


for (r in 1:length(PPV_Sequence)) {

#Para mostrar en la grafica cual es la prevalencia en la que el PPV = r
PPV_Umbral = PPV_Sequence[r]
  # PPV_Umbral = 0.9


Graph_FP_DF <- data.frame(Prevalence = integer(),
                       FP = integer(),
                       Prevalence_y_Umbral = integer(),
                       PPV = double())


#Calcula PPV para cada una de las prevalencias 1 de i, hasta y=Prevalence_y
for (j in seq(Min_FP, Max_FP, Step_size_FP)) {
 
    #Creamos data.frame
    Graph_DF <- data.frame(Prevalence = integer(),
                     Prevalence_y = integer(),
                     PPV = double())
    
    FP = j/100
    #Calcula PPV para cada una de las prevalencias 1 de i, hasta y=Prevalence_y
    for (i in seq(1, Prevalence_y, 10)) {
      
      Prevalence = Prevalence_x/i
      
      PPV = (Sensitivity * Prevalence) / ((Sensitivity * Prevalence) + (1-Prevalence)*(FP) )
      
      Graph_DF = rbind(Graph_DF, data.frame(Prevalence_y = i, Prevalence = Prevalence, PPV = PPV))
      
    }
    
    
    Prevalence_y_Umbral = tryCatch({
        with(Graph_DF, approx(PPV, Prevalence_y, xout=c(PPV_Umbral)))
    }, error = function(e) {
        list(x = PPV_Umbral, y = 0) #If error, there is no Prevalence where PPV = PPV_Umbral
    })
    
    # if(is.na(Prevalence_y_Umbral$y)){ Prevalence_y_Umbral$y = Prevalence_y }
        
        # paste("When Prevalence is 1 out of", Prevalence_y, "- PPV is", round(Graph_DF$PPV[Prevalence_y]*100,2) ,"%")
        # paste("When Prevalence is 1 out of", round(Prevalence_Umbral$y), "- PPV is", PPV_Umbral*100 ,"%")
    
        # plot(Graph_DF$PPV, type = "l", xlab="Prevalencia 1 de y")
        #   abline(h = PPV_Umbral, v = Prevalence_y_Umbral$y, col = "gray60")
        # Prevalence_Umbral = 
        # PPV
        Graph_FP_DF = rbind(Graph_FP_DF, data.frame(FP = FP, Prevalence = Prevalence, Prevalence_y_Umbral = round(Prevalence_y_Umbral$y), PPV = PPV))
        
        cat('Loop', j, 'of 100 \n')
        
}

Graph_FP_DF = Graph_FP_DF[complete.cases(Graph_FP_DF), ]

# Graph_FP_DF = rbind(Graph_FP_DF, data.frame(FP = NA, Prevalence = NA, Prevalence_y_Umbral = 1000, PPV = NA))


# png(paste0("PPV", PPV_Umbral*100, " - Sensitivity", Sensitivity*100, ".png"),
#     width     = 1600,
#     height    = 1200,
#     units     = "px")
# 
#     op <- par(mar = c(4,6,2,2) + 0.1)  

Graph_Colors = c("darkgreen", "green", "blue", "orange", "red", "grey", "black")
if(r == 1) {
        plot(Graph_FP_DF$Prevalence_y_Umbral, type = "l",  ylim=c(Prevalence_x,Prevalence_y), xlab="FP (%)", ylab="", xaxt="n", yaxt="n", main=paste("Prevalence required for certain PPV ; Sensitivity ", Sensitivity*100, "%"), col = Graph_Colors[r])
        axis(1, at=seq(1, Number_of_Steps_FP+1), labels=format(round(seq(Min_FP, Max_FP, Step_size_FP), 2), nsmall = 1), col.axis="grey", las=2)        
        axis(2, at=seq(Prevalence_x, Prevalence_y, 25), labels=paste( "1 out of ", seq(Prevalence_x, Prevalence_y, 25)), col.axis="grey", las=2)

} else {

          lines(Graph_FP_DF$Prevalence_y_Umbral, type = "l",  ylim=c(Prevalence_x, Prevalence_y), xlab="FP (%)", ylab="", xaxt="n", yaxt="n", main=paste("PPV ", PPV_Umbral*100, "%, Sensitivity ", Sensitivity*100, "%"), col=Graph_Colors[r], lty=r)  

}
#     par(op) ## reset      
# 
# dev.off()

legend("topleft", legend=paste("PPV:", PPV_Sequence), col=Graph_Colors[1:length(PPV_Sequence)], lty=1:r, cex=0.8, box.lty=0)

}

format(round(Graph_FP_DF$FP[seq(1, length(Graph_FP_DF$FP), 1)], 4)*100, nsmall = 2)
par(op) ## reset      

# dev.off()
