# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(reshape2)
library(dplyr)



#On recupere nos données calculés dans Frequency.R
diagEnglish<-read.table("diagEnglish.txt")
diagFrench<-read.table("diagFrench.txt")

mattransEnglish<-read.csv("bigramEnglish.csv")
mattransFrench<-read.csv("bigramFrench.csv")

mattransEnglish<-as.matrix(mattransEnglish)
colnames(mattransEnglish)<-c(letters,"")
rownames(mattransEnglish)<-c(letters,"")

mattransFrench<-as.matrix(mattransFrench)
colnames(mattransFrench)<-c(letters,"")
rownames(mattransFrench)<-c(letters,"")


clean<-function(txt){ #On remplace les caracteres spéciaux par des caracteres "normaux" (pour le français)
    txt<-gsub("é","e",txt)
    txt<-gsub("è","e",txt)
    txt<-gsub("ç","c",txt)
    return(txt)
}

decrypt <-function(txt,permu) { #Decrypte un text a partir d'une permutationn  de l'alphabet
    decoded<-txt
    for (i in 1:nchar(txt)) {
        if (substring(txt,i,i) %in% letters) {
            substring(decoded,i,i)=letters[permu==substring(txt,i,i)] #On remplace la lettre par la nouvelle lettre
        }
    }
    return(decoded)
}

score<-function(txt,M){ #On calcul le score du texte en fonction de la matrice de transition M
    score<-0
    lastletter<-""
    for (i in 1:nchar(txt)) { #On parcout tout le txt et on calcul la probabilites des paires de lettre dans le texte
        curletter<-substring(txt,i,i)
        if (curletter %in% letters) {
            score<-score+log(M[rownames(M)==lastletter,colnames(M)==curletter]) #On passe au log car plus simple pour les calculs (on passera a l'exp dans l'algo de Metropolis)
            lastletter<-curletter
        } 
        else {
            if (lastletter!="") { #Cas de fin de mot
                score<-score+log(M[rownames(M)==lastletter,27])
                lastletter<-""
            }
        }
    }
    
    if (lastletter!="") { #Cas de fin de mot
        score<-score+log(M[rownames(M)==lastletter,27])
        lastletter<-""
    }
    return(score)
}


MCMC<-function(txt,M){
    txt<-tolower(txt)
    mapping=sample(letters) # initialize a random mapping
    iters<-4000
    decode_cur<-decrypt(txt,mapping)
    score_cur<-score(decode_cur,M)
    
    for(i in 1:iters) {
        
        proposal=sample(1:26,2)
        mapping_prop<-mapping
        mapping_prop[proposal[1]]=mapping[proposal[2]]
        mapping_prop[proposal[2]]=mapping[proposal[1]]
        
        decode_prop<-decrypt(txt,mapping_prop)
        score_prop<-score(decode_prop,M)
        
        if(score_prop>score_cur){
            mapping<-mapping_prop
            decode_cur<-decode_prop
            score_cur<-score_prop
        }
        else{
            if(runif(1)<exp(score_prop-score_cur)){
                mapping<-mapping_prop
                decode_cur<-decode_prop
                score_cur<-score_prop
            }
        }
    }
    return(decode_cur)
}





# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),     #Simple mise en page
                navbarPage(
                    tags$h4("Déchiffre un Texte"),
                    tabPanel(tags$h4("Textes"),
                             sidebarPanel(radioButtons("langue", tags$h3("Choix de la Langue"),
                                 c("Français"="f", "Anglais"="a"))
                             ), # sidebarPanel
                             mainPanel(
                                 textInput("txt", tags$h3("Texte à déchiffrer "), ""),
                                 h3("Résultat :"),
                                 verbatimTextOutput("txtout"),
                             ) # mainPanel
                    ),
                    tabPanel(tags$h4("Graphiques"), 
                             mainPanel(fluidRow(
                                 splitLayout(cellWidths = c("78%", "78%"), plotOutput("histPlot"), plotOutput("bigramPlot"))))
                             ),
                    tabPanel(tags$h4("Méthodes"),"A faire")
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
    
    diagChoose<-reactive(switch(input$langue,"f"=diagFrench,"a"=diagEnglish)) #Pour afficher le bon graphique et les bonnes options
    
    matChoose<-reactive(switch(input$langue,"f"=mattransFrench,"a"=mattransEnglish)) #Pour afficher la bon heatmap de la matrice
    
    titlePlot<-reactive(switch(input$langue,"f"="Fréquences des lettres en Français","a"="Letters frequency in English"))
    
    titlePlot2<-reactive(switch(input$langue,"f"= labs(x="Probabilités de la seconde lettre",y="Conditionnement sur la première lettre",fill="Prob"),"a"=labs(x="Probability of Second Letter",y="Conditioning on First Letter",fill="Prob")))
    
    output$txtout <- renderText({
        MCMC(input$txt,matChoose())
    })
    
    output$histPlot<-renderPlot({
        ggplot(diagChoose(), aes(x=reorder(letters,-diagChoose()[,1]), y=diagChoose()[,1])) + geom_bar(stat = "identity")+ggtitle(titlePlot()) +xlab("") + ylab("")+ theme_bw() #Diagrammes des fréquences des lettres
    })
    output$bigramPlot<-renderPlot({ #Heatmap de la matrice de transition
        ggplot(melt(matChoose()),aes(Var2,Var1))+geom_tile(aes(fill=value))+
            scale_fill_gradient(low="white",high="black",limits=c(0,1))+
            titlePlot2()+
            scale_y_discrete(limits = rev(levels(melt(matChoose())$Var1)))+
            coord_equal()
    })
    
    
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)