library(stringr)


clean<-function(txt){ #On remplace les caracteres spéciaux par des caracteres "normaux" (pour le français)
 txt<-gsub("é","e",txt)
 txt<-gsub("è","e",txt)
 txt<-gsub("ç","c",txt)
 return(txt)
}


diagramme<-function(txt){ #On cree un un diagramme sur les frequences des lettes présentent dans txt
  txt<-tolower(txt) #On travaille sur un text en miniscule
  m<-matrix(0,1,26) #On cree une matrice pour stocker le nombre d'occurence de chaque lettre
  colnames(m)<-letters 
  for(i in 1:length(letters)){# Grace a str_count() on compte le nombre d'occurence de chaque lettre dans le texte
    m[1,letters[i]]<-sum(str_count(txt,letters[i]))
  }
  m<-m/sum(m) #On divise par la somme totale pour obtenir la fréquence
  m<-as.data.frame(m[1,]) #On cree un data frame (plus facile à manipuler)
  return(m)
}


french<-readLines("livres.txt") #On recupere un fichier texte et on regarde la fréquence des lettres dans celui ci
french<-clean(french)
diagfrench<-diagramme(french)
write.table(diagfrench,"diagFrench.txt")  #On enregistre le data frame dans un fichier car long à calculer

english<-readLines("books.txt") #On fait ça en francais et en anglais !
diagenglish<-diagramme(english)
write.table(diagenglish,"diagEnglish.txt") 


#Passons aux calculs des matrices de transitions



matTransition<-function(txt){
  txtw<-tolower(txt) #On travaille sur un text en miniscule
  mat<-matrix(0,27,27,dimnames=list(c(letters,""),c(letters,""))) #On crée une matrice 27*27 pour les 26 lettres de l'alphabet et pour le caractere "autre" (utile pour savoir si une lettre commence ou finit un mot)
  
  lastletter="" #Variable last letter pour connaitre la derniere lettre visité
 
   for (i in 1:length(txt)) { #On parcours chaque ligne du texte
    
     for(j in 1:nchar(txt[i])){#On parcours chaque caractere de chaque ligne
      
       curletter=substring(txt[i],j,j) #On analyse chaque caractere de chaque ligne
     
        if (curletter %in% letters) { #Si le caractere est une lettre on peut travailler dessus
        
        mat[rownames(mat)==lastletter,colnames(mat)==curletter]=mat[rownames(mat)==lastletter,colnames(mat)==curletter]+1 #Si on reconnait le motif lastletter*curletter dans le texte on ajoute +1 dans la matrice à l'endroit correspondant
        lastletter=curletter #On update la derniere lettre visité
        
      }
      else{
        if (lastletter!="") {
          mat[rownames(mat)==lastletter,27]=mat[rownames(mat)==lastletter,27]+1 #Sinon on voit que la lettre termine le mot
          lastletter=""
        }
      }
      
    }
    curletter=""
    if (lastletter!="") { # Quand on change de ligne
      mat[rownames(mat)==lastletter,27]=mat[rownames(mat)==lastletter,27]+1
    }
    lastletter=""
  }
  mat<-sweep(mat+1,1,rowSums(mat+1),FUN="/") #On somme et divise par chaque ligne pour obtenir une matrice de transition
  return(mat)
}

english<-readLines("books.txt")
write.csv(mattransition(english), "bigramEnglish.csv",row.names=FALSE,col.names = FALSE ,quote = F) #On enregistre car tres longue a construire

french<-readLines("livres.txt") #Meme chose sur le texte francais
french<-clean(french)
write.csv(mattransition(french), "bigramFrench.csv",row.names=FALSE,col.names = FALSE ,quote = F)





