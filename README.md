# Library-s-project

Project conducted under an internship in the University of Málaga Main Library

# Remove blank elements

```
Datos.Brutos<-read.csv("Alumnosprestamo.csv", sep=";")

# Separate every line into a list

registros.l<-split(Datos.Brutos, seq(nrow(Datos.Brutos)))

# Since length is the same for every register because they're filled with blank elements, we find those empty positions.

positions.blanks.registros.l<-list()
for(i in 1:length(registros.l)){
  positions.blanks.registros.l[[i]] <- which(registros.l[[i]]=="")
}

# But some of them doesn't have any blank element, so now we have some lists with NULL. We are going to avoid them with an if stament.

lists.null.v<-vector()
for(i in 1:length(positions.blanks.registros.l)){
  lists.null.v[i]<-length(positions.blanks.registros.l[[i]]) == 0
}

positions.null.v<-as.integer(which(lists.null.v==TRUE))

positions.data.l <- list()
for(i in 1:length(positions.blanks.registros.l)){
  if(i == positions.null.v){
    positions.data.l[[i]]<- c(1:33)
  }
  else{
    positions.data.l[[i]] <- c(1:(positions.blanks.registros.l[[i]][1]-1))
  }
}

# Now that we have the every position containing data, we can finally get a list with it.

data.l <- list()
for(i in 1:length(registros.l)){
  data.l[[i]] <- registros.l[[i]] [positions.data.l[[i]]]
}

rm(Datos.Brutos, positions.blanks.registros.l, positions.data.l, registros.l)
```

# Filter simple registers

```
# Now we are going to separate the multiple registers into simplier ones, but first we are adding an id so we don't lose that info (there was an empty column that we are reusing).

for (i in 1:length(data.l)){
  data.l[[i]][1]<-i
}

# First, filter the simple registers and remove NAs

data.Rsimples.df<-data.frame(data.l[[4]], stringsAsFactors = FALSE)
for(i in 1:length(data.l)){
  if(data.l[[i]][2]==1){
  data.Rsimples.df[i,]<-data.l[[i]]
  }
  else{
    next
  }
}

data.Rsimples.sinNA.df<-data.Rsimples.df[complete.cases(data.Rsimples.df),]

colnames(data.Rsimples.sinNA.df)<-c("Identificador", "Préstamos activos", "C. Procedencia", "Prestamista", "Fecha")
data.Rsimples.sinNA.df<-data.Rsimples.sinNA.df[-1,]

blanks.v<-vector()
for (i in 1:length(data.l)){
  if (data.l[[i]][2] == 0){
    blanks.v<-c(blanks.v, i)
  } 
  if (i==6712){
    break()
  }
  else{
    next()
  }
}

data.l<-data.l[-blanks.v]

```

# Separate multiple registers

```
# Separate multiple registers

data.multiples.df<-data.frame(NA,NA,NA,NA,NA)
colnames(data.multiples.df)<-c("Identificador", "Préstamos activos", "C. Procedencia", "Prestamista", "Fecha")
cod<-0
for(i in 1:length(data.l)){
  for(j in data.l[[i]] [,2]){
    if (j==1){
      next()
    }
    else{
      repeat{
        cod<-cod+1
        nombres<-data.frame(data.l[[i]][1:5], stringsAsFactors = FALSE)
        
        nombres<-as.list(nombres)
        nombres<-data.frame(lapply(nombres, as.character), stringsAsFactors = FALSE)
        colnames(nombres)<-c("Identificador", "Pr?stamos activos", "C. Procedencia", "Prestamista", "Fecha")
        
        nombres$Prestamista<-as.character(unlist(data.l[[i]][cod + 3]))
        nombres$Fecha<-as.character(unlist(data.l[[i]][1,3 + cod + j]))
        data.multiples.df<-rbind.data.frame(data.multiples.df, nombres, stringsAsFactors = FALSE)
        if(cod==15){
          cod<-0
          break()
        }
        else{
          if(j==cod){
            cod<-0
            break()
          }
        }
      }
    }
  }
}

# Bind both df

data.multiples.df<-data.multiples.df[-1,]
data.complete.df<-rbind.data.frame(data.Rsimples.sinNA.df, data.multiples.df, stringsAsFactors = FALSE)
data.complete.df<-data.complete.df[order(as.integer(data.complete.df$Identificador)),]

```

# Rename

```
# Rename

dosletras<-substr(data.complete.df$Prestamista, 1,2)
data.complete.df$Prestamista<-dosletras

data.complete.df$Prestamista[data.complete.df$Prestamista=="ab"]<-"Arquitectura y BBAA"
data.complete.df$Prestamista[data.complete.df$Prestamista=="fl"]<-"Filosof?a y Letras"
data.complete.df$Prestamista[data.complete.df$Prestamista=="bg"]<-"Biblioteca general"
data.complete.df$Prestamista[data.complete.df$Prestamista=="ci"]<-"C.Comunicaci?n"
data.complete.df$Prestamista[data.complete.df$Prestamista=="ee"]<-"C.Salud"
data.complete.df$Prestamista[data.complete.df$Prestamista=="ei"]<-"Telecomunicaciones"
data.complete.df$Prestamista[data.complete.df$Prestamista=="em"]<-"Empresariales"
data.complete.df$Prestamista[data.complete.df$Prestamista=="ep"]<-"Educaci?n y Psicolog?a"
data.complete.df$Prestamista[data.complete.df$Prestamista=="pe"]<-"Educaci?n y Psicolog?a"
data.complete.df$Prestamista[data.complete.df$Prestamista=="et"]<-"Estudios sociales"
data.complete.df$Prestamista[data.complete.df$Prestamista=="fe"]<-"Econ?micas"
data.complete.df$Prestamista[data.complete.df$Prestamista=="fd"]<-"Derecho"
data.complete.df$Prestamista[data.complete.df$Prestamista=="fm"]<-"Medicina"
data.complete.df$Prestamista[data.complete.df$Prestamista=="ft"]<-"Industriales"
data.complete.df$Prestamista[data.complete.df$Prestamista=="tu"]<-"Turismo"
data.complete.df$Prestamista[data.complete.df$Prestamista=="fc"]<-"Ciencias"
data.complete.df$Prestamista[data.complete.df$Prestamista=="8u"]<-"CBUA"

data.complete.df$`C. Procedencia`<-as.character(data.complete.df$`C. Procedencia`)

data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="l"]<-"Arquitectura"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="k"]<-"BBAA"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="c"]<-"Filosof?a y Letras"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="f"]<-"Biblioteca general"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="1"]<-"C.Comunicaci?n"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="3"]<-"C.Salud"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="6"]<-"Telecomunicaciones"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="9"]<-"Empresariales"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="d"]<-"C.Educaci?n"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="e"]<-"Psicolog?a"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="b"]<-"Estudios sociales"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="8"]<-"Econ?micas"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="a"]<-"Derecho"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="2"]<-"Medicina"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="5"]<-"Industriales"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="7"]<-"Turismo"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="4"]<-"Ciencias"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="g"]<-"E.U. Antequera"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="h"]<-"E.U. Ronda"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="i"]<-"E.U. Diputaci?n"
data.complete.df$`C. Procedencia`[data.complete.df$`C. Procedencia`=="j"]<-"E.U. Trabajo Social"

```

# Export

```
# Export it to a .csv

write.csv(data.complete.df, file = "data.complete.df.csv")

```

