#Ejemplo de información
#devtools::install_github("mattflor/chorddiag")
library(chorddiag)

m <- matrix(c(11975,  5871, 8916, 2868,10832,
              1951, 10048, 2060, 6171,345,
              8010, 16145, 8090, 8045,3346,
              1013,   990,  940, 6907,1553,
              2398,672,1238,2383,7720),
            byrow = TRUE,
            nrow = 5, ncol = 5)
Trazabilidad <- c("Saldos", "Transf.",
                  "Mov. de tarjeta", "Promos","Puntos")
dimnames(m) <- list(have = Trazabilidad,
                    prefer = Trazabilidad)
m


groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223","#ccf0e7")
chorddiag(m, groupColors = groupColors, groupnamePadding = 20)
