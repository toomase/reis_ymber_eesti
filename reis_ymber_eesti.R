library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggmap)
library(TSP)
library(leaflet)
library(purrr)
library(htmlwidgets)
library(webshot)

# Funktsioon muudab täpitähed õigeteks
# Kodeeringu viga, mis tuleb sisse peamiselt veebi lehtede sisu kraapimisega
tapitahed <- function(x){
    library(stringr)
    
    y <- str_replace_all(x, "Ć¤", "ä")
    y <- str_replace_all(y, "Ćµ", "õ")
    y <- str_replace_all(y, "Ć¼", "ü")
    return(y)
}

# Funktsioon formatib täpitähed ilma täppideta tähtedeks
# Vajalik teekondade arvutamise funktsiooniks
ilma_tapitahtedeta <- function(x){
    library(stringr)
    
    y <- str_replace_all(x, "ä", "a")
    y <- str_replace_all(y, "õ", "o")
    y <- str_replace_all(y, "ü", "u")
    return(y)
}

# Eesti linnad
linnad_url <- read_html("https://et.wikipedia.org/wiki/Eesti_linnad")

linnad_raw <- linnad_url %>%
    html_table() %>%
    .[[3]] %>%
    select(linn = Linn, maakond = Maakond) %>%
    mutate_each(funs(tapitahed), linn, maakond) %>%
    mutate(linn_propercase = linn) %>%
    mutate_each(funs(str_to_lower), linn, maakond) %>%
    mutate(maakond = str_c(maakond, "maa", sep = ""))

# Linnad koordinaatidega ja maakondadega
linnad <- linnad_raw %>%
    # ilma täpitähdeteta nimed, mis on vajalikud route() funktsioonis
    mutate(linn_ilma_tapitahtedeta = ilma_tapitahtedeta(linn),
           maakond_ilma_tapitahtedeta = ilma_tapitahtedeta(maakond),
           linn_maakond_riik = str_c(linn_ilma_tapitahtedeta,
                                     maakond_ilma_tapitahtedeta,
                                     "estonia", sep = ", ")) %>%
    select(-linn_ilma_tapitahtedeta, -maakond_ilma_tapitahtedeta) %>%
    # geocode linna nimest koordinaadid
    bind_cols(geocode(.$linn_maakond_riik))

# arvuta linnade-vaheline optimaalne linnulennuline teekond 
# traveling salesman problem 
linnad_tsp <- linnad %>%
    select(lon, lat) %>%
    dist %>%
    TSP %>%
    solve_TSP(method = "2-opt", control = list(rep = 60))

# linnad sellisesse järjekorda nagu neid tuleks optimaalse reisi korral läbida
# esimene linn on kaks korda, et ring saaks täis
linnad_jarjekorras <- linnad %>%
    .[linnad_tsp, ] %>%
    bind_rows(linnad %>%
                  .[linnad_tsp, ] %>%
                  head(1)) %>%
    mutate(jrk = row_number())

# Funktsioon, mis teisendab route(output = "all") funktsiooni tuemuse lat/lon dataframeks
# Võetud: http://s4rdd.blogspot.com/2012/12/google-maps-api-decoding-polylines-for.html
decodeLine <- function(encoded){
    require(bitops)
    
    vlen <- nchar(encoded)
    vindex <- 0
    varray <- NULL
    vlat <- 0
    vlng <- 0
    
    while(vindex < vlen){
        vb <- NULL
        vshift <- 0
        vresult <- 0
        repeat{
            if(vindex + 1 <= vlen){
                vindex <- vindex + 1
                vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
            }
            
            vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
            vshift <- vshift + 5
            if(vb < 32) break
        }
        
        dlat <- ifelse(
            bitAnd(vresult, 1)
            , -(bitShiftR(vresult, 1)+1)
            , bitShiftR(vresult, 1)
        )
        vlat <- vlat + dlat
        
        vshift <- 0
        vresult <- 0
        repeat{
            if(vindex + 1 <= vlen) {
                vindex <- vindex+1
                vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
            }
            
            vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
            vshift <- vshift + 5
            if(vb < 32) break
        }
        
        dlng <- ifelse(
            bitAnd(vresult, 1)
            , -(bitShiftR(vresult, 1)+1)
            , bitShiftR(vresult, 1)
        )
        vlng <- vlng + dlng
        
        varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
    }
    coords <- data.frame(varray)
    names(coords) <- c("lat", "lon")
    coords
}

# funktsioon arvutab kõigi linnade vahelise autosõidu teekonna
teekonnad <- function(jarjekord){
    Sys.sleep(2)
    tryCatch(
        {
            # järjekorra numbri järgi linna nimi
            x <- linnad_jarjekorras %>%
                filter(jrk == jarjekord) %>%
                .$linn_maakond_riik
            
            # x linnale järgneva linna järjekorra nr
            y_row_number <- jarjekord + 1
            
            # x linnale järgnev linn
            y <- linnad_jarjekorras %>%
                filter(jrk == y_row_number) %>%
                .$linn_maakond_riik
            
            # teekond linna x ja sellele järgneva linna y vahel
            rada <- route(from = x, to = y, structure = "route", output = "all")
            
            # teekond lat/long dataframe formaati
            decodeLine(rada$routes[[1]]$overview_polyline$points )
        }, error=function(e) NULL
    )
}

# kõiki linnu läbiv teekond data frame-na
rajad <- map_df(linnad_jarjekorras$jrk, teekonnad)

# interaktiivne kaart kõiki linnu läbivast teekonnast
kaart <- leaflet() %>% 
    addProviderTiles("MapQuestOpen.OSM") %>%
    addPolylines(rajad$lon, rajad$lat, fill = FALSE) %>%
    addCircleMarkers(linnad$lon, linnad$lat, popup = linnad$linn,
                     radius = 6, stroke = FALSE,  fillOpacity = 0.8, 
                     color = "red")

## save html to png
saveWidget(kaart, file = "reis_labi_eesti_linnade.html", selfcontained = TRUE)
webshot("reis_labi_eesti_linnade.html", file = "output/reis_labi_eesti_linnade.png",
        cliprect = "viewport", vwidth = 1250, vheight = 800)

# funktsioon koostab taeli kus on kõigi teekondade pikkused ja läbimise aeg
teekonna_lisaandmed <- function(jarjekord){
    Sys.sleep(2)
    tryCatch(
        {
            # järjekorra numbri järgi linna nimi
            x <- linnad_jarjekorras %>%
                filter(jrk == jarjekord) %>%
                .$linn_maakond_riik
            
            # x linnale järgneva linna järjekorra nr
            y_row_number <- jarjekord + 1
            
            # x linnale järgnev linn
            y <- linnad_jarjekorras %>%
                filter(jrk == y_row_number) %>%
                .$linn_maakond_riik
            
            # teekond linna x ja sellele järgneva linna y vahel
            route(from = x, to = y, structure = "route")
        }, error=function(e) NULL
    )
}

# kõiki linnu läbiv teekonna pikkus ja kuluv aeg
rajad_lisaandmed <- map_df(linnad_jarjekorras$jrk, teekonna_lisaandmed)

rajad_lisaandmed %>%
    summarise(sum(km, na.rm = TRUE))