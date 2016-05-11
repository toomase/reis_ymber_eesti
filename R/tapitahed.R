# Funktsioon muudab täpitähed õigeteks
# Kodeeringu viga, mis tuleb sisse peamiselt veebi lehtede sisu kraapimisega
tapitahed <- function(x){
    library(stringr)
    
    y <- str_replace_all(x, "Ć¤", "ä")
    y <- str_replace_all(y, "Ćµ", "õ")
    y <- str_replace_all(y, "Ć¼", "ü")
    return(y)
}
