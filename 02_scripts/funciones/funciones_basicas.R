library(extrafont)
loadfonts(device = "win")

#Función para poner los decimales con ,
coma=function(x){
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}
#Notin
`%notin%` <- Negate(`%in%`)

replicate_df <- function(d, n) {
  if ("data.table" %in% class(d)) return(d[rep(seq_len(nrow(d)), n)])
  return(d[rep(seq_len(nrow(d)), n), ])
}