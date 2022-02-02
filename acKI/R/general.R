#' Converts a data.frame to a string
#'
#' easy printable using cat()
#' @export
data2string = function(data){
  out = NULL
  if(!(is.null(data))){
  out = paste(sapply(1:length(data),
               function(x){
                 return(
                   paste(names(data[x]), "=",
                         paste(unique(data[,x]), collapse = ", "),
                         collapse = ", " ))}),
        collapse = ";\n")}
  out
}
