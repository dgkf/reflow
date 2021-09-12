token <- function(x) {
  UseMethod("token")
}

token.xml_node <- function(x) {
  token(xml2::xml_name(x))
}

token.character <- function(x) {
  gsub("[^a-z]", "_", tolower(x))
}
