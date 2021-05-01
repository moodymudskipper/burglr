
get_license <- function(pkg){

  license = packageDescription(pkg, fields="License")
  authors = packageDescription(pkg, fields = "Author")

  list(license = license, authors = authors)
}

license_message <- function(pkg){
  license = get_license(pkg)

  cat(crayon::cyan("\n", pkg,
                   " is licensed: ",
                   license$license,
                   "\n and is by:\n ",
                   license$authors,"\n"))


}
