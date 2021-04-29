
#' @importFrom utils capture.output
#' @importFrom methods allNames
NULL

burgled_path <- "R/burgled.R"

# a less strict verion of utils::isS3stdGeneric
is_s3_std_generic <- function(fun) {
  "UseMethod" %in% all.names(suppressWarnings(body(fun)))
}

burgle1 <- function(
  fun,
  pkg,
  export = FALSE,
  first = TRUE,
  new_name = fun) {

  own_deps <- desc::desc_get_deps()
  own_deps <- own_deps[own_deps$type %in% c("Imports","Depends"), "package"]
  export <- if(export) "\n#' @export" else ""

  ## do we already import this package ?
  if(pkg %in% own_deps) {
    ## do nothing and exit
    return(invisible(NULL))
  }


  burgled_env <- new.env()
  source(file = burgled_path, burgled_env, keep.source = FALSE)
  burgled_nms <- names(burgled_env)
  fun_env <- asNamespace(pkg)
  fun_val <- get(fun, fun_env)



  ## have we already burgled a function of this name ?
  if(fun %in% burgled_nms) {
    ## are these functions the same ?
    if(
      suppressWarnings(
        identical(
          `attributes<-`(body(burgled_env[[fun]]), NULL),
          `attributes<-`(body(fun_val), NULL),
          ignore.bytecode = TRUE,
          ignore.environment = TRUE) &&
        identical(
          #  can't compare arg values because it might comparse quoted character with character
          names(args(burgled_env[[fun]])),
          names(args(fun_val)),
          ignore.bytecode = TRUE,
          ignore.environment = TRUE))) {
      ## do nothing and exit
      return(invisible(NULL))
    } else {
      ## signal conflict and fail
      if(identical(
        #  can't compare arg values because it might comparse quoted character with character
        names(args(burgled_env[[fun]])),
        names(args(fun_val)),
        ignore.bytecode = TRUE,
        ignore.environment = TRUE)) {
        warning(sprintf(
          paste(
            "conflict! We already have copied a function `%s` with different code,",
            "since they have the same arguments we keep the first found and hope",
            "they do the same thing"), fun), call. = FALSE)
        return(invisible(NULL))
      } else {
        stop(sprintf(
          "conflict! We already have copied a function `%s` with different code and argments",
          fun))
      }
    }
  }

  ## display currently copied function
  cat(crayon::cyan(sprintf("%s:::%s\n", pkg, fun)))

  ## is it the first call (on the target function)
  if(first) {
    ## build a title
    title <- c(
      paste0("#", strrep("~", 79)),
      sprintf("# %s (copied from %s:::%s)\n", new_name, pkg, fun)
    )
  } else {
    ## leave title empty
    title <- ""
  }

  header <- sprintf("# from %s %s%s", pkg, getNamespaceVersion(pkg), export)

  if(is.environment(fun_val)) {
    warning(sprintf(
      "The object `%s` in `%s` is an environment, the copy might not be robust",
      fun, pkg), call. = FALSE)
    code <- c(
      "`parent.env<-`(as.environment(",
      deparse(as.list(fun_val)),
      "), .GlobalEnv)")
  } else {
    code <- deparse(fun_val)
  }

  code <- sprintf("`%s` <- %s", new_name, paste(code, collapse = "\n"))


  lines <- c(title, header, code)
  lines <- styler::style_text(lines)

  test <- try(cat(lines, sep = "\n", file = burgled_path, append = TRUE), silent = TRUE)
  if(inherits(test, "try-error")) {
    test <- try(cat(lines, "\n", sep = "\n", file = burgled_path, append = TRUE))
    if(inherits(test, "try-error")) {
      Sys.sleep(.5)
      stop("Issue writing to file")
    }
  }


  if(!is.function(fun_val)) {
    return(invisible(NULL))
  }
  ## now let's recurse
  # we use as.list.function because as.list doesn't always dispatch right
  nms <- unlist(lapply(as.list.function(fun_val), all.names))

  low_level_calls <- intersect(nms,  c(".Call", ".Fortran", ".C"))
  if(length(low_level_calls)) {
    stop(sprintf("Calls to `%s` are not supported by {bruglr}", low_level_calls))
  }

  ## looping on all objects found in function
  for (nm in nms) {
    # print(nm)
    ## if the object can be found (not defined in the function)
    if (exists(nm, fun_env)) {
      #if(nm == "context") browser()
      ## fetch object and its environment
      obj <- get(nm, fun_env, inherits = TRUE)

      if(!is.function(obj)) {
        env <- pryr::where(nm, env = fun_env)
      } else {
        env <- environment(obj)
        ## is it a primitive ?
        if(is.null(env)) {
          ## ignore and move on to next object
          next
        }
      }

      pkg_i <- sub("<environment: namespace:(.*?)>", "\\1", capture.output(env)[[1]])
      ## did we NOT detect a package ?
      if(startsWith(pkg_i, "<")) {
        ## is the parent a package ?
        p_env <- parent.env(env)
        pkg_i <- sub("<environment: namespace:(.*?)>", "\\1", capture.output(p_env)[[1]])

        if(startsWith(pkg_i, "<")) {
          stop(sprintf("can't make sense of environment of `%s`", nm))
        }
        warning(sprintf("`%s` was found in a child of {%s}'s namespace", nm, pkg_i), call. = FALSE)
      }
      if(pkg_i %in% c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats")) {
        next
      }
      burgle1(nm, pkg_i, first = FALSE)
    }
  }

  # if fun is S3 generic, steal the methods too
  if(is_s3_std_generic(fun_val)) {
    pkg_funs <- ls(fun_env, all.names = TRUE)
    methods <- pkg_funs[startsWith(pkg_funs, paste0(fun, "."))]
    for (nm in methods) {
      burgle1(nm, pkg, export = TRUE, first = FALSE)
    }
  }
}


#' Copy functions from other packages
#'
#' `burgle()` provides a way to copy the code of another package's function
#' along with its dependencies.
#'
#' @param ... functions, must be given in the `pkg::fun` or `pkg:::fun` format
#'
#' @details
#'
#' The file `"R/burgled.R"` is created or updated with the definition of the
#' copied objects.
#'
#' When copying S3 generics all S3 methods will be copied as well.
#'
#' The copy of functions calling C, C++ or Fortran, in their own body or through their
#' unimported dependencies is not supported.
#'
#' The README shows examples.
#'
#' @return returns `invisible(NULL)` (called for side effects)
#' @export
burgle <- function(...) {
  if(!file.exists(burgled_path)) {
    writeLines("# generated by {burglr}\n", burgled_path)
  }

  bkp = readLines(burgled_path)
  dots <- eval(substitute(alist(...)))
  nms <- allNames(dots)
  test <- tryCatch(
  for (i in seq_along(dots)) {
    arg <- dots[[i]]
    if(!deparse(arg[[1]]) %in% c("::", ":::")) {
      stop("Arguments must all be of the form `pkg::fun` or `pkg:::fun`")
    }
    ## assert that input is legit
    eval.parent(arg)
    pkg <- as.character(arg[[2]])
    fun <- as.character(arg[[3]])
    new_name <- nms[[i]]
    if(new_name == "") new_name <- fun
    cat(crayon::cyan(sprintf("Copying %s:::%s and its dependencies\n", pkg, fun)))
    burgle1(fun, pkg, new_name = new_name)
  },
  error = function(e) {
    writeLines(bkp, burgled_path)
    stop(e, call. = FALSE)
  })
  message("Please consider giving credit to the authors by adding them as contributors in your package's DESCRIPTION file!")
}
