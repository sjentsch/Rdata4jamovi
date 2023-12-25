# DESCRIPTION

# =================================================================================================

clnRd  <- function(pkgRd = NULL) {
    pkgRd <- pkgRd[sapply(pkgRd, tools:::.Rd_get_metadata, "keyword") == "datasets"]
    pkgRd <- pkgRd[sapply(sapply(pkgRd, tools:::.Rd_get_metadata, "format"), length) == 1]
    pkgRd
}

defYML <- function() {
    paste0("title: Rdata4jamovi\nname: R datasets for jamovi\nversion: ", packageVersion("Rdata4jamovi"), "\njms: \'1.0\'\nauthors:\n",
           "  - The R Core Team\n  - The authors of the R packages that contained the data sets (see information in each data set)\n",
           "maintainer:\n  - Sebastian Jentschke <sebastian.jentschke@uib.no>\ndate: \'", Sys.Date(), "\'\n",
           "description: >\n  This module provides data sets to be used with jamovi (e.g. in teaching). It assembles\n",
                           "  data sets that are supplied together with different R-packages, and is based upon the\n",
                           "  <a href=\"https://vincentarelbundock.github.io/Rdatasets/\" target=\"_blank\">Rdatasets</a>\n",
                           "  by Vincent Arel-Bundock. Once installed, the data files appear in a sub-folder\n",
                           "  “Rdata4jamovi” when using the menu “Open” → “Data Library” in jamovi.<br>\n",
                           "  Most of the included data sets are based upon empirical work, the authors of which are\n",
                           "  given in the “References”-section of each data set. Please cite the respective reference(s)\n",
                           "  when using these datasets.\nanalyses: []\ndatasets:")
}

detMth <- function(inpStr = "", dtaNme = "") {
    crrMth <- c()
    if (length(inpStr) < 1 || !nzchar(inpStr)) return(crrMth)
    crrExm <- strsplit(inpStr, "\n")[[1]]
    selLst <- grep("^list\\(",  crrExm)
    while (length(selLst) > 0) {
        selLst <- selLst[1] + seq(grep(")\\s*$", crrExm[seq(selLst[1], length(crrExm))])) - 1
        try(eval(parse(text = paste(c("tmpLst <- ", crrExm[selLst]), collapse = ""))), silent = TRUE)
        if (exists("tmpLst")) {
           crrExm <- c(crrExm[-selLst], unlist(tmpLst))
        } else {
           crrExm <- crrExm[-selLst]
        }
        selLst <- grep("^list\\(",  crrExm)
    }
    # filter out those lines that contain function calls ("\\w+\\(" and not empty or just comments)
    crrExm <- trimws(crrExm[grepl("\\w+\\(.*)", crrExm) & !grepl("^\\s*$|^#", crrExm)])
    # 
    crrExm <- unname(sapply(gsub(".*;\\s*", "", gsub("\\w+\\s*<-\\s*", "", gsub("#\\s+.*", "", crrExm))), function(x) strsplit(x, "\\(")[[1]][1]))
    crrExm <- crrExm[grepl("^\\w+$|^\\w+\\.\\w+", crrExm) & !grepl("^\\d+\\.\\d+", crrExm) & !grepl("\\w+\\s*=\\s*\\w+", crrExm) &
                    !grepl("^attach$|^c$|^data$|^detach$|^for$|^head$|^if$|^library$|^list$|^pause$|^par$|^print$|^require$|^while$", crrExm)]
#    selCnt <- which(grepl("^\\s+\\w+", crrExm[-1]) & grepl("\\)\\s*$", crrExm[-1]) & grepl("lm\\(", crrExm[-length(crrExm)]))
#    if (length(selCnt) > 0) {
#        for (k in selCnt) crrExm[k] <- paste(trimws(crrExm[k]), trimws(crrExm[k + 1]))
#        crrExm <- crrExm[(-selCnt - 1)]
#    }
return(crrExm)
    if (length(grep(paste0("summary\\(,", dtaNme), crrExm)) > 2 || length(grep(paste0("summary\\(", dtaNme), crrExm)) / length(crrExm) > 0.2) {
        crrMth <- c(crrMth, "Descriptives")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "Graphs")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "t-Test (Indep.)")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "t-Test (Paired)")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "UNIANOVA")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "ANOVA")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "rmANOVA")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "ANCOVA")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "MANOVA")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "Correlation")
    }
    if (length(grep(" lm\\(", crrExm)) > 2 || length(grep(" lm\\(", crrExm)) / length(crrExm) > 0.2) {
        crrMth <- c(crrMth, "Linear Regression")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "Logistic Regression")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "Factor analysis")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "Binomial Test")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "χ² Goodness of fit")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "χ² Test of association")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "McNemar Test")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "Multilevel Models")
    }
    if (FALSE) {
        crrMth <- c(crrMth, "Generalized Linear Models")
    }

    crrMth
}

fmtRef <- function(inpStr = "") {
    if (length(inpStr) < 1 || !nzchar(inpStr)) return(character(0))

    crrRef <- strsplit(inpStr, "\\n\\n\\s*\\n\\n")[[1]]
    
    crrRef <- unname(sapply(crrRef, function(x) gsub("\")\n,\n\nlist(\"", ", ", gsub("\")\n, \nlist(\"", ", ", x, fixed = TRUE), fixed = TRUE)))
#    if (any(unname(sapply(crrRef, function(x) length(gregexpr('list\\(', x)[[1]]))) != 1)) {
#        stop(sprintf("Reference could not be correctly formatted:\n%s\n", inpStr))
#    }
    crrRef <- unname(sapply(crrRef, function(x) gsub("\\s*list\\(\"(.*)\"\\)\\s*", " <em>\\1</em>", x)))
    for (k in seq_along(crrRef)) {
        # get rid of parentheses not matching up
        crrRef[k] <- paste(strsplit(paste(strsplit(crrRef[k], "\\(+")[[1]], collapse = "("), "\\)+")[[1]], collapse = ")")
        splRef <- trimws(strsplit(crrRef[k], "\\.")[[1]])
        # if there is a combination of place and publisher, it will be in the last split,
        # colons in other places shouldn't be changed
        splRef[length(splRef)] <- gsub(".*: (.*)$", "\\1", splRef[length(splRef)])
        splRef <- gsub("(\\(\\d+\\))\\s+", "\\1. ", gsub("--", "-", gsub(" and ", " & ", splRef)))
        splRef <- gsub("\\s*,\\s+(\\d+\\w+)\\s+ed", " (\\1 ed.)", gsub("\\s*,\\s+(\\d+\\w+)\\s+edition", " (\\1 ed.)", splRef))
        # add a full stop at the end of the reference and remove multiple full stops if there already was one
        crrRef[k] <- gsub("\\.+$", ".", paste0(trimws(paste(splRef, collapse = ". ")), "."))
    }
    crrRef <- unname(sapply(crrRef, function(x) gsub("\\.\\s+,", ".,", gsub("\\.\\s*\\)+\\s*$", ".", rmvWSs(x)))))
    paste(crrRef, collapse = "<br/>")
}

fmtVar <- function(inpStr = "") {
    crrVar <- eval(parse(text = gsub(".*\n\n", "", inpStr)))
    crrVar <- crrVar[sapply(crrVar, length) > 1 & !sapply(crrVar, function(x) grepl("\\s+", x[[1]]))]
    
    nmeVar <- unlist(sapply(crrVar, "[[", 1))
    dscVar <- sapply(crrVar, function(x) { x <- trimws(unlist(x[[2]])); gsub("\\.$", "", gsub(" ,", ",", gsub("--", " - ", paste0(x[nzchar(x)], collapse = " ")))) })
    dscVar <- trimws(gsub("^factor. |^numeric. |^character. |^numeric variable coding |^factor indicating |^ordered factor indicating ", "", dscVar, ignore.case = TRUE))
    dscVar <- sapply(dscVar, function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2)), USE.NAMES = FALSE)

    lstVar <- setNames(as.list(dscVar), nmeVar)
#   attr(lstVar, "df_Dsc") <- gsub("(.*)\n\n.*", "\\1", inpStr)
    
    lstVar
}

getDta <- function(pkgDir = "", dtaNme = "") {
    # check whether the file is in data
    fleNme <- list.files(file.path(pkgDir, "data"), dtaNme)
    if (length(fleNme) == 1) {
        fleExt <- tools::file_ext(fleNme[1])
        if        (fleExt %in% c("rda", "Rdata", "RData")) {
            dtaFrm <- getRda(file.path(pkgDir, "data", fleNme))
        } else if (fleExt %in% c("csv")) {
            stop(sprintf("Write CSV-reading-routine: %s.", fleNme))
        } else if (fleExt %in% c("tab")) {
            dtaFrm <- read.table(file.path(pkgDir, "data", fleNme))
        } else if (fleExt %in% c("R")) {
            dtaFrm <- getR(file.path(pkgDir, "data", fleNme))
        } else {
            stop(sprintf("File extension not implemented: %s", fleNme))
        }
    } else if (length(fleNme) == 0 && file.exists(file.path(pkgDir, "R", "sysdata.rda"))) {
        dtaFrm <- getRda(file.path(pkgDir, "R", "sysdata.rda"), dtaNme)
    } else {
        warning(sprintf("Data set %s was not found!", dtaNme))
        return(invisible(NULL))
    }
    if (any(class(dtaFrm) %in% c("matrix", "tbl_df")) && length(dim(dtaFrm)) == 2) dtaFrm <- as.data.frame(dtaFrm)
    if (any(class(dtaFrm) %in% c("character", "dist", "list", "mts", "numeric", "ts", "zoo"))) return(invisible(NULL))
    
    dtaFrm
}

getR <- function(fleNme = "") {
    tmpEnv <- new.env()
    sys.source(file.path(pkgDir, "data", "flower.R"), e = tmpEnv)
    if (length(ls(envir = tmpEnv)) == 1) {
        return(get(ls(envir = tmpEnv), envir = tmpEnv))
    } else {
        return(invisible(NULL))
    }
}

getRda <- function(fleNme = "", dtaNme = "") {
    tmpEnv <- new.env()
    load(file = fleNme, envir = tmpEnv)

    if (nzchar(dtaNme)) {
        return(get(dtaNme, envir = tmpEnv))
    } else if (length(ls(envir = tmpEnv)) == 1) {
        return(get(ls(envir = tmpEnv), envir = tmpEnv))
    } else {
        return(invisible(NULL))
    }
}

getRd  <- function(pkgNme = c()) {
    pkgURL <- paste0("https://cran.r-project.org/package=", pkgNme)
    if (!is.null(suppressWarnings(try({ tmpURL <- url(pkgURL); open.connection(tmpURL, open = "r", timeout = 5); close(tmpURL); }, silent=T)[1]))) {
        return(invisible(NULL))
    }
    pkgHTM <- readLines(pkgURL)
    pkgTAR <- gsub(".*<a href=\"(.*?)\">.*$", "\\1", pkgHTM[grepl(".tar.gz", pkgHTM)])
    pkgDst <- file.path(tempdir(), paste0(pkgNme, ".tar.gz"))
    download.file(paste0("https://cran.r-project.org/package=", pkgNme, "/", pkgTAR), destfile = pkgDst, quiet = TRUE)
    if (any(grepl("data/$", untar(pkgDst, list = TRUE)))) {
        untar(pkgDst, exdir = dirname(pkgDst))
        unlink(pkgDst)
        pkgDir <- gsub(".tar.gz", "", pkgDst)
        pkgRd  <- clnRd(tools::Rd_db(dir = pkgDir))
        pkgDsc <- readLines(file.path(pkgDir, "DESCRIPTION"))
        # attach whatever is needed to pkgRd 
        attr(pkgRd, "pkgDir") <- pkgDir
        attr(pkgRd, "pkgLic") <- sprintf(paste("This data set is included in the R-package %s (available at %s). It was originally published in the",
                                               "references shown above. The R-package (including this data set) uses the following license(s): %s."),
                                               pkgNme, pkgURL, gsub("\\|", "or", gsub("^License: ", "", pkgDsc[grepl("^License:", pkgDsc)])))
        return(pkgRd)
    } else {
        unlink(pkgDst)
        return(invisible(NULL))
    }
}

prcDta <- function(pkgRd = NULL, allDta = c()) {
    for (j in seq_along(pkgRd)) {
        dtaNme <- gsub("\\.Rd", "", names(pkgRd[j]))
        dtaOut <- file.path("data", paste0(dtaNme, ".omv"))
        # check whether there is already a data set with the same name
        if (file.exists(dtaOut)) stop(sprintf("File %s already exists (%d in %s).", dtaOut, j, pkgNme))
        dtaFrm <- getDta(pkgDir, dtaNme)
        # check whether the data set should be excluded
        if (is.null(dtaFrm)) next
        dtaTtl <- tools:::.Rd_get_metadata(pkgRd[[j]], "title")
        dtaDsc <- rmvWSs(tools:::.Rd_get_metadata(pkgRd[[j]], "description"))
        dtaVar <- fmtVar(tools:::.Rd_get_metadata(pkgRd[[j]], "format"))
        dtaRef <- fmtRef(tools:::.Rd_get_metadata(pkgRd[[j]], "references"))
        dtaMth <- detMth(tools:::.Rd_get_metadata(pkgRd[[j]], "examples"))
        if (all(names(dtaVar) == names(dtaFrm))) {
            stop(sprintf("Mismatch between data set and variable description for %s (%d in %s).", dtaNme, j, pkgNme))
        }
        # write data set
        jmvReadWrite::describe_omv(dtaInp = dtaFrm, fleOut = dtaOut, dtaTtl = paste0(dtaNme, ": ", dtaTtl),
                                   dtaDsc = list(description = dtaDsc, variables = dtaVar, references = dtaRef, license = dtaLic))
        # return entry for the data set to 0000.yaml
        c(allDta, paste0("  - name: ", dtaNme, "\n    path: ", dtaNme, ".omv\n    description: ",
                    ifelse(any(nchar(dtaDsc) < c(80, nchar(dtaTtl))), dtaDsc, dtaTtl),
                    ifelse(length(dtaMth) > 0, paste0(c("    tags:", paste0("     - ", dtaMth)), collapse = "\n"), "")))
    }
}

rmvWSs <- function(inpStr = "") {
    gsub("list\\(\"(\\w+)\"\\)", "“\\1”", gsub("\\s+", " ", inpStr))
}

do_not_run <- function() {

if (!jmvReadWrite:::hasPkg("Rdpack")) install.packages("Rdpack", dependencies = TRUE)

allPkg <- readLines("https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/DESCRIPTION")
allPkg <- trimws(gsub(",$", "", allPkg[seq(grep("^Imports:", allPkg) + 1, grep("^Suggests:", allPkg) - 1)]))

allDta <- c()

if (file.exists("data")) unlink("data", recursive = TRUE)
dir.create("data")

for (i in seq_along(allPkg)) {
    pkgNme <- allPkg[i]
    # packages in base R are not on CRAN
    if (pkgNme %in% c("datasets")) {
        pkgRd <- clnRd(tools::Rd_db(package = pkgNme))
        attr(pkgRd, "pkgDir") <- system.file(package = pkgNme)
        attr(pkgRd, "pkgLic") <- sprintf(paste("This data set is included in the R-package %s (and part of base R). It was originally",
                                               "published in the references shown above. The R-package (including this data set) uses",
                                               "the following license(s): GPL (>= 2)."), pkgNme)
    # download tar.gz from CRAN, extract it, and read the Rd-database
    } else {
        pkgRd <- getRd(pkgNme)
    }
    if (is.null(pkgRd)) next
    
    cat(sprintf("\n\n%s\n\n%s\n", paste(rep("=", 80), collapse = ""), pkgNme))
    pkgDir <- attr(pkgRd, "pkgDir")
    crrNme <- gsub("\\.Rd", "", names(pkgRd))
    if (length(crrNme) > 0) print(table(sapply(crrNme, function(x) class(suppressWarnings(getDta(pkgDir, x)))[1])))
    crrExm <- unname(unlist(sapply(sapply(pkgRd, tools:::.Rd_get_metadata, "examples"), detMth)))
    if (length(crrExm) > 0) print(sort(table(crrExm), decreasing = TRUE))

# perhaps: exclude data sets
    if (!is.null(attr(pkgRd, "pkgDir")) && grepl(tempdir(), attr(pkgRd, "pkgDir"))) unlink(attr(pkgRd, "pkgDir"), recursive = TRUE)
}

}
