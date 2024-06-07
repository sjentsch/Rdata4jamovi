# DESCRIPTION

# =================================================================================================

addAtt <- function(dtaFrm = NULL, attNme = "", attVal = NULL) {
    attr(dtaFrm, attNme) <- attVal
    dtaFrm
}

clnRd  <- function(pkgRd = NULL) {
    pkgRd <- pkgRd[grepl("^dataset", sapply(pkgRd, tools:::.Rd_get_metadata, "keyword"))]
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

fmtLic <- function(pkgNme = "", pkgURL = "", pkgDsc = c()) {
    sprintf(paste("This data set is included in the R-package %s (%s). It was originally published in the references shown above. The R-package (including",
                  "this data set) uses the following license(s): %s."), pkgNme, ifelse(pkgURL == "", "part of base R", paste("available at", pkgURL)),
                  gsub("\\|", "or", gsub("^License: ", "", pkgDsc[grepl("^License:", pkgDsc)])))
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

getDta <- function(pkgLoc = "", dtaNme = "") {
    print(unname(dtaNme))
    tmpEnv <- new.env()
    if        (file.exists(pkgLoc) && file.info(pkgLoc)[["isdir"]]) {
        crrDir <- getwd()
        setwd(pkgLoc)
        data(list = dtaNme, envir = tmpEnv)
        setwd(crrDir)
    } else if (pkgLoc %in% .packages(all.available = TRUE)) {
        data(list = dtaNme, package = pkgLoc, envir = tmpEnv)
    } else {
        warning(sprintf("Data set %s (package: %s) was not found!", dtaNme, basename(pkgLoc)))
        return(invisible(NULL))
    }
    if (dtaNme %in% ls(envir = tmpEnv)) {
        dtaFrm <- get(dtaNme, envir = tmpEnv)
    } else {
        warning(sprintf("Data set %s (package: %s) was not found!", dtaNme, basename(pkgLoc)))
        return(invisible(NULL))
    }
   
    # array, data.frame, matrix, tbl_df
    if        (any(class(dtaFrm) %in% c("array", "data.frame", "matrix", "tbl_df"))) {
        if (length(dim(dtaFrm)) == 2) {
            dtaFrm <- as.data.frame(dtaFrm)
        } else {
            if (dtaNme %in% c("iris3")) return(invisible(NULL))
            stop(sprintf("Data set %s (package: %s) is of class (%s) but has not two dimensions.", dtaNme, basename(pkgLoc), class(dtaFrm)[1]))
        }
    # character
    } else if (any(class(dtaFrm) %in% c("character"))) {
        dtaFrm <- xfmSep(dtaFrm)
        if (!is.null(dtaFrm)) {
            cat(str(dtaFrm))
        } else {
            stop(sprintf("Data set %s (package: %s) is of class (%s) but not yet defined", dtaNme, basename(pkgLoc), class(dtaFrm)[1]))
            # if (dtaNme %in% c("raw_data_research_funding_rates", "country_colors", "continent_colors", "openintro_colors")) return(invisible(NULL))
        }
    # dist
    } else if (any(class(dtaFrm) %in% c("dist"))) {
        dtaFrm <- addAtt(as.data.frame(as.matrix(dtaFrm)), "origclass", "dist")
    # integer, numeric
    } else if (any(class(dtaFrm) %in% c("integer", "numeric"))) {
        if (is.atomic(dtaFrm)) {
            return(invisible(NULL))
        } else {
            cat(str(dtaFrm))
            stop("Numeric but not yet defined")
        }
    # list, rollcall
    } else if (any(class(dtaFrm) %in% c("list", "rollcall"))) {
        if (dtaNme %in% c("pew_energy_2018")) {
            stop("Find a better way to describe pew_energy_2018")
            dtaFrm <- as.data.frame(as.matrix(dtaFrm))
        } else if (any(unlist(lapply(dtaFrm, is.character)))) {
            return(invisible(NULL))
        } else if (dtaNme %in% c("DAAGxdb", "two65", "zzDAAGxdb", "brca", "mnist_127", "mnist_27", "tissue_gene_expression", "Khan", "NCI60",
                                 "children_gender_stereo", "fish_oil_18", "nj07", "openintro_palettes", "simulated_dist", "simulated_normal",
                                 "partycodes", "s109", "sc9497", "state.info", "Peirce", "mm_randhie")) {
            return(invisible(NULL))
        } else {
            cat(str(dtaFrm))
            stop("List but not yet defined")
        }
            print(str(dtaFrm))
            stop(sprintf("Data set %s (package: %s) is of class (%s) but not yet defined", dtaNme, basename(pkgLoc), class(dtaFrm)[1]))

    # mts
    } else if (any(class(dtaFrm) %in% c("mts"))) {
        dtaFrm <- addAtt(cbind(getTme(dtaFrm), as.data.frame(dtaFrm)), "origclass", "mts")
    # table
    } else if (any(class(dtaFrm) %in% c("table"))) {
        if (dtaNme %in% c("crimtab", "orallesions", "pistonrings", "suicides", "Butterfly", "Federalist", "HorseKicks", "Hospital",
                          "Rochdale", "Saxony", "WeldonDice", "WomenQueue", "Depends", "Heckman", "HospVisits")) {
            print(str(dtaFrm))
            return(invisible(NULL))
        } else if (dtaNme %in% c("HairEyeColor", "occupationalStatus", "Titanic", "UCBAdmissions", "incidents.byCountryYr", "rearrests",
                                 "Bundestag2005", "CoalMiners", "Employment", "PreSex", "RepVict", "SexualFun", "UKSoccer", "Abortion",
                                 "Bartlett", "Caesar", "Cancer", "Detergent", "Draft1970table", "Dyke", "Gilby", "Heart", "HouseTasks",
                                 "Hoyt", "JobSat", "Mobility")) {
            print(str(dtaFrm))
            dtaFrm <- addAtt(as.data.frame(dtaFrm), "origclass", "table")
            print(str(dtaFrm))
        } else {
            cat(str(dtaFrm))
            stop("Table but not yet defined")
        }
    # ts, zoo
    } else if (any(class(dtaFrm) %in% c("ts", "zoo"))) {
        return(invisible(NULL))
    } else {
        cat(str(dtaFrm))
        stop(sprintf("Unrecognized class (%s) for data set %s (package location: %s)", class(dtaFrm)[1], dtaNme, pkgLoc))
    }

    attr(dtaFrm, "examples") <- tools:::.Rd_get_metadata(pkgRd[[names(dtaNme)]], "examples")
    return(dtaFrm)
}

#getR <- function(fleNme = "") {
#    tmpEnv <- new.env()
#    sys.source(fleNme, e = tmpEnv)
#    if (length(ls(envir = tmpEnv)) == 1) {
#        return(get(ls(envir = tmpEnv), envir = tmpEnv))
#    } else {
#        return(invisible(NULL))
#    }
#}

#getRda <- function(fleNme = "", dtaNme = "") {
#    tmpEnv <- new.env()
#    load(file = fleNme, envir = tmpEnv)

#    if (nzchar(dtaNme)) {
#        return(get(dtaNme, envir = tmpEnv))
#    } else if (length(ls(envir = tmpEnv)) == 1) {
#        return(get(ls(envir = tmpEnv), envir = tmpEnv))
#    } else {
#        return(invisible(NULL))
#    }
#}

getRd  <- function(pkgNme = c()) {
    # try to download packages from CRAN
    pkgURL <- paste0("https://cran.r-project.org/package=", pkgNme)
    if (is.null(suppressWarnings(try({ tmpURL <- url(pkgURL); open.connection(tmpURL, open = "r", timeout = 15); close(tmpURL); }, silent=T)[1]))) {
        pkgHTM <- readLines(pkgURL)
        pkgTAR <- gsub(".*<a href=\"(.*?)\">.*$", "\\1", pkgHTM[grepl(".tar.gz", pkgHTM)])
        pkgDst <- file.path(tempdir(), paste0(pkgNme, ".tar.gz"))
        download.file(paste0("https://cran.r-project.org/package=", pkgNme, "/", pkgTAR), destfile = pkgDst, quiet = TRUE)
        if (any(grepl("data/$", untar(pkgDst, list = TRUE)))) {
            untar(pkgDst, exdir = dirname(pkgDst))
            unlink(pkgDst)
            pkgDir <- gsub(".tar.gz", "", pkgDst)
            pkgRd  <- clnRd(tools::Rd_db(dir = pkgDir))
            if (length(pkgRd) > 0) {
                attr(pkgRd, "pkgLoc") <- pkgDir
                attr(pkgRd, "pkgLic") <- fmtLic(pkgNme, pkgURL, readLines(file.path(pkgDir, "DESCRIPTION")))
                return(pkgRd)
            } else {
                stop(sprintf("Could not process Rd for %s", pkgNme))
                return(invisible(NULL))
            }    
        } else {
            unlink(pkgDst)
            return(invisible(NULL))
        }
    # packages in base R are not on CRAN, use the installed version instead
    } else if (pkgNme %in% .packages(all.available = TRUE)) {
        pkgRd <- clnRd(tools::Rd_db(package = pkgNme))
        if (length(pkgRd) > 0) {
            attr(pkgRd, "pkgLoc") <- pkgNme
            attr(pkgRd, "pkgLic") <- fmtLic(pkgNme, "", "License: GPL-2 | GPL-3")
            return(pkgRd)
        } else {
            stop(sprintf("Could not process Rd for %s", pkgNme))
            return(invisible(NULL))
        }    
    # throw an error for now
    } else {
        warning(sprintf("Package %s not found.", pkgNme))
        return(invisible(NULL))
    }
}

getTme <- function(dtaFrm = NULL) {
    if        (attr(dtaFrm, "tsp")[3] == 1) {
        data.frame(period = as.integer(time(dtaFrm)))
    } else if (attr(dtaFrm, "tsp")[3] == 4) {
        data.frame(year = as.integer(time(dtaFrm)), quarter = as.integer(round(time(dtaFrm) %% 1 * 4 + 1)))
    } else if (attr(dtaFrm, "tsp")[3] == 12) {
        data.frame(year = as.integer(time(dtaFrm)),   month = as.integer(round(time(dtaFrm) %% 1 * 12 + 1)))
    } else if (attr(dtaFrm, "tsp")[3] %in% c(13, 52)) {
        data.frame(year = as.integer(time(dtaFrm)),    week = as.integer(round(time(dtaFrm) %% 1 * 52 + 1)))
    } else if (attr(dtaFrm, "tsp")[3] == 365) {
        as.data.frame(as.matrix(dtaFrm))[c(1, 2, 3)]
    } else {
        stop()
    }
}

prcDta <- function(pkgRd = NULL, allDta = c()) {
    for (i in seq_along(pkgRd)) {
        dtaNme <- setNames(unique(unlist(pkgRd[[i]][sapply(pkgRd[[i]], attr, "Rd_tag") %in% c("\\name", "\\alias")])), names(pkgRd[i]))
        for (j in seq_along(dtaNme)) {
            dtaOut <- file.path("data", paste0(dtaNme[j], ".omv"))
            # check whether there is already a data set with the same name
            if (file.exists(dtaOut)) stop(sprintf("File %s already exists (%d in %s).", dtaOut, j, pkgNme))
            dtaFrm <- getDta(attr(pkgRd, "pkgLoc"), dtaNme[j])
            # check whether the data set should be excluded
            if (is.null(dtaFrm)) next
            dtaTtl <- tools:::.Rd_get_metadata(pkgRd[[i]], "title")
            dtaDsc <- rmvWSs(tools:::.Rd_get_metadata(pkgRd[[i]], "description"))
            dtaVar <- fmtVar(tools:::.Rd_get_metadata(pkgRd[[i]], "format"))
            dtaRef <- fmtRef(tools:::.Rd_get_metadata(pkgRd[[i]], "references"))
            dtaMth <- detMth(tools:::.Rd_get_metadata(pkgRd[[i]], "examples"))
            if (!all(names(dtaFrm) %in% names(dtaVar))) {
                stop(sprintf("Mismatch between data set and variable description for %s (%d in %s).", dtaNme[j], i, pkgNme))
            }
            # write data set
            jmvReadWrite::describe_omv(dtaInp = dtaFrm, fleOut = dtaOut, dtaTtl = paste0(dtaNme[j], ": ", dtaTtl),
                                       dtaDsc = list(description = dtaDsc, variables = dtaVar[names(dtaVar) %in% names(dtaFrm)],
                                                     references = dtaRef, license = attr(pkgRd, "pkgLic")))
            # return entry for the data set to 0000.yaml
            c(allDta, paste0("  - name: ", dtaNme, "\n    path: ", dtaNme, ".omv\n    description: ",
                        ifelse(any(nchar(dtaDsc) < c(80, nchar(dtaTtl))), dtaDsc, dtaTtl),
                        ifelse(length(dtaMth) > 0, paste0(c("    tags:", paste0("     - ", dtaMth)), collapse = "\n"), "")))
        }
    }
}

rmvWSs <- function(inpStr = "") {
    gsub("list\\(\"(\\w+)\"\\)", "“\\1”", gsub("\\s+", " ", inpStr))
}

xfmSep <- function(inpChr = NULL) {
    for (S in c("\t", ";", ",")) {
        numSep <- lengths(regmatches(inpChr, gregexpr(S, inpChr)))
        if (length(setdiff(unique(numSep), 0)) == 1) {
            hdrLne <- strsplit(inpChr[numSep > 0][+1], S)[[1]]
            if (all(is.na(suppressWarnings(as.numeric(hdrLne))))) {
                crrDF <- setNames(data.frame(do.call(rbind, strsplit(inpChr[numSep > 0][-1], S, fixed = TRUE))), hdrLne)
            } else {
                crrDF <-          data.frame(do.call(rbind, strsplit(inpChr[numSep > 0],     S, fixed = TRUE)))
            }
            numClm <- unlist(lapply(crrDF, function(x) !any(is.na(suppressWarnings(as.numeric(x))))))
            crrDF[numClm]  <- lapply(crrDF[numClm],  as.numeric)
            crrDF[!numClm] <- lapply(crrDF[!numClm], as.factor)
            return(crrDF)
        }
    }
    return(invisble(NULL))
}

do_not_run <- function() {

if (!jmvReadWrite:::hasPkg("Rdpack")) install.packages("Rdpack", dependencies = TRUE)

allPkg <- readLines("https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/DESCRIPTION")
allPkg <- trimws(gsub(",$", "", allPkg[seq(grep("^Imports:", allPkg) + 1, grep("^Suggests:", allPkg) - 1)]))
allPkg <- sort(c(allPkg,                              readLines("addPkg")))
allPkg <- sort(setdiff(allPkg, trimws(gsub("#.*", "", readLines("excPkg")))))

allDta <- c()

if (file.exists("data")) unlink("data", recursive = TRUE)
dir.create("data")

for (i in seq_along(allPkg)) {
    pkgNme <- allPkg[i]
    pkgRd  <- getRd(pkgNme)
    if (is.null(pkgRd)) next
    
    cat(sprintf("\n\n%s\n\n%s\n", paste(rep("=", 80), collapse = ""), pkgNme))
    crrNme <- unlist(sapply(names(pkgRd), function(x) unique(unlist(pkgRd[[x]][sapply(pkgRd[[x]], attr, "Rd_tag") %in% c("\\name", "\\alias")]))))
    crrDta <- setNames(vector(mode = "list", length = length(crrNme)), crrNme)
    for (j in seq_along(crrNme)) {
        crrDta[[j]] <- suppressWarnings(getDta(attr(pkgRd, "pkgLoc"), crrNme[j]))
    }
    print(table(vapply(crrDta, class, character(1), USE.NAMES = FALSE)))
#   print(sort(table(unlist(sapply(lapply(crrDta[!vapply(crrDta, is.null, logical(1))], attr, "examples"), detMth))), decreasing = TRUE))
#   crrExm <- unname(unlist(sapply(sapply(pkgRd, tools:::.Rd_get_metadata, "examples"), detMth)))
#   if (length(crrExm) > 0) print(sort(table(crrExm), decreasing = TRUE))

# perhaps: exclude data sets
    if (!is.null(attr(pkgRd, "pkgLoc")) && grepl(tempdir(), attr(pkgRd, "pkgLoc"))) unlink(attr(pkgRd, "pkgLoc"), recursive = TRUE)
}

}
