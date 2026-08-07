makeFileLinter <- function(checkFile) {
    seenFiles <- new.env(parent = emptyenv())

    lintr::Linter(function(sourceExpression) {
        filePath <- sourceExpression$filename
        if (is.null(filePath) || !nzchar(filePath)) {
            return(list())
        }

        if (!is.null(seenFiles[[filePath]])) {
            return(list())
        }

        seenFiles[[filePath]] <- TRUE
        checkFile(filePath)
    })
}

.newLint <- function(
    filePath,
    lineNumber,
    columnNumber,
    message,
    linterName,
    lines
) {
    lintr::Lint(
        filename = filePath,
        line_number = lineNumber,
        column_number = columnNumber,
        type = "style",
        message = message,
        line = lines[[as.character(lineNumber)]]
    )
}

.isFunctionAssignment <- function(expr) {
    if (!is.call(expr)) {
        return(FALSE)
    }

    head <- as.character(expr[[1]])
    if (length(head) != 1L) {
        return(FALSE)
    }

    if (!(head %in% c("<-", "="))) {
        return(FALSE)
    }

    if (!is.symbol(expr[[2]]) || !is.call(expr[[3]])) {
        return(FALSE)
    }

    identical(as.character(expr[[3]][[1]]), "function")
}

.extractFunctionInfos <- function(filePath) {
    sourceExpressions <- lintr::get_source_expressions(filePath)
    expressions <- sourceExpressions$expressions

    functions <- list()
    idx <- 0L

    for (sourceExpression in expressions) {
        parsed <- tryCatch(
            parse(text = sourceExpression$content)[[1]],
            error = function(...) NULL
        )
        if (is.null(parsed) || !.isFunctionAssignment(parsed)) {
            next
        }

        lineNumber <- sourceExpression$line
        if (length(lineNumber) != 1L || is.na(lineNumber)) {
            next
        }

        columnNumber <- sourceExpression$column
        if (length(columnNumber) != 1L || is.na(columnNumber)) {
            columnNumber <- 1L
        }

        functionName <- as.character(parsed[[2]])
        if (length(functionName) != 1L || !nzchar(functionName)) {
            next
        }
        functionExpr <- parsed[[3]]
        rawFormals <- as.list(functionExpr[[2]])
        formalNames <- names(rawFormals)
        formalNames <- formalNames[!is.na(formalNames) & nzchar(formalNames)]

        complexity <- tryCatch(
            cyclocomp::cyclocomp(functionExpr),
            error = function(...) NA_integer_
        )

        idx <- idx + 1L
        functions[[idx]] <- list(
            name = functionName,
            line = lineNumber,
            column = columnNumber,
            formals = formalNames,
            complexity = complexity,
            functionExpr = functionExpr
        )
    }

    list(functions = functions, lines = sourceExpressions$lines)
}

.roxygenBlockForFunction <- function(lines, functionLine) {
    if (length(functionLine) != 1L || is.na(functionLine)) {
        return(character())
    }

    if (functionLine <= 1L) {
        return(character())
    }

    idx <- functionLine - 1L
    block <- character()

    while (idx >= 1L && grepl("^[[:space:]]*#'", lines[[as.character(idx)]])) {
        block <- c(lines[[as.character(idx)]], block)
        idx <- idx - 1L
    }

    block
}

.parseRoxygenTags <- function(roxygenBlock) {
    tags <- list(
        hasTitle = FALSE,
        hasReturn = FALSE,
        hasDetails = FALSE,
        hasKeywordsInternal = FALSE,
        hasNoRd = FALSE,
        paramNames = character()
    )

    if (!length(roxygenBlock)) {
        return(tags)
    }

    for (line in roxygenBlock) {
        content <- sub("^[[:space:]]*#'[[:space:]]?", "", line)
        trimmed <- trimws(content)

        if (grepl("^@title([[:space:]]|$)", trimmed)) {
            tags$hasTitle <- TRUE
        }

        if (grepl("^@param([[:space:]]|$)", trimmed)) {
            paramName <- sub(
                "^@param[[:space:]]+([^[:space:]]+).*$",
                "\\1",
                trimmed
            )
            tags$paramNames <- unique(c(tags$paramNames, paramName))
        }

        if (grepl("^@return([[:space:]]|$)", trimmed)) {
            tags$hasReturn <- TRUE
        }

        if (grepl("^@details([[:space:]]|$)", trimmed)) {
            tags$hasDetails <- TRUE
        }

        if (
            grepl("^@keywords([[:space:]]|$)", trimmed) &&
                grepl("(^|[[:space:]])internal($|[[:space:]])", trimmed)
        ) {
            tags$hasKeywordsInternal <- TRUE
        }

        if (grepl("^@noRd([[:space:]]|$)", trimmed)) {
            tags$hasNoRd <- TRUE
        }

        if (!grepl("^@", trimmed) && nzchar(trimmed)) {
            tags$hasTitle <- TRUE
        }
    }

    tags
}

.endsWithExplicitReturn <- function(functionExpr) {
    bodyExpr <- functionExpr[[3]]

    finalExpr <- bodyExpr
    if (is.call(bodyExpr) && identical(as.character(bodyExpr[[1]]), "{")) {
        if (length(bodyExpr) <= 1L) {
            return(FALSE)
        }
        finalExpr <- bodyExpr[[length(bodyExpr)]]
    }

    is.call(finalExpr) && identical(as.character(finalExpr[[1]]), "return")
}

roxygenRequiredLinter <- function() {
    makeFileLinter(function(filePath) {
        data <- .extractFunctionInfos(filePath)
        lines <- data$lines
        lints <- list()

        for (fun in data$functions) {
            roxygenBlock <- .roxygenBlockForFunction(lines, fun$line)
            if (!length(roxygenBlock)) {
                lints[[length(lints) + 1L]] <- .newLint(
                    filePath,
                    fun$line,
                    fun$column,
                    sprintf(
                        "Function '%s' must have a roxygen header.",
                        fun$name
                    ),
                    "roxygenRequiredLinter",
                    lines
                )
                next
            }

            tags <- .parseRoxygenTags(roxygenBlock)

            if (!tags$hasTitle) {
                lints[[length(lints) + 1L]] <- .newLint(
                    filePath,
                    fun$line,
                    fun$column,
                    sprintf(
                        "Function '%s' roxygen header must include a title line or @title.",
                        fun$name
                    ),
                    "roxygenRequiredLinter",
                    lines
                )
            }

            missingParams <- setdiff(fun$formals, tags$paramNames)
            if (length(missingParams)) {
                lints[[length(lints) + 1L]] <- .newLint(
                    filePath,
                    fun$line,
                    fun$column,
                    sprintf(
                        "Function '%s' roxygen header is missing @param for: %s.",
                        fun$name,
                        paste(missingParams, collapse = ", ")
                    ),
                    "roxygenRequiredLinter",
                    lines
                )
            }

            if (!tags$hasReturn) {
                lints[[length(lints) + 1L]] <- .newLint(
                    filePath,
                    fun$line,
                    fun$column,
                    sprintf(
                        "Function '%s' roxygen header must include @return.",
                        fun$name
                    ),
                    "roxygenRequiredLinter",
                    lines
                )
            }
        }

        lints
    })
}

roxygenDetailsForComplexLinter <- function(limit = 10L) {
    makeFileLinter(function(filePath) {
        data <- .extractFunctionInfos(filePath)
        lines <- data$lines
        lints <- list()

        for (fun in data$functions) {
            if (is.na(fun$complexity) || fun$complexity <= limit) {
                next
            }

            roxygenBlock <- .roxygenBlockForFunction(lines, fun$line)
            tags <- .parseRoxygenTags(roxygenBlock)

            if (!tags$hasDetails) {
                lints[[length(lints) + 1L]] <- .newLint(
                    filePath,
                    fun$line,
                    fun$column,
                    sprintf(
                        "Function '%s' has complexity %d and must include @details in roxygen.",
                        fun$name,
                        as.integer(fun$complexity)
                    ),
                    "roxygenDetailsForComplexLinter",
                    lines
                )
            }
        }

        lints
    })
}

internalFunctionRulesLinter <- function() {
    makeFileLinter(function(filePath) {
        data <- .extractFunctionInfos(filePath)
        lines <- data$lines
        lints <- list()

        for (fun in data$functions) {
            roxygenBlock <- .roxygenBlockForFunction(lines, fun$line)
            tags <- .parseRoxygenTags(roxygenBlock)

            internalCandidate <- startsWith(fun$name, ".") ||
                tags$hasKeywordsInternal ||
                tags$hasNoRd
            if (!internalCandidate) {
                next
            }

            if (!startsWith(fun$name, ".")) {
                lints[[length(lints) + 1L]] <- .newLint(
                    filePath,
                    fun$line,
                    fun$column,
                    sprintf(
                        "Internal function '%s' must start with '.'.",
                        fun$name
                    ),
                    "internalFunctionRulesLinter",
                    lines
                )
            }

            if (!tags$hasKeywordsInternal) {
                lints[[length(lints) + 1L]] <- .newLint(
                    filePath,
                    fun$line,
                    fun$column,
                    sprintf(
                        "Internal function '%s' must include @keywords internal.",
                        fun$name
                    ),
                    "internalFunctionRulesLinter",
                    lines
                )
            }

            if (!tags$hasNoRd) {
                lints[[length(lints) + 1L]] <- .newLint(
                    filePath,
                    fun$line,
                    fun$column,
                    sprintf(
                        "Internal function '%s' must include @noRd.",
                        fun$name
                    ),
                    "internalFunctionRulesLinter",
                    lines
                )
            }
        }

        lints
    })
}

internalFunctionReturnLinter <- function() {
    makeFileLinter(function(filePath) {
        data <- .extractFunctionInfos(filePath)
        lines <- data$lines
        lints <- list()

        for (fun in data$functions) {
            roxygenBlock <- .roxygenBlockForFunction(lines, fun$line)
            tags <- .parseRoxygenTags(roxygenBlock)

            requiresExplicitReturn <- startsWith(fun$name, ".") ||
                tags$hasKeywordsInternal ||
                tags$hasNoRd
            if (!requiresExplicitReturn) {
                next
            }

            if (!.endsWithExplicitReturn(fun$functionExpr)) {
                lints[[length(lints) + 1L]] <- .newLint(
                    filePath,
                    fun$line,
                    fun$column,
                    sprintf(
                        "Internal/noRd function '%s' should end with explicit return(...).",
                        fun$name
                    ),
                    "internalFunctionReturnLinter",
                    lines
                )
            }
        }

        lints
    })
}
