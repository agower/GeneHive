#' @import XML
#' @importFrom utils download.file

#' @export
#' @rdname importPublication
#' @name importPublication
#' @title Create a Publication record using a PubMed identifier
#' @description
#' This function adds or updates a single Publication record using metadata
#' associated with a PubMed identifier.
#' @param PMID
#' A numeric or character vector of length one specifying a PubMed identifier
#' @param .permissions
#' A \code{\linkS4class{hivePermissions}} object specifying the permissions to
#' be used when creating or updating the record
#' @param con
#' A \code{\linkS4class{hiveConnection}} object;
#' if not provided, a new connection will be established
#' @param verbose
#' A logical value specifying whether messages should be printed
#' @return
#' If the operation is successful, a \code{\linkS4class{hiveEntity}}
#' object corresponding to the Publication record is invisibly returned;
#' otherwise, the function terminates with an error message.
#' @details
#' The fields of the Publication record are populated from the PubMed XML
#' record. If a DOI is present, it is added; otherwise, that field is left
#' blank.
#' @author Adam C. Gower \email{agower@@bu.edu}

importPublication <- function (
  PMID, 
  .permissions=getOption("GeneHive.permissions"),
  con=hiveConnection(), verbose=getOption("GeneHive.verbose")
)
{
  # Check arguments for errors
  if (missing(PMID)) stop("Argument 'PMID' is required")
  if (!((is.numeric(PMID) || is.character(PMID)) && length(PMID) == 1)) {
    stop("Argument 'PMID' must be a numeric or character vector of length 1")
  }
  if (!is(.permissions, "hivePermissions")) {
    stop("Argument '.permissions' must be a hivePermissions object")
  }
  if (!is(con, "hiveConnection")) {
    stop("Argument 'con' must be a hiveConnection object")
  }
  if (!(is.logical(verbose) && length(verbose) == 1)) {
    stop("Argument 'verbose' must be a logical vector of length 1")
  }

  # Base URL for all EFetch queries
  efetch.url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/",
    "efetch.fcgi?db=pubmed&retmode=xml"
  )
  
  # Retrieve and parse XML object from NCBI
  xml.tempfile <- tempfile()
  # As the default methods (*nix: "libcurl"; Windows: "wininet") support the
  # retrieval of data from https:// URLs, download.file() uses the default
  # setting (method="auto")
  suppressWarnings(
    download.file(
      url=paste0(efetch.url, "&id=", PMID), destfile=xml.tempfile, quiet=TRUE
    )
  )
  result <- suppressWarnings(
    try(xmlToList(readLines(xml.tempfile)), silent=TRUE)
  )
  # If the result was not a list, terminate with an appropriate error message;
  # otherwise, proceed
  if (!is.list(result)) {
    if (inherits(result, "try-error")) {
      stop(paste(sQuote(PMID), "is not a valid PMID"))
    } else {
      stop(paste("PMID", sQuote(PMID), "was not found"))
    }
  } else {
    # Extract PubMed citation information to argument list
    # Note: for some reason, R CMD check throws a NOTE when following is used:
    #       with(result$PubmedArticle$MedlineCitation$Article, ...)
    #       Consequently, the "Article" sublist is stored in an explicitly
    #       named variable.
    #       Giving up on trying to figure this out for now, 2017-10-18 ACG
    Article <- result$PubmedArticle$MedlineCitation$Article
    arglist <- list(
      # Note: If the article title has embedded HTML tags, it will be split
      #       into a list of character vectors, with spaces flanking the tag;
      #       this workaround collapses it back into a single string
      title=paste(Article$ArticleTitle, collapse=""),
      # Note: The following line is a hack, designed to work for the two
      #       possible cases:
      #       1. If there are multiple sections of abstract, each
      #          'AbstractText' element is a list, the first element of which is
      #          named 'text' and holds the text for that section.
      #       2. Otherwise, each 'AbstractText' element is a vector of length 1.
      abstract=paste(
        sapply(
          Article$Abstract[names(Article$Abstract) == "AbstractText"], "[[", 1
        ),
        collapse=" "
      ),
      authors=paste(
        sapply(
          Article$AuthorList[names(Article$AuthorList) == "Author"],
          "[[", "LastName"
        ),
        sapply(Article$AuthorList[names(Article$AuthorList) == "Author"],
          "[[", "Initials"
        )
      ),
      journal=Article$Journal$Title,
      year=Article$Journal$JournalIssue$PubDate$Year,
      PMID=PMID
    )
    # If a DOI exists, add that to the argument list as well
    article.idlist <- result$PubmedArticle$PubmedData$ArticleIdList
    i <- which(sapply(article.idlist, "[[", ".attrs") == "doi")
    if (length(i)) arglist$DOI <- article.idlist[[i]]$text
    # Add Publication record to hive
    do.call(
      addEntity,
      args=c(arglist, .class="Publication", con=con, verbose=verbose)
    )
  }
}
