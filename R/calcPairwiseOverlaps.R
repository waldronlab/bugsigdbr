#' Calculate pairwise overlaps between all signatures
#'
#' @param sets
#' a named list of signatures
#' @param targetset
#' NULL to test all pairwise overlaps, or the name of one element of the `sets` list.
#' If the name of one element of `sets` is provided, overlaps will be calculated only with that one signature.
#'
#' @return a `data.frame` with one row per pairwise overlap calculated, and colnames:
#'
#' name1 name2 length_set1 length_set2 length_union length_intersection overlap   jaccard
#' @export
#' @importFrom stats as.dist
#'
#' @examples
#' testlist <- list(a = 1:3, b = 3, c = 3:4)
#' (all <- calcPairwiseOverlaps(testlist))
#' calcPairwiseOverlaps(testlist, targetset = "b")
#' ##
#' ## Calculate overlaps between existing signatures with one additional signature
#' testlist <- c(testlist, d = list(4:5))
#' calcPairwiseOverlaps(testlist, targetset = "d")
#' 
#' @details 
#' The hard work in this code is is by John Blischak from the blog post on
#' efficient calculation of pairwise overlaps between list elements at
#' https://blog.jdblischak.com/posts/pairwise-overlaps/. See that page for definitions 
#' of overlap and jaccard. This function adds a few columns, an option to calculate 
#' pairwise overlaps with one list element only, and documentation.
calcPairwiseOverlaps <- function(sets, targetset = NULL) {
  # Ensure that all sets are unique character vectors
  sets_are_vectors <- vapply(sets, is.vector, logical(1))
  if (any(!sets_are_vectors)) {
    stop("Sets must be vectors")
  }
  sets_are_atomic <- vapply(sets, is.atomic, logical(1))
  if (any(!sets_are_atomic)) {
    stop("Sets must be atomic vectors, i.e. not lists")
  }
  sets <- lapply(sets, as.character)
  is_unique <- function(x)
    length(unique(x)) == length(x)
  sets_are_unique <- vapply(sets, is_unique, logical(1))
  if (any(!sets_are_unique)) {
    stop("Sets must be unique, i.e. no duplicated elements")
  }
  
  if (!is.null(targetset) &&
      !identical(length(match(targetset, names(sets))), 1L))
    stop("targetset must be NULL or the name of one of the list elements of 'sets'")
  
  n_sets <- length(sets)
  
  if (is.null(targetset)) {
    n_overlaps <- choose(n = n_sets, k = 2)
    iseq <- seq_len(n_sets - 1)
  } else{
    n_overlaps <- n_sets
    ipos <- match(targetset, names(sets))
    sets <- c(sets[ipos], sets[-ipos])
    iseq <- 1
  }
  
  set_names <- names(sets)
  
  vec_name1 <- character(length = n_overlaps)
  vec_name2 <- character(length = n_overlaps)
  vec_length_intersection <- integer(length = n_overlaps)
  vec_length_union <- integer(length = n_overlaps)
  vec_length_set1 <- integer(length = n_overlaps)
  vec_length_set2 <- integer(length = n_overlaps)
  vec_overlap <- numeric(length = n_overlaps)
  vec_jaccard <- numeric(length = n_overlaps)
  overlaps_index <- 1
  
  for (i in iseq) {
    name1 <- set_names[i]
    set1 <- sets[[i]]
    for (j in seq(i + 1, n_sets)) {
      name2 <- set_names[j]
      set2 <- sets[[j]]
      
      set_intersect <- set1[match(set2, set1, 0L)]
      set_union <-
        unique(
          c(set1, set2),
          incomparables = FALSE,
          fromLast = FALSE,
          nmax = NA
        )
      length_intersection <- length(set_intersect)
      length_union <- length(set_union)
      length_set1 <- length(set1)
      length_set2 <- length(set2)
      overlap <- length_intersection / min(length_set1, length_set2)
      jaccard <- length_intersection / length(set_union)
      
      vec_name1[overlaps_index] <- name1
      vec_name2[overlaps_index] <- name2
      vec_length_intersection[overlaps_index] <- length_intersection
      vec_length_union[overlaps_index] <- length_union
      vec_length_set1[overlaps_index] <- length_set1
      vec_length_set2[overlaps_index] <- length_set2
      vec_overlap[overlaps_index] <- overlap
      vec_jaccard[overlaps_index] <- jaccard
      
      overlaps_index <- overlaps_index + 1
    }
  }
  
  result <- data.frame(
    name1 = vec_name1,
    name2 = vec_name2,
    length_set1 = vec_length_set1,
    length_set2 = vec_length_set2,
    length_union = vec_length_union,
    length_intersection = vec_length_intersection,
    overlap = vec_overlap,
    jaccard = vec_jaccard,
    stringsAsFactors = FALSE
  )
  if (!is.null(targetset)) {
    result <- result[-nrow(result),]
  }
  return(result)
}

#' Title Create a Jaccard or overlap dissimilarity matrix from the
#' output of calcPairwiseOverlaps
#'
#' @param overlaps 
#' A data.frame output from the calcPairwiseOverlaps function
#' @param use 
#' Either "overlap" or "jaccard"
#'
#' @return a Dist object of (1 - overlap) or (1 - jaccard)
#' @export
#' @examples
#' testlist <- list(a = 1:3, b = 3, c = 3:4)
#' (all <- calcPairwiseOverlaps(testlist))
#' makeDist(all, "jaccard")
#' makeDist(all, "overlap")
makeDist <- function(overlaps, use = "jaccard"){
  if(!identical(use %in% c("jaccard", "overlap"), TRUE))
    stop("overlap must be either 'jaccard' or 'overlap'")
  names <- unique(c(overlaps$name1, overlaps$name2))
  jsim <- matrix(NA, length(names), length(names), dimnames=list(names, names))
  for (i in seq(nrow(overlaps))){
   jsim[overlaps[i, "name1"], overlaps[i, "name2"]] <- overlaps[i, use]
   jsim[overlaps[i, "name2"], overlaps[i, "name1"]] <- overlaps[i, use]
  }
  jdiff <- as.dist(1 - jsim)
  return(jdiff)
}
