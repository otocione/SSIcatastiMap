
link_html <- function(label, url) {
  n <- max(length(label), length(url))
  label <- rep_len(label, n)
  url   <- rep_len(url, n)
  
  ok <- !(is.na(url) | !nzchar(url))
  
  ext_svg <- paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' width='14' height='14' viewBox='0 0 24 24' ",
    "style='vertical-align:-2px; margin-left:4px'>",
    "<path fill='currentColor' d='M14 3h7v7h-2V6.41l-9.29 9.3-1.42-1.42 9.3-9.29H14V3Z'/>",
    "<path fill='currentColor' d='M5 5h6v2H7v10h10v-4h2v6H5V5Z'/>",
    "</svg>"
  )
  
  out <- rep("", n)
  out[ok] <- paste0(
    label[ok], " ",
    "<a href='", htmlEscape(url[ok]), "' target='_blank' rel='noopener noreferrer'>",
    ext_svg,
    "</a>"
  )
  out
}

