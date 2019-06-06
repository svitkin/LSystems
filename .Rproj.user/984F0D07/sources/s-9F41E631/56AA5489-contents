library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

library(grid)


forward <- function(len, angle) {
  grid.lines(x = c(0.5, 0.5), 
             y = c(0, len*sin((angle*pi)/180)))
  pushViewport(
    viewport(x = 0.5,
             y = len*sin((angle*pi)/180),
             clip = 'off', 
             just = c('centre', 'bottom')),
    recording = FALSE
  )
  invisible()
}

move_forward <- function(len, angle) {
  pushViewport(
    viewport(x = 0.5, 
             y = len*sin((angle*pi)/180), 
             clip = 'off', 
             just = c('centre', 'bottom')),
    recording = FALSE
  )
}

rotate_right <- function(angle) {
  pushViewport(
    viewport(angle =  angle, 
             clip = 'off', 
             x = 0.5, 
             y = 0, 
             just = c('centre', 'bottom')),
    recording = FALSE
  )
}

rotate_left <- function(angle) {
  pushViewport(
    viewport(angle = -angle, 
             clip = 'off', 
             x = 0.5, 
             y = 0, 
             just = c('centre', 'bottom')),
    recording = FALSE
  )
}


expand_system <- function(axiom, rules, depth) {
  final_string <- flatten_chr(str_split(axiom, ""))
  clean_rules <-
    map(rules, ~flatten_chr(str_split(.x, "")))
  
  for (step in 1:depth) {
    final_string <-
      map(final_string, ~clean_rules[[.x]] %||% .x) %>% 
      flatten_chr()
  }
  final_string
}

#axiom  <- "FX"

#rules <- list(
#  'X' = 'X+YF+',
#  'Y' = '-FX-Y'
#)

#string <- expand_system(axiom, rules, depth = 10)
render_LSystem_string <- function(L_string, len, angle) {
  grid.newpage()
  pushViewport(viewport(x=0.5, y=0.35, 
                        just = c('centre', 'bottom'), 
                        angle = angle))
  
  for (move in L_string) {
    # message(move)
    #    browser()
    switch(
      move,
      'F' = forward(len, angle),
      'G' = forward(len, angle),
      'f' = move_forward(len, angle),
      '+' = rotate_left(angle),
      '-' = rotate_right(angle),
      'X' = invisible(),
      'Y' = invisible()
    )
    #grid.rect(gp=gpar(lty="dashed"))
  }
}

#render_LSystem_string(string, 0.02, 90)
