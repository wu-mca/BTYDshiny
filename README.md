# BTYDshiny

This is the source code for an interactive online demo of the R packages `BTYD` and `BTYDplus`.

Currently deployed to:
* https://mplatzer.shinyapps.io/BTYD/
* https://mplatzer.shinyapps.io/BTYDplus/

## Style Guidelines

* use `<-` and not `=` as assignment operator
* use double quotes `"` rather than single quotes `'`
* use two spaces for intendation
* always a space after a comma
* spaces around all infix operators (`=`, `+`, `-`, '>', `<-`, etc.), except for `:` and `::`
* always a space before left parentheses, except in a function call; i.e. `if (foo) bar(x = 3)`
* extra spacing (i.e., more than one space in a row) is ok if it improves alignment of equal signs or assignments
* do not place spaces around code in parentheses or square brackets (unless there's a comma, in which case see above).
* an opening curly brace should never go on its own line and should always be followed by a new line
* a closing curly brace should always go on its own line, unless it's followed by `else`, closing parenthesis or a comma
* always indent the code inside curly braces
* ensure that there are no trailing white spaces at the end of the line, and that files end with a new line. you can do so by adapting the settings in RStudio `Tools > Global Options > Code > Saving`.
* it's ok to leave short statements on the same line
* prefer lapply/sapply over for-loops

```{r eval=F}
# Good Style
if (foo) do()
if (foo && bar > 0) {
  y <- bar + 2 
  message("y is ", y)
}
if (y == 0) {
  log(x)
} else {
  y + x
}
saySomething <- function(a = "a long argument", 
                         b = "another argument",
                         c = "another long argument") {
}
```
