# Notes

## Re-arch 10/28/17

Instead of processing PDFs on their own process them as part of the README processing. When scanning READMEs check to see if the link is an external url or an internal file. If its an internal file then store in the `files` table but also pass in the `title` - this should help querying semanticscholar significantly

``` racket
(if (is-external? url)
    (process-link ...)
    (process-file ...))
```
