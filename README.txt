HCSV: a tool for manipulating CSV data
======================================

Note: This file was written in the future. `hcvs` is not yet complete
and doesn't work yet.

`hcsv` is a small command-line utility for manipulating CSV data. You
should probably think of it as a `cut` which knows about CSV quoting
(but only cares about fields, not bytes or characters). If you've used
`cut` before, `hcsv` should be easy!

     hcsv -f1,12 input.csv

