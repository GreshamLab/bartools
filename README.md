# bartools

## Input format

Your files should be text flat files, then with a sample sheet. 
There's two formats we're aiming to support.
Both require a sample sheet of the format

    File_path,Sample,...

### barnone

This is your old school Barseq format. Give me a sample sheet that
points at your file and gives me the SampleNames you actually want.
It'll give you a big tibble.

### bartender

This is that bartender output. It'll handle one or multiple samples
per file. Give me a sample sheet that
points at your file and gives me the SampleNames you actually want.
It'll give you a big tibble.
