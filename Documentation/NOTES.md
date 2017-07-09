# Helpful notes
There are quite a few remarks on how to use SyntaxNet output on StackOverflow:
https://stackoverflow.com/questions/37875614/how-to-use-syntaxnet-output

Steps:
1. Copy `demo.sh` script to `run.sh`, and remove last step in the pipeline.
Now SyntaxNet works as a pipeline converting sentences into tables.
Example output tables are in examples/
2. Use `run.sh` on any data to convert to CNLL format.
3. Parse CNLL files, just like examples in `examples/*.cnll`.
