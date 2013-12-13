This is the course project of COMS-W4115 Programming Language and Translator at Columbia.

It is contributed by Tong Ge, Jingsi Li and Shuo Yang.

We designed a music language called Melody, and used the OCaml to build a translator for it.

To compile the scanner.mll, ast.ml, parser.mly, compile.ml and melody.ml, simply run "make -f Makefile" in the Unix-like system.

To test the AST or COMPILE, please follow this format: "melody -a \<testing_file_name\> testing_result_file_name" for AST, and "melody -c \<testing_file_name\> testing_result_file_name" for COMPILE.

(For contributors: If you find some intermediate files generated (such as *.cmi, *.cmo), please add them to the .gitignore file such that they will not be pushed into the online repository.)
