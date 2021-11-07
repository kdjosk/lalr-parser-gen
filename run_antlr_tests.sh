#!/bin/bash
echo "Running ANTLR4 on example code snippets"

#export CLASSPATH=".:/usr/local/lib/antlr-4.9-complete.jar:"
antlr4='java -Xmx500M -cp "/usr/local/lib/antlr-4.9-complete.jar:.:/usr/local/lib/antlr-4.9-complete.jar:" org.antlr.v4.Tool'
grun='java -Xmx500M -cp "/usr/local/lib/antlr-4.9-complete.jar:.:/usr/local/lib/antlr-4.9-complete.jar:" org.antlr.v4.gui.TestRig'

cd antlr
$antlr4 DflowParser.g4
javac DflowParser*.java

$grun DflowParser program ../snippets_test -gui 

