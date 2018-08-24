@echo off
"C:\Program Files\swipl\bin\swipl.exe" -l swrl2shacl.pl -g swrl2shacl('%1',[%2]) -g halt
