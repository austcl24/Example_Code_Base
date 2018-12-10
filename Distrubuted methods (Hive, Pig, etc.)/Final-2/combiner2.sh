cat part-* > output2a.txt
sort --version-sort < output2a.txt > final2.txt
head final2.txt -n1 > output2.txt
cat output2.txt
