find ./**/*.java -type f -print0 | xargs -0 -I % sh -c '
    mv "%" temp
    (echo "class A {\n" ; cat temp ; echo "\n}") > "%"
'
rm temp

