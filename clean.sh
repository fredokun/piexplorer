# ! /bin/env sh

echo "=== Removing temp files ==="
find ./ -name "*~" -exec rm -v -f {} \; -print

#echo "=== Removing dot files ==="
#rm -v -f *.dot

#echo "=== Removing dot.pdf files ==="
#rm -v -f *.dot.pdf


echo "=== Cabal cleanup ==="
cabal clean

