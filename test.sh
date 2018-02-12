echo "probSAT:";
for f in tests/*.cnf
do
  ./probSAT -caching=1 ${f} | grep 'numFlips' >OUT1;
done;
runhaskell eval.hs <OUT1

echo "probSAT_H:";
for f in tests/*.cnf
do
  ./probSAT_H -caching=1 ${f} |  grep 'numFlips' >OUT2;
done;
runhaskell eval.hs <OUT2
  
  
