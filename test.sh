echo "probSAT:";
for f in tests/*.cnf
do
  ./probSAT ${f} | grep numFlips;
done;

echo "probSAT_H:";
for f in tests/*.cnf
do
  ./probSAT_H ${f} | grep numFlips;
done;
  
  
