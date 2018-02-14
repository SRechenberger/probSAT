PENALTY=10
RUNS=1000
FLIPS=300000

rm OUT1 OUT2

for f in $1/*.cnf
do
  ./probSAT --caching 1 --maxflips ${FLIPS} --runs ${RUNS} ${f} $2 >OUT
  status=$?
  if [ $status -eq "10" ]
  then
    echo "SAT  probSAT --maxflips ${FLIPS} --runs ${RUNS} ${f}"
    grep 'numFlips' <OUT >>OUT1
  else
    if [ $status -eq "0" ]
    then
      echo "UNK  probSAT --maxflips ${FLIPS} --runs ${RUNS} ${f}"
      echo "c numFlips : $(( RUNS * FLIPS * PENALTY ))" >>OUT1
    else
      echo "ERROR $?"
    fi;
  fi;
done;

for f in $1/*.cnf
do
  ./probSAT_H --caching 1 --maxflips ${FLIPS} --runs ${RUNS} ${f} $2 >OUT
  status=$?
  if [ $status -eq "10" ]
  then
    echo "SAT  probSAT_H --maxflips ${FLIPS} --runs ${RUNS} ${f}"
    grep 'numFlips' <OUT >>OUT2
  else
    if [ $status -eq "0" ]
    then
      echo "UNK  probSAT_H --maxflips ${FLIPS} --runs ${RUNS} ${f}"
      echo "c numFlips : $(( RUNS * FLIPS * PENALTY ))" >>OUT2
    else
      echo "ERROR $?"
    fi;
  fi;
done;

echo "probSAT:";
runhaskell eval.hs <OUT1
echo "probSAT_H:";
runhaskell eval.hs <OUT2
