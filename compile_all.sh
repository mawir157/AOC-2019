#!/bin/bash
for i in $(seq -f "%02g" 1 25)
do
  if test -f jmt_haskell_$i.hs;
  then
    if [[ [] =~ $i ]]
    then
      ghc jmt_haskell_$i.hs -O2 -XBangPatterns
    else
      ghc jmt_haskell_$i.hs -O2
    fi
  fi
done

for i in $(seq -f "%02g" 1 25)
do
  echo "Day $i" 
  if test -f jmt_haskell_$i;
  then
    ./jmt_haskell_$i
    rm jmt_haskell_$i.hi
    rm jmt_haskell_$i.o
    rm jmt_haskell_$i
  else
    echo "Does not exist"
  fi
done
