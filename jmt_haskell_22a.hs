pI x
  |head x=='c'=('c',read$(drop 4)x::Integer)
  |(head$(drop 5)x)=='w'=('i',read$(drop 19)x::Integer)
  |(head$(drop 5)x)=='i'=('s',0)

mP b 1 p=mod b p
mP b a p
  |even a=mod((mP b(div a 2)p)^2)p
  |odd a=mod((mP b(div(a-1)2)p)^2*b)p

uM d(m,c)(s,k)
  |s=='c'=(m,mod(k*m+c)d)
  |s=='i'=(mod(m*(mP k(d-2)d))d,c)
  |s=='s'=(-m,mod(c-m)d)

main=do 
  f<-readFile"jmt_input_22.txt"
  let t=map pI$lines f
  putStr"Part 1: "
  let(p,q)=(10007,119315717514047)
  let(m,c)=foldl(uM p)(1,0)t
  putStrLn.show$mod((2019-c)*(mP m(p-2)p))p
  putStr"Part 2: "
  let(n,d)=foldl(uM q)(1,0)t
  let m'=mP n 101741582076661 q
  let c'=mod(d*((m'-1)*(mP(m-1)(q-2)q)))q
  putStrLn.show$mod((m'*2020)+c')q
