main=interact$show.count.f.map read.lines
count=length.filter id
f x@(_:(y@(_:z)))=g$zipWith3(((+).).(+))x y z
g x@(_:y)=zipWith(<)x y
