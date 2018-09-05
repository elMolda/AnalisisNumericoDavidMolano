lagrange = function(x,y,a){
  n = length(x)
  if(a < min(x) || max(x) < a) stop("No está interpolando")
  X = matrix(rep(x, times=n), n, n, byrow=T)
  mN = a - X; diag(mN) = 1
  mD = X - t(X); diag(mD) = 1
  Lnk = apply(mN, 1, prod)/apply(mD, 2, prod)
  sum(y*Lnk)
}

x = c((30+40)/2, (40+50)/2, (50+60)/2, (60+70)/2, (70+80)/2)
y = c(35, 48, 70, 40, 22)

print(lagrange(x,y, 35))
print(lagrange(x,y, 40))
print(lagrange(x,y, 45))
print(lagrange(x,y, 50))
print(lagrange(x,y, 55))

polyAjuste = poly.calc(x,y)
plot(x,y, pch=19, cex=1, col = "red", asp=1)
curve(polyAjuste,add=T)
print(polyAjuste)

polyAjuste(35)
polyAjuste(40)
polyAjuste(45)
polyAjuste(50)
polyAjuste(55)