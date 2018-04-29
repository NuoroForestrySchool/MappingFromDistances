ComputeP3coords <- function(x1,y1,x2,y2,d1,d2,s) 
  ##--Copiato dal veccio C3.prg del '90 circa
  #in cui le variabili rappresentano le seguenti grandezze
  #x1, y1 - coordinate note punto 1
  #x2, y2 - coordinate note punto 2
  #d1,d2  - distanze rispettivamente da P1 e da P2 al punto 3
  #  s =- 1 se guardando da P1 a P2, P3 è a sinistra
{
k1=d1^2-x1^2-y1^2
k2=d2^2-x2^2-y2^2
if(y2-y1!=0){
a=(x1-x2)^2/(y2-y1)-(y1-y2)
b=(x1-x2)*(k1-k2)/(y2-y1)-2*(x1*y2-x2*y1)
c=(k1-k2)^2/(4*(y2-y1))-(y2*k1-y1*k2)
delta=b^2-4*a*c
xx=(-b+s*sqrt(delta))/(2*a)
yy=((k1-k2)/2 + (x1-x2) *xx )/(y2-y1)} else {
  xx=(k1-k2)/(2*(x2-x1))
yy=y1+s*sqrt(d1^2-(xx-x1)^2)}
return(c(xx,yy))
}
