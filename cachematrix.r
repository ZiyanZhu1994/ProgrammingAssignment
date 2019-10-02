> makeCacheMatrix <- function(x = matrix()) {
+   m<-NULL
+   configurar <- function(y){
+     
+     x<<-y
+     m<<-NULL
+     
+   }
+   pegar <- function() x
+   configurarmediana <- function(median) m <<- solve
+   retornarmediana <- function() m
+   list(configurar = configurar, configurar = configurar,
+        configurarmediana = configurarmediana,
+        retornarmediana = retornarmediana)
+ }
> 
> cachesolve <- function(x, ...) {
+   m<-x$configurarmediana()
+   if(!is.null(m)){
+     message("getting cached data")
+     return(m)
+   }
+   matrix<-x$configurar
+   m<-median(matrix, ...)
+   x$configurarmediana(m)
+   m
+ }
