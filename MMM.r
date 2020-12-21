library(IRdisplay)

display_html(
'<script>  
code_show=true; 
function code_toggle() {
  if (code_show){
    $(\'div.input\').hide();
  } else {
    $(\'div.input\').show();
  }
  code_show = !code_show
}  
$( document ).ready(code_toggle);
</script>
  <form action="javascript:code_toggle()">
    <input type="submit" value="Click here to toggle on/off the code.">
 </form>'
)

sales <- c(37, 89, 82, 58, 120, 77, 100, 78, 95, 106, 98, 96, 78, 96, 157, 198, 145, 132, 96, 135)
ad <- c(6, 27, 0, 0, 20, 0, 20, 0, 0, 18, 9, 0, 0, 0, 13, 25, 0, 15, 0, 0)


plot(ad, sales, main = "Sales vs Spends",
     xlab = "ad", ylab = "sales",
     pch = 19, frame = FALSE)
abline(lm(sales ~ ad), col = "blue")

modFit.0 <- lm(sales~ad)
summary(modFit.0)

sales <- c(37, 89, 82, 58, 110, 77, 103, 78, 95, 106, 98, 96, 68, 96, 157, 198, 145, 132, 96, 135)
ad <- c(6, 27, 0, 0, 20, 0, 20, 0, 0, 18, 9, 0, 0, 0, 13, 25, 0, 15, 0, 0)

ad.adstock <- as.numeric(filter(x=ad, filter=.50, method="recursive"))

modFit.1 <- lm(sales~ad.adstock)
summary(modFit.1)

plot(ad.adstock, sales, main = "Sales vs Spends",
     xlab = "ad with adstock", ylab = "sales",
     pch = 19, frame = FALSE)
abline(lm(sales ~ ad.adstock), col = "blue")

# Graph ad vs adstock Data
plot(seq(1,length(ad)), ad, type="h", main = "ad vs adstock Data", 
     xlab="Time in Weeks", ylab="Advertising", 
     ylim=c(0, max(c(ad, ad.adstock))),
     frame.plot=FALSE)
lines(ad.adstock)

ad1 <- c(6, 27, 0, 0, 20, 0, 20, 0, 0, 18, 9, 0, 0, 0, 13, 25, 0, 15, 0, 0)
ad2 <- c(3, 0, 4, 0, 5, 0, 0, 0, 8, 0, 0, 5, 0, 11, 16, 11, 5, 0, 0, 15)

ad1.adstock <- as.numeric(filter(x=ad1, filter=.3, method="recursive"))
ad2.adstock <- as.numeric(filter(x=ad2, filter=.3, method="recursive"))

modFit2 <- lm(sales~ad1.adstock+ad2.adstock)
summary(modFit2)

trend <- 1:20 

modFit.3 <- lm(sales~trend+ad1.adstock+ad2.adstock)
summary(modFit.3)

library("IRdisplay")
display_png(file="/Users/adityakamboj/Desktop/Screen\ Shot\ 2020-11-15\ at\ 5.07.16\ PM.png") 
