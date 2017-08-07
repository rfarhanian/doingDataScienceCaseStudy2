data Items;
input item;
DATALINES;
4 
5 
1 
4 
1 
1 
4 
5 
2 
1 
0 
2
;

proc print data=Items;
run;

proc iml;
use Items;
read all var{item} into y;
X = shape(y, 3, 4);
print X;
quit;
