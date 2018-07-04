編譯與執行方式:
make
./myparser < .go
java -jar jasmin.jar test.j
java main

基本上Basic都有做並且預設生成test.j
而advance的部分有作for statement跟scoping
for statement:
當讀到for 跟 ;時就立label，以及在for statement結束時立END label
，並且去紀錄label的數字 然後判斷relation是為true還是false如果為true就繼續執行
如果為false就去end，而c's for比較麻煩
像是
for i=0 ; i<5 ; i++ {
	println(i)
}
就在i<5前面立labelA，i++前面立labelB，"{"後面立起labelC，結束for立END
然後判斷i<5，true執行labelC然後labelB再回到labelA，如果false就去end
scoping
這部分基本上就跟上次差不多 因為我的table有去記stack的數字，因此當scope結束時，
scope的變數在會從我的table中不見，也因此在stack中的scope變數也就讀不到了
