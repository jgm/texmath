UnicodeTable.hs: UnicodeData.txt mkUnicodeTable.hs
	./mkUnicodeTable.hs

UnicodeData.txt:
	curl https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt -o $@


.PHONY: clean
clean:
	-rm UnicodeTable.hs UnicodeData.txt
