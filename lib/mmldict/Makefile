mmldict.hs: dictionary.xml
	./generateMMLDict.hs

unicode.xml:
	curl -o $@ https://www.w3.org/2003/entities/2007xml/unicode.xml

dictionary.xml: operatorDictionary.xsl unicode.xml
	xsltproc -o $@ $^

.PHONY: clean
clean:
	-rm mmldict.hs dictionary.xml unicode.xml
