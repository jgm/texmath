mmldict.hs: dictionary.xml
	runghc generateMMLDict.hs

dictionary.xml: operatorDictionary.xsl unicode.xml
	xsltproc -o $@ $^

.PHONY: clean
clean:
	-rm mmldict.hs dictionary.xml
