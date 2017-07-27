private - encoding
encode: aString

	^ ((ZnCharacterEncoder newForEncoding: 'utf8') encodeString: aString) asString