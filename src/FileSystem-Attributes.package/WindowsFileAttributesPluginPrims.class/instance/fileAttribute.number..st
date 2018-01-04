attributes
fileAttribute: aString number: attributeNumber
	"Answer the attribute identified by attributeNumber for the specified file (aString)."

	"Windows doesn'ts support symbolic links"
	attributeNumber = 16 ifTrue: [ ^false ].
	^super fileAttribute: aString number: attributeNumber.
