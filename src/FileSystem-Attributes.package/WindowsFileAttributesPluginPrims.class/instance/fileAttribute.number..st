attributes
fileAttribute: aString number: attributeNumber
	"Answer the attribute identified by attributeNumber for the specified file (aString)."
	| result |

	result := self primFileAttribute: (self encode: aString) number: attributeNumber.
	result isArray ifTrue: 
		[ self signalError: result first for: aString ].
	^result