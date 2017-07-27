private - encoding
decode: aString
	"On Windows the primitives returned the string with native encoding.
	Just ensure it isn't a ByteArray."

	aString isString ifFalse: [ ^aString asString ].
	^aString