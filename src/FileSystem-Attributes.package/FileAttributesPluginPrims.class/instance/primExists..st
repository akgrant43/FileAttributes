primitives
primExists: aString
	"Answer a boolean indicating whether the supplied file exists."

	<primitive: 'primitiveFileExists' module: 'FileAttributesPlugin' error: error>
	^(error isPrimitiveError)
		ifTrue: [ self signalError: error errorCode for: aString ]
		ifFalse: [ self primitiveFailed ].