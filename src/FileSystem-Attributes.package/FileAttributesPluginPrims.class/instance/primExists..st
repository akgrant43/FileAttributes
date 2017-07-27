primitives
primExists: aString
	"Answer a boolean indicating whether the supplied file exists."

	<primitive: 'primitiveFileExists' module: 'FileAttributesPlugin'>
	^self primitiveFailed