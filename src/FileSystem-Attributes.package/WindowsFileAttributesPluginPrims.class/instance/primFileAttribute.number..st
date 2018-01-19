primitives
primFileAttribute: aString number: attributeNumber
	"Answer a single attribute for the supplied file.

stat() information:
	
	1: name
	2: mode
	3: ino
	4: dev
	5: nlink
	6: uid
	7: gid
	8: size
	9: accessDate
	10: modifiedDate
	11: changeDate
	12: creationDate

access() information

	13: is readable
	14: is writeable
	15: is executable

symbolic link information

	16: is symbolic link
	"
	<primitive: 'primitiveFileAttribute' module: 'FileAttributesPlugin' error: error>
	^error isPrimitiveError 
		ifTrue: [ self signalError: error errorCode for: aString ]
		ifFalse: 	[ self primitiveFailed ]