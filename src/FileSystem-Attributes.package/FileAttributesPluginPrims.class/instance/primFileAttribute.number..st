primitives
primFileAttribute: aString number: attributeNumber
	"Answer a single attribute for the supplied file.
	On error, answer a single element array containining the error code.

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
	"
	<primitive: 'primitiveFileAttribute' module: 'FileAttributesPlugin'>
	^self primitiveFailed