primitives
primFileAttributes: aString mask: attributeMask
	"Answer an array of attributes for the supplied file.  The size and contents of the array are determined by the attributeMask:

Bit 0: stat() information
Bit 1: access() information
Bit 2: use lstat() (instead of stat())

On error, answer an error code (Integer).

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
	11: creationDate

access() information

	1: is readable
	2: is writeable
	3: is executable
	"
	<primitive: 'primitiveFileAttributes' module: 'FileAttributesPlugin' error: error>
	^error isPrimitiveError ifTrue:
		[ self signalError: error errorCode for: aString ]
	ifFalse: 
		[ self primitiveFailed ]