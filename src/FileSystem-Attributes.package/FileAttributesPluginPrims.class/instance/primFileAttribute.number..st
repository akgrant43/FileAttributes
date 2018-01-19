primitives
primFileAttribute: aString number: attributeNumber
	"Answer a single attribute for the supplied file.
	For backward compatibility (on Unix) with FileReference if the file doesn't exist, and the specified path is a (broken) symbolic link, answer the requested attribute for the symbolic link.

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
	^error isPrimitiveError ifTrue: [ 
		(error errorCode = self cantStatPath and: [ attributeNumber ~= 16 ]) ifTrue: 
			[ (self fileAttributeNumberMap at: attributeNumber) value: 
				(DiskSymlinkAttributes store: DiskStore currentFileSystem store path: aString asPath) ]
		ifFalse:
			[ self signalError: error errorCode for: aString ] ]
	ifFalse:
		[ self primitiveFailed ]