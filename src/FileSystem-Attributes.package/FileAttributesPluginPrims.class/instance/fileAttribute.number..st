attributes
fileAttribute: aString number: attributeNumber
	"Answer the attribute identified by attributeNumber for the specified file (aString).
	For backward compatibility with FileReference if the file doesn't exist, and the specified path is a (broken) symbolic link, answer the requested attribute for the symbolic link."
	| result |

	result := self primFileAttribute: (self encode: aString) number: attributeNumber.
	result isArray ifTrue: 
		[ "If the error is file not found, attempt to retrieve the symbolic link attribute"
		^(result first = self cantStatPath and: [ attributeNumber ~= 16 ]) ifTrue: 
			[ (self fileAttributeNumberMap at: attributeNumber) value: 
					(DiskSymlinkAttributes store: DiskStore currentFileSystem store path: aString asPath) ]
		ifFalse:
			[ self signalError: result first for: aString ]
		].
	^result