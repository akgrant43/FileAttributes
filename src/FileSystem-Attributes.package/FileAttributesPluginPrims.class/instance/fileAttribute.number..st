attributes
fileAttribute: aString number: attributeNumber
	"Answer the attribute identified by attributeNumber for the specified file (aString).
	For backward compatibility (on Unix) with FileReference if the file doesn't exist, and the specified path is a (broken) symbolic link, answer the requested attribute for the symbolic link."

	^self primFileAttribute: (self encode: aString) number: attributeNumber