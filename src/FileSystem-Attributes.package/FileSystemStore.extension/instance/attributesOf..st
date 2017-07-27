*FileSystem-Attributes
attributesOf: aPath
	"Answer the FileAttributes of the supplied path in the receiver"

	^self attributesClass store: self path: aPath