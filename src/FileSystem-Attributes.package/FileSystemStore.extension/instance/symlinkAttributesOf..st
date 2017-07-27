*FileSystem-Attributes
symlinkAttributesOf: aPath
	"Answer the FileAttributes of the supplied path in the receiver"

	^self symlinkAttributesClass store: self path: aPath