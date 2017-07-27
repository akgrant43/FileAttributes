*FileSystem-Attributes
symlinkAttributesOf: aPath ifPresent: presentBlock ifAbsent: absentBlock
	"Answer the FileAttributes of the supplied path in the receiver"

	^[ presentBlock value: (self symlinkAttributesClass store: self path: aPath) ]
		on: FileDoesNotExist 
		do: [ :error | absentBlock value ]