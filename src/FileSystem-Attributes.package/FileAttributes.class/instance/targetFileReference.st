accessing
targetFileReference
	"Answer a FileReference representing the receivers target file"
	
	^FileReference fileSystem: store class currentFileSystem path: self targetFile.