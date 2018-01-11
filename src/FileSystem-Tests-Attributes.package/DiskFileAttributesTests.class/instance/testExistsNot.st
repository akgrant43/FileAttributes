tests
testExistsNot

	| file |
	file := '/thisfileshouldnot.exist' asFileReference.
	self assert: (DiskFileAttributes exists: file path in: file fileSystem) not.
