tests
testExists

	| file |
	file := self tempFileResource file resolve.
	self assert: (DiskFileAttributes exists: file path in: file fileSystem)