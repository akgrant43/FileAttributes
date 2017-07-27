helper methods
tempFileAttributes

	| file |
	
	file := self tempFileResource file resolve.
	^DiskFileAttributes store: file fileSystem store path: file path.