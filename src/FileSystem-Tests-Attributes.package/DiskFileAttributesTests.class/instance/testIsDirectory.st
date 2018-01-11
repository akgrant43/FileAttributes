tests
testIsDirectory

	| file |

	self collectionAssert: [ :each | each isDirectory ] equals: false.
	file := FileLocator temp resolve.
	self assert: (DiskFileAttributes store: file fileSystem store path: file path) isDirectory.