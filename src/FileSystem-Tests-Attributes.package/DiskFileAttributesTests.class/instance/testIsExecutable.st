tests
testIsExecutable

	| file filesystem path |
	file := FileLocator vmBinary resolve.
	filesystem := file fileSystem.
	path := file path.
	self assert: (DiskFileAttributes store: filesystem store path: path) isExecutable.
