tests
testIsDirectory

	self assert: self tempFileResource file isDirectory equals: false.
	self assert: self tempFileResource file attributes isDirectory equals: self tempFileResource file isDirectory.
	self assert: FileLocator temp isDirectory.