tests
testIsFile

	self assert: self tempFileResource file isFile equals: true.
	self assert: self tempFileResource file attributes isFile equals: self tempFileResource file isFile.
	self assert: FileLocator temp isFile equals: false.