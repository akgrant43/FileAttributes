tests
testIsSymlink

	self assert: self tempFileResource file isSymlink equals: false.
	self assert: self tempFileResource file attributes isSymlink equals: self tempFileResource file isSymlink.