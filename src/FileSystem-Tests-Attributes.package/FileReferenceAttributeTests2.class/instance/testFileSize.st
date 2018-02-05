tests
testFileSize

	self assert: self tempFileResource file size equals: 72.
	self assert: self tempFileResource file attributes fileSize equals: self tempFileResource file size.
