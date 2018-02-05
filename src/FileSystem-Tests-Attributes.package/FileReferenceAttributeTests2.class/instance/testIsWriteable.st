tests
testIsWriteable

	self assert: self tempFileResource file isWritable.
	self assert: self tempFileResource file attributes isWritable equals: self tempFileResource file isWritable.