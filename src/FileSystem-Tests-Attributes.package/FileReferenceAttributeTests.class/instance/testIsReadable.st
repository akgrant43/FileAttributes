tests
testIsReadable

	self assert: self tempFileResource file isReadable.
	self assert: self tempFileResource file attributes isReadable equals: self tempFileResource file isReadable.