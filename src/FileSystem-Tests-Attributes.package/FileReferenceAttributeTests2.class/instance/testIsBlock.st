tests
testIsBlock

	self assert: self tempFileResource file isBlock equals: false.
	self assert: self tempFileResource file attributes isBlock equals: self tempFileResource file isBlock.
