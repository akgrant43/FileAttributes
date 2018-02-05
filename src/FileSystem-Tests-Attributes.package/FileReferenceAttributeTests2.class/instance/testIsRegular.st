tests
testIsRegular

	self assert: self tempFileResource file isRegular.
	self assert: self tempFileResource file attributes isRegular equals: self tempFileResource file isRegular