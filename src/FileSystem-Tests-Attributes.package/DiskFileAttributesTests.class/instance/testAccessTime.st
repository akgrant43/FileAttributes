tests - single attribute
testAccessTime

	| resource accessTime |
	resource := self tempFileResource.
	self attributesCollectionDo: [ :each |
		accessTime := each accessTime.
		self assert: resource beforeCreationTime <= accessTime.
		self assert: resource afterCreationTime >= accessTime.
		].