tests
testModificationTime

	| resource modificationTime |
	resource := self tempFileResource.
	self attributesCollectionDo: [ :each |
		modificationTime := each modificationTime.
		self assert: resource beforeCreationTime <= modificationTime.
		self assert: resource afterCreationTime >= modificationTime.
		].