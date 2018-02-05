tests - single attribute
testAccessTime

	| resource accessTime |
	resource := self tempFileResource.
	accessTime := resource file accessTime.
	self assert: resource beforeCreationTime <= accessTime.
	self assert: resource afterCreationTime >= accessTime.
	self assert: resource file attributes accessTime equals: accessTime.
