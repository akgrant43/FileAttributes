tests
testChangeTime

	| resource changeTime |
	resource := self tempFileResource.
	changeTime := resource file changeTime.
	self assert: resource beforeCreationTime <= changeTime.
	self assert: resource afterCreationTime >= changeTime.
	self assert: resource file attributes changeTime equals: changeTime.
