tests
testExistsPathTooLong

	| pathMax filename |
	pathMax := primitives primPathMax.
	filename := String new: pathMax * 2 withAll: $a.
	self 
		should: [ primitives exists: filename ]
		raise: IllegalName.