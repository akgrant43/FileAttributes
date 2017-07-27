instance creation
store: aDiskStore path: aPath node: aNode

	^(super store: aDiskStore path: aPath)
		statAttributes: (aNode at: 2);
		yourself.