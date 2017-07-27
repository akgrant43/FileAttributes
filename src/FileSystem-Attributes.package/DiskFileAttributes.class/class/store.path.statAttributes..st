instance creation
store: aDiskStore path: aPath statAttributes: anArray

	^(super store: aDiskStore path: aPath)
		statAttributes: anArray;
		yourself.