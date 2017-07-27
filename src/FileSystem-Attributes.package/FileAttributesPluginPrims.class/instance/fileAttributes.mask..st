attributes
fileAttributes: aString mask: attributeMask

	| result |
	result := self primFileAttributes: (self encode: aString) mask: attributeMask.
	result isNumber ifTrue: [ ^self signalError: result for: aString ].
	^result