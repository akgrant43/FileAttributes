testing
exists: aString

	| result |
	result := self primExists: (self encode: aString).
	result isNumber ifTrue: [ ^self signalError: result for: aString ].
	^result