private
signalError: aNumber for: aString
	"Raise the appropriate signal for the supplied error number"

	aNumber = self stringTooLong ifTrue: 
		[ ^IllegalName signalWith: aString ].
	aNumber = self cantStatPath ifTrue: 
		[ ^FileDoesNotExist signalWith: aString ].
	aNumber = self getAttributesFailed ifTrue: 
		[ ^FileDoesNotExist signalWith: aString ].
	^FileSystemError signalWith: aString.