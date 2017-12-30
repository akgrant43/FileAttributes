attributes
fileAttribute: aString number: attributeNumber
	"Answer the attribute identified by attributeNumber for the specified file (aString)."

	^[ self primFileAttribute: (self encode: aString) number: attributeNumber]
		on: PrimitiveFailed 
		do: [ :exception |
			exception additionalInformation isPrimitiveOSError ifTrue:
					[ self signalError: exception additionalInformation errorCode for: aString ] ].
