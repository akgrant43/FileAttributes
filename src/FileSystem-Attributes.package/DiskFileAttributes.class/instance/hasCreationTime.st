testing
hasCreationTime
	"Answer a boolean indicating whether the receiver has a creation timestamp, or is just using the change timestamp."

	^(self statAttributes at: 12) isNotNil