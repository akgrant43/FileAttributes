helper methods
attributesCollectionDo: aBlock
	"Evaluate the supplied block for each of the attribute cache modes"

	^self attributesCollection do: aBlock