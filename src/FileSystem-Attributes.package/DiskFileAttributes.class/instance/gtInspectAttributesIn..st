inspecting
gtInspectAttributesIn: composite
	<gtInspectorPresentationOrder: 2>

	| attrDictionary |

	attrDictionary := Dictionary new.
	self class methodsDo: [ :method |
		method methodNode arguments size = 0 ifTrue:
			[ attrDictionary at: method selector put:
				(method selector value: self) ] ].
	^attrDictionary gtInspectorItemsIn: composite