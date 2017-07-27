testing - stat
isBlock
	"Answer true if the receiver is a block device"
	
	^Primitives modeIsBlock: self mode.