initialize
reset
	"Reload the masks"

	| masks |

	masks := self new primFileMasks.
	S_IFMT := masks at: 1.
	S_IFSOCK := masks at: 2.
	S_IFLNK := masks at: 3.
	S_IFREG := masks at: 4.
	S_IFBLK := masks at: 5.
	S_IFDIR := masks at: 6.
	S_IFCHR := masks at: 7.
	S_IFIFO := masks at: 8.
