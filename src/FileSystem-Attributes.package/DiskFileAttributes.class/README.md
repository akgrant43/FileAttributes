As the name suggests, FileAttributes provides access to the stat() and access() attributes of the supplied FileReference.

While providing an intuitive API, FileAttributes attempts to be as efficient as possible by supporting three different access models:

1.  If only a single attribute is required, the default mode is to retrieve attributes one at a time.  This becomes expensive if multiple attributes for a given file will be retrieved / tested (due to the overhead of calling a named primitive).
2.  If multiple stat() or access() attributes will be used, #getStatAttributes and #getAccessAttributes can be used to cache a subset of all attributes.
3. If a mixture of stat() and access() attributes will be used, #getAllAttributes can be used to cache the entire set.

It is up to the owner to manage how long the cache is valid.

For example, to see the creation time of the smalltalk vm being run:

  (FileAttributes forFile: FileLocator vmBinary) creationTime

Internal Representation and Key Implementation Points.

    Instance Variables
	file:					<FileReference>
	accessAttributes:		<Array>
	statAttributes:		<Array>


    Implementation Points

File attribute information was originally retrieved through the FilePlugin, which had a single primitive to test for file existence and return some file attributes.  As the amount of information returned increased over time, the performance of the #exists method progressively deteriated as all the additional information was retrieved, but just discarded.

The current design of FileAttributes is based on the following observations and assumptions:

- The most often called method is checking file existence (by a factor of 3 or 4).
- Most other callers retireve and use a single piece of information.
- Named primitive calls are relatively expensive, so it makes sense to return as much information as possible.
- Calling access() is relatively expensive.
- We can't make assumptions about how long it is reasonable to cache any attribute information, since it can be changed externally to Pharo.
- The attribute information is normally very short lived.

Based on this, FileAttributes implements the three access modes as described earlier in the comments.

To minimise overheads, FileAttributes simply stores and accesses the information as returned by the primitives.