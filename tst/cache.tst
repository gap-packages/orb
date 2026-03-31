gap> START_TEST("Orb package: cache.tst");

# create an empty cache
gap> c := LinkedListCache(10);;
gap> [IsCache(c), c!.head = fail, c!.tail = fail, c!.nrobs, c!.memory,
>      c!.memorylimit];
[ true, true, true, 0, 0, 10 ]

# cache objects at the front of the linked list
gap> n1 := CacheObject(c, "a", 3);;
gap> [c!.head = n1, c!.tail = n1, c!.nrobs, c!.memory, n1!.prev = fail,
>      n1!.next = fail, n1!.ob, n1!.mem];
[ true, true, 1, 3, true, true, "a", 3 ]
gap> n2 := CacheObject(c, "b", 4);;
gap> [c!.head = n2, c!.tail = n1, c!.nrobs, c!.memory, n2!.prev = fail,
>      n2!.next = n1, n1!.prev = n2];
[ true, true, 2, 7, true, true, true ]

# inserting past the limit evicts from the tail
gap> n3 := CacheObject(c, "c", 5);;
gap> [c!.head = n3, c!.tail = n2, c!.nrobs, c!.memory, n3!.next = n2,
>      n2!.prev = n3, n2!.next = fail, n1!.prev = false];
[ true, true, 2, 9, true, true, true, true ]

# using a cached node moves it to the front
gap> UseCacheObject(c, n2);;
gap> [c!.head = n2, c!.tail = n3, n2!.prev = fail, n2!.next = n3,
>      n3!.prev = n2, n3!.next = fail, c!.nrobs, c!.memory];
[ true, true, true, true, true, true, 2, 9 ]

# using an evicted node caches it again and may evict the old tail
gap> UseCacheObject(c, n1);;
gap> [c!.head = n1, c!.tail = n2, n1!.prev = fail, n1!.next = n2,
>      n2!.prev = n1, n2!.next = fail, n3!.prev = false, c!.nrobs,
>      c!.memory];
[ true, true, true, true, true, true, true, 2, 7 ]

# clearing removes all cached nodes
gap> ClearCache(c);;
gap> [c!.head = fail, c!.tail = fail, c!.nrobs, c!.memory];
[ true, true, 0, 0 ]

# an oversized object is immediately evicted and marked as out of cache
gap> c := LinkedListCache(4);;
gap> n := CacheObject(c, "big", 5);;
gap> [c!.head = fail, c!.tail = fail, c!.nrobs, c!.memory, n!.prev = false];
[ true, true, 0, 0, true ]

#
gap> STOP_TEST("Orb package: cache.tst", 0);
