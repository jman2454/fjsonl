# FJsonl

A generator for rapidly-formattable-to-jsonl C++ data types. 
The general idea is that fields are formatted into fixed-length regions of the ultimate JSON output. 

Currently supports a limited set of types and fixed-length-only members. 

Example: 

Input YAML:

```
name: test_struct_orig
members: 
- x: int32
- y: int32
- b: bool

name: outer_struct
members: 
- x: int32
- inner: test_struct_orig
- b: bool
- z: test_struct_orig
- f: uint64
```

Output C++:

```
#include <cstdint>
#include <iostream>
#include <string>

struct test_struct_orig
{	int32_t x;
	int32_t y;
	bool b;
	static std::string empty()
	{
		return "{\"x\":           ,\"y\":           ,\"b\":      }";
	}
	void format(char* buf) const
	{
		auto head { buf };
		head += 5;
		snprintf(head, 11, "%*d", 10, x);
		head += 16;
		snprintf(head, 11, "%*d", 10, y);
		head += 16;
		snprintf(head, 6, "%*s", 5, b ? "true" : "false");
	}
};
struct outer_struct
{	int32_t x;
	test_struct_orig inner;
	bool b;
	test_struct_orig z;
	uint64_t f;
	static std::string empty()
	{
		return "{\"x\":           ,\"inner\":{\"x\":           ,\"y\":           ,\"b\":      },\"b\":      ,\"z\":{\"x\":           ,\"y\":           ,\"b\":      },\"f\":                     }";
	}
	void format(char* buf) const
	{
		auto head { buf };
		head += 5;
		snprintf(head, 11, "%*d", 10, x);
		head += 20;
		inner.format(head);
		head += 49;
		snprintf(head, 6, "%*s", 5, b ? "true" : "false");
		head += 11;
		z.format(head);
		head += 49;
		snprintf(head, 21, "%*llu", 20, f);
	}
};
```