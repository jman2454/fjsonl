# FJsonl

A generator for rapidly-formattable-to-jsonl C++ data types. 
The general idea is that fields are formatted into fixed-length regions of the ultimate JSON output. 

Currently supports a limited set of types and fixed-length-only members. 

Migrating away from snprintf to get psychotic speed over other libraries like RapidJSON. 
RapidJSON has specialized methods for writing the various primitive types, and I was originally using snprintf which proved to be a bottleneck w.r.t. beating it on format calls. There's no need to be this insane, but it's fun. 
For the current (limited) test set, we are over 2x RapidJSON.

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
#include "util.h"

struct test_struct_orig
{	int32_t x;
	int32_t y;
	bool b;
	static std::string empty()
	{
		return "{\"x\":            ,\"y\":            ,\"b\":      }";
	}
	void format(char* buf) const
	{
		auto head { buf };
		head += 5;
		write_backwards(x, head + 11, 12, ' ');
		head += 17;
		write_backwards(y, head + 11, 12, ' ');
		head += 17;
		write_backwards(b, head + 5, 6, ' ');
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
		return "{\"x\":            ,\"inner\":{\"x\":            ,\"y\":            ,\"b\":      },\"b\":      ,\"z\":{\"x\":            ,\"y\":            ,\"b\":      },\"f\":                      }";
	}
	void format(char* buf) const
	{
		auto head { buf };
		head += 5;
		write_backwards(x, head + 11, 12, ' ');
		head += 21;
		inner.format(head);
		head += 51;
		write_backwards(b, head + 5, 6, ' ');
		head += 11;
		z.format(head);
		head += 51;
		write_backwards(f, head + 21, 22, ' ');
	}
};
```