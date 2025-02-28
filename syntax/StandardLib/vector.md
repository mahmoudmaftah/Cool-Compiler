# Vector Class Documentation

A dynamic array implementation in the Cool programming language that provides efficient resizing, automatic memory management, and a familiar vector interface.

## Overview

The `Vector` class implements a dynamically resizable array of integers. It provides standard vector operations with automatic growth and shrinking of the underlying storage for efficient memory usage.

## Features

- **Dynamic resizing**: Automatically grows when adding elements to a full vector
- **Memory efficiency**: Shrinks when utilization is low to save memory
- **Bounds checking**: Provides safety checks with detailed error messages
- **Standard operations**: Supports common vector operations like push, pop, get, set
- **Self-returning methods**: Uses method chaining pattern for operations that modify the vector

## Interface

### Initialization

```cool
-- Initialize a new Vector with default capacity of 8
myVector : Vector <- (new Vector).init();

-- Initialize a Vector with custom capacity
myVector : Vector <- (new Vector).init_capacity(20);
```

### Basic Operations

```cool
-- Add elements to the end
myVector.push(10).push(20).push(30);

-- Get the number of elements
size : Int <- myVector.size();  -- returns 3

-- Check if vector is empty
isEmpty : Bool <- myVector.is_empty();  -- returns false

-- Get first element
first : Int <- myVector.front();  -- returns 10

-- Get element at index
element : Int <- myVector.get(1);  -- returns 20

-- Update element at index
myVector.set(1, 25);  -- element at index 1 is now 25

-- Remove and return last element
last : Int <- myVector.pop();  -- returns 30, vector now has 2 elements
```

### Debug Support

```cool
-- Print the contents of the vector
myVector.print();  -- outputs: Vector[10, 25]
```

## Implementation Details

### Attributes

| Name | Type | Description |
|------|------|-------------|
| `data` | `Array[Int]` | Underlying array that stores the elements |
| `count` | `Int` | Number of elements currently in the vector |
| `capacity` | `Int` | Total capacity of the underlying array |

### Growth Strategy

The Vector uses a doubling strategy for growth:

- When pushing an element to a full vector (count = capacity), the capacity is doubled
- This amortizes the cost of resizing over multiple operations

```cool
-- In push() method when vector is full:
resize(capacity * 2)
```

### Shrinking Strategy

To prevent wasted memory, the vector can shrink:

- When the vector is less than 25% full after a `pop()` operation
- The capacity is halved, but never shrinks below 8 elements

```cool
-- In pop() method when utilization is low:
if count < (capacity / 4) then
    if capacity < 8 then
        self
    else
        resize(capacity / 2)
    fi
fi
```

### Error Handling

The Vector implements various safety checks:

- Accessing an out-of-bounds index produces an error message
- Operations on an empty vector when elements are required (like `pop()` or `front()`) produce error messages
- Negative indices are rejected with an error message

## Code Example

Here's a complete example of how to use the Vector class:

```cool
class Main inherits IO {
    main() : Object {
        let vec : Vector <- (new Vector).init() in {
            -- Add some elements
            vec.push(10).push(20).push(30).push(40);
            
            -- Display the vector
            out_string("Initial vector: ");
            vec.print();
            
            -- Get and display an element
            out_string("Element at index 2: ");
            out_int(vec.get(2));
            out_string("\n");
            
            -- Modify an element
            vec.set(1, 25);
            out_string("After setting index 1 to 25: ");
            vec.print();
            
            -- Remove elements
            out_string("Popped value: ");
            out_int(vec.pop());
            out_string("\n");
            
            out_string("Vector after pop: ");
            vec.print();
            
            -- Check size
            out_string("Vector size: ");
            out_int(vec.size());
            out_string("\n");
        }
    };
};
```

## Performance Considerations

- **Time Complexity**:
  - Access/Set (`get`/`set`): O(1)
  - Add to end (`push`): O(1) amortized, O(n) worst case (when resizing)
  - Remove from end (`pop`): O(1) amortized, O(n) worst case (when resizing)
  - Size check (`size`/`is_empty`): O(1)

- **Space Complexity**:
  - O(n) where n is the number of elements
  - Space usage is optimized by shrinking when utilization is low

## Limitations

- Currently only supports integer values
- No iterator support
- No insert/remove at arbitrary positions
- No search functionality

---

*Author: Mahmoud MAFTAH*