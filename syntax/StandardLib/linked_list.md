# Cool Compiler: LinkedList Module

## Overview

This repository contains the **LinkedList module** for the **Cool (Classroom Object-Oriented Language) standard library**. The module provides a **doubly linked list implementation** with common operations such as adding, removing, and retrieving elements.

---

## Features

### 1. **Node Structure**
- Each `Node` stores an **Object** and a reference to the **next Node**.
- Methods include:
  - `init(d: Object) : Node` → Initializes a node with data.
  - `getData() : Object` → Returns the stored data.
  - `getNext() : Node` → Returns the next node.
  - `setNext(n: Node) : Node` → Sets the next node reference.

### 2. **LinkedList Class**
- **Attributes**:
  - `head` → Points to the first node.
  - `tail` → Points to the last node.
  - `count` → Tracks the number of elements.

- **Methods**:
  - `init() : LinkedList` → Initializes an empty list.
  - `isEmpty() : Bool` → Checks if the list is empty.
  - `size() : Int` → Returns the number of elements.
  - `addFirst(elem: Object) : LinkedList` → Adds an element to the beginning.
  - `addLast(elem: Object) : LinkedList` → Adds an element to the end.
  - `removeFirst() : Object` → Removes and returns the first element.
  - `removeLast() : Object` → Removes and returns the last element.
  - `get(index: Int) : Object` → Retrieves an element at a specific index.
  - `toString() : String` → Returns a string representation of the list.

## Example Usage

### **Cool Program Using LinkedList**
```cool
class Main inherits IO {
   main() : Object {
      {
         let list : LinkedList <- new LinkedList.init() in {
            list.addFirst(1);
            list.addFirst(2);
            list.addLast(3);
            out_string(list.toString().concat("\n"));
            out_int(list.get(1));
         };
      }
   };
};
```

### **Expected Output**
```
LinkedList [size=3]: 2 -> 1 -> 3
1
```

## Usage

To use the **LinkedList** module in Cool, simply create a new instance and use its methods:

```cool
let list : LinkedList <- new LinkedList.init();
list.addFirst(10);
list.addLast(20);
out_string(list.toString());
```

## Conclusion

The **LinkedList module** provides an efficient, reusable data structure within the Cool language. With its **dynamic storage**, **efficient insertions and deletions**, and **intuitive API**, it serves as a useful utility for data manipulation in Cool programs.

