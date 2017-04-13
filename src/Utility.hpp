#pragma once

#include <assert.h>
#include <string.h>

//This class is used to allocated persistant memory
//It contains no free mechanisims and is intended for
//data that will be persistant throught the duration of
//the application.  Does not attempt to be effeciant with space
//wastage.  Only intended for realitivily small and similar sized allocations
class PersistantBlockAllocator {

  //Aligned on 16byte boundries
  //Raw memory can procede this
  struct MemoryBlockHeader {
    size_t used;      //8bytes
    MemoryBlockHeader *next;//16bytes
  };

  MemoryBlockHeader *firstBlock;
  MemoryBlockHeader *currentBlock;
  size_t blockSize;

  MemoryBlockHeader *AllocateBlock(MemoryBlockHeader *parent) {
    MemoryBlockHeader *block = (MemoryBlockHeader *)malloc(sizeof(MemoryBlockHeader) + blockSize);
    memset(block, 0x00, sizeof(MemoryBlockHeader) + blockSize);
    parent->next = block;
    return block;
  }

public:

  PersistantBlockAllocator(size_t blockSize) {
    this->blockSize = blockSize;
    firstBlock = AllocateBlock(nullptr);
    currentBlock = firstBlock;
  }

  uint8_t *Allocate(size_t size, size_t alignment) {
    assert(size <= blockSize);
    currentBlock->used = (currentBlock->used + (alignment - 1)) & ~(alignment - 1);
    if (currentBlock->used + size > blockSize) {
      currentBlock = AllocateBlock(currentBlock);
    }

    uint8_t *result = (uint8_t *)(((uintptr_t)(currentBlock + 1)) + currentBlock->used);
    currentBlock->used += size;
    return result;
  }

  template<typename T, typename... Args>
  T *Allocate(Args... args) {
    uint8_t *memory = Allocate(sizeof(T), 8);
    T *result = new (memory) T(args...);
    return result;
  }
};

struct InternString {
  size_t length;
  const char *string;
};

class InternStringAllocator {
  PersistantBlockAllocator blockAllocator;
public:
  InternStringAllocator(size_t blockSize) : blockAllocator(blockSize) {}
  InternString CreateString(const char *string, size_t length) {
    uint8_t *memory = blockAllocator.Allocate(length, 1);
    memcpy(memory, string, length);
    InternString result = { length, (char *)memory };
    return result;
  }
};