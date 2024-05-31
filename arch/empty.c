#include "archiver.h"

TTree treeEmpty() {
    TTree tree;
    tree.left = NULL;
    tree.right = NULL;
    tree.value = 0;
    tree.size = 0;
    return tree;
}

THeap heapEmpty() {
    THeap heap;
    heap.arr = NULL;
    heap.cap = 0;
    heap.cnt = 0;
    return heap;
}

TVector vectorEmpty() {
    TVector vector;
    vector.arr = NULL;
    vector.cap = 0;
    vector.cnt = 0;
    return vector;
}

TBitArray bitArrayEmpty()
{
    TBitArray bitArray;
    bitArray.arr = NULL;
    bitArray.cap = 0;
    bitArray.cnt = 0;
    return bitArray;
}