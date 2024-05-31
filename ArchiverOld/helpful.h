#include "definesANDstructs.h"

void returnError(const char* msg);
void warning(const char* msg);

// if (file exists)  return 1;
// else  return 0;
int fileExists(char* fileName);

// 0 - no rewrite
// 1 - rewrite
// 2 - don't save
int doYouWantRewriteFile();

char* getOtherName();
const char* discardPath(const char* filePath);


void swapTree(TTree** a, TTree** b);

// 0 - tree is branch
// 1 - tree is leaf
// 2 - it's broken tree (only 1 of 2 sons)
int isLeaf(TTree* tree);


// insert value (1 int) in "vershina" like in stack
// return index where pushed value "lejit"
int vectorPush(TVector* vector, int value);
int vectorPop(TVector* vector);

void heapSiftUp(THeap* heap, int index);
void heapSiftDown(THeap* heap, int index);
void heapPush(THeap* heap, TTree* tree);
TTree* heapPop(THeap* heap);

void bitArrayPushBit(TBitArray* bitArray, int bitValue);
void bitArrayPushByte(TBitArray* bitArray, int byteValue);

void bitArrayWriteBit(TBitArray* bitArray, int value, int index);
void bitArrayWriteByte(TBitArray* bitArray, int value, int index);
void bitArrayWriteInt(TBitArray* bitArray, int value, int index);

int bitArrayReadBit(TBitArray* bitArray, int index);
int bitArrayReadByte(TBitArray* bitArray, int index);
int bitArrayReadInt(TBitArray* bitArray, int index);

void createFileFromBitArray(TBitArray* bitArray, char** fileName);