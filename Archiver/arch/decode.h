#include "definesANDstructs.h"

TTree* treeTreversal_array2tree(TBitArray* bitArray, int* ptr);

TTree* bitArray2HaffmanTree(TBitArray* bitArray);

TBitArray* readArchiveFromFile(char* archiveName);

int treeTreversal_symbol(TTree* tree, TBitArray* bitArray, int* ptr);

TBitArray* decompressFile(TBitArray* fileBitArray, TTree* tree);

void decompressArchive(TBitArray* archiveBitArray);