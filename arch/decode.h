#include "definesANDstructs.h"

TTree* treeTreversal_array2tree(TBitArray* bitArray, int* ptr);

TTree* bitArray2HaffmanTree(TBitArray* bitArray);

TBitArray* readArchiveFromFile(char* archiveName);

int treeTreversal_symbol(FILE* archive, TTree* tree, int* ptr, int lenCompressedFile);

int decompressFile(FILE* archive, TTree* tree, int lenCompressedFile, char** nameFile);

void decompressArchive(char* name);