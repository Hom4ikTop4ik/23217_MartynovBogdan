#include "definesANDstructs.h"

int* gysto(const char* fileName);

TTree* haffmanTree(int* gystogram);

int treeTreversal_tree2array(TTree* tree, TBitArray* bitArray, TBitArray** arrays, TVector* stack);

TBitArray* haffmanTree2BitArray(TTree* tree, TBitArray** arrays);

int compress(FILE* file, const char* fileName);

void createArchive(char** archiveName, char** fileNames, int cnt);