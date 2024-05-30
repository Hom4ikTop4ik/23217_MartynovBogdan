#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// === Constant and Macro Definitions ===
#define DEBUG 1
#define compressTree 1

#ifndef min
#define min(a, b) ((a < b) ? a : b)
#endif

#ifndef max
#define max(a, b) ((a > b) ? a : b)
#endif

typedef unsigned char word;
#define bitInByte 8 // 1 word = 8 bit
#define differentBytes (1ll << (  bitInByte * sizeof(word)  ))
#define bufferSize 1024
#define fileNameBufferSize 1024

#define bytes2int(a,b,c,d) ((a << 3 * bitInByte) | (b << 2 * bitInByte) | (c << 1 * bitInByte) | (d))
#define int2bytes(val, a,b,c,d) \
            (a = 0xff & val >> 3 * bitInByte); \
            (b = 0xff & val >> 2 * bitInByte); \
            (c = 0xff & val >> 1 * bitInByte); \
            (d = 0xff & val);

// === Structs ===

typedef struct STree {
    int value;
    struct STree* left;
    struct STree* right;
    int size;
} TTree;

typedef struct SHeap {
    TTree** arr;
    int cap;
    int cnt;
} THeap;

typedef struct SVector {
    int* arr;
    int cap;
    int cnt;
} TVector;

typedef struct SBitArray {
    word* arr;
    int cap;
    int cnt;
} TBitArray;

// === Functions ===

void swapTree(TTree* *a, TTree* *b) {
    TTree* t = *a;
    *a = *b;
    *b = t;
}

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

void printIntArray(int* arr, int cnt)
{
    for (int i = 0; i < cnt; i++)
        printf("%2x - %d\n", i, arr[i]);
}

void printByteArray(word* arr, int cnt)
{
    for (int i = 0; i < cnt; i++)
        printf("%2x - %x\n", i, arr[i]);
}

void returnError(const char* msg)
{
    printf(msg);
    exit(EXIT_FAILURE);
}

void warning(const char* msg)
{
    printf(msg);
}

// insert value (1 word) in "vershina" like in stack
// return index where pushed value "lejit"
int vectorPush(TVector* vector, int value)
{
    if (vector->cnt >= vector->cap)
    {
        vector->cap = 1 + 2 * vector->cap;
        vector->arr = realloc(vector->arr, vector->cap * sizeof(int));

        if (vector->arr == NULL) 
            returnError("realloc returned NULL (vectorPush)\n");
    }

    vector->arr[vector->cnt++] = value;

    return vector->cnt - 1; // index of pushed elems
}

int vectorPop(TVector* vector) {
    int ret = vector->arr[vector->cnt];
    vector->cnt--;
    return ret;
}

void heapSiftUp(THeap* heap, int index)
{
    if (index == 0) return;

    int parent = (index - 1) / 2;

    // if cur value is less than parent value
    if ((heap->arr[index]->size) < (heap->arr[parent]->size)) {
        swapTree(&heap->arr[index], &heap->arr[parent]);
        heapSiftUp(heap, parent);
    }
}

void heapSiftDown(THeap* heap, int index)
{
    int left = 1 + 2 * index;
    int right = 2 + 2 * index;
    int child;

    if (left >= heap->cnt)
        return;
    else if (right >= heap->cnt)
        child = left;
    else {
        if (heap->arr[left]->size < heap->arr[right]->size)
            child = left;
        else
            child = right;
    }

    if (heap->arr[child]->size < heap->arr[index]->size) {
        swapTree(&heap->arr[child], &heap->arr[index]);
        heapSiftDown(heap, child);
    }
}

void heapPush(THeap* heap, TTree* tree)
{
    if (heap->cnt >= heap->cap)
    {
        heap->cap = 1 + 2 * heap->cap;
        heap->arr = realloc(heap->arr, heap->cap * sizeof(TTree*));
        if (heap->arr == NULL) 
            returnError("realloc returned NULL [heapPush]\n");
    }

    heap->arr[heap->cnt] = tree;
    heapSiftUp(heap, heap->cnt);
    heap->cnt++;
}

TTree* heapPop(THeap* heap)
{
    TTree* ret = heap->arr[0];
    heap->cnt--;
    swapTree(&heap->arr[0], &heap->arr[heap->cnt]);
    heapSiftDown(heap, 0);
    return ret;
}

int* gysto(const char* fileName)
{
    FILE* file = fopen(fileName, "rb");
    if (file == NULL)
        return NULL;

    int* gystogram = (int*)malloc(differentBytes * sizeof(int));
    for (int i = 0; i < differentBytes; i++)
        gystogram[i] = 0;

    // file size
    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    word* buffer = (word*)malloc(bufferSize);

    size_t done = 0;
    while (done < file_size)
    {
        int cnt = fread(buffer, sizeof(word), bufferSize, file);
        if (ferror(file) != 0)
            returnError("Error in file [gysto, while(done < dataSize)]\n");

        for (int i = 0; (i < cnt) && (i < bufferSize); i++)
            gystogram[buffer[i] % differentBytes]++;

        done += cnt;
        if (cnt < bufferSize)
            break; // eof
    }
    fclose(file);
    return gystogram;
}

TTree* haffmanTree(int* gystogram)
{
    THeap heap = heapEmpty();

    TTree* arr[differentBytes];
    for (int i = 0; i < differentBytes; i++)
    {
        if (compressTree == 1 && gystogram[i] == 0)
            continue;

        arr[i] = malloc(sizeof(TTree));
            *(arr[i]) = treeEmpty();
            arr[i]->size = gystogram[i];
            arr[i]->value = i;

        heapPush(&heap, arr[i]);
    }

    while (heap.cnt > 1)
    {
        TTree* tree1 = heapPop(&heap);
        TTree* tree2 = heapPop(&heap);

        TTree* treeNew = malloc(sizeof(TTree));
        *treeNew = treeEmpty();
        treeNew->left  = tree1;
        treeNew->right = tree2;
        treeNew->size  = tree1->size + tree2->size;

        heapPush(&heap, treeNew);
    }

    TTree* ret = heapPop(&heap);

    return ret;
}

void bitArrayPushBit(TBitArray* bitArray, int bitValue)
{
    if (bitArray->cnt * (bitInByte * sizeof(word)) >= bitArray->cap)
    {
        bitArray->cap = 1 + 2 * bitArray->cap;
        bitArray->arr = realloc(bitArray->arr, bitArray->cap * sizeof(word));

        if (bitArray->arr == NULL) 
            returnError("realloc returned NULL (bitArrayPushBit)\n");
    }

    int curBit = bitArray->cnt % (bitInByte * sizeof(word));
    int curByte = bitArray->cnt / (bitInByte * sizeof(word)); // word where at least 1 free bit

    int mask = 0;
    for (int i = 0; i < curBit; i++) {
        mask <<= 1;
        mask |= 1;
    }
    mask <<= ((bitInByte * sizeof(word)) - curBit);

    bitArray->arr[curByte] &= mask; // clear bits after curBit
    bitArray->arr[curByte] |= (bitValue << ((bitInByte * sizeof(word)) - 1 - curBit)); // add bit to array

    bitArray->cnt++;
}

void bitArrayPushByte(TBitArray* bitArray, int byteValue)
{
    if ((bitArray->cnt) * (bitInByte * sizeof(word)) + 1/*word*/ >= bitArray->cap)
    {
        bitArray->cap = 1 + 2 * bitArray->cap;
        bitArray->arr = realloc(bitArray->arr, bitArray->cap * sizeof(word));

        if (bitArray->arr == NULL) 
            returnError("realloc returned NULL (bitArrayPushBit)\n");
    }

    for (int i = bitInByte *sizeof(word) -1;   i >= 0;   i--)
        bitArrayPushBit(bitArray, (byteValue >> i) & 1);
}


// 0 - tree is branch
// 1 - tree is leaf
// 2 - it's broken tree (only 1 of 2 sons)
int isLeaf(TTree* tree)
{
    if (tree->left == NULL && tree->right == NULL)
        return 1; // it's leaf
    else if (tree->left == NULL || tree->right == NULL)
        return 2; // broken tree
    else
        return 0; // it's branch
}

int treeTreversal_tree2array(TTree* tree, TBitArray* bitArray, TBitArray** arrays, TVector* stack)
{
    if (tree == NULL) return 0;

    int temp = isLeaf(tree);
    if (temp == 1)
    {
        bitArrayPushBit(bitArray, 1); // push "message": "now will word, after read word go up"
        bitArrayPushByte(bitArray, tree->value); // push word

        for (int i = 0; i < stack->cnt; i++) 
            bitArrayPushBit(arrays[tree->value], stack->arr[i]);

        return 1 + bitInByte * sizeof(word);
    }
    else if (temp == 0)
    {
        int cnt = 2; // push 2 bits
        bitArrayPushBit(bitArray, 0); // push "message": "go down in left branch"
        vectorPush(stack, 0); // left branch
        cnt += treeTreversal_tree2array(tree->left, bitArray, arrays, stack); // go to left branch
        vectorPop(stack); // up from left branch

        bitArrayPushBit(bitArray, 0); // push "message": "go down in right branch"
        vectorPush(stack, 1); // right branch
        cnt += treeTreversal_tree2array(tree->right, bitArray, arrays, stack); // go to right branch
        vectorPop(stack); // up from right branch

        return cnt;
    }
    else
        returnError("Tree broke (1 of 2 branch only) [treeTrevesal_tree2array]\n");
}

void bitArrayWriteBit(TBitArray* bitArray, int value, int index)
{
    if (index < 0)
        returnError("Write bit before array [bitArrayWrireBit]\n");
    if (index >= bitArray->cnt) {
        bitArrayPushBit(bitArray, value);
        return;
    }

    int curByte = index / (bitInByte * sizeof(word));
    int curBit = index % (bitInByte * sizeof(word));

    int mask = ~ (1 << (bitInByte*sizeof(word) -1 -curBit)); // set zero in mask[curBit] and set one otherwise
    bitArray->arr[curByte] &= mask;
    bitArray->arr[curByte] |= (1 & value) << (  (bitInByte*sizeof(word) -1) -curBit  ); // set value in mask[curBit]

}

void bitArrayWriteByte(TBitArray* bitArray, int value, int index)
{
    for (int i = 0; i < bitInByte; i++) {
        int bit = (value >> (bitInByte-1 - i)) & 1;
        bitArrayWriteBit(bitArray, bit, index + i);
    }
}

void bitArrayWriteInt(TBitArray* bitArray, int value, int index)
{
    // little-endian
    int loLo = value & 0xff;
    int loHi = (value >> bitInByte) & 0xff;
    int hiLo = (value >> (2 * bitInByte)) & 0xff;
    int hiHi = (value >> (3 * bitInByte)) & 0xff;

    bitArrayWriteByte(bitArray, loLo, index);
    bitArrayWriteByte(bitArray, loHi, index + bitInByte);
    bitArrayWriteByte(bitArray, hiLo, index + 2*bitInByte);
    bitArrayWriteByte(bitArray, hiHi, index + 3*bitInByte);
}


int bitArrayReadBit(TBitArray* bitArray, int index)
{
    if (index < 0)
        returnError("Read bit before array [bitArrayReadBit]\n");
    if (index >= bitArray->cnt)
        return 0;
        //returnError("Read bit after array [bitArrayReadBit]\n");

    int curByte = index / (bitInByte * sizeof(word));
    int curBit = index % (bitInByte * sizeof(word));
    
    int value = bitArray->arr[curByte];
        value &= (1 << bitInByte*sizeof(word) -1 -curBit);
        value >>= bitInByte*sizeof(word) -1 -curBit; // shift back
    return value;
}

int bitArrayReadByte(TBitArray* bitArray, int index)
{
    int value = 0;
    for (int i = 0 ; i < bitInByte*sizeof(word); i++) {
        value <<= 1;
        value |= (1 & bitArrayReadBit(bitArray, index + i));
    }
    return value;
}

int bitArrayReadInt(TBitArray* bitArray, int index)
{
    int value = 0;
        value |= bitArrayReadByte(bitArray, index + (0 * bitInByte)) << (0 * bitInByte);
        value |= bitArrayReadByte(bitArray, index + (1 * bitInByte)) << (1 * bitInByte);
        value |= bitArrayReadByte(bitArray, index + (2 * bitInByte)) << (2 * bitInByte);
        value |= bitArrayReadByte(bitArray, index + (3 * bitInByte)) << (3 * bitInByte);

    return value;
}

TBitArray* haffmanTree2BitArray(TTree* tree, TBitArray** arrays)
{
    TBitArray* ret = malloc(sizeof(TBitArray));
    *ret = bitArrayEmpty();
        bitArrayPushByte(ret, 0);
        bitArrayPushByte(ret, 0);
        bitArrayPushByte(ret, 0);
        bitArrayPushByte(ret, 0);

    TVector stack = vectorEmpty();

    int treeLen = treeTreversal_tree2array(tree, ret, arrays, &stack);

    bitArrayWriteInt(ret, treeLen, 0);

    return ret;
}

// if (file exists)  return 1;
// else  return 0;
int fileExists(char* fileName)
{
    FILE* file = fopen(fileName, "rb");
    if (file == NULL)
        return 0;
    return 1;
}

// 0 - no rewrite
// 1 - rewrite
// 2 - don't save
int doYouWantRewriteFile()
{
    int rewrite = 0;
    printf("do You Want Rewrite File? (0 - no, 1 - yes, 2 - don't save file): ");
    int temp = scanf("%d", &rewrite);
    getc(stdin); // skip '\n'
    if (temp == 1)
        return rewrite;
    else
        returnError("Error in scanf [doYouWantRewriteFile]\n");
}

char* getOtherName()
{
    int size = fileNameBufferSize;

    char* str = malloc(sizeof(char) * (size + 1));
    printf("Input new file name (not more %d symbols): ", size);

    char temp = 0;
    int i = 0;
    do {
        scanf("%c", &temp);
        if (i < size && (temp != '\0') && (temp != '\n') && (temp != '\r')) 
        {
            str[i] = temp;
            i++;
        }
    } while ((temp != '\0') && (temp != '\n') && (temp != '\r'));

    str[i] = '\0';

    return str;
}

char* discardPath(char* filePath)
{
    if (filePath == NULL)
        return NULL;

    int len = strlen(filePath);
    for (int i = len - 1; i >= 0; i--)
        if (filePath[i] == '\\' || filePath[i] == '/')
            return filePath + (i + 1);

    return filePath;
}


TBitArray* compress(const char* fileName)
{
    FILE* file = fopen(fileName, "rb");
    if (file == NULL) {
        printf("File name: %s\n", fileName);
        returnError("Can't open file [compress]\n");
    }
    
    int* gystogram = gysto(fileName);
    if (gystogram == NULL) 
        return NULL;

    fileName = discardPath(fileName); // in archive I want save only name of file, not path
    int strLenght = strlen(fileName) + 1;

    TTree* tree = haffmanTree(gystogram);

    TBitArray** arrays = malloc(differentBytes * sizeof(TBitArray*));
    for (int i = 0; i < differentBytes; i++) 
    {
        arrays[i] = malloc(sizeof(TBitArray));
        *(arrays[i]) = bitArrayEmpty();
    }
    TBitArray* haffmanBitArray = haffmanTree2BitArray(tree, arrays);

    TBitArray* compressedFile = malloc(sizeof(TBitArray));
        *compressedFile = bitArrayEmpty();

        // reserve memory for len compressed file, len uncompressed file, len its name and fileName
        bitArrayPushByte(compressedFile, 0);
        bitArrayPushByte(compressedFile, 0);
        bitArrayPushByte(compressedFile, 0);
        bitArrayPushByte(compressedFile, 0);

        bitArrayPushByte(compressedFile, 0);
        bitArrayPushByte(compressedFile, 0);
        bitArrayPushByte(compressedFile, 0);
        bitArrayPushByte(compressedFile, 0);

        bitArrayPushByte(compressedFile, 0);
        bitArrayPushByte(compressedFile, 0);
        bitArrayPushByte(compressedFile, 0);
        bitArrayPushByte(compressedFile, 0);

        for (int i = 0; i < strLenght; i++)
            bitArrayPushByte(compressedFile, 0);

    int bitLenFile = 3*sizeof(int)*bitInByte + strLenght*bitInByte;

    // push tree len and haffman tree into file
    for (int i = 0 ; i < haffmanBitArray->cnt; i++)
    {
        int bit = bitArrayReadBit(haffmanBitArray, i);
        bitArrayPushBit(compressedFile, bit);
        bitLenFile++;
    }

    word buffer[bufferSize] = { 0 };

    int byteCnt = 0; // count of bytes in uncompressed file
    while (feof(file) == 0)
    {
        int cnt = fread(buffer, sizeof(word), bufferSize, file);
        byteCnt += cnt;

        for (int i = 0; (i < bufferSize) && (i < cnt); i++)
        {
            int byte = buffer[i] & 0xff;
            for (int j = 0; j < arrays[byte]->cnt; j++)
            {
                int bit = bitArrayReadBit(arrays[byte], j);
                bitArrayPushBit(compressedFile, bit);
                bitLenFile++;
            }
            //bitLenFile += arrays[byte]->cnt;
        }

        if (cnt < bufferSize)
            break;
    }

    if (bitLenFile != compressedFile->cnt)
        returnError("Error in compressing file: bitLenFile != compressedFile->cnt [compress]\n");

    bitArrayWriteInt(compressedFile, bitLenFile, 0);
    bitArrayWriteInt(compressedFile, byteCnt, sizeof(int) * bitInByte);
    bitArrayWriteInt(compressedFile, strLenght, 2 * sizeof(int) * bitInByte);
    for (int i = 0; i < strLenght; i++) 
        bitArrayWriteByte(compressedFile, fileName[i], (3 * sizeof(int) + i) * bitInByte);

    return compressedFile;
}

TBitArray* createArchive(char** fileNames, int cnt)
{
    int realCnt = 0;
    int fileLenght = 2*sizeof(int)*bitInByte; // for realCnt and fileLenght

    TBitArray* file = malloc(sizeof(TBitArray));
        *file = bitArrayEmpty();

        bitArrayPushByte(file, 0);
        bitArrayPushByte(file, 0);
        bitArrayPushByte(file, 0);
        bitArrayPushByte(file, 0);

        bitArrayPushByte(file, 0);
        bitArrayPushByte(file, 0);
        bitArrayPushByte(file, 0);
        bitArrayPushByte(file, 0);

    for (int i = 0; i < cnt; i++)
    {
        TBitArray* temp = compress(fileNames[i]);
        if (temp == NULL) 
            continue;
        else
        {
            for (int j = 0; j < temp->cnt; j++) {
                int bit = bitArrayReadBit(temp, j);
                bitArrayPushBit(file, bit);
            }
            fileLenght += temp->cnt;
            realCnt++;
        }
    }
    bitArrayWriteInt(file, fileLenght, 0);
    bitArrayWriteInt(file, realCnt, bitInByte * sizeof(int));
    return file;
}

void createFileFromBitArray(TBitArray* bitArray, char* fileName)
{
    FILE* file;
    while (fileExists(fileName) != 0) {
        int res = doYouWantRewriteFile();
        if (res == 2)
            return; // don't save file
        else if (res == 1)
            break; // rewrite file
        else if (res == 0)
            fileName = getOtherName();
    }
    file = fopen(fileName, "wb");

    word buffer[bufferSize];
    int i = 0;
    for (i = 0; i < bitArray->cnt; )
    {
        int j = 0;
        for (j = 0; (j < bufferSize) && (i < bitArray->cnt); j++) {
            buffer[j] = bitArrayReadByte(bitArray, i);
            i += bitInByte * sizeof(word);
        }
        fwrite(buffer, sizeof(word), j, file);
    }
    fclose(file);
}


TTree* treeTreversal_array2tree(TBitArray* bitArray, int* ptr)
{
    int bit = bitArrayReadBit(bitArray, *ptr);
    (*ptr)++;

    TTree* ret = malloc(sizeof(TTree));
    *ret = treeEmpty();

    if (bit == 1) { // isLeaf()
        int byte = bitArrayReadByte(bitArray, *ptr);
        (*ptr) += bitInByte;
        ret->value = byte;
    }

    else //if (bit == 0)
    {
        ret->left = treeTreversal_array2tree(bitArray, ptr);
        bit = bitArrayReadBit(bitArray, *ptr);
        (*ptr)++;

        if (bit == 0) 
            ret->right = treeTreversal_array2tree(bitArray, ptr);
        else // if (bit == 1)
            returnError("Tree broke (right branch should be branch, but it's value) [treeTreversal_array2tree]\n");
    }
    return ret;
}

TTree* bitArray2HaffmanTree(TBitArray* bitArray)
{
    int ptr = 0;
    TTree* tree = treeTreversal_array2tree(bitArray, &ptr);
    return tree;
}

TBitArray* readArchiveFromFile(char* archiveName)
{
    FILE* file = fopen(archiveName, "rb");
    if (file == NULL)
        returnError("Can't open archive [readArchiveFromFile]\n");

    TBitArray* bitArray = malloc(sizeof(TBitArray));
    if (bitArray == NULL)
        returnError("malloc returned NULL [readArchiveFromFile]\n");
    *bitArray = bitArrayEmpty();

    word buffer[bufferSize];
    

    while (feof(file) == 0)
    {
        int cnt = fread(buffer, sizeof(word), bufferSize, file);

        for (int i = 0; (i < cnt) && (i < bufferSize); i++) {
            int byte = buffer[i] & 0xff;
            bitArrayPushByte(bitArray, byte); 
        }
        if (cnt < bufferSize)
            break;
    }
    return bitArray;
}

int treeTreversal_symbol(TTree* tree, TBitArray* bitArray, int* ptr)
{
    if (tree == NULL)
        returnError("Tree broke [treeTreversal_symbol]\n");

    if (isLeaf(tree) == 2)
        returnError("Tree broke [treeTreversal_symbol]\n");
    if (isLeaf(tree) == 1)
        return (0xff & tree->value);

    int bit = bitArrayReadBit(bitArray, *ptr);
    (*ptr)++;

    if (bit == 0)
        return treeTreversal_symbol(tree->left, bitArray, ptr);
    else // if (bit == 1)
        return treeTreversal_symbol(tree->right, bitArray, ptr);
}

TBitArray* decompressFile(TBitArray* fileBitArray, TTree* tree)
{
    TBitArray* ret = malloc(sizeof(TBitArray));
    *ret = bitArrayEmpty();

    int ptr = 0;
    for (; ptr < fileBitArray->cnt; )
    {
        int symbol = 0xff & treeTreversal_symbol(tree, fileBitArray, &ptr);
        bitArrayPushByte(ret, symbol);
    }

    return ret;
}

void decompressArchive(TBitArray* archiveBitArray)
{
    int ptr = 0;
    int lenCompressedArchive = bitArrayReadInt(archiveBitArray, ptr);
    ptr += bitInByte * sizeof(int);

    int cntOfFiles = bitArrayReadInt(archiveBitArray, ptr);
    ptr += bitInByte * sizeof(int);

    for (int i = 0; i < cntOfFiles; i++)
    {
        int filePtr = 0;

        int lenFile = bitArrayReadInt(archiveBitArray, ptr);
        ptr += bitInByte * sizeof(int);
        filePtr += bitInByte * sizeof(int);

        int lenFileUncompressed = bitArrayReadInt(archiveBitArray, ptr);
        ptr += bitInByte * sizeof(int);
        filePtr += bitInByte * sizeof(int);

        int lenName = bitArrayReadInt(archiveBitArray, ptr);
        ptr += bitInByte * sizeof(int);
        filePtr += bitInByte * sizeof(int);
        char* nameFile = malloc((lenName + 1) * sizeof(char));

        for (int j = 0; j < lenName; j++) {
            nameFile[j] = bitArrayReadByte(archiveBitArray, ptr);
            ptr += bitInByte;
            filePtr += bitInByte;
        }
        nameFile[lenName] = '\0';

        
        int lenTree = bitArrayReadInt(archiveBitArray, ptr);
        ptr += bitInByte * sizeof(int);
        filePtr += bitInByte * sizeof(int);

        TBitArray* treeBitArray = malloc(sizeof(TBitArray));
        *treeBitArray = bitArrayEmpty();

        for (int j = 0; j < lenTree; j++)
        {
            int bit = bitArrayReadBit(archiveBitArray, ptr);
            ptr++;
            filePtr++;

            bitArrayPushBit(treeBitArray, bit);
        }

        int tempPtr = 0;
        TTree* tree = treeTreversal_array2tree(treeBitArray, &tempPtr);

        TBitArray* file = malloc(sizeof(TBitArray));
        *file = bitArrayEmpty();

        for (; filePtr < lenFile; )
        {
            int bit = bitArrayReadBit(archiveBitArray, ptr);
            ptr++;
            filePtr++;
            bitArrayPushBit(file, bit);
        }

        TBitArray* decompressedFile = decompressFile(file, tree);
        int cntByte = decompressedFile->cnt / (bitInByte * sizeof(word));
        
        if (cntByte != lenFileUncompressed) {
            printf("File name: %s\n", nameFile);
            warning("Size of file doesn't match\n");
        }

        createFileFromBitArray(decompressedFile, nameFile);
    }
}

int main()
{
    int mode = 1;
    if (mode == 0)
    {
        char name0[] = "F:/Programms/Bandicam/Capture/project.mp4";
        char name1[] = "tests/archiver.bmp";
        char** link = malloc(2*sizeof(char*));
        link[0] = name0;
        link[1] = name1;

        TBitArray* temp = createArchive(link, 2);

        createFileFromBitArray(temp, "biba");
    }

    else if (mode == 1)
    {
        char name[] = "biba";

        TBitArray* temp = readArchiveFromFile(name);

        decompressArchive(temp);
    }

    return 0;
}
