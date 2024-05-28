#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// for UNIX or LINUX (i'm not sure): 
//#include <unistd.h> // узнать есть ли такой файл
//#include <libgen.h> // отбросить путь до файла, оставив только название

// === Constant and Macro Definitions ===
#define DEBUG 0 

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

typedef struct STree {
    int value;
    struct STree* left;
    struct STree* right;
    int size;
} TTree;

typedef struct SHeap {
    TTree* arr;
    int cap;
    int cnt;
} THeap;

typedef struct SVector {
    int* arr;
    int cap;
    int cnt;
} TVector;

// ByteArray:
// ARRay of bytes
// CAPacity (in bytes)
// CouNT of bits
typedef struct SBitArray {
    word* arr;
    int cap;
    int cnt;
} TBitArray;


void swapTree(TTree* a, TTree* b) {
    TTree t = *a;
    *a = *b;
    *b = t;
}

TTree treeEmpty() {
    TTree tree;
    tree.left = NULL;
    tree.right = NULL;
    tree.value = 0;
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

void returnError(const char* msg)
{
    printf(msg);
    exit(EXIT_FAILURE);
}

// insert value (1 word) in "vershina" like in stack
// return index where value "lejit"
int vectorPush(TVector* vector, int value)
{
    if (vector->cnt >= vector->cap)
    {
        vector->cap = 1 + 2 * vector->cap;
        vector->arr = realloc(vector->arr, vector->cap);

        if (vector->arr == NULL) 
            returnError("realloc returned NULL (vectorPush)\n");
    }

    vector->arr[vector->cnt++] = value;

    return vector->cnt - 1;
}

int vectorPop(TVector* vector) {
    return vector->arr[vector->cnt--];
}

void heapSiftUp(THeap* heap, int index)
{
    if (index == 0) return;

    int parent = (index - 1) / 2;

    // if cur value is less than parent value
    if ((heap->arr[index].value) < (heap->arr[parent].value)) {
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
        if (heap->arr[left].value < heap->arr[right].value)
            child = left;
        else
            child = right;
    }

    if (heap->arr[child].value < heap->arr[index].value) {
        swapTree(&heap->arr[child], &heap->arr[index]);
        heapSiftDown(heap, child);
    }
}

void heapPush(THeap* heap, TTree tree)
{
    if (heap->cnt >= heap->cap)
    {
        heap->cap = 1 + 2 * heap->cap;
        heap->arr = realloc(heap->arr, heap->cap);
        if (heap->arr == NULL) 
            returnError("realloc returned NULL (heapPush)\n");
    }

    heap->arr[heap->cnt] = tree;
    heapSiftUp(heap, heap->cnt);
    heap->cnt++;
}

TTree heapPop(THeap* heap)
{
    TTree ret = heap->arr[0];
    swapTree(&heap->arr[0], &heap->arr[heap->cnt - 1]);
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

    fseek(file, 0, SEEK_END);
    size_t data_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    word* buffer = (word*)malloc(bufferSize);

    size_t done = 0;
    while (done < data_size)
    {
        int cnt = fread(buffer, sizeof(word), bufferSize, file);

        if (ferror(file) != 0)
            returnError("Error in file [gysto, while(done < dataSize]\n");

        for (int i = 0; i < bufferSize; i++) 
            gystogram[buffer[i] % differentBytes]++;

        done += cnt;

        if (cnt < bufferSize)
            break; // eof
    }

    fclose(file);
    return gystogram;
}

TTree haffmanTree(int* gystogram)
{
    THeap heap = heapEmpty();

    TTree arr[differentBytes];
    for (int i = 0; i < differentBytes; i++)
    {
        arr[i] = treeEmpty();
        arr[i].size = gystogram[i];
        arr[i].value = i;

        heapPush(&heap, arr[i]);
    }

    while (heap.cnt > 1)
    {
        TTree* tree1 = malloc(sizeof(TTree));
        *tree1 = heapPop(&heap);
        TTree* tree2 = malloc(sizeof(TTree));
        *tree2 = heapPop(&heap);

        TTree treeNew = treeEmpty();
        treeNew.left = tree1;
        treeNew.right = tree2;
        treeNew.size = tree1->size + tree2->size;

        heapPush(&heap, treeNew);
    }

    TTree ret = heapPop(&heap);

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

    for (int i = bitInByte * sizeof(word) - 1; i >= 0; i--)
        bitArrayPushBit(bitArray, (byteValue >> i) & 1);

    //int curBit = bitArray->cnt % (bitInByte * sizeof(word));
    //int curByte = bitArray->cnt / (bitInByte * sizeof(word));

    //int mask = 0;
    //for (int i = 0; i < curBit; i++) {
    //    mask <<= 1;
    //    mask |= 1;
    //}
    //mask <<= ((bitInByte * sizeof(word)) - curBit);

    //bitArray->arr[curByte] &= mask; // clear bits after curBit

    //bitArray->arr[curByte] |= (byteValue >> curBit);
    //bitArray->arr[curByte + 1] |= byteValue << ((bitInByte * sizeof(word)) - curBit);

    //bitArray->cnt += (bitInByte * sizeof(word));
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

        vectorPop(stack);

        return 1 + bitInByte * sizeof(word);
    }
    else if (temp == 0)
    {
        int cnt = 2; // push 2 bits
        bitArrayPushBit(bitArray, 0); // push "message": "go down in left branch"
        vectorPush(stack, 0);
        cnt += treeTreversal_tree2array(tree->left, bitArray, arrays, stack); // open left branch

        bitArrayPushBit(bitArray, 0); // push "message": "go down in right branch"
        vectorPush(stack, 1);
        cnt += treeTreversal_tree2array(tree->right, bitArray, arrays, stack); // open right branch

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
        //returnError("Write bit after array [bitArrayWrireBit]\n");

        bitArrayPushBit(bitArray, value);
        return;
    }

    int curByte = index / (bitInByte * sizeof(word));
    int curBit = index % (bitInByte * sizeof(word));

    int mask = ~ (1 << (bitInByte*sizeof(word) -1 -curBit)); // set zero in mask[curBit] and set one otherwise
    mask |= (1 & value) << (  (bitInByte*sizeof(word) -1) -curBit  ); // set value in mask[curBit]

    bitArray->arr[curByte] &= mask;
}

void bitArrayWriteByte(TBitArray* bitArray, int value, int index)
{
    for (int i = 0; i < bitInByte; i++) {
        int bit = (value >> i) & 1;
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
        /*returnError("Read bit after array [bitArrayReadBit]\n");*/

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
    for (int i = 0 ; i < bitInByte*sizeof(word); i++) 
        value |= (1 & bitArrayReadBit(bitArray, index + i)) << (bitInByte - 1);

    return value;
}

int bitArrayReadInt(TBitArray* bitArray, int index)
{
    int value = 0;

    for (int i = 0; i < sizeof(int); i++)
        value |= (0xff & bitArrayReadByte(bitArray, index + i * bitInByte)) << i * bitInByte;
}


TBitArray* haffmanTree2BitArray(TTree* tree, TBitArray*** pleaseReturnArrays)
{
    TBitArray* ret = malloc(sizeof(TBitArray));
    ret->cap = sizeof(int); // 1 byte/word, int is 4 byte usually 
    ret->cnt = ret->cap * bitInByte * sizeof(word);
    ret->arr = malloc(ret->cap * sizeof(word));

    TBitArray** arrays = malloc(differentBytes * sizeof(TBitArray*));
    for (int i = 0; i < differentBytes; i++) {
        arrays[i] = malloc(sizeof(TBitArray));
        *arrays[i] = bitArrayEmpty();
    }
    TVector stack = vectorEmpty();

    int treeLen = ret->cnt; // bytes for int counting too
    treeLen += treeTreversal_tree2array(tree, ret, arrays, &stack);

    bitArrayWriteInt(ret, treeLen, 0);

    *pleaseReturnArrays = arrays;
    return ret;
}


// returned value is 3 bytes:  (no, msg, no, bit)
// messages:
// 2 - error. Reading before array (bit_index < 0)
// 3 - error. Reading after array (bit_index > array.len)
// 4 - error. Array is NULL
int bitArrayGetBit(TBitArray* bitArray, int bitPtr)
{
    if (bitPtr < 0)
        return bytes2int(0, 2, 0, 0); // trying read bit before array
    if (bitPtr >= bitArray->cnt)
        return bytes2int(0, 3, 0, 0); // trying read bit after array
    if (bitArray->arr == NULL)
        return bytes2int(0, 4, 0, 0); // array is NULL

    int curBit = bitArray->cnt % (bitInByte * sizeof(word));
    int curByte = bitArray->cnt / (bitInByte * sizeof(word));

    int mask = 0;
    mask |= 1 << ((bitInByte * sizeof(word)) - 1 - curBit);

    int bit = mask & bitArray->arr[curByte];

    return bytes2int(0, 0, 0, bit);
}

// returned value is 3 bytes:  (no, msg, cnt, word)
// messages:
// 1 - not full word read.
// 2 - error. Reading before array (bit_index < 0)
// 3 - error. Reading after array (bit_index > array.len)
// 4 - error. Array is NULL.
// 5 - error. Error while reading bits
int bitArrayGetByte(TBitArray* bitArray, int leftBitPtr)
{
    if (bitArray->arr == NULL)
        return bytes2int(0, 4, 0, 0);

    if (leftBitPtr >= bitArray->cnt)
        return bytes2int(0, 3, 0, 0);

    // [left; right)
    int rightBitPtr = leftBitPtr + bitInByte * sizeof(word);
    if (rightBitPtr <= 0)
        return bytes2int(0, 2, 0, 0);

    leftBitPtr = max(leftBitPtr, 0); // if (left < 0) left = 0
    rightBitPtr = min(rightBitPtr, bitArray->cnt); // if (right > array.len) right = array.len; 

    int cntBits = rightBitPtr - leftBitPtr;
    int retByte = 0;

    int realCnt = 0;

    for (int i = 0; i < cntBits; i++)
    {
        int temp = bitArrayReadBit(bitArray, leftBitPtr + i);
        int musor, msg, bit;
        int2bytes(temp, musor, msg, musor, bit);

        if (msg == 0) {
            retByte <<= 1;
            retByte |= (1 & bit);
            realCnt++;
        }
    }

    if (realCnt < cntBits)
        return bytes2int(0, 1, realCnt, retByte);

    // otherwise all good
    return bytes2int(0, 0, cntBits, retByte);
}

TBitArray* compress(const char* fileName)
{
    int strLenght = strlen(fileName) + 1;

    int* gystogram = gysto(fileName);
    if (gystogram == NULL) 
        return NULL;

    TTree tree = haffmanTree(gystogram);

    TBitArray** arrays;
    TBitArray* bitArray = haffmanTree2BitArray(&tree, &arrays);

    TBitArray* compressedFile = malloc(sizeof(TBitArray));
        *compressedFile = bitArrayEmpty();
        compressedFile->cap = 2*sizeof(int) + strLenght; // reserve memory for len compressed file, len its name and fileName
        compressedFile->cnt = compressedFile->cap * bitInByte * sizeof(word);
        compressedFile->arr = malloc(compressedFile->cap * sizeof(word));

    FILE* file = fopen(fileName, "rb");
    if (file == NULL) {
        printf("File name: %s\n", fileName);
        returnError("Can't open file [compress]\n");
    }

    word buffer[bufferSize] = { 0 };

    int bitLenFile = 0;

    while (feof(file) == 0)
    {
        int cnt = fread(buffer, sizeof(word), bufferSize, file);

        for (int i = 0; (i < bufferSize) && (i < cnt); i++)
        {
            int byte = buffer[i] & 0xff;
            for (int j = 0; j < arrays[byte]->cnt; j++)
            {
                int bit = bitArrayReadBit(arrays[byte]->arr, j);
                bitArrayPushBit(compressedFile, bit);
            }
            bitLenFile += arrays[byte]->cnt;
        }

        if (cnt < bufferSize)
            break;
    }

    bitLenFile += sizeof(int) * bitInByte;
    bitLenFile += sizeof(int) * bitInByte;
    bitLenFile += strLenght * bitInByte;

    bitArrayWriteInt(compressedFile, bitLenFile, 0);
    bitArrayWriteInt(compressedFile, strLenght, 0);
    for (int i = 0; i < strLenght; i++) 
        bitArrayWriteByte(compressedFile, fileName[i], sizeof(int) + i);

    return compressedFile;
}

TBitArray* createArchive(char** fileNames, int cnt)
{
    int realCnt = 0;
    int fileLenght = 2*sizeof(int); // for realCnt and fileLenght

    TBitArray* file = malloc(sizeof(TBitArray));
        *file = bitArrayEmpty();
        file->cap = fileLenght; // memory for archive's len and count of successful files
        file->cnt = file->cap * bitInByte * sizeof(word);
        file->arr = malloc(file->cap * sizeof(word));

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
    bitArrayWriteInt(file, realCnt, sizeof(int));

    return file;
}

// if (file exists) 
//      return 1;
// else 
//      return 0;
int fileExists(char* fileName)
{
    FILE* file = fopen(fileName, "rb");
    if (file == NULL)
        return 0;
    
    return 1;
}

int doYouWantRewriteFile()
{
    int rewrite = 0;
    printf("do You Want Rewrite File? (1 - yes, 0 - no): ");
    int temp = scanf("%d", &rewrite);

    if (temp == 1)
        return rewrite;
    else
        returnError("Error in scanf [doYouWantRewriteFile]\n");
}

char* getOtherName()
{
    int size = max(200, fileNameBufferSize);

    char* str = malloc(sizeof(char) * (size + 1));
    printf("Input new file name (not more 15 symbols): ");

    char temp = 0;
    int i = 0;
    do {
        scanf("%c", &temp);
        if (i < size && (temp != '\0') && (temp != '\n') && (temp != '\r')) {
            str[i] = temp;
            i++;
        }
        #ifdef DEBUG
        else if (DEBUG > 0)
            printf("'%d %c' ", temp, temp);
        #endif 
    } while ((temp != '\0') && (temp != '\n') && (temp != '\r'));

    str[i] = '\0';

    return str;
}

char* discardPath(char* filePath)
{
    if (filePath == NULL)
        return NULL;

    int len = strlen(filePath);
    for (int i = len - 1; i >= 0; i++)
        if (filePath[i] == '\\' || filePath[i] == '/')
            return filePath + (i + 1);

    return filePath;
}


void createFileFromBitArray(TBitArray* bitArray, char* fileName)
{
    FILE* file;
    while (fileExists(fileName) != 0) {
        if (doYouWantRewriteFile() == 1)
            break;
        else
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
    *ptr++;

    TTree* ret = malloc(sizeof(TTree));
    *ret = treeEmpty();

    if (bit == 1) { // isLeaf()
        int byte = bitArrayReadByte(bitArray, *ptr);
        *ptr += bitInByte;
        ret->value = byte;
    }

    else //if (bit == 0)
    {
        ret->left = treeTreversal_array2tree(bitArray, ptr);
        bit = bitArrayReadBit(bitArray, *ptr);
        *ptr++;

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

        for (int i = 0; (i < bufferSize) && (i < cnt); i++)
        {
            int byte = buffer[i] & 0xff;
            bitArrayPushByte(bitArray, byte); 
        }

        if (cnt < bufferSize)
            break;
    }

    return bitArray;
}

void decompressFile(TBitArray* fileBitArray, TTree* tree)
{
    // принять bitArray со сжатым файлом и дерево Хаффмана
    // разжать файл, создав новый bitArray
    // вернуть bitArray с разжатым файлом
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
        // считать длину сжатого файла
        // считать длину названия файла
        // считать название файла
        
        // считать длину дерева
        // считать дерево
        // считать сжатый файл, запихать в bitArray
        
        // передать дерево и bitArray в decompressFile

        // получить разжатый bitArray и передать в создание файла createFileFromBitArray
    }
}

int main()
{
    return 0;
}
