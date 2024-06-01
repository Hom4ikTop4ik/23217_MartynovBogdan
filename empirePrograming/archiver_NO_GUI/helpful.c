#include "archiver.h"

void returnError(const char* msg) {
    printf(msg);
    exit(EXIT_FAILURE);
}
void warning(const char* msg) {
    printf(msg);
}



// if (file exists)  return 1;
// else  return 0;
int fileExists(char* fileName)
{
    FILE* file = fopen(fileName, "rb");

    int ret = 0;
    if (file == NULL)
        ret = 0;
    else {
        fclose(file);
        ret = 1;
    }

    return ret;
}

// 0 - no rewrite
// 1 - rewrite
// 2 - don't save
int doYouWantRewriteFile(const char* fileName)
{
    int rewrite = 0;
    printf("Do you want rewrite file \"%s\"? (0 - input other name, 1 - rewrite file, 2 - don't save file): ", fileName);
    int scanRes = scanf("%d", &rewrite);

    getc(stdin); // skip '\n'

    if (scanRes == 1)
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

const char* discardPath(const char* filePath)
{
    if (filePath == NULL)
        return NULL;

    int len = strlen(filePath);
    for (int i = len - 1; i >= 0; i--)
        if (filePath[i] == '\\' || filePath[i] == '/')
            return filePath + (i + 1);

    return filePath;
}


void swapTree(TTree** a, TTree** b) {
    TTree* t = *a;
    *a = *b;
    *b = t;
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


// insert value (1 int) in "vershina" like in stack
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
    if (vector == NULL)
        return 0;

    if (vector->cnt <= 0)
        return 0;

    vector->cnt--;
    int ret = vector->arr[vector->cnt];
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
    if (heap == NULL)
        return NULL;
    if (heap->arr == NULL)
        return NULL;
    if (heap->cnt <= 0)
        return NULL;

    TTree* ret = heap->arr[0];
    heap->cnt--;
    swapTree(&heap->arr[0], &heap->arr[heap->cnt]);
    heapSiftDown(heap, 0);
    return ret;
}

void bitArrayPushBit(TBitArray* bitArray, int bitValue)
{
    if (bitArray->cnt * (bitInByte * sizeof(byte)) >= bitArray->cap)
    {
        bitArray->cap = 1 + 2 * bitArray->cap;
        bitArray->arr = realloc(bitArray->arr, bitArray->cap * sizeof(byte));

        if (bitArray->arr == NULL)
            returnError("realloc returned NULL (bitArrayPushBit)\n");
    }

    int curBit = bitArray->cnt % (bitInByte * sizeof(byte));
    int curByte = bitArray->cnt / (bitInByte * sizeof(byte)); // byte where at least 1 free bit

    int mask = 0;
    for (int i = 0; i < curBit; i++) {
        mask <<= 1;
        mask |= 1;
    }
    mask <<= ((bitInByte * sizeof(byte)) - curBit);

    bitArray->arr[curByte] &= mask; // clear bits after curBit
    bitArray->arr[curByte] |= (bitValue << ((bitInByte * sizeof(byte)) - 1 - curBit)); // add bit to array

    bitArray->cnt++;
}

void bitArrayPushByte(TBitArray* bitArray, int byteValue)
{
    if ((bitArray->cnt) * (bitInByte * sizeof(byte)) + 1/*byte*/ >= bitArray->cap)
    {
        bitArray->cap = 1 + 2 * bitArray->cap;
        bitArray->arr = realloc(bitArray->arr, bitArray->cap * sizeof(byte));

        if (bitArray->arr == NULL)
            returnError("realloc returned NULL (bitArrayPushBit)\n");
    }

    for (int i = bitInByte * sizeof(byte) - 1; i >= 0; i--)
        bitArrayPushBit(bitArray, (byteValue >> i) & 1);
}

int bitArrayReadBit(TBitArray* bitArray, int index)
{
    if (index < 0)
        returnError("Read bit before array [bitArrayReadBit]\n");
    if (index >= bitArray->cnt)
        return 0;
    //returnError("Read bit after array [bitArrayReadBit]\n");

    int curByte = index / (bitInByte * sizeof(byte));
    int curBit = index % (bitInByte * sizeof(byte));

    int value = bitArray->arr[curByte];
    value &= (1 << bitInByte * sizeof(byte) - 1 - curBit);
    value >>= bitInByte * sizeof(byte) - 1 - curBit; // shift back
    return value;
}

int bitArrayReadByte(TBitArray* bitArray, int index)
{
    int value = 0;
    for (int i = 0; i < bitInByte * sizeof(byte); i++) {
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

void bitArrayWriteBit(TBitArray* bitArray, int value, int index)
{
    if (index < 0)
        returnError("Write bit before array [bitArrayWrireBit]\n");
    if (index >= bitArray->cnt) {
        bitArrayPushBit(bitArray, value);
        return;
    }

    int curByte = index / (bitInByte * sizeof(byte));
    int curBit = index % (bitInByte * sizeof(byte));

    int mask = ~(1 << (bitInByte * sizeof(byte) - 1 - curBit)); // set zero in mask[curBit] and set one otherwise
    bitArray->arr[curByte] &= mask;
    bitArray->arr[curByte] |= (1 & value) << ((bitInByte * sizeof(byte) - 1) - curBit); // set value in mask[curBit]

}

void bitArrayWriteByte(TBitArray* bitArray, int value, int index)
{
    for (int i = 0; i < bitInByte; i++) {
        int bit = (value >> (bitInByte - 1 - i)) & 1;
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
    bitArrayWriteByte(bitArray, hiLo, index + 2 * bitInByte);
    bitArrayWriteByte(bitArray, hiHi, index + 3 * bitInByte);
}



void fileWriteByte(FILE* file, byte value)
{
    fwrite(&value, sizeof(byte), 1, file);
}

void fileWriteInt(FILE* file, int value)
{
    fwrite(&value, sizeof(int), 1, file);
}

void fileRewriteByte(FILE* file, byte value, long long pos)
{
    long long curPos = ftell(file);
    fseek(file, pos - curPos, SEEK_CUR);
    fwrite(&value, sizeof(byte), 1, file);
    fseek(file, curPos - pos, SEEK_CUR);
}

void fileRewriteInt(FILE* file, int value, long long pos)
{
    long long curPos = ftell(file);
    fseek(file, pos - curPos, SEEK_CUR);
    fwrite(&value, sizeof(int), 1, file);
    fseek(file, curPos - pos - sizeof(int), SEEK_CUR);
}