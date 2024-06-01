#include "archiver.h"

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
    {
        printf("Archive name: %s\n", archiveName);
        returnError("Can't open archive [readArchiveFromFile]\n");
    }

    TBitArray* bitArray = malloc(sizeof(TBitArray));
    if (bitArray == NULL)
        returnError("malloc returned NULL [readArchiveFromFile]\n");
    *bitArray = bitArrayEmpty();

    word* buffer = (word*)malloc(bufferSize);


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

    fclose(file);
    free(buffer);
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
        freeBitArray(treeBitArray);

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
        freeBitArray(file);
        freeTree(tree);
        int cntByte = decompressedFile->cnt / (bitInByte * sizeof(word));

        if (cntByte != lenFileUncompressed) {
            printf("File name: %s\n", nameFile);
            warning("Size of file doesn't match\n");
        }

        createFileFromBitArray(decompressedFile, &nameFile);
        freeBitArray(decompressedFile);
        free(nameFile);
    }
}