#include "archiver.h"

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
    free(buffer);
    
    return gystogram;
}

TTree* haffmanTree(int* gystogram)
{
    THeap* heap = malloc(sizeof(THeap));
    *heap = heapEmpty();

    TTree* arr[differentBytes];
    for (int i = 0; i < differentBytes; i++)
    {
        if (compressTree == 1 && gystogram[i] == 0)
            continue;

        arr[i] = malloc(sizeof(TTree));
        *(arr[i]) = treeEmpty();
        arr[i]->size = gystogram[i];
        arr[i]->value = i;

        heapPush(heap, arr[i]);
    }

    while (heap->cnt > 1)
    {
        TTree* tree1 = heapPop(heap);
        TTree* tree2 = heapPop(heap);

        TTree* treeNew = malloc(sizeof(TTree));
        *treeNew = treeEmpty();
        treeNew->left = tree1;
        treeNew->right = tree2;
        treeNew->size = tree1->size + tree2->size;

        heapPush(heap, treeNew);
    }

    TTree* ret = heapPop(heap);

    freeHeap(heap);

    return ret;
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



TBitArray* haffmanTree2BitArray(TTree* tree, TBitArray** arrays)
{
    TBitArray* ret = malloc(sizeof(TBitArray));
        *ret = bitArrayEmpty();
        bitArrayPushByte(ret, 0);
        bitArrayPushByte(ret, 0);
        bitArrayPushByte(ret, 0);
        bitArrayPushByte(ret, 0);

    TVector* stack = malloc(sizeof(TVector)); 
    *stack = vectorEmpty();

    int treeLen = treeTreversal_tree2array(tree, ret, arrays, stack);
    freeVector(stack);

    bitArrayWriteInt(ret, treeLen, 0);

    return ret;
}

TBitArray* compress(const char* fileName)
{
    int* gystogram = gysto(fileName);
    if (gystogram == NULL)
        return NULL;

    FILE* file = fopen(fileName, "rb");
    if (file == NULL) {
        printf("File name: %s\n", fileName);
        returnError("Can't open file [compress]\n");
    }

    fileName = discardPath(fileName); // in archive I want save only name of file, not path
    int strLenght = strlen(fileName) + 1;

    TTree* tree = haffmanTree(gystogram);
    free(gystogram);

    TBitArray** arrays = malloc(differentBytes * sizeof(TBitArray*));   
    for (int i = 0; i < differentBytes; i++)
    {
        arrays[i] = malloc(sizeof(TBitArray));
        *(arrays[i]) = bitArrayEmpty();
    }
    TBitArray* haffmanBitArray = haffmanTree2BitArray(tree, arrays);
    freeTree(tree);

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

    int bitLenFile = 3 * sizeof(int) * bitInByte + strLenght * bitInByte;

    // push tree len and haffman tree into file
    for (int i = 0; i < haffmanBitArray->cnt; i++)
    {
        int bit = bitArrayReadBit(haffmanBitArray, i);
        bitArrayPushBit(compressedFile, bit);
        bitLenFile++;
    }
    freeBitArray(haffmanBitArray);

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

    fclose(file);

    for (int i = 0; i < differentBytes; i++)
        freeBitArray(arrays[i]);
    free(arrays);

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
    int fileLenght = 2 * sizeof(int) * bitInByte; // for realCnt and fileLenght

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
        if (fileExists(fileNames[i]) == 0) {
            printf("Can't open file: %s\n", fileNames[i]);
            continue;
        }

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
        freeBitArray(temp);
    }

    bitArrayWriteInt(file, fileLenght, 0);
    bitArrayWriteInt(file, realCnt, bitInByte * sizeof(int));
    return file;
}