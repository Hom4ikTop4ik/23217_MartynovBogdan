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

    byte* buffer = (byte*)malloc(bufferSize);

    size_t done = 0;
    while (done < file_size)
    {
        int cnt = fread(buffer, sizeof(byte), bufferSize, file);
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
        bitArrayPushBit(bitArray, 1); // push "message": "now will byte, after read byte go up"
        bitArrayPushByte(bitArray, tree->value); // push byte

        for (int i = 0; i < stack->cnt; i++)
            bitArrayPushBit(arrays[tree->value], stack->arr[i]);

        return 1 + bitInByte * sizeof(byte);
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
    *ret = bitArrayEmpty_tree();
        //*ret = bitArrayEmpty();
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

// return 1 if file writed
int compress(FILE* file, const char* fileName)
{
    int* gystogram = gysto(fileName);
    if (gystogram == NULL)
        return 0;

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

    long long fileStartPtr = ftell(file);

    // reserve memory for len compressed file, len uncompressed file, len its name and fileName
    fileWriteInt(file, 0);
        long long intSizeInFile = ftell(file) - fileStartPtr;
    fileWriteInt(file, 0);
    fileWriteInt(file, strLenght);

    for (int i = 0; i < strLenght; i++)
        fileWriteByte(file, 0xff & fileName[i]);

    int bitLenFile = (haffmanBitArray->cnt / bitInByte + (haffmanBitArray->cnt % bitInByte > 0)) + (3 * sizeof(int) + strLenght);
    bitLenFile *= bitInByte;

    // push tree len and haffman tree into file
    for (int i = 0; i < haffmanBitArray->cnt;)
    {
        int byteValue = 0;
        int cnt = min(bitInByte, haffmanBitArray->cnt -i);
        for (int j = 0; j < cnt; j++)
        {
            byteValue <<= 1;
            byteValue |= (1 & bitArrayReadBit(haffmanBitArray, i));
            i++;
        }
        byteValue <<= (bitInByte - cnt);

        fileWriteByte(file, byteValue);
    }
    freeBitArray(haffmanBitArray);

    byte buffer[bufferSize] = { 0 };
    TBitArray bufferSend = bitArrayEmpty();
        // + differentBytes because max arrays[byte]->cnt is max tree depth, i.e. differentBytes
        bufferSend.cap = (bufferSize + differentBytes + 1);
        bufferSend.cnt = 0;
        bufferSend.arr = malloc(bufferSend.cap);

    int byteCnt = 0; // count of bytes in uncompressed file

    FILE* fileToCompress = fopen(fileName, "rb");

    int bitLen = 0;
    while (feof(fileToCompress) == 0)
    {
        int cnt = fread(buffer, sizeof(byte), bufferSize, fileToCompress);
        byteCnt += cnt;

        for (int i = 0; (i < bufferSize) && (i < cnt); i++)
        {
            // send from bufferSend to file
            if (bufferSend.cnt >= bufferSize * bitInByte)
            {
                for (int i = 0; i < bufferSize; i++) 
                    fileWriteByte(file, bufferSend.arr[i]);

                for (int i = bufferSize; i < (bufferSend.cnt / bitInByte + (bufferSend.cnt % bitInByte > 0)); i++)
                    bufferSend.arr[i - bufferSize] = bufferSend.arr[i];

                bufferSend.cnt -= bufferSize * bitInByte;
            }

            // read byte from file and compress its
            int byte = buffer[i] & 0xff;
            for (int j = 0; j < arrays[byte]->cnt; j++)
            {
                int bit = bitArrayReadBit(arrays[byte], j);
                bitArrayPushBit(&bufferSend, bit);
                bitLen++;
            }
        }

        if (cnt < bufferSize)
            break;
    }
    bitLenFile += bitLen;

    for (int i = 0; i < bufferSend.cnt / bitInByte + (bufferSend.cnt % bitInByte > 0); i++)
        fileWriteByte(file, bufferSend.arr[i]);

    fclose(fileToCompress);
    
    free(bufferSend.arr);

    for (int i = 0; i < differentBytes; i++)
        freeBitArray(arrays[i]);
    free(arrays);

    fileRewriteInt(file, bitLen, fileStartPtr);
    fileRewriteInt(file, byteCnt, fileStartPtr + intSizeInFile);

    return 1;
}

void createArchive(char** archiveName, char** fileNames, int cnt)
{
    if (archiveName == NULL) {
        archiveName = malloc(2 * sizeof(char));
        *(archiveName[0]) = '1';
        *(archiveName[1]) = '\0';
    }

    int realCnt = 0;
    int fileLenght = 2 * sizeof(int) * bitInByte; // for realCnt and fileLenght

    while (fileExists(*archiveName) != 0) {
        int res = doYouWantRewriteFile(*archiveName);
        if (res == 2) {
            return; // don't save file
        }
        else if (res == 1)
            break; // rewrite file
        else if (res == 0) {
            free(*archiveName);
            *archiveName = getOtherName();
        }
    }
    FILE* file = fopen(*archiveName, "wb");

    long long startArchivePos = ftell(file);
    fileWriteInt(file, 0); // archive size
    long long intSizeInFile = ftell(file) - startArchivePos;
    fileWriteInt(file, 0); // count of files

    for (int i = 0; i < cnt; i++)
    {
        if (fileExists(fileNames[i]) == 0) {
            printf("Can't open file: %s\n", fileNames[i]);
            continue;
        }

        realCnt += compress(file, fileNames[i]);
    }

    fseek(file, 0, SEEK_END);
    long long archSize = ftell(file);

    fileRewriteInt(file, archSize, startArchivePos);
    fileRewriteInt(file, realCnt , startArchivePos + intSizeInFile);

    fclose(file);
}