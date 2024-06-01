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

    byte* buffer = (byte*)malloc(bufferSize);


    while (feof(file) == 0)
    {
        int cnt = fread(buffer, sizeof(byte), bufferSize, file);

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

int treeTreversal_symbol(FILE* archive, TTree* tree, int* ptr, int lenCompressedFile)
{
    if (tree == NULL)
        returnError("Tree broke - tree is NULL [treeTreversal_symbol].\n");

    if (isLeaf(tree) == 2)
        returnError("Tree broke - only 1 of 2 branches [treeTreversal_symbol].\n");
    if (isLeaf(tree) == 1)
        return (0xff & tree->value);


    if (*ptr == lenCompressedFile)
        warning("Tree broke - not enough bits [treeTreversal_symbol].\n");

    if (feof(archive) != 0) // if eof
        returnError("Tree broke - not enough bits (eof) [treeTreversal_symbol].\n");

    if (ferror(archive) != 0)
        returnError("Error with archive file [treeTreversal_symbol].\n");

    int byteValue = 0;
    fread(&byteValue, sizeof(byte), 1, archive);

    int shift = (*ptr) % bitInByte;
    int bit = byteValue & (1 << (bitInByte -1 -shift));

    (*ptr)++;
    if (   (((*ptr) % bitInByte) != 0)   &&   ((*ptr) < lenCompressedFile)   )
    {
        int check = ungetc(byteValue, archive);
        if (check != byteValue)
            printf("%d %d\n", check, byteValue);
            //warning("Liitle problem while reading archive - ungetc [treeTreversal_symbol].\n");
    }

    if (bit == 0)
        return treeTreversal_symbol(archive, tree->left, ptr, lenCompressedFile);
    else // if (bit == 1)
        return treeTreversal_symbol(archive, tree->right, ptr, lenCompressedFile);
}

// user don't want save file — return 1
// otherwise — return 0
int decompressFile(FILE* archive, TTree* tree, int lenCompressedFile, char** nameFile)
{
    while (fileExists(*nameFile) != 0) {
        int res = doYouWantRewriteFile(*nameFile);
        if (res == 2) {
            return 1; // don't save file
        }
        else if (res == 1)
            break; // rewrite file
        else if (res == 0) {
            free(*nameFile);
            *nameFile = getOtherName();
        }
    }
    FILE* file = fopen(*nameFile, "wb");

    int i = 0;
    while (i < lenCompressedFile)
    {
        byte byteValue = 0xff & treeTreversal_symbol(archive, tree, &i, lenCompressedFile);

        fwrite(&byteValue, sizeof(byte), 1, file);
    }

    fclose(file);
}

void decompressArchive(char* archiveName)
{
    FILE* archive = fopen(archiveName, "rb");
    if (archive == NULL)
    {
        printf("Archive name: %s\n", archiveName);
        returnError("Can't open archive [readArchiveFromFile]\n");
    }

    int ptr = 0;
    int lenCompressedArchive = 0;;
    fread(&lenCompressedArchive, sizeof(int), 1, archive);
    ptr += sizeof(int);

    int cntOfFiles = 0;
    fread(&cntOfFiles, sizeof(int), 1, archive);
    ptr += sizeof(int);

    for (int i = 0; i < cntOfFiles; i++)
    {
        int filePtr = 0;

        int lenFile = 0;
        fread(&lenFile, sizeof(int), 1, archive);
        ptr += sizeof(int);

        int lenFileUncompressed = 0;
        fread(&lenFileUncompressed, sizeof(int), 1, archive);
        ptr += sizeof(int);

        int lenName = 0;
        fread(&lenName, sizeof(int), 1, archive);
        ptr += sizeof(int);

        char* nameFile = malloc((lenName + 1) * sizeof(char));
        fread(nameFile, sizeof(char), lenName, archive);
        nameFile[lenName] = '\0';
        ptr += lenName;

        int lenTree = 0;
        fread(&lenTree, sizeof(int), 1, archive);
        ptr += sizeof(int);

        int byteLenTree = lenTree / bitInByte + (lenTree % bitInByte > 0);

        TBitArray treeBitArray = bitArrayEmpty();
            treeBitArray.cap = byteLenTree;
            treeBitArray.arr = malloc(treeBitArray.cap);

        fread(treeBitArray.arr, sizeof(byte), byteLenTree, archive);
        treeBitArray.cnt = lenTree;
        ptr += byteLenTree;

        int tempPtr = 0;
        TTree* tree = treeTreversal_array2tree(&treeBitArray, &tempPtr);
        free(treeBitArray.arr);


        int flag = 0;
        // (1 different byte in file -> only 1 leaf in tree)
        // (1 leaf in tree -> compressed file is empty)
        // (so uncompressed file is leaf.value * lenOfUncompressedFile)
        if (lenFile == 0) // size of compressed file is 0
        {
            byte value = tree->value;
            FILE* file = fopen(nameFile, "wb");
            for (int i = 0; i < lenFileUncompressed; i++) 
                fwrite(&value, sizeof(byte), 1, file);
            fclose(file);
        }
        else 
            flag = decompressFile(archive, tree, lenFile, &nameFile);
        
                if (flag == 1)
                    fseek(archive, (lenFile / bitInByte + ((lenFile % bitInByte) > 0)), SEEK_CUR);
        
        ptr += (lenFile / bitInByte + ((lenFile % bitInByte) > 0));

        freeTree(tree);

        free(nameFile);
    }

    fclose(archive);
}