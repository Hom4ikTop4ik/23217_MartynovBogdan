#include "archiver.h"

// 0 - compress
// 1 - decompress
void example(int mode)
{
    if (mode == 0)
    {
        int cnt = 3;


        const char fileName0[] = "da1.txt";
        int len0 = strlen(fileName0) + 1;

        char* name0 = malloc(len0 * sizeof(char));
        for (int i = 0; i < len0; i++)
            name0[i] = fileName0[i];

        const char fileName1[] = "da2.txt";
        int len1 = strlen(fileName1) + 1;

        char* name1 = malloc(len1 * sizeof(char));
        for (int i = 0; i < len1; i++)
            name1[i] = fileName1[i];

        const char fileName2[] = "da3.txt";
        int len2 = strlen(fileName2) + 1;

        char* name2 = malloc(len2 * sizeof(char));
        for (int i = 0; i < len2; i++)
            name2[i] = fileName2[i];

        char** link = malloc(cnt * sizeof(char*));
        //for (int i = 0; i < cnt; i++)
            //link[i] = name1;
        link[0] = name0;
        link[1] = name1;
        link[2] = name2;


        const char fileName[] = "biba";
        int len = strlen(fileName) + 1;

        char* name = malloc(len * sizeof(char));
        for (int i = 0; i < len; i++)
            name[i] = fileName[i];


        createArchive(&name, link, cnt);

        free(name0);
        free(name1);
        free(link);
        free(name);
    }

    else if (mode == 1)
    {
        const char archiveName[] = "biba";
        int len = strlen(archiveName);
        char* name = malloc((len + 1) * sizeof(char));
        for (int i = 0; i < len; i++)
            name[i] = archiveName[i];
        name[len] = '\0';

        decompressArchive(name);

        free(name);
    }
}

void howToUse()
{
    printf("Run program without arguments to get this instruction: \n");
    printf("If you want compress/encode file(s) use: compress [archive name] [1+ files].\n");
    printf("If you want decompress file(s) use: decompress [1+ archives].\n");
}

int main(int argc, char* argv[])
{
    //example(1);

    //printf("\ncnt: %d\nargs:\n", argc);
    //for (int i = 0; i < argc; i++)
    //    printf("%s\n", argv[i]);
    
    if (argc <= 2) { // without mode or without files
        howToUse();
        return 0;
    }

    int mode = 2;
    char* archiveName = NULL;
    if (argv[1][0] == 'd' || argv[1][0] == 'D')
        mode = 0; // decode / decompress
    else if ((argv[1])[0] == 'e' || (argv[1])[0] == 'E' || (argv[1])[0] == 'c' || (argv[1])[0] == 'C')
    {
        if (argc <= 3) { // no files to compress
            howToUse();
            return 0;
        }

        int len = 1 + strlen(argv[2]);
        archiveName = malloc(len * sizeof(char));
        for (int j = 0; j < len; j++) {
            archiveName[j] = argv[2][j];
        }

        mode = 1; // encode / compress
    }
    else
    {
        printf("Mode isn't correctly.\nUse [decode/decompress/d] if you want decompress archive.\nUse [encode/e/compress/c] if you want compress your files.\n");
        return 0;
    }

    // +2 because first arg is program's name and second arg is [mode]
    // +mode: of compress (mode == 1) we should read arhcive name too
    int skip = 2 + mode;

    int cntOfFiles = argc -skip; 

    char** files = malloc(cntOfFiles * sizeof(char*)); 

    for (int i = 0; i < cntOfFiles; i++)
    {
        int len = 1 + strlen(argv[i + skip]); 

        files[i] = malloc(len * sizeof(char));
        for (int j = 0; j < len; j++) {
            files[i][j] = argv[i + skip][j];
        }
    }

    if (mode == 0) // decompress / decode
    {
        for (int i = 0; i < cntOfFiles; i++)
        {
            if (fileExists(files[i]) == 0)
            {
                printf("Can't open archive: %s\n", files[i]);
                continue;
            }

            decompressArchive(files[i]);
        }
    }
    else if (mode == 1) // compress / encode
    {

        createArchive(&archiveName, files, cntOfFiles);
        free(archiveName);
    }
        

    for (int i = 0; i < cntOfFiles; i++)
        free(files[i]);
    free(files);
    

    return 0;
}