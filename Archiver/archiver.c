#include "archiver.h"

// 0 - compress
// 1 - decompress
void example(int mode)
{
    if (mode == 0)
    {
        const char fileName0[] = "tests/project.mp4";
        int len0 = strlen(fileName0) + 1;

        char* name0 = malloc(len0 * sizeof(char));
        for (int i = 0; i < len0; i++)
            name0[i] = fileName0[i];

        const char fileName1[] = "tests/archiver.bmp";
        int len1 = strlen(fileName1) + 1;

        char* name1 = malloc(len1 * sizeof(char));
        for (int i = 0; i < len1; i++)
            name1[i] = fileName1[i];

        char** link = malloc(2 * sizeof(char*));
        link[0] = name0;
        link[1] = name1;

        TBitArray* fileBitArray = createArchive(link, 2);

        const char fileName[] = "biba";
        int len = strlen(fileName) + 1;

        char* name = malloc(len * sizeof(char));
        for (int i = 0; i < len; i++)
            name[i] = fileName[i];

        createFileFromBitArray(fileBitArray, &name);

        freeBitArray(fileBitArray);
        free(name0);
        free(name1);
        free(name);
        free(link);
    }

    else if (mode == 1)
    {
        const char archiveName[] = "biba";
        int len = strlen(archiveName) + 1;
        char* name = malloc(len * sizeof(char));
        for (int i = 0; i < len; i++)
            name[i] = archiveName[i];

        TBitArray* tempArchive = readArchiveFromFile(name);
        decompressArchive(tempArchive);

        freeBitArray(tempArchive);
        free(name);
    }
}

char* constStr2MallocStr(const char* str)
{
    int len = 1 + strlen(str);
    char* ret = malloc(len * sizeof(char));
    for (int i = 0; i < len - 1; i++)
        ret[i] = str[i];
    ret[len] = '\0';

    return ret;
}

char** constStrs2MallocStrs(int cnt, char** array, int skip)
{
    int newCnt = cnt - skip;
    char** strs = malloc(cnt * sizeof(char*));

    for (int i = 0; i < newCnt; i++)
        strs[i] = constStr2MallocStr(array[i + skip]);

    return strs;
}


#ifdef _WIN32
#include <windows.h>
int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR lpCmdLine, int nCmdShow)
{
    return main(__argc, __argv);
}

// global vars:

int checkDoYouWantRewrite = 0;

#define btQuit_id           ((1000-7) +  0) // button
#define instruction_id      ((1000-7) +  1) // static
#define instructionSteps_id ((1000-7) +  2) // static

#define archiveText_id      ((1000-7) +  3) // static
#define inputText_id        ((1000-7) +  4) // edit
#define btCompress_id       ((1000-7) +  5) // button
#define btDecompress_id     ((1000-7) +  6) // button

#define files_id            ((1000-7) +  7) // static
#define messages_id         ((1000-7) +  8) // static
#define listOfFiles_id      ((1000-7) +  9) // listbox
#define info_id             ((1000-7) +  10) // listbox


char** global_files = NULL;
int global_cnt = 0;
char* global_archiveName = NULL;
HBRUSH hBrush;
HWND listOfFiles;
HWND info;

int global_cntOfMessages = 0;

void SetBackgroundColor(HWND hwnd, COLORREF bgColor) {
    // Создаем кисть с указанным цветом фона
    HBRUSH hBrush = CreateSolidBrush(bgColor);
    // Устанавливаем кисть в качестве фона для статического текста
    SetClassLongPtr(hwnd, GCLP_HBRBACKGROUND, (LONG_PTR)hBrush);
    // Перерисовываем статический текст
    RedrawWindow(hwnd, NULL, NULL, RDW_ERASE | RDW_INVALIDATE);
}

void MySetTextColor(HWND hwnd, COLORREF textColor) {
    // Получаем дескриптор контекста устройства
    HDC hdc = GetDC(hwnd);
    // Устанавливаем цвет текста
    SetTextColor(hdc, textColor);
    // Освобождаем контекст устройства
    //ReleaseDC(hwnd, hdc);
    // Перерисовываем статический текст
    //RedrawWindow(hwnd, NULL, NULL, RDW_ERASE | RDW_INVALIDATE);
}


char** DropFile(HDROP hDrop, int* returnCnt)
{
    char fName[fileNameBufferSize];
    int cnt = DragQueryFile(hDrop, 0xffffffff, NULL, 0);
    *returnCnt = cnt;
    //printf("\t%d\n", cnt);

    POINT pt;
    DragQueryPoint(hDrop, &pt);
    //printf("(%d,%d)\n", pt.x, pt.y);

    char** ret = malloc(cnt * sizeof(char*));

    for (int i = 0; i < cnt; i++)
    {
        DragQueryFile(hDrop, i, fName, MAX_PATH);
        ret[i] = constStr2MallocStr(fName);
        //printf("\t  %s\n", fName);
    }
    DragFinish(hDrop);


    return ret;
}

LRESULT WndProc(HWND hwnd, UINT message, WPARAM wparam, LPARAM lparam)
{
    if (message == WM_DESTROY)
        PostQuitMessage(0);

    else if (message == WM_KEYDOWN) {
        if (wparam == VK_ESCAPE) PostQuitMessage(0);

        //printf("code = %d [%c]\n", wparam, wparam);
    }
    else if (message == WM_CHAR) {
        //printf("\tcode = %d [%c]\n", wparam, wparam);
    }
    else if (message == WM_MOUSEMOVE) {
        int xPos = LOWORD(lparam);
        int yPos = HIWORD(lparam);
        //printf("mouse [%d, %d]\n", xPos, yPos);
    }
    else if (message == WM_LBUTTONDOWN) {
        //printf("LBM Downed\n");
    }
    else if (message == WM_DROPFILES)
    {
        for (int i = 0; i < global_cnt+1; i++) {
            SendMessage(listOfFiles, LB_DELETESTRING, i, 0);
        }

        global_files = DropFile(wparam, &global_cnt);

        for (int i = 0; i < global_cnt; i++) {
            SendMessage(listOfFiles, LB_ADDSTRING, 0, global_files[i]);
        }
        //printf("DROP\n");
    }
    
    else if (message == WM_COMMAND)
    {
        if (LOWORD(wparam) == btQuit_id)
            PostQuitMessage(0);
        if (LOWORD(wparam) == inputText_id)
        {
            int nc = HIWORD(wparam);

            char c[100];

            if (nc == EN_UPDATE)
            {
                global_archiveName = malloc(fileNameBufferSize);
                GetWindowText(lparam, global_archiveName, fileNameBufferSize);
            }
        }
        if (LOWORD(wparam) == btCompress_id)
        {
            if (global_archiveName != NULL)
            {
                TBitArray* fileBitArray = createArchive(global_files, global_cnt);

                if (fileExists(global_archiveName) == 0)
                {
                    createFileFromBitArray(fileBitArray, &global_archiveName);

                    freeBitArray(fileBitArray);
                    free(global_archiveName);
                    global_archiveName = NULL;
                }
            }
        }
        if (LOWORD(wparam) == btDecompress_id)
        {
            for (int i = 0; i < global_cnt; i++)
            {
                if (fileExists(global_files[i]) == 0) {
                    printf("Can't open archive (probably, it doesn't exist): %s\n", global_files[i]);
                    continue;
                }

                TBitArray* tempArchive = readArchiveFromFile(global_files[i]);
                decompressArchive(tempArchive);
                freeBitArray(tempArchive);
            }
        }
    }

    else if (message == WM_CTLCOLORSTATIC)
    {
        HDC hdc = (HDC)wparam;
        HWND hwndStatic = (HWND)lparam;

        if (hwndStatic == GetDlgItem(hwnd, instruction_id) || hwndStatic == GetDlgItem(hwnd, instructionSteps_id))
        {
            SetBkColor(hdc, RGB(239, 228, 176)); 
            SetTextColor(hdc, RGB(0, 0, 0)); // Зеленый цвет текста
        }
        return (LRESULT)GetStockObject(NULL_BRUSH);
    }
    else if (message == WM_CTLCOLORBTN)
    {
        HDC hdc = (HDC)wparam;
        // Устанавливаем цвет текста кнопки
        SetTextColor(hdc, RGB(255, 255, 255)); // Белый цвет текста
        // Устанавливаем цвет фона кнопки
        SetBkColor(hdc, RGB(0, 0, 255)); // Синий цвет фона
        // Возвращаем кисть для фона кнопки
        return (LRESULT)hBrush;
    }
    else if (message == WM_CTLCOLOREDIT)
    {
        // Получаем устройство рисования
        HDC hdcEdit = (HDC)wparam;
        // Устанавливаем цвет текста
        SetTextColor(hdcEdit, RGB(0, 0, 0));
        // Устанавливаем цвет фона
        SetBkColor(hdcEdit, RGB(181, 230, 29));
        // Возвращаем кисть для фона
        return (LRESULT)hBrush;
    }

    
    return DefWindowProcA(hwnd, message, wparam, lparam);
}



void GUI()
{
    WNDCLASSA wcl;
    memset(&wcl, 0, sizeof(WNDCLASSA));
    wcl.lpszClassName = "my Window";
    wcl.lpfnWndProc = WndProc;
    RegisterClassA(&wcl);

    HWND hwnd; // class like in wcl  window's name  window's style
    hwnd = CreateWindow("my Window", "Archiver by Hom4ik", WS_OVERLAPPEDWINDOW,
        10,10, 640,480, NULL, NULL, NULL, NULL);

    ShowWindow(hwnd, SW_SHOWNORMAL);
    DragAcceptFiles(hwnd, TRUE);

    HWND btQuit = CreateWindow("button", "Quit",
                               WS_VISIBLE | WS_CHILD,
                               560,10, 50,50,
                               hwnd, btQuit_id, NULL, NULL);

    HWND instruction = CreateWindow("static", text_instruction,
                                     WS_VISIBLE | WS_CHILD,
                                     10,10, 120,20,
                                     hwnd, instruction_id, NULL, NULL);

    HWND instructionSteps = CreateWindow("static", text_instructionSteps,
                                         WS_VISIBLE | WS_CHILD,
                                         10,30, 360,60,
                                         hwnd, instructionSteps_id, NULL, NULL);

    HWND archiveText = CreateWindow("static", text_archive,
                                    WS_VISIBLE | WS_CHILD,
                                    10,90, 360,60,
                                    hwnd, archiveText_id, NULL, NULL);

    HWND inputText = CreateWindow("edit", "",
                            WS_VISIBLE | WS_CHILD | WS_BORDER | ES_RIGHT | ES_AUTOHSCROLL,
                            10,115, 360,20,
                            hwnd, inputText_id, NULL, NULL);

    HWND btCompress = CreateWindow("button", "Compress",
                                   WS_VISIBLE | WS_CHILD,
                                   10,145, 100,50,
                                   hwnd, btCompress_id, NULL, NULL);

    HWND btDecompress = CreateWindow("button", "Decompress",
                                     WS_VISIBLE | WS_CHILD,
                                     120,145, 100,50,
                                     hwnd, btDecompress_id, NULL, NULL);

    HWND files = CreateWindow("static", text_files,
                              WS_VISIBLE | WS_CHILD,
                              230,200, 120,20,
                              hwnd, files_id, NULL, NULL);

    listOfFiles = CreateWindow("listbox", text_empty,
                             WS_VISIBLE | WS_CHILD | WS_BORDER,
							 230,220, 385,210,
							 hwnd, listOfFiles_id, NULL, NULL);

    HWND messages = CreateWindow("static", text_messages,
                                 WS_VISIBLE | WS_CHILD,
                                 10,200, 120,20,
                                 hwnd, messages_id, NULL, NULL); 

    info = CreateWindow("listbox", text_empty,
                        WS_VISIBLE | WS_CHILD | WS_BORDER,
                        10,220, 210,210,
                        hwnd, info_id, NULL, NULL);



    MSG msg;

    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
}



#endif // !_WIN32

void NO_GUI(int argc, char** argv)
{
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
        howToUse();
        return 0;
    }

    // argv if encode (skip 3 args): [programName], [mode], [archive], [1+ files]
    // argv if decode (skip 2 args): [programName], [mode], [1+ files]
    int skip = 2 + mode;
    int cntOfFiles = argc - skip;

    char** files = constStrs2MallocStrs(argc, argv, skip);

    if (mode == 0) // decompress / decode
    {
        for (int i = 0; i < cntOfFiles; i++)
        {
            if (fileExists(files[i]) == 0) {
                printf("Can't open archive (probably, it doesn't exist): %s\n", files[i]);
                continue;
            }

            TBitArray* tempArchive = readArchiveFromFile(files[i]);
            decompressArchive(tempArchive);
            freeBitArray(tempArchive);
        }
    }
    else if (mode == 1) // compress / encode
    {
        TBitArray* fileBitArray = createArchive(files, cntOfFiles);
        createFileFromBitArray(fileBitArray, &archiveName);

        freeBitArray(fileBitArray);
        free(archiveName);
        archiveName = NULL;
    }


    for (int i = 0; i < cntOfFiles; i++)
        free(files[i]);
    free(files);
}


int main(int argc, char* argv[])
{
    if (argc <= 2)  // without mode or without files
    {
        #ifdef _WIN32
            GUI();
        #else
            howToUse();
        #endif // DEBUG

        return 0;
    }

    else {
        NO_GUI(argc, argv);
    }

    return 0;
}