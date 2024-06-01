#ifndef definesANDstructs123
#define definesANDstructs123

	#define compressTree 1
	
	typedef unsigned char word;
	#define bitInByte 8 // 1 word = 8 bit
	#define differentBytes (1ll << (  bitInByte * sizeof(word)  ))
	#define bufferSize 1024
	#define fileNameBufferSize 1024

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

    extern int checkDoYouWantRewrite;

#endif // !definesANDstructs123
