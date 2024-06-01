#ifndef definesANDstructs123
#define definesANDstructs123

	#define compressTree 1
	
	typedef unsigned char byte;
	#define bitInByte 8 // 1 byte = 8 bit
	#define differentBytes (1ll << (  bitInByte * sizeof(byte)  ))
	#define bufferSize 1024
	#define fileNameBufferSize 1024

    #define maxTreeBitLen (2814 + 32) // 32 for int

    #ifndef min
        #define min(a,b) (  (a<b) ? a : b )
    #endif 
    #ifndef max
        #define max(a,b) (  (a>b) ? a : b )
    #endif 


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
        byte* arr;
        int cap;
        int cnt;
    } TBitArray;

#endif // !definesANDstructs123
