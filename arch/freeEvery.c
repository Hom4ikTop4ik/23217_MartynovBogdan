#include "archiver.h"

void freeTree(TTree* tree)
{
	if (tree == NULL)
		return;

	freeTree(tree->left);
	tree->left = NULL;

	freeTree(tree->right);
	tree->right = NULL;

	tree->size  = 0;
	tree->value = 0;

	free(tree);
}

void freeHeap(THeap* heap)
{
	if (heap == NULL)
		return;

	if (heap->arr != NULL)
	{
		free(heap->arr);
		heap->arr = NULL;
		heap->cap = 0;
		heap->cnt = 0;
	}
	
	#ifdef freeStructsToo
		if (freeStructsToo == 1)
			free(heap);
	#endif
}

void freeVector(TVector* vector)
{
	if (vector == NULL)
		return;

	if (vector->arr != NULL)
	{
		free(vector->arr);
		vector->arr = NULL;
		vector->cap = 0;
		vector->cnt = 0;
	}

	#ifdef freeStructsToo
		if (freeStructsToo == 1)
			free(vector);
	#endif
}

void freeBitArray(TBitArray* bitArray)
{
	if (bitArray == NULL)
		return;

	if (bitArray->arr != NULL)
	{
		free(bitArray->arr);
		bitArray->arr = NULL;
		bitArray->cap = 0;
		bitArray->cnt = 0;
	}

	#ifdef freeStructsToo
		if (freeStructsToo == 1)
			free(bitArray);
	#endif
}