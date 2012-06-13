#define BLOCK_SIZE 1
  
__kernel void MatrixMul(__global float* A, __global float* B, __global float* C, int wA, int wB)
{
    int bx = get_group_id(0);
    int by = get_group_id(1);
 
    int tx = get_local_id(0);
    int ty = get_local_id(1);
 
    int aBegin = wA * BLOCK_SIZE * by;
    int aEnd   = aBegin + wA - 1;
    int aStep  = BLOCK_SIZE;
 
    int bBegin = BLOCK_SIZE * bx;
    int bStep  = BLOCK_SIZE * wB;

    float Csub = 0;
 
    for (int a = aBegin, b = bBegin;
             a <= aEnd;
             a += aStep, b += bStep) 
    {
        __local float As[BLOCK_SIZE][BLOCK_SIZE];
        __local float Bs[BLOCK_SIZE][BLOCK_SIZE];
 
        As[ty][tx] = A[a + wA * ty + tx];
        Bs[ty][tx] = B[b + wB * ty + tx];
 
        barrier(CLK_LOCAL_MEM_FENCE);
 
        for (int k = 0; k < BLOCK_SIZE; ++k)
            Csub += As[ty][k] * Bs[k][tx];
 
        barrier(CLK_LOCAL_MEM_FENCE);
    }
 
    int c = wB * BLOCK_SIZE * by + BLOCK_SIZE * bx;
    C[c + wB * ty + tx] = Csub;
}