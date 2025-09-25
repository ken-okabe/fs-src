// src/4-Hardware/hip/f2_kernels.hip
#include <hip/hip_runtime.h>
#include <stdint.h>

#define BLOCK_SIZE 256
#define WARP_SIZE 64

extern "C" {

// デバイス初期化
__host__ int hip_init(int deviceId) {
    hipError_t err = hipSetDevice(deviceId);
    if (err != hipSuccess) {
        return -1;
    }

    // デバイスプロパティ確認
    hipDeviceProp_t prop;
    err = hipGetDeviceProperties(&prop, deviceId);
    if (err != hipSuccess) {
        return -2;
    }

    return 0;
}

// クリーンアップ
__host__ int hip_cleanup() {
    hipDeviceReset();
    return 0;
}

// メモリ管理
__host__ void* hip_malloc(size_t size) {
    void* ptr = nullptr;
    hipError_t err = hipMalloc(&ptr, size);
    if (err != hipSuccess) {
        return nullptr;
    }
    return ptr;
}

__host__ int hip_free(void* ptr) {
    if (ptr) {
        hipFree(ptr);
    }
    return 0;
}

// メモリコピー
__host__ int hip_memcpy_h2d(void* dst, const void* src, size_t size, hipStream_t stream) {
    hipError_t err = hipMemcpyAsync(dst, src, size, hipMemcpyHostToDevice, stream);
    return (err == hipSuccess) ? 0 : -1;
}

__host__ int hip_memcpy_d2h(void* dst, const void* src, size_t size, hipStream_t stream) {
    hipError_t err = hipMemcpyAsync(dst, src, size, hipMemcpyDeviceToHost, stream);
    return (err == hipSuccess) ? 0 : -1;
}

// ストリーム管理
__host__ hipStream_t hip_stream_create() {
    hipStream_t stream;
    hipStreamCreate(&stream);
    return stream;
}

__host__ int hip_stream_destroy(hipStream_t stream) {
    hipStreamDestroy(stream);
    return 0;
}

__host__ int hip_stream_synchronize(hipStream_t stream) {
    hipStreamSynchronize(stream);
    return 0;
}

// F₂行列積カーネル
__global__ void f2_matrix_multiply_kernel(
    const uint64_t* __restrict__ left,
    const uint64_t* __restrict__ right,
    uint64_t* __restrict__ result,
    int rows, int cols, int inner
) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;

    if (tid >= rows * cols) return;

    int row = tid / cols;
    int col = tid % cols;

    uint64_t sum = 0;

    // F₂での行列積
    for (int k = 0; k < inner; k++) {
        int left_word = (row * inner + k) / 64;
        int left_bit = (row * inner + k) % 64;
        int right_word = (k * cols + col) / 64;
        int right_bit = (k * cols + col) % 64;

        uint64_t left_val = (left[left_word] >> left_bit) & 1;
        uint64_t right_val = (right[right_word] >> right_bit) & 1;

        sum ^= (left_val & right_val);
    }

    // 結果を書き込み
    int result_word = (row * cols + col) / 64;
    int result_bit = (row * cols + col) % 64;

    if (sum) {
        atomicOr((unsigned long long*)&result[result_word], 1ULL << result_bit);
    }
}

// ピボット探索カーネル
__global__ void f2_find_pivots_kernel(
    const uint64_t* __restrict__ matrix,
    int* __restrict__ row_weights,
    int* __restrict__ col_weights,
    int* __restrict__ scores,
    int rows, int cols, int current_rank
) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;
    int total = rows * cols;

    if (tid >= total) return;

    int row = tid / cols;
    int col = tid % cols;

    int word_idx = tid / 64;
    int bit_idx = tid % 64;

    if ((matrix[word_idx] >> bit_idx) & 1) {
        // 行と列の重みを計算
        int row_weight = 0;
        int col_weight = 0;

        for (int c = 0; c < cols; c++) {
            int idx = row * cols + c;
            if ((matrix[idx / 64] >> (idx % 64)) & 1) {
                row_weight++;
            }
        }

        for (int r = 0; r < rows; r++) {
            int idx = r * cols + col;
            if ((matrix[idx / 64] >> (idx % 64)) & 1) {
                col_weight++;
            }
        }

        scores[tid] = row_weight * col_weight;
    } else {
        scores[tid] = -1;
    }
}

// 最大スコアのピボット選択
__global__ void f2_select_pivot_kernel(
    const int* __restrict__ scores,
    int* __restrict__ pivot_idx,
    int total
) {
    __shared__ int shared_max[BLOCK_SIZE];
    __shared__ int shared_idx[BLOCK_SIZE];

    int tid = threadIdx.x;
    int bid = blockIdx.x;
    int idx = bid * blockDim.x + tid;

    // 各スレッドのローカル最大値
    int local_max = -1;
    int local_idx = -1;

    if (idx < total && scores[idx] > local_max) {
        local_max = scores[idx];
        local_idx = idx;
    }

    shared_max[tid] = local_max;
    shared_idx[tid] = local_idx;
    __syncthreads();

    // リダクション
    for (int s = blockDim.x / 2; s > 0; s >>= 1) {
        if (tid < s) {
            if (shared_max[tid + s] > shared_max[tid]) {
                shared_max[tid] = shared_max[tid + s];
                shared_idx[tid] = shared_idx[tid + s];
            }
        }
        __syncthreads();
    }

    // ブロックの最大値を書き込み
    if (tid == 0 && shared_max[0] > -1) {
        atomicMax(pivot_idx, shared_idx[0]);
    }
}

// ガウス消去カーネル
__global__ void f2_gaussian_elimination_kernel(
    uint64_t* __restrict__ matrix,
    int pivot_row, int pivot_col,
    int rows, int cols
) {
    int row = blockIdx.x * blockDim.x + threadIdx.x;

    if (row >= rows || row == pivot_row) return;

    int pivot_idx = pivot_row * cols + pivot_col;
    int pivot_word = pivot_idx / 64;
    int pivot_bit = pivot_idx % 64;

    int row_idx = row * cols + pivot_col;
    int row_word = row_idx / 64;
    int row_bit = row_idx % 64;

    // ピボット列の値をチェック
    if ((matrix[row_word] >> row_bit) & 1) {
        // 行全体をXOR
        int words_per_row = (cols + 63) / 64;
        for (int w = 0; w < words_per_row; w++) {
            matrix[row * words_per_row + w] ^= matrix[pivot_row * words_per_row + w];
        }
    }
}

// 並列XORカーネル
__global__ void f2_parallel_xor_kernel(
    const uint64_t* __restrict__ vectors,
    uint64_t* __restrict__ result,
    int num_vectors, int vector_length
) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;

    if (tid >= vector_length) return;

    uint64_t sum = 0;
    for (int v = 0; v < num_vectors; v++) {
        sum ^= vectors[v * vector_length + tid];
    }

    result[tid] = sum;
}

// エクスポート関数
__host__ int f2_matrix_multiply_gpu(
    void* d_left, void* d_right, void* d_result,
    int rows, int cols, int inner,
    hipStream_t stream
) {
    // 結果を初期化
    hipMemsetAsync(d_result, 0, rows * ((cols + 63) / 64) * sizeof(uint64_t), stream);

    int total = rows * cols;
    int blocks = (total + BLOCK_SIZE - 1) / BLOCK_SIZE;

    hipLaunchKernelGGL(
        f2_matrix_multiply_kernel,
        blocks, BLOCK_SIZE, 0, stream,
        (const uint64_t*)d_left,
        (const uint64_t*)d_right,
        (uint64_t*)d_result,
        rows, cols, inner
    );

    return hipGetLastError();
}

__host__ int f2_topological_rank_gpu(
    void* d_matrix, int rows, int cols,
    void* d_rank, void* d_pivots,
    hipStream_t stream
) {
    int* row_weights;
    int* col_weights;
    int* scores;
    int* pivot_idx;

    hipMalloc(&row_weights, rows * sizeof(int));
    hipMalloc(&col_weights, cols * sizeof(int));
    hipMalloc(&scores, rows * cols * sizeof(int));
    hipMalloc(&pivot_idx, sizeof(int));

    int current_rank = 0;
    int max_rank = (rows < cols) ? rows : cols;

    for (int iter = 0; iter < max_rank; iter++) {
        int total = rows * cols;
        int blocks = (total + BLOCK_SIZE - 1) / BLOCK_SIZE;

        // ピボット探索
        hipLaunchKernelGGL(
            f2_find_pivots_kernel,
            blocks, BLOCK_SIZE, 0, stream,
            (const uint64_t*)d_matrix,
            row_weights, col_weights, scores,
            rows, cols, iter
        );

        // 最大スコアのピボット選択
        hipMemsetAsync(pivot_idx, -1, sizeof(int), stream);
        hipLaunchKernelGGL(
            f2_select_pivot_kernel,
            blocks, BLOCK_SIZE, 0, stream,
            scores, pivot_idx, total
        );

        // ピボット取得
        int host_pivot_idx;
        hipMemcpyAsync(&host_pivot_idx, pivot_idx, sizeof(int), hipMemcpyDeviceToHost, stream);
        hipStreamSynchronize(stream);

        if (host_pivot_idx < 0) break;

        // ピボット記録
        hipMemcpyAsync((int*)d_pivots + current_rank, &host_pivot_idx, sizeof(int),
                      hipMemcpyHostToDevice, stream);
        current_rank++;

        // ガウス消去
        int pivot_row = host_pivot_idx / cols;
        int pivot_col = host_pivot_idx % cols;

        blocks = (rows + BLOCK_SIZE - 1) / BLOCK_SIZE;
        hipLaunchKernelGGL(
            f2_gaussian_elimination_kernel,
            blocks, BLOCK_SIZE, 0, stream,
            (uint64_t*)d_matrix,
            pivot_row, pivot_col,
            rows, cols
        );
    }

    // ランク書き込み
    hipMemcpyAsync(d_rank, &current_rank, sizeof(int), hipMemcpyHostToDevice, stream);

    // クリーンアップ
    hipFree(row_weights);
    hipFree(col_weights);
    hipFree(scores);
    hipFree(pivot_idx);

    return hipGetLastError();
}

__host__ int f2_parallel_xor_gpu(
    void* d_vectors, int num_vectors, int vector_length,
    void* d_result, hipStream_t stream
) {
    int blocks = (vector_length + BLOCK_SIZE - 1) / BLOCK_SIZE;

    hipLaunchKernelGGL(
        f2_parallel_xor_kernel,
        blocks, BLOCK_SIZE, 0, stream,
        (const uint64_t*)d_vectors,
        (uint64_t*)d_result,
        num_vectors, vector_length
    );

    return hipGetLastError();
}

} // extern "C"