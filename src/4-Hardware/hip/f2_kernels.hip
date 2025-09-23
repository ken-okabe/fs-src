// 4-Hardware/HIP/kernels/f2_kernels.hip
#include <hip/hip_runtime.h>
#include <hip/hip_cooperative_groups.h>
#include <stdint.h>

namespace cg = cooperative_groups;

// AMD GPUのウェーブフロントサイズ
#define WAVE_SIZE 64
#define BLOCK_SIZE 256
#define TILE_SIZE 16

// ポピュレーションカウント
__device__ inline int popcnt64(uint64_t x) {
    return __popcll(x);
}

// F₂行列積カーネル - 完全実装
__global__ void f2_matrix_multiply_kernel(
    const uint64_t* __restrict__ left,
    const uint64_t* __restrict__ right,
    uint64_t* __restrict__ result,
    int rows, int cols, int inner
) {
    // グリッド座標
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;

    // 共有メモリでタイル化
    __shared__ uint64_t tile_left[TILE_SIZE][TILE_SIZE];
    __shared__ uint64_t tile_right[TILE_SIZE][TILE_SIZE];

    // スレッドローカル座標
    int ty = threadIdx.y;
    int tx = threadIdx.x;

    // 結果の累積
    uint64_t acc = 0;

    // タイル数計算
    int num_tiles = (inner + TILE_SIZE - 1) / TILE_SIZE;

    for (int tile = 0; tile < num_tiles; tile++) {
        // 左行列のタイルをロード
        int left_col = tile * TILE_SIZE + tx;
        int left_row = row;

        if (left_row < rows && left_col < inner) {
            tile_left[ty][tx] = left[left_row * inner + left_col];
        } else {
            tile_left[ty][tx] = 0;
        }

        // 右行列のタイルをロード
        int right_row = tile * TILE_SIZE + ty;
        int right_col = col;

        if (right_row < inner && right_col < cols) {
            tile_right[ty][tx] = right[right_row * cols + right_col];
        } else {
            tile_right[ty][tx] = 0;
        }

        // 同期（タイルのロード完了を待つ）
        __syncthreads();

        // タイル内で内積計算
        #pragma unroll
        for (int k = 0; k < TILE_SIZE; k++) {
            // F₂での乗算（AND）と加算（XOR）
            acc ^= tile_left[ty][k] & tile_right[k][tx];
        }

        // 次のタイルのために同期
        __syncthreads();
    }

    // 結果を書き込み（パリティ計算）
    if (row < rows && col < cols) {
        result[row * cols + col] = popcnt64(acc) & 1;
    }
}

// 行列等価性チェックカーネル
__global__ void f2_matrix_equals_kernel(
    const uint64_t* __restrict__ matrix1,
    const uint64_t* __restrict__ matrix2,
    int size,
    int* __restrict__ differences
) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;
    int stride = blockDim.x * gridDim.x;

    // 各スレッドが複数要素を処理
    int local_diff = 0;

    for (int i = tid; i < size; i += stride) {
        if (matrix1[i] != matrix2[i]) {
            local_diff++;
        }
    }

    // ワープ内でリダクション
    auto warp = cg::tiled_partition<WAVE_SIZE>(cg::this_thread_block());
    local_diff = cg::reduce(warp, local_diff, cg::plus<int>());

    // ワープのリーダーがブロック共有メモリに書き込み
    __shared__ int warp_diffs[BLOCK_SIZE / WAVE_SIZE];
    if (warp.thread_rank() == 0) {
        warp_diffs[threadIdx.x / WAVE_SIZE] = local_diff;
    }

    __syncthreads();

    // ブロックのスレッド0が最終集計
    if (threadIdx.x == 0) {
        int block_diff = 0;
        for (int i = 0; i < BLOCK_SIZE / WAVE_SIZE; i++) {
            block_diff += warp_diffs[i];
        }

        // アトミック加算で全体の差分を更新
        atomicAdd(differences, block_diff);
    }
}

// バッチ行列積カーネル - 完全実装
__global__ void f2_batch_matrix_multiply_kernel(
    const uint64_t* __restrict__ all_data,
    const int* __restrict__ dimensions,
    const int* __restrict__ offsets,
    uint64_t* __restrict__ results,
    int batch_size
) {
    // バッチインデックス
    int batch_id = blockIdx.z;
    if (batch_id >= batch_size) return;

    // このバッチの情報取得
    int rows = dimensions[batch_id * 3 + 0];
    int cols = dimensions[batch_id * 3 + 1];
    int inner = dimensions[batch_id * 3 + 2];

    int left_offset = offsets[batch_id * 3 + 0];
    int right_offset = offsets[batch_id * 3 + 1];
    int result_offset = offsets[batch_id * 3 + 2];

    const uint64_t* left = all_data + left_offset;
    const uint64_t* right = all_data + right_offset;
    uint64_t* result = results + result_offset;

    // グリッド内の位置
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;

    if (row >= rows || col >= cols) return;

    // 内積計算
    uint64_t acc = 0;
    for (int k = 0; k < inner; k++) {
        acc ^= left[row * inner + k] & right[k * cols + col];
    }

    // 結果書き込み
    result[row * cols + col] = popcnt64(acc) & 1;
}

// トポロジカルランク計算 - ピボット探索カーネル
__global__ void f2_find_pivots_kernel(
    uint64_t* __restrict__ matrix,
    int* __restrict__ row_weights,
    int* __restrict__ col_weights,
    int* __restrict__ scores,
    int rows, int cols,
    int iteration
) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;

    // 行の重み計算
    if (tid < rows) {
        int weight = 0;
        for (int j = 0; j < cols; j++) {
            if (matrix[tid * cols + j] != 0) {
                weight++;
            }
        }
        row_weights[tid] = weight;
    }

    __syncthreads();

    // 列の重み計算
    if (tid < cols) {
        int weight = 0;
        for (int i = 0; i < rows; i++) {
            if (matrix[i * cols + tid] != 0) {
                weight++;
            }
        }
        col_weights[tid] = weight;
    }

    __syncthreads();

    // スコア計算
    int total = rows * cols;
    if (tid < total) {
        int row = tid / cols;
        int col = tid % cols;

        if (matrix[tid] != 0) {
            scores[tid] = row_weights[row] * col_weights[col];
        } else {
            scores[tid] = 0;
        }
    }
}

// 最大スコアのピボット選択カーネル
__global__ void f2_select_pivot_kernel(
    const int* __restrict__ scores,
    int* __restrict__ pivot_idx,
    int size
) {
    __shared__ int shared_max[BLOCK_SIZE];
    __shared__ int shared_idx[BLOCK_SIZE];

    int tid = threadIdx.x;
    int i = blockIdx.x * blockDim.x + tid;

    // 各スレッドが最大値を探索
    int local_max = 0;
    int local_idx = -1;

    if (i < size) {
        local_max = scores[i];
        local_idx = i;
    }

    shared_max[tid] = local_max;
    shared_idx[tid] = local_idx;
    __syncthreads();

    // ブロック内でリダクション
    for (int s = blockDim.x / 2; s > 0; s >>= 1) {
        if (tid < s) {
            if (shared_max[tid] < shared_max[tid + s]) {
                shared_max[tid] = shared_max[tid + s];
                shared_idx[tid] = shared_idx[tid + s];
            }
        }
        __syncthreads();
    }

    // ブロックの最大値を書き込み
    if (tid == 0) {
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

    // ピボット列の値をチェック
    if (matrix[row * cols + pivot_col] != 0) {
        // 行全体をXOR
        for (int col = 0; col < cols; col++) {
            matrix[row * cols + col] ^= matrix[pivot_row * cols + col];
        }
    }
}

// 並列XORカーネル
__global__ void f2_parallel_xor_kernel(
    const uint64_t* __restrict__ vectors,
    uint64_t* __restrict__ result,
    int num_vectors,
    int vector_length
) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;

    if (tid >= vector_length) return;

    uint64_t acc = 0;

    // すべてのベクトルの対応する要素をXOR
    for (int v = 0; v < num_vectors; v++) {
        acc ^= vectors[v * vector_length + tid];
    }

    result[tid] = acc;
}

// オンデマンド局所計算カーネル - 4方向完全実装
__global__ void f2_local_computation_kernel(
    const uint64_t* __restrict__ environments,
    const uint64_t* __restrict__ peps_tensor,
    const int* __restrict__ requests,
    uint64_t* __restrict__ results,
    int num_requests,
    int chi,
    int bond_dim
) {
    int request_id = blockIdx.x;
    if (request_id >= num_requests) return;

    // リクエスト情報
    int center_x = requests[request_id * 3 + 0];
    int center_y = requests[request_id * 3 + 1];
    int radius = requests[request_id * 3 + 2];

    int tid = threadIdx.x;
    int wave_id = tid / WAVE_SIZE;
    int lane_id = tid % WAVE_SIZE;

    // 共有メモリ
    extern __shared__ uint64_t shared_mem[];
    uint64_t* local_env = shared_mem;
    uint64_t* direction_results = shared_mem + (2 * radius + 1) * (2 * radius + 1);

    // 局所環境のロード
    int local_size = (2 * radius + 1) * (2 * radius + 1);
    for (int i = tid; i < local_size; i += blockDim.x) {
        int dx = (i / (2 * radius + 1)) - radius;
        int dy = (i % (2 * radius + 1)) - radius;
        int global_x = center_x + dx;
        int global_y = center_y + dy;

        // 境界チェック付きでロード
        if (global_x >= 0 && global_x < chi && global_y >= 0 && global_y < chi) {
            local_env[i] = environments[global_x * chi + global_y];
        } else {
            local_env[i] = 0;
        }
    }

    __syncthreads();

    // 4方向の処理（各ウェーブフロントが1方向を担当）

    // 方向0: 左方向の成長
    if (wave_id == 0) {
        uint64_t acc = 0;
        for (int i = lane_id; i < radius; i += WAVE_SIZE) {
            for (int j = 0; j < radius; j++) {
                int env_idx = i * (2 * radius + 1) + j;
                int peps_idx = (i * radius + j) % bond_dim;
                acc ^= local_env[env_idx] & peps_tensor[peps_idx];
            }
        }

        // ウェーブフロント内リダクション
        for (int offset = WAVE_SIZE / 2; offset > 0; offset /= 2) {
            acc ^= __shfl_xor(acc, offset, WAVE_SIZE);
        }

        if (lane_id == 0) {
            direction_results[0] = popcnt64(acc) & 1;
        }
    }

    // 方向1: 右方向の成長
    if (wave_id == 1) {
        uint64_t acc = 0;
        for (int i = lane_id; i < radius; i += WAVE_SIZE) {
            for (int j = radius + 1; j < 2 * radius + 1; j++) {
                int env_idx = i * (2 * radius + 1) + j;
                int peps_idx = (i * radius + (j - radius - 1)) % bond_dim;
                acc ^= local_env[env_idx] & peps_tensor[peps_idx];
            }
        }

        for (int offset = WAVE_SIZE / 2; offset > 0; offset /= 2) {
            acc ^= __shfl_xor(acc, offset, WAVE_SIZE);
        }

        if (lane_id == 0) {
            direction_results[1] = popcnt64(acc) & 1;
        }
    }

    // 方向2: 上方向の成長
    if (wave_id == 2) {
        uint64_t acc = 0;
        for (int i = lane_id; i < radius; i += WAVE_SIZE) {
            for (int j = 0; j < 2 * radius + 1; j++) {
                int env_idx = i * (2 * radius + 1) + j;
                int peps_idx = (i * (2 * radius + 1) + j) % bond_dim;
                acc ^= local_env[env_idx] & peps_tensor[peps_idx];
            }
        }

        for (int offset = WAVE_SIZE / 2; offset > 0; offset /= 2) {
            acc ^= __shfl_xor(acc, offset, WAVE_SIZE);
        }

        if (lane_id == 0) {
            direction_results[2] = popcnt64(acc) & 1;
        }
    }

    // 方向3: 下方向の成長
    if (wave_id == 3) {
        uint64_t acc = 0;
        for (int i = radius + 1; i < 2 * radius + 1; i += 1) {
            for (int j = lane_id; j < 2 * radius + 1; j += WAVE_SIZE) {
                int env_idx = i * (2 * radius + 1) + j;
                int peps_idx = ((i - radius - 1) * (2 * radius + 1) + j) % bond_dim;
                acc ^= local_env[env_idx] & peps_tensor[peps_idx];
            }
        }

        for (int offset = WAVE_SIZE / 2; offset > 0; offset /= 2) {
            acc ^= __shfl_xor(acc, offset, WAVE_SIZE);
        }

        if (lane_id == 0) {
            direction_results[3] = popcnt64(acc) & 1;
        }
    }

    __syncthreads();

    // 最終結果の集計（スレッド0が実行）
    if (tid == 0) {
        uint64_t final_result = 0;
        for (int d = 0; d < 4; d++) {
            final_result ^= direction_results[d];
        }
        results[request_id] = final_result;
    }
}

// C++インターフェース
extern "C" {

int f2_matrix_multiply_gpu(
    void* d_left, void* d_right, void* d_result,
    int rows, int cols, int inner,
    hipStream_t stream
) {
    dim3 block(TILE_SIZE, TILE_SIZE);
    dim3 grid((cols + TILE_SIZE - 1) / TILE_SIZE,
              (rows + TILE_SIZE - 1) / TILE_SIZE);

    hipLaunchKernelGGL(
        f2_matrix_multiply_kernel,
        grid, block, 0, stream,
        (const uint64_t*)d_left,
        (const uint64_t*)d_right,
        (uint64_t*)d_result,
        rows, cols, inner
    );

    return hipGetLastError();
}

int f2_matrix_equals_gpu(
    void* d_matrix1, void* d_matrix2,
    int size, void* d_differences,
    hipStream_t stream
) {
    // 差分カウンタをゼロクリア
    hipMemsetAsync(d_differences, 0, sizeof(int), stream);

    int threads = BLOCK_SIZE;
    int blocks = (size + threads - 1) / threads;

    hipLaunchKernelGGL(
        f2_matrix_equals_kernel,
        blocks, threads, 0, stream,
        (const uint64_t*)d_matrix1,
        (const uint64_t*)d_matrix2,
        size,
        (int*)d_differences
    );

    return hipGetLastError();
}

int f2_batch_matrix_multiply_gpu(
    void* d_data, void* d_dimensions, void* d_offsets,
    void* d_results, int batch_size,
    hipStream_t stream
) {
    dim3 block(16, 16, 1);
    dim3 grid(16, 16, batch_size);  // Z次元でバッチを処理

    hipLaunchKernelGGL(
        f2_batch_matrix_multiply_kernel,
        grid, block, 0, stream,
        (const uint64_t*)d_data,
        (const int*)d_dimensions,
        (const int*)d_offsets,
        (uint64_t*)d_results,
        batch_size
    );

    return hipGetLastError();
}

int f2_compute_topological_rank_gpu(
    void* d_matrix, int rows, int cols,
    void* d_rank, void* d_pivots,
    hipStream_t stream
) {
    int total = rows * cols;
    int* d_row_weights;
    int* d_col_weights;
    int* d_scores;
    int* d_pivot_idx;

    // 作業用メモリ確保
    hipMalloc(&d_row_weights, rows * sizeof(int));
    hipMalloc(&d_col_weights, cols * sizeof(int));
    hipMalloc(&d_scores, total * sizeof(int));
    hipMalloc(&d_pivot_idx, sizeof(int));

    uint64_t* matrix = (uint64_t*)d_matrix;
    int* rank = (int*)d_rank;
    int* pivots = (int*)d_pivots;

    int current_rank = 0;
    int max_rank = (rows < cols) ? rows : cols;

    for (int iter = 0; iter < max_rank; iter++) {
        // ピボット探索
        int threads = BLOCK_SIZE;
        int blocks = (total + threads - 1) / threads;

        hipLaunchKernelGGL(
            f2_find_pivots_kernel,
            blocks, threads, 0, stream,
            matrix, d_row_weights, d_col_weights, d_scores,
            rows, cols, iter
        );

        // 最大スコアのピボット選択
        hipMemsetAsync(d_pivot_idx, -1, sizeof(int), stream);

        hipLaunchKernelGGL(
            f2_select_pivot_kernel,
            blocks, threads, 0, stream,
            d_scores, d_pivot_idx, total
        );

        // ピボットインデックス取得
        int pivot_idx;
        hipMemcpyAsync(&pivot_idx, d_pivot_idx, sizeof(int),
                      hipMemcpyDeviceToHost, stream);
        hipStreamSynchronize(stream);

        if (pivot_idx < 0) break;  // ピボットが見つからない

        int pivot_row = pivot_idx / cols;
        int pivot_col = pivot_idx % cols;

        // ピボット記録
        hipMemcpyAsync(pivots + current_rank, &pivot_idx, sizeof(int),
                      hipMemcpyHostToDevice, stream);
        current_rank++;

        // ガウス消去
        blocks = (rows + threads - 1) / threads;

        hipLaunchKernelGGL(
            f2_gaussian_elimination_kernel,
            blocks, threads, 0, stream,
            matrix, pivot_row, pivot_col, rows, cols
        );
    }

    // ランク書き込み
    hipMemcpyAsync(rank, &current_rank, sizeof(int),
                  hipMemcpyHostToDevice, stream);

    // クリーンアップ
    hipFree(d_row_weights);
    hipFree(d_col_weights);
    hipFree(d_scores);
    hipFree(d_pivot_idx);

    return hipGetLastError();
}

int f2_parallel_xor_gpu(
    void* d_vectors, int num_vectors, int vector_length,
    void* d_result, hipStream_t stream
) {
    int threads = BLOCK_SIZE;
    int blocks = (vector_length + threads - 1) / threads;

    hipLaunchKernelGGL(
        f2_parallel_xor_kernel,
        blocks, threads, 0, stream,
        (const uint64_t*)d_vectors,
        (uint64_t*)d_result,
        num_vectors, vector_length
    );

    return hipGetLastError();
}

} // extern "C"