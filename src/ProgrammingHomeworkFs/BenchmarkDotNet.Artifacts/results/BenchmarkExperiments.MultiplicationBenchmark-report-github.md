``` ini

BenchmarkDotNet=v0.13.4, OS=macOS Monterey 12.5 (21G72) [Darwin 21.6.0]
Intel Core i5-5350U CPU 1.80GHz (Broadwell), 1 CPU, 4 logical and 2 physical cores
.NET SDK=6.0.401
  [Host]     : .NET 6.0.9 (6.0.922.41905), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 6.0.9 (6.0.922.41905), X64 RyuJIT AVX2


```
|                 Method | Length1 | Length2 | MultiParallelLevel | AddParallelLevel | DensityLevel |        Mean |     Error |    StdDev |      Median | Ratio | RatioSD |
|----------------------- |-------- |-------- |------------------- |----------------- |------------- |------------:|----------:|----------:|------------:|------:|--------:|
| **BaselineMultiplication** |    **2500** |    **2500** |                  **1** |                **0** |            **1** |  **1,662.5 ms** |  **30.69 ms** |  **70.52 ms** |  **1,649.7 ms** |  **1.00** |    **0.00** |
| ParallelMultiplication |    2500 |    2500 |                  1 |                0 |            1 |  1,674.9 ms |  31.35 ms |  30.79 ms |  1,661.1 ms |  0.98 |    0.04 |
|                        |         |         |                    |                  |              |             |           |           |             |       |         |
| **BaselineMultiplication** |    **2500** |    **2500** |                  **1** |                **0** |           **10** |    **535.3 ms** |   **4.95 ms** |   **4.13 ms** |    **535.5 ms** |  **1.00** |    **0.00** |
| ParallelMultiplication |    2500 |    2500 |                  1 |                0 |           10 |    533.8 ms |  10.63 ms |  11.81 ms |    531.4 ms |  1.00 |    0.03 |
|                        |         |         |                    |                  |              |             |           |           |             |       |         |
| **BaselineMultiplication** |    **2500** |    **2500** |                  **1** |                **1** |            **1** |  **1,648.8 ms** |  **24.96 ms** |  **20.84 ms** |  **1,646.5 ms** |  **1.00** |    **0.00** |
| ParallelMultiplication |    2500 |    2500 |                  1 |                1 |            1 | 24,096.0 ms | 226.85 ms | 177.11 ms | 24,074.0 ms | 14.61 |    0.19 |
|                        |         |         |                    |                  |              |             |           |           |             |       |         |
| **BaselineMultiplication** |    **2500** |    **2500** |                  **1** |                **1** |           **10** |    **858.3 ms** |  **71.77 ms** | **210.50 ms** |    **816.6 ms** |  **1.00** |    **0.00** |
| ParallelMultiplication |    2500 |    2500 |                  1 |                1 |           10 | 12,102.0 ms | 314.86 ms | 888.06 ms | 11,733.0 ms | 14.63 |    3.39 |
|                        |         |         |                    |                  |              |             |           |           |             |       |         |
| **BaselineMultiplication** |    **2500** |    **2500** |                  **2** |                **0** |            **1** |  **1,651.5 ms** |  **26.81 ms** |  **39.30 ms** |  **1,648.2 ms** |  **1.00** |    **0.00** |
| ParallelMultiplication |    2500 |    2500 |                  2 |                0 |            1 |  1,609.8 ms |  30.86 ms |  37.90 ms |  1,601.1 ms |  0.97 |    0.04 |
|                        |         |         |                    |                  |              |             |           |           |             |       |         |
| **BaselineMultiplication** |    **2500** |    **2500** |                  **2** |                **0** |           **10** |    **533.0 ms** |   **9.72 ms** |   **8.61 ms** |    **532.5 ms** |  **1.00** |    **0.00** |
| ParallelMultiplication |    2500 |    2500 |                  2 |                0 |           10 |    486.6 ms |   9.64 ms |  10.72 ms |    485.5 ms |  0.91 |    0.03 |
|                        |         |         |                    |                  |              |             |           |           |             |       |         |
| **BaselineMultiplication** |    **2500** |    **2500** |                  **2** |                **1** |            **1** |  **1,657.0 ms** |  **32.85 ms** |  **40.34 ms** |  **1,644.7 ms** |  **1.00** |    **0.00** |
| ParallelMultiplication |    2500 |    2500 |                  2 |                1 |            1 | 16,202.9 ms | 321.28 ms | 641.62 ms | 16,070.3 ms |  9.84 |    0.51 |
|                        |         |         |                    |                  |              |             |           |           |             |       |         |
| **BaselineMultiplication** |    **2500** |    **2500** |                  **2** |                **1** |           **10** |    **546.1 ms** |   **9.27 ms** |  **12.69 ms** |    **544.1 ms** |  **1.00** |    **0.00** |
| ParallelMultiplication |    2500 |    2500 |                  2 |                1 |           10 |  7,693.1 ms | 148.08 ms | 145.44 ms |  7,679.1 ms | 14.08 |    0.45 |
