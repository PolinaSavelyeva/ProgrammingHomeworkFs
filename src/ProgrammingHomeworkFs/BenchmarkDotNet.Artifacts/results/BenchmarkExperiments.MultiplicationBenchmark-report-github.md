``` ini

BenchmarkDotNet=v0.13.4, OS=macOS Monterey 12.5 (21G72) [Darwin 21.6.0]
Intel Core i5-5350U CPU 1.80GHz (Broadwell), 1 CPU, 4 logical and 2 physical cores
.NET SDK=6.0.401
  [Host]     : .NET 6.0.9 (6.0.922.41905), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 6.0.9 (6.0.922.41905), X64 RyuJIT AVX2


```
|                 Method | Length | Level |    Mean |    Error |   StdDev | Ratio | RatioSD |
|----------------------- |------- |------ |--------:|---------:|---------:|------:|--------:|
| **BaselineMultiplication** |   **3000** |     **2** | **2.078 s** | **0.0390 s** | **0.0365 s** |  **1.00** |    **0.00** |
| ParallelMultiplication |   3000 |     2 | 1.993 s | 0.0290 s | 0.0257 s |  0.96 |    0.01 |
|                        |        |       |         |          |          |       |         |
| **BaselineMultiplication** |   **3000** |     **4** | **2.112 s** | **0.0411 s** | **0.0615 s** |  **1.00** |    **0.00** |
| ParallelMultiplication |   3000 |     4 | 3.001 s | 0.0568 s | 0.0503 s |  1.42 |    0.06 |
