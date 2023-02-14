``` ini

BenchmarkDotNet=v0.13.4, OS=macOS Monterey 12.5 (21G72) [Darwin 21.6.0]
Intel Core i5-5350U CPU 1.80GHz (Broadwell), 1 CPU, 4 logical and 2 physical cores
.NET SDK=6.0.401
  [Host]     : .NET 6.0.9 (6.0.922.41905), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 6.0.9 (6.0.922.41905), X64 RyuJIT AVX2


```
|           Method |  Length | Level |    Mean |    Error |   StdDev | Ratio | RatioSD |
|----------------- |-------- |------ |--------:|---------:|---------:|------:|--------:|
| **BaselineAddition** | **2500000** |     **2** | **1.207 s** | **0.0238 s** | **0.0255 s** |  **1.00** |    **0.00** |
| ParallelAddition | 2500000 |     2 | 1.147 s | 0.0229 s | 0.0289 s |  0.96 |    0.03 |
|                  |         |       |         |          |          |       |         |
| **BaselineAddition** | **2500000** |     **4** | **1.191 s** | **0.0208 s** | **0.0204 s** |  **1.00** |    **0.00** |
| ParallelAddition | 2500000 |     4 | 1.201 s | 0.0204 s | 0.0181 s |  1.01 |    0.03 |
