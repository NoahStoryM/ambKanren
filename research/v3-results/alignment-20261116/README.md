# 关联表对齐后的性能测量（#24）

本目录是新的结果，旧数据保持原样。参照库与依赖均未修改。源码逐定义差异见 [ALIGNMENT.md](ALIGNMENT.md)。

## 环境与方法

- 2026-11-16，Racket 9.3 CS，Linux 6.18.57 x86-64／glibc 2.41；Intel Xeon Platinum 8573C，9 个可见 vCPU，affinity 0–8。未锁频，未固定到单核；容器和宿主负载会影响耗时。
- ambKanren 基于 #23 合并后的 `ece8b5c` 加本 PR 补丁；amb `1223459`、miniKanren-typed `c197a5c`。`environment.json` 保存链接安装路径、源码补丁和被测模块的 SHA-256。
- 所有 Racket 命令均使用 `LC_ALL=C.UTF-8`。先 `raco make` 编译四个入口和测量脚本，再串行运行，测量时没有并行启动其他 Racket 编译／测试。
- TRS1 顺序：流、amb、流、amb、流、amb；TRS2 随后同样运行。每个样本都是新的进程，每组各三次，未删去任何正式样本，没有根据结果追加重试。
- `time-apply` 围住已编译测试模块的 dynamic-require：不计进程启动和预编译，计模块加载、断言框架与测试本身。计时前完整 GC，两边方法相同。
- 累计分配是计时前后 `current-memory-use` 的 cumulative 差，单位字节；表中 GB = 10^9 字节，不是峰值内存。计时的 CPU、real、GC 均为毫秒原值。
- 每个单元格为中位数（最小值–最大值）；比值是两边 CPU 中位数之比。`samples.jsonl` 保存全部 stdout、stderr、断言数、精确命令与原始数字，`summary.json` 保存汇总。

## 同一组书中断言

| 版本 | ambKanren 入口 | 流实现入口 | 共享的 `.rktl` | 每个样本断言数 | 顺序例外 |
| --- | --- | --- | --- | ---: | --- |
| TRS1 | `miniKanren/amb/trs1/tests/main.rkt` | `tools/native-trs1.rkt` 的 test 子模块 | miniKanren-typed 的第 1–10 章 | 254 | 无 |
| TRS2 | `miniKanren/amb/trs2/tests/main.rkt` | `tools/native-trs2.rkt` 的 test 子模块 | miniKanren-typed 的第 1–9 章（含 arith） | 285 | 无 |

第 9 章直接 include 原文件，包括其局部定义的 `project`。两边保留原文件已有的 `#;` 注释，没有修改断言或新增跳过；并未把它们改成答案袋比较。本库全部 tests 目录加两份原生入口共 1353 个断言通过，输出见 `validation.txt`。

## 耗时

| 版本 | 实现 | CPU 秒 | real 秒 | GC 秒 |
| --- | --- | ---: | ---: | ---: |
| TRS1 | 流 | 27.953 (26.978–29.294) | 27.999 (27.119–29.338) | 0.681 (0.618–1.176) |
| TRS1 | amb | 54.009 (53.314–62.942) | 54.184 (53.612–63.110) | 2.468 (2.275–2.661) |
| TRS2 | 流 | 22.124 (20.432–22.813) | 22.155 (20.463–22.847) | 4.781 (3.924–5.617) |
| TRS2 | amb | 31.438 (29.219–33.950) | 31.680 (29.290–33.999) | 5.461 (5.203–6.955) |

## 累计分配与 CPU 比值

| 版本 | 流 GB | amb GB | CPU 比值（amb／流） |
| --- | ---: | ---: | ---: |
| TRS1 | 2.717815 (2.717814–2.717816) | 26.817041 (26.816964–26.817112) | 1.93 |
| TRS2 | 6.104530 (6.104518–6.104530) | 19.389179 (19.389148–19.389869) | 1.42 |

## 解读

对齐代换和辅助代码后，本次 amb 在两版上都更慢、分配更多。差距不再混有哈希表与关联表、ground-data 捷径、match 与 cond 等非搜索选择。TRS1 每个非末尾合取通过局部搜索绑定，TRS2 的暂停与合取也通过 amb 的生成器／merger 处理；捕获与恢复续延、生成器结构等会产生额外工作。分配差距与这种机制一致。

本轮没有做 profiler、效应计数器或原语消融，不能从这些整模块数字断言“CPU 时间主要花在捕获／恢复续延”。历史 #18 的单次开销表属于旧代码，只作为机制背景，不套作当前原语测量。这里仍保留了模块组织与编译器响应的差别，加载与断言也包含在数字中。

没有在本次环境中另跑哈希版基线，因此不把与旧手册数字的变化全部归因于代换；机器、覆盖范围（TRS1 新增第 9 章）与源码都不同。三次样本只说明这次运行，不能当作通用性能保证。

## 复现

在同时包含 ambKanren、miniKanren-typed、amb 三个 clone 的父目录执行，先按 issue #22 链接安装并核对 `raco pkg show -l`。另存输出，避免覆盖本目录归档：

```sh
export LC_ALL=C.UTF-8
raco make ambKanren/miniKanren/amb/trs1/tests/main.rkt \
  ambKanren/miniKanren/amb/trs2/tests/main.rkt \
  ambKanren/tools/native-trs1.rkt ambKanren/tools/native-trs2.rkt \
  ambKanren/tools/measure-aligned.rkt
python ambKanren/tools/measure-aligned.py --racket "$(command -v racket)" \
  --reference miniKanren-typed --output /tmp/ambkanren-aligned-repeat --repeats 3
racket ambKanren/tools/alignment-diff.rkt miniKanren-typed > /tmp/aligned-forms.json
```

## 手册验证

`raco setup --pkgs amb miniKanren-typed ambKanren` 退出码 0，无未定义交叉引用或警告；完整日志追加在 `validation.txt`。
